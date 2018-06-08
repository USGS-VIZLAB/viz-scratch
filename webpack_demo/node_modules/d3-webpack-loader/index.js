var path = require('path');
var fs = require('fs');
var loaderUtils = require('loader-utils');
var parallel = require('async').parallel;

/**
 * Helper function that checks if a directory exists at the specified absolute path.
 *
 * @param {String} dirPath Absolute path to directory to check
 * @param {Function} callback Callback resolves to true if the directory exists, false otherwise
 * @return {void}
 */
function directoryExists(dirPath, callback) {
  return fs.lstat(dirPath, function (err, stats) {
    if (err) {
      return callback(null, false);
    }

    return callback(null, stats.isDirectory());
  });
}

/**
 * Helper to get a list of d3 modules in the given directoryToScan
 */
function findD3ModulesInDirectory(directoryToScan, callback) {
  directoryExists(directoryToScan, function (_, isDirectory) {
    if (isDirectory) {
      fs.readdir(directoryToScan, function (err, keys) {
        if (err) {
          callback(err);
          return;
        }

        // Filter to only include subdirectories that start with "d3-"
        callback(null, keys.filter(function (key) { return /^d3-/.test(key); }));
      });
    } else {
      callback(null, []);
    }
  });
}

/**
 * Helper to get the directories to scan for d3 modules based on the
 * configuration of webpack config `resolve`
 */
function getModuleDirectories(webpackContext, resolveOptions) {
  var directories = [].concat(
    resolveOptions.root,
    resolveOptions.modulesDirectories.map(function (dir) {
      return path.join(webpackContext, dir);
    }),
    resolveOptions.fallback
  );

  return directories.filter(function (item, i, arr) {
    // remove null entries and make sure the list is unique
    return item != null && i === arr.indexOf(item);
  });
}

/**
 * The main d3 loader function.
 * Intended usage:
 * import d3 from 'd3!';
 *
 * or
 *
 * var d3 = require('d3!');
 *
 * Can be configured in webpack.config via the key `d3Loader`.
 * Available configuration parameters:
 *   - exclude: An array of d3 module names to exclude. (e.g., ['d3-shape', 'd3-array'])
 */
module.exports = function d3Loader() {
  var done = this.async();

  var directoriesToScan = getModuleDirectories(this.options.context, this.options.resolve);
  var modulesToExclude = ['d3-webpack-loader'];
  var config = loaderUtils.getLoaderConfig(this, 'd3Loader');
  // read in extra exclusions from config
  if (config.exclude) {
    modulesToExclude = modulesToExclude.concat(config.exclude);
  }

  // mark this loader as cacheable
  this.cacheable();

  // scan through all the module directories to find the list of available d3 plugins
  new Promise(function (resolve, reject) {
    parallel(directoriesToScan.map(function (directoryToScan) {
      return function (callback) {
        findD3ModulesInDirectory(directoryToScan, callback);
      };
    }), function (err, results) {
      var allD3Modules;
      if (err) {
        reject(err);
      }

      // combine results and filter out excluded modules and repeated
      allD3Modules = [].concat.apply([], results)
        .filter(function (moduleDirectoryName, i, arr) {
          return modulesToExclude.indexOf(moduleDirectoryName) === -1 &&
            arr.indexOf(moduleDirectoryName) === i;
        });

      resolve(allD3Modules);
    });
  })
  // generate the code for bundling all the d3 modules together
  .then(function (moduleDirectoryNames) {
    var output = '// d3-webpack-loader output:\n';
    var modules = [];
    var assign;

    // add in UMD
    output = "(function (global, factory) {\n typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :\n typeof define === 'function' && define.amd ? define(['exports'], factory) :\n (factory((global.d3 = global.d3 || {})));\n }(this, (function (exports) { 'use strict';";

    // add a line importing each of the d3 libraries
    moduleDirectoryNames.forEach(function (key) {
      var d3ModuleName = key.substring(3); // remove d3-
      modules.push(d3ModuleName.replace(/-/g, ''));
      output += 'var ' + d3ModuleName.replace(/-/g, '') + ' = require("d3-' + d3ModuleName + '");\n';
    });

    // inline object assign
    assign = 'var assign = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; }';
    output += assign + '\n';

    // add in exports for each of the modules
    if (modules.length) {
      output += 'assign(exports,\n  ' + modules.join(',\n  ') + '\n);';
    }

    // close UMD
    output += "Object.defineProperty(exports, '__esModule', { value: true });\n })));";

    return output;
  })
  .then(function (output) {
    done(null, output);
  }, done);
};
