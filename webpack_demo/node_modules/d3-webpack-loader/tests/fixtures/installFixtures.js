// From: https://github.com/reactjs/redux/blob/master/examples/buildAll.js
/**
 * Runs an ordered set of commands within each of the build directories.
 * In this case, npm installs in each of the test fixtures
 */

var fs = require('fs');
var path = require('path');
var spawnSync = require('child_process').spawnSync;

var exampleDirs = fs.readdirSync(__dirname).filter(function (file) {
  return fs.statSync(path.join(__dirname, file)).isDirectory();
});

// Ordering is important here. `npm install` must come first.
var cmdArgs = [
  { cmd: 'npm', args: ['uninstall', 'd3-webpack-loader'] },
  { cmd: 'npm', args: ['install'] }
];

exampleDirs.forEach(function (dir) {
  cmdArgs.forEach(function (cmdArg) {
    // declare opts in this scope to avoid https://github.com/joyent/node/issues/9158
    var opts = {
      cwd: path.join(__dirname, dir),
      stdio: 'inherit'
    };
    var result = {};
    if (process.platform === 'win32') {
      result = spawnSync(cmdArg.cmd + '.cmd', cmdArg.args, opts);
    } else {
      result = spawnSync(cmdArg.cmd, cmdArg.args, opts);
    }
    if (result.status !== 0) {
      throw new Error('Building examples exited with non-zero');
    }
  });
});
