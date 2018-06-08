var expect = require('chai').expect;
var path = require('path');
var webpack = require('webpack');


function fixtureCompiler(fixtureName) {
  var fixtureRoot = path.join(__dirname, 'fixtures');
  var fixturePath = path.join(fixtureRoot, fixtureName);
  var configPath = path.join(fixturePath, 'webpack.config.js');
  var webpackConfig = require(configPath);
  var compiler = webpack(webpackConfig);

  return compiler;
}

it('typical - works with require("d3!") with a couple d3 modules', function (done) {
  var compiler = fixtureCompiler('default-module-directories');

  compiler.run(function () {
    var compiledFile = path.join(compiler.options.output.path, compiler.options.output.filename);
    var d3Functions = require(compiledFile).default.d3Functions;

    // uses d3-array and d3-selection so check they have functions in there.
    expect(d3Functions).to.include('sum');
    expect(d3Functions).to.include('select');
    done();
  });
});

it('does not fail when no d3 modules installed', function (done) {
  var compiler = fixtureCompiler('no-d3-modules');

  compiler.run(function () {
    var compiledFile = path.join(compiler.options.output.path, compiler.options.output.filename);
    var d3Functions = require(compiledFile).default.d3Functions;

    expect(d3Functions).to.be.empty;
    done();
  });
});

it('looks in configured module directories', function (done) {
  var compiler = fixtureCompiler('configured-module-directories');

  compiler.run(function () {
    var compiledFile = path.join(compiler.options.output.path, compiler.options.output.filename);
    var d3Functions = require(compiledFile).default.d3Functions;

    // uses d3-array and d3-selection so check they have functions in there.
    expect(d3Functions).to.include('sum'); // from resolve.root
    expect(d3Functions).to.include('select'); // from resolve.moduleDirectories
    expect(d3Functions).to.include('nest'); // from resolve.fallbacks
    done();
  });
});
