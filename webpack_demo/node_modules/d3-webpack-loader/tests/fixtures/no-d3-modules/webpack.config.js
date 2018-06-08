// Webpack config for development
var path = require('path');

module.exports = {
  context: __dirname,
  entry: {
    main: [
      './index.js'
    ]
  },
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'output.js',
    libraryTarget: 'umd'
  }
};
