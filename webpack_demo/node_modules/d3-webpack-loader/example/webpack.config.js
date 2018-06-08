// Webpack config for development
var path = require('path');

module.exports = {
  devtool: 'eval',
  entry: {
    main: [
      './index.js'
    ]
  },
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'example.js'
  }
};
