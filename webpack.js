const path = require('path');

module.exports = {
  entry: {
     app: './src/frontend.js',
  },
  devtool: 'inline-source-map',
  devServer: {
    static: './public',
    hot: true,
    port: 8080
  },
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'public'),
  },
};
