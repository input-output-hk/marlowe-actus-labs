const path = require('path');
const webpack = require('webpack');

module.exports = (env, argv) => {
  const develMode = argv.mode == "development";

  if (develMode) {
    console.log("Building development version");
    if(!process.env.MARLOWE_WEB_SERVER_URL) {
      console.log("You should setup MARLOWE_WEBSERVER_URL before starting the devel server - for example: $ export MARLOWE_WEB_SERVER_URL='http://127.0.0.1:479001'");
    } else {
      console.log("Checking MARLOWE_WEB_SERVER_URL: " + process.env.MARLOWE_WEB_SERVER_URL);
      fetch(process.env.MARLOWE_WEB_SERVER_URL + "/contracts").catch(function (error) {
        throw ("You should start the marlowe-web-server or change the MARLOWE_WEBSERVER_URL environment variable value.");
      }).then(function (response) {
        console.log("marlowe-web-server is running");
      });
    }
  }

  const webServerUrl = process.env.MARLOWE_WEB_SERVER_URL;

  return {
    entry: {
       app: './src/frontend.js',
    },
    devtool: 'inline-source-map',
    devServer: {
      static: './public',
      hot: true,
      port: 8080
    },
    plugins: [
      new webpack.EnvironmentPlugin({
        MARLOWE_WEB_SERVER_URL: process.env.MARLOWE_WEB_SERVER_URL,
        DEVEL_MODE: develMode,
      }),
    ],
    output: {
      filename: 'bundle.js',
      path: path.resolve(__dirname, 'public'),
    },
  };
};
