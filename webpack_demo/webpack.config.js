const path = require('path');
const webpack = require("webpack");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

module.exports = {
	entry: ['./src/index.js', './src/input.js', './src/d3Module.js'],
	output: {
	  filename: 'bundle.js',
	  path: path.resolve(__dirname, 'dist')
	},
	optimization: {
		splitChunks: {
			cacheGroups: {
				styles: {
					name: 'main',
					test: /\.less$/,
					chunks: 'all',
					enforce: true
				}
			}
		}
	},
	module: {
	 rules: [
		 {
			 test: /\.js$/,
			 exclude: /node_modules/,
			 use:[ 'babel-loader'],
        },
		{
			test: /\.less$/,
			use: [
				MiniCssExtractPlugin.loader,
				'css-loader',
				'less-loader'
			]
		},
		{
			test: /\.(woff|woff2|eot|ttf|otf)$/,
			use: [
			  'file-loader'
			]
		}
	 ]
	},
	plugins: [
		new MiniCssExtractPlugin({
			filename: "[name].css",
		}),
		new webpack.ProvidePlugin({
            $: "jquery",
			jQuery: "jquery"
        })
	]
};