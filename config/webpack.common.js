const path = require('path');
const webpack = require('webpack');

module.exports = {
    entry: './src/index.js',
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: [/node_modules/],
                loader: 'file-loader?name=[name].[ext]'
            }
        ]
    },
};
