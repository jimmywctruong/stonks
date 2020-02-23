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
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],

                use: [
                    {loader: 'elm-hot-webpack-loader'},
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname,
                            debug: true,
                        }
                    }
                ]
            }
        ]
    },

    mode: 'development',

    devServer: {
        inline: true,
        hot: true,
        stats: 'errors-only'
    }
};


