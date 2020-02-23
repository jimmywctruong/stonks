const path = require('path');
const webpack = require('webpack');
const PnpWebpackPlugin = require('pnp-webpack-plugin');

module.exports = {
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: [/node_modules/],
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/node_modules/, /elm-stuff/],

                use: [
                    {loader: 'elm-hot-webpack-loader'},
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname,
                            debug: false
                        }
                    }
                ]
            }
        ]
    },

    resolve: {
        plugins: [
            PnpWebpackPlugin,
        ],
    },

    resolveLoader: {
        plugins: [
            PnpWebpackPlugin.moduleLoader(module),
        ],
    },

    plugins: [
        new webpack.HotModuleReplacementPlugin()
    ],

    mode: 'development',

    devServer: {
        inline: true,
        hot: true,
        stats: 'errors-only'
    }
};


