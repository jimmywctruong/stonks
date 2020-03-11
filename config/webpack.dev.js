const merge = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge(common, {
    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    { loader: 'elm-hot-webpack-loader' },
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
});

