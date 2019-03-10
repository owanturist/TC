const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    entry: [
        'normalize.css',
        path.resolve('./src/styles.pcss'),
        path.resolve('./src/app.js')
    ],

    output: {
        path: path.resolve('./build'),
        filename: '[name]-[hash].js'
    },

    module: {
        noParse: /\.elm$/,
        rules: [
            {
                test: /\.elm$/,
                exclude: [
                    /elm-stuff/,
                    /node_modules/
                ],
                use: {
                    loader: 'elm-webpack-loader',
                    options: {
                        debug: true
                    }
                }
            },
            {
                test: /\.p?css$/,
                use: [
                    {
                        loader: 'style-loader'
                    },
                    {
                        loader: 'css-loader',
                        options: {
                            importLoaders: 1
                        }
                    },
                    {
                        loader: 'postcss-loader'
                    }
                ]
            }
        ]
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: path.resolve('./src/index.html'),
            inject: 'body'
        })
    ],

    mode: 'development'
};
