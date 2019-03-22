const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const {
    BundleAnalyzerPlugin
} = require('webpack-bundle-analyzer');

module.exports = _env => {
    const env = _env || {};

    return {
        entry: {
            fixture: path.resolve('./fixture.js'),
            app: [
                'normalize.css',
                path.resolve('./src/styles.pcss'),
                path.resolve('./src/app.js')
            ]
        },

        output: {
            path: path.resolve('./build'),
            filename: '_[name]-[hash].js'
        },

        module: {
            noParse: /\.elm$/,
            rules: env.prod ? [
                {
                    test: /\.elm$/,
                    exclude: [
                        /elm-stuff/,
                        /node_modules/
                    ],
                    use: {
                        loader: 'elm-webpack-loader',
                        options: {
                            optimize: true
                        }
                    }
                },
                {
                    test: /\.p?css$/,
                    use: [
                        MiniCssExtractPlugin.loader,
                        'css-loader',
                        'postcss-loader'
                    ]
                }
            ] : [
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
                            'style-loader',
                            {
                                loader: 'css-loader',
                                options: {
                                    importLoaders: 1
                                }
                            },
                            'postcss-loader'
                        ]
                    }
                ]
        },

        plugins: [
            new HtmlWebpackPlugin({
                template: path.resolve('./src/index.html'),
                inject: 'body',
                chunks: [ 'fixture', 'app' ],
                chunksSortMode: 'manual',
                minify: env.prod && {
                    caseSensitive: true,
                    collapseBooleanAttributes: true,
                    collapseInlineTagWhitespace: true,
                    collapseWhitespace: true,
                    quoteCharacter: '"',
                    removeAttributeQuotes: true,
                    removeComments: true,
                    removeEmptyAttributes: true,
                    useShortDoctype: true
                }
            })
        ].concat(env.prod ? [
            new MiniCssExtractPlugin({
                filename: "_[name]-[hash].css",
                chunkFilename: "[id].css"
            }),
            new BundleAnalyzerPlugin({
                analyzerMode: 'static',
                reportFilename: 'bundle-report.html'
            })
        ] : []),

        mode: env.prod ? 'production' : 'development',

        optimization: env.prod && {
            minimizer: [
                new UglifyJsPlugin({
                    cache: true,
                    parallel: true,
                    sourceMap: false
                }),
                new OptimizeCSSAssetsPlugin({})
            ]
        }
    };
};
