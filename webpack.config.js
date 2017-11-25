var path = require("path");
var webpack = require("webpack");
var merge = require("webpack-merge");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var CopyWebpackPlugin = require("copy-webpack-plugin");
var entryPath = path.join(__dirname, "src/static/index.js");
var outputPath = path.join(__dirname, "dist");
var entry = {
    "lsf-quizz": entryPath,
};

console.log("WEBPACK GO!");

// determine build env
var TARGET_ENV =
    process.env.npm_lifecycle_event === "build" ? "production" : "development";
//var outputFilename = TARGET_ENV === 'production' ? '[name]-[hash].js' : '[name].js'
var outputFilename = "[name].js";

// common webpack config
var commonConfig = {
    output: {
        path: outputPath,
        filename: outputFilename,
        // publicPath: '/'
    },

    resolve: {
        extensions: [".js", ".elm"],
    },

    module: {
        // noParse: /\.elm$/,
        rules: [
            {
                test: /\.(eot|ttf|woff|woff2|svg)$/,
                use: [{ loader: "file-loader" }],
            },
        ],
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: "src/static/index.html",
            inject: "body",
            filename: "index.html",
        }),
    ],
};

// additional webpack settings for local env (when invoked by 'npm start')
if (TARGET_ENV === "development") {
    console.log("Serving locally...");

    module.exports = merge(commonConfig, {
        entry: entry,

        devServer: {
            // serve index.html in place of 404 responses
            historyApiFallback: true,
            contentBase: "./src",
        },

        module: {
            rules: [
                {
                    test: /\.(png|jpg|gif)$/,
                    use: [
                        {
                            loader: "url-loader",
                            options: {
                                limit: 8192,
                            },
                        },
                    ],
                },
                {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: [
                        {
                            loader: "elm-hot-loader",
                        },
                        {
                            loader: require.resolve("./loader.js"),
                        },
                        {
                            loader: "elm-webpack-loader",
                            options: {
                                verbose: true,
                                warn: true,
                                debug: true,
                            },
                        },
                    ],
                },
            ],
        },

        plugins: [new webpack.NamedModulesPlugin()],
    });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if (TARGET_ENV === "production") {
    console.log("Building for prod...");

    module.exports = merge(commonConfig, {
        entry: entryPath,

        module: {
            rules: [
                {
                    test: /\.(png|jpg|gif)$/,
                    use: [
                        {
                            loader: "url-loader",
                            options: {
                                limit: 8192,
                            },
                        },
                    ],
                },
                {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: [
                        {
                            loader: require.resolve("./loader.js"),
                        },
                        { loader: "elm-webpack-loader" },
                    ],
                },
            ],
        },

        plugins: [
            new CopyWebpackPlugin([
                {
                    from: "src/static/img/",
                    to: "static/img/",
                },
                {
                    from: "src/favicon.ico",
                },
            ]),

            new webpack.optimize.UglifyJsPlugin({
                minimize: true,
                compressor: { warnings: false },
            }),
        ],
    });
}
