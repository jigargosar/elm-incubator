const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

function createConfig(outputPublicPath, entry, template) {
  {
    const isProd = false
    const isElmDebuggerDisabled = false

    return {
      mode: 'development',
      entry: entry,
      output: {
        publicPath: outputPublicPath,
        path: __dirname + '/dist/' + outputPublicPath,
      },
      resolve: {
        extensions: ['.js', '.elm'],
      },
      plugins: [
        new CleanWebpackPlugin(),
        new HtmlWebpackPlugin({
          template: template,
        }),
      ],

      module: {
        rules: [
          {
            include: /\.elm/,
            use: [
              //'elm-hot-webpack-loader',
              {
                loader: 'elm-webpack-loader',
                options: {
                  optimize: isProd,
                  debug: !isProd && !isElmDebuggerDisabled,
                },
              },
            ],
          },
          {
            include: /\.css/,
            use: ['style-loader', 'css-loader'],
          },
        ],
      },
      // stats: {
      //   children: false,
      //   modules: false,
      // },
      devtool: isProd ? 'source-map' : 'eval-source-map',
      devServer: {
        // https://v4.webpack.js.org/configuration/dev-server/
        // noInfo: true,
        // proxy: {
        //   '/api': {
        //     target: 'http://localhost:3000',
        //     pathRewrite: { '^/api': '' },
        //   },
        // },
        historyApiFallback: false,
        hot: false,
        overlay: true,
      },
    }
  }
}

module.exports = (_, config) => {
  const c1 = createConfig(
    '/app1',
    './src/ElmGoogleSearch/index.js',
    './src/ElmGoogleSearch/index.html',
  )
  const c2 = createConfig('/app2', './src/index.js', './src/index.html')
  return [c1, c2]
}
