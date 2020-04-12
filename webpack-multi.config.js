const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

const path = require('path')

function createConfig(contextDir, outputPublicPath, entry, template) {
  {
    const isProd = false
    const isElmDebuggerDisabled = false

    const context = path.resolve(__dirname, contextDir)
    console.log(context)
    return {
      context: context,
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
      stats: {
        // children: true,
        modules: false,
      },
      devtool: isProd ? 'source-map' : 'eval-source-map',
      devServer: {
        historyApiFallback: false,
        hot: false,
        overlay: true,
      },
    }
  }
}

module.exports = (_, config) => {
  const c1 = createConfig(
    '.',
    '/app1',
    './src/ElmGoogleSearch/index.js',
    './src/ElmGoogleSearch/index.html',
  )
  const c2 = createConfig(
    '.',
    '/app2',
    './src/index.js',
    './src/index.html',
  )
  const c3 = createConfig(
    './modules/playing-with-elm-style-anim',
    '/playing-with-elm-style-anim',
    './src/index.js',
    './src/index.html',
  )
  return [c1, c2, c3]
}
