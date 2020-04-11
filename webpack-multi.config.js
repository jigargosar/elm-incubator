const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

function createConfig(entry, template, config) {
  {
    const isProd = config.mode === 'production'
    const isElmDebuggerDisabled = false

    return {
      entry: entry,
      output: {
        publicPath: '/',
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
        children: false,
        modules: false,
      },
      devtool: isProd ? 'source-map' : 'eval-source-map',
      devServer: {
        // https://v4.webpack.js.org/configuration/dev-server/
        // noInfo: true,
        proxy: {
          '/api': {
            target: 'http://localhost:3000',
            pathRewrite: { '^/api': '' },
          },
        },
        historyApiFallback: true,
        hot: true,
        overlay: true,
      },
    }
  }
}

module.exports = (_, config) => {
  const c1 = createConfig('./src/index.js', './src/index.html', config)
  return [c1]
}
