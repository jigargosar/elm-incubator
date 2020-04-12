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
        path: path.resolve(__dirname, '/dist/', outputPublicPath),
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
                  cwd: context,
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

const globby = require('globby')

module.exports = (_, config) => {
  const modules = globby.sync(['modules/*'], { onlyDirectories: true })
  console.log(modules)
  return modules.map(moduleContext => {
    return createConfig(
      moduleContext,
      moduleContext.split('/')[1],
      './src/index.js',
      './src/index.html',
    )
  })
}
