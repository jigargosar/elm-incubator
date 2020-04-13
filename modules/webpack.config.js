const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

const path = require('path')

const globby = require('globby')

module.exports = () => {
  const modules = globby.sync(['*'], {
    gitignore: true,
    onlyDirectories: true,
    cwd: __dirname,
  })
  console.log(modules)
  const modulesConfig = modules.map(moduleContext => {
    return createConfig(
      moduleContext,
      '/' + moduleContext,
      './src/index.js',
      './src/index.html',
    )
  })
  const indexConfig = createConfig('.', '/', './index.js', 'index.html')
  return [indexConfig, ...modulesConfig]
}

function createConfig(contextDir, outputPublicPath, entry, template) {
  {
    // console.log('arguments', arguments)
    const isProd = false
    const isElmDebuggerDisabled = false

    const context = path.resolve(__dirname, contextDir)
    const outputPath = path.join(__dirname, 'dist', outputPublicPath)
    console.log('context', context)
    console.log('outputPath', outputPath)
    console.log('outputPublicPath', outputPublicPath)

    return {
      context: context,
      mode: 'development',
      entry: entry,
      output: {
        publicPath: outputPublicPath,
        path: outputPath,
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
      // stats: {
      //   // children: true,
      //   modules: false,
      // },
      stats: 'errors-only',
      devtool: isProd ? 'source-map' : 'eval-source-map',
      devServer: {
        historyApiFallback: false,
        hot: false,
        overlay: true,
      },
    }
  }
}
