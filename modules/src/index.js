require('tachyons')

console.log(process.env.MODULE_NAMES)
console.dir(process.env)

require('./Main.elm')['Elm']['Main']['init']({
  node: document.getElementById('elm'),
  flags: { modules: process.env.MODULE_NAMES },
})
