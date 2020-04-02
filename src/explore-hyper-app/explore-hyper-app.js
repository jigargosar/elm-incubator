import { h, app } from 'hyperapp'
import 'tachyons'

function tagData(tagName, dataObject) {
  return { t: tagName, d: dataObject }
}

function mapTagData(func, td) {
  return { t: td.t, d: func(td.d) }
}

function init() {
  return tagData('model', { ct: 0 })
}

app({
  init: init(),
  view: state => {
    return h('div', {}, [
      h('h1', {}, state.d.ct),
      h(
        'button',
        {
          onclick: model => {
            return mapTagData(function({ ct }) {
              return { ct: ct - 1 }
            }, model)
          },
        },
        'subtract',
      ),
      h(
        'button',
        {
          onclick: function(model) {
            const {
              d: { ct },
            } = model
            return { ...model, d: { ct: ct + 1 } }
          },
        },
        'add',
      ),
    ])
  },
  node: document.getElementById('app'),
})
