import { h, app } from 'hyperapp'
import 'tachyons'


function tagged(tagName, dataObject) {
  return {t: tagName, d: dataObject}
}

function init(){
  return tagged('model', {ct:0})
}

app({
  init: init(),
  view: state => {
    return h('div', {}, [
      h('h1', {}, state),
      h(
        'button',
        {
          onclick: model => {
            return {
              ...model,
              d: { ...model.d, ct: model.d.ct - 1 },
            }
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
