$(function () {
  Shiny.addCustomMessageHandler(
    'move-element', (m) => {
      $(m.to).append($(m.from));
    }
  )
})
