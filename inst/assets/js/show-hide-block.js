Shiny.addCustomMessageHandler(
  'hide-block',
  (m) => {
    $(m.offcanvas).find('.offcanvas-body').append($(m.block_id).find('.card'));
  }
)

Shiny.addCustomMessageHandler(
  'show-block', (m) => {
    $(m.panel_id).append($(m.block_id));
  }
)
