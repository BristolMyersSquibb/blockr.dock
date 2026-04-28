$(function () {
  Shiny.addCustomMessageHandler(
    'move-element', (m) => {
      if ($(m.from).length === 0) {
        console.warn(`move-element: 'from' selector ${m.from} not found in DOM`);
        return;
      };
      if ($(m.to).length === 0) {
        console.warn(`move-element: 'to' selector ${m.to} not found in DOM`);
        return;
      }
      // Don't move elements into inactive view docks
      var $wsDock = $(m.to).closest('.blockr-view-dock');
      if ($wsDock.length > 0 && !$wsDock.hasClass('blockr-view-dock-active')) {
        return;
      }
      $(m.to).append($(m.from));
    }
  )
})
