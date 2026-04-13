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
      // Don't move elements into inactive workspace docks
      var $wsDock = $(m.to).closest('.blockr-ws-dock');
      if ($wsDock.length > 0 && !$wsDock.hasClass('blockr-ws-dock-active')) {
        return;
      }
      var $moved = $(m.from);
      $(m.to).append($moved);
      // Notify Shiny to re-evaluate output hidden state for all bound
      // outputs. The moved card's output is now inside a visible container
      // (dock panel), so clientData `_hidden` should flip to FALSE and
      // blockr.core's lazy-eval will resume the block.
      $moved.trigger('shown.sendOutputHiddenState');
    }
  )
})
