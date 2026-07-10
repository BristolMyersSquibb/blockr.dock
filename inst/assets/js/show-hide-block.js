$(function () {
  Shiny.addCustomMessageHandler(
    'move-element', (m) => {
      var $to = $(m.to);
      // Don't move elements into inactive view docks. Checked first so an
      // off-screen view whose card isn't built yet is skipped silently rather
      // than warning about a missing 'from'.
      var $wsDock = $to.closest('.blockr-view-dock');
      if ($wsDock.length > 0 && !$wsDock.hasClass('blockr-view-dock-active')) {
        return;
      }
      if ($(m.from).length === 0) {
        console.warn(`move-element: 'from' selector ${m.from} not found in DOM`);
        return;
      }
      if ($to.length === 0) {
        console.warn(`move-element: 'to' selector ${m.to} not found in DOM`);
        return;
      }
      $to.append($(m.from));
    }
  )
})
