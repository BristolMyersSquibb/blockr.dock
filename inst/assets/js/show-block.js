$(function () {
  // Moves whose target panel is not in the DOM yet. dockView mounts a group's
  // background tabs lazily (onlyWhenVisible), so a card's move-element can
  // arrive before its panel's content element exists; dropping it then strands
  // the card in the offcanvas and the tab stays blank. Stash such a move and
  // replay it when dockView reports the panel active -- the same client tick its
  // content element mounts, before paint.
  var pending = [];

  // A stashed move is dead once its card has left the DOM: the block was removed
  // before its target panel ever mounted. Drop those so `pending` holds only
  // moves that can still land, rather than growing with every removed block.
  function alive(m) {
    return $(m.from).length > 0;
  }

  function applyMove(m) {
    var $to = $(m.to);

    // Target not mounted yet -- caller should stash and replay on activation.
    if ($to.length === 0) {
      return false;
    }

    // Don't move into an inactive view dock: an off-screen view whose card
    // isn't built is skipped silently rather than warning about a missing
    // 'from'.
    var $wsDock = $to.closest('.blockr-view-dock');
    if ($wsDock.length > 0 && !$wsDock.hasClass('blockr-view-dock-active')) {
      return true;
    }

    if ($(m.from).length === 0) {
      console.warn(`move-element: 'from' selector ${m.from} not found in DOM`);
      return true;
    }

    $to.append($(m.from));
    return true;
  }

  Shiny.addCustomMessageHandler(
    'move-element', (m) => {
      pending = pending.filter(alive);
      if (!applyMove(m)) {
        pending.push(m);
      }
    }
  )

  // dockViewR re-broadcasts panel activation as this bubbling DOM event; the
  // panel's content element exists by the time it fires, so replay any stashed
  // move whose target has now appeared, dropping any whose block was removed.
  document.addEventListener('dockview:active-panel', () => {
    pending = pending.filter((m) => alive(m) && !applyMove(m));
  })
})
