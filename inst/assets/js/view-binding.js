$(function () {
  // Bootstrap modal cleanup corrupts the body's inline styles: it sets
  // `padding: 0px` (shorthand) on open but only restores `padding-right`
  // on close, leaving stale padding-top/bottom/left inline values.
  // Fix: save the full style attribute before the modal opens and
  // restore it after the modal closes.
  $(document.body).on('show.bs.modal', function () {
    document.body._preModalStyle = document.body.getAttribute('style') || '';
  });
  $(document.body).on('hidden.bs.modal', function () {
    if (document.body._preModalStyle !== undefined) {
      document.body.setAttribute('style', document.body._preModalStyle);
      delete document.body._preModalStyle;
    }
  });

  var showNotification = function (message, type, duration) {
    Shiny.notifications.show({
      html: message,
      type: type || 'warning',
      duration: duration != null ? duration : 3000
    });
  };

  // Views are addressed by a stable id (`data-view-id`); the visible
  // text (`.blockr-view-item-name`) is a free-form display label. Switch,
  // remove and rename all travel by id, so a rename never re-keys.
  // While a name is being edited, the field stands in for it.
  var itemName = function ($item) {
    var $name = $item.find('.blockr-view-item-name');
    if ($name.length) return $name.text();
    return $item.find('.blockr-view-rename-input').val() || '';
  };

  var setToggleLabel = function ($el, text) {
    $el
      .closest('.blockr-view-dropdown')
      .find('.blockr-view-toggle-label')
      .text(text);
  };

  // The tab line of a board with the `view_tabs` option (view_tabs_ui()),
  // found by the nav's id; empty on a board without it. It mirrors the
  // nav's views, so every update of the nav's views updates it too.
  var tabsOf = function (el) {
    return $(document.getElementById(el.id + '-tabs'));
  };

  var tabOf = function (el, viewId) {
    return tabsOf(el).children('.blockr-view-tab[data-view-id="' + viewId + '"]');
  };

  // Scroll the line sideways, and only sideways, to the current view's tab:
  // the page itself stays where it is.
  var revealTab = function (el) {
    var line = tabsOf(el)[0];
    if (!line || line.hidden) return;
    var tab = line.querySelector('.blockr-view-tab.is-active');
    if (!tab) return;
    var l = line.getBoundingClientRect();
    var t = tab.getBoundingClientRect();
    if (t.left < l.left) line.scrollLeft -= l.left - t.left;
    else if (t.right > l.right) line.scrollLeft += t.right - l.right;
  };

  // Mark the current view: its row, the toggle's label and its tab.
  var markActive = function (el, viewId) {
    $(el).find('.blockr-view-item').removeClass('active');
    var $item = $(el)
      .find('.blockr-view-item[data-view-id="' + viewId + '"]')
      .addClass('active');
    if ($item.length) {
      setToggleLabel($(el), itemName($item));
    }
    tabsOf(el).children('.blockr-view-tab').each(function () {
      var on = this.getAttribute('data-view-id') === viewId;
      this.classList.toggle('is-active', on);
      this.setAttribute('aria-selected', on ? 'true' : 'false');
    });
    revealTab(el);
  };

  // Show or hide the tab line, and with it the check of the nav's row for it.
  var showTabs = function (el, on) {
    tabsOf(el).prop('hidden', !on);
    $(el).find('.blockr-view-tabs-toggle').attr('aria-checked', on ? 'true' : 'false');
    revealTab(el);
  };

  // The name of the view each nav's "New view" made, until the view arrives
  // (`rename_new` from the server).
  var renameNew = new WeakMap();

  var closeMenu = function ($el) {
    var toggle = $el.closest('.blockr-view-dropdown')
      .find('[data-bs-toggle="dropdown"]')[0];
    if (toggle) bootstrap.Dropdown.getOrCreateInstance(toggle).hide();
  };

  var isManaging = function ($el) {
    return $el.closest('.blockr-view-nav').hasClass('is-managing');
  };

  // In manage mode a click on a name renames it, which blockr.ui's editable
  // marker says with the text cursor and a tooltip.
  var markEditable = function ($scope, on) {
    $scope.find('.blockr-view-item-name').each(function () {
      if (on) this.setAttribute('data-blockr-editable', 'Click to rename');
      else this.removeAttribute('data-blockr-editable');
    });
  };

  var setManaging = function (el, on) {
    $(el).toggleClass('is-managing', on);
    markEditable($(el), on);
    if (!on) renameNew.delete(el);
  };

  // Swap a page's name for a field. Enter and blur commit, Escape restores.
  // Only in manage mode, which stays open, so a commit never closes the menu.
  var startRename = function ($item) {
    var $name = $item.find('.blockr-view-item-name');
    if (!$name.length) return;
    var currentName = $name.text();

    var $input = $('<input>')
      .addClass('blockr-view-rename-input')
      .val(currentName)
      .attr('type', 'text');

    $name.replaceWith($input);
    $input.focus().select();

    var committed = false;
    var layer = null;
    var restore = function (text) {
      layer.remove();
      var $back = $('<span>').addClass('blockr-view-item-name').text(text);
      if (isManaging($input)) $back.attr('data-blockr-editable', 'Click to rename');
      $input.replaceWith($back);
    };
    var commit = function () {
      if (committed) return;
      committed = true;

      var rawName = $input.val().trim();
      // The name is a free-form display label: the only checks are
      // non-empty and not a duplicate of another view's name.
      var errorMsg = null;
      if (rawName.length === 0) {
        errorMsg = 'View name cannot be empty.';
      } else {
        var $siblings = $item.closest('.blockr-view-nav').find('.blockr-view-item');
        $siblings.each(function () {
          if (this !== $item[0] && itemName($(this)) === rawName) {
            errorMsg = 'A view with this name already exists.';
            return false; // break
          }
        });
      }
      if (errorMsg) {
        showNotification(errorMsg);
      }
      var newName = errorMsg ? currentName : rawName;
      restore(newName);

      if (newName !== currentName) {
        // The id is stable across a rename; only the label changes.
        if ($item.hasClass('active')) {
          setToggleLabel($item.closest('.blockr-view-dropdown'), newName);
        }
        var nav = $item.closest('.blockr-view-nav')[0];
        tabOf(nav, $item.attr('data-view-id')).text(newName);
        Shiny.setInputValue(nav.id + '_rename', {
          id: $item.attr('data-view-id'),
          to: newName
        }, { priority: 'event' });
      }
    };

    // An edit not yet committed is a layer (Blockr.layer): Escape restores
    // the name and leaves the menu open.
    layer = Blockr.layer($input[0], {
      inPage: true,
      escape: function () {
        committed = true;
        restore(currentName);
      }
    });

    $input.on('click', function (e) { e.stopPropagation(); });
    $input.on('keydown', function (e) {
      e.stopPropagation();
      if (e.key === 'Enter') {
        e.preventDefault();
        commit();
      }
    });
    $input.on('blur', commit);
  };

  // Native drag to reorder, from the grip only (so clicking into a name
  // never starts a drag). The row moves as the pointer passes other rows;
  // on drop the resulting order goes to the server, which applies it and
  // pushes it back through `order`.
  var enableDrag = function (el) {
    var list = el.querySelector('.blockr-view-list');
    if (!list) return;
    var dragging = null;
    var before = null;

    list.addEventListener('mousedown', function (e) {
      var row = e.target.closest('.blockr-view-item');
      if (!row) return;
      if (e.target.closest('.blockr-view-grip') && isManaging($(el))) {
        row.setAttribute('draggable', 'true');
      } else {
        row.removeAttribute('draggable');
      }
    });
    list.addEventListener('dragstart', function (e) {
      var row = e.target.closest && e.target.closest('.blockr-view-item');
      if (!row) return;
      dragging = row;
      before = $(list).children('.blockr-view-item').map(function () {
        return this.getAttribute('data-view-id');
      }).get().join('|');
      row.classList.add('is-dragging');
      e.dataTransfer.effectAllowed = 'move';
      // Firefox does not start a drag without data being set.
      e.dataTransfer.setData('text/plain', row.getAttribute('data-view-id'));
    });
    list.addEventListener('dragover', function (e) {
      if (!dragging) return;
      e.preventDefault();
      var over = e.target.closest && e.target.closest('.blockr-view-item');
      if (!over || over === dragging) return;
      var r = over.getBoundingClientRect();
      var after = e.clientY > r.top + r.height / 2;
      list.insertBefore(dragging, after ? over.nextSibling : over);
    });
    list.addEventListener('drop', function (e) {
      if (dragging) e.preventDefault();
    });
    list.addEventListener('dragend', function () {
      if (!dragging) return;
      dragging.classList.remove('is-dragging');
      dragging.removeAttribute('draggable');
      dragging = null;
      var ids = $(list).children('.blockr-view-item').map(function () {
        return this.getAttribute('data-view-id');
      }).get();
      if (ids.join('|') === before) return;
      Shiny.setInputValue(el.id + '_reorder', { order: ids }, { priority: 'event' });
    });
  };

  var viewBinding = new Shiny.InputBinding();

  $.extend(viewBinding, {
    find: function (scope) {
      return $(scope).find('.blockr-view-nav');
    },

    getValue: function (el) {
      return $(el).find('.blockr-view-item.active').attr('data-view-id') || null;
    },

    setValue: function (el, value) {
      markActive(el, value);
    },

    subscribe: function (el, callback) {
      // A real DOM change event on the nav. Programmatic updates do NOT come
      // through here: receiveMessage no longer triggers 'change' (see there).
      $(el).on('change.viewBinding', function () {
        callback(true);
      });

      // View switch: a click on a page, unless the menu is managing pages
      // (then a click on the name renames it) or the click hit a tool.
      $(el).on('click.viewBinding', '.blockr-view-item', function (e) {
        if ($(e.target).closest('.blockr-view-remove, .blockr-view-grip').length) {
          return;
        }
        e.preventDefault();
        e.stopPropagation();
        var $item = $(this);

        if (isManaging($item)) {
          if ($(e.target).closest('.blockr-view-item-name').length) {
            startRename($item);
          }
          return;
        }

        markActive(el, $item.attr('data-view-id'));
        callback(true);
        closeMenu($(el));
      });

      // A tab switches views as a pick in the menu does: it marks the view
      // and reports it as the nav's value.
      tabsOf(el).on('click.viewBinding', '.blockr-view-tab', function (e) {
        e.preventDefault();
        markActive(el, this.getAttribute('data-view-id'));
        callback(true);
      });

      // "Show views as tabs" asks for the state it wants; the option's server
      // answers with `tabs`, which shows or hides the line. The menu closes,
      // since the toggle it hangs from moves between the bar and the line.
      $(el).on('click.viewBinding', '.blockr-view-tabs-toggle', function (e) {
        e.preventDefault();
        e.stopPropagation();
        var on = this.getAttribute('aria-checked') !== 'true';
        Shiny.setInputValue(el.id + '_tabs', on, { priority: 'event' });
        closeMenu($(el));
      });

      $(el).on('click.viewBinding', '.blockr-view-manage', function (e) {
        e.preventDefault();
        e.stopPropagation();
        setManaging(el, true);
      });
      $(el).on('click.viewBinding', '.blockr-view-done', function (e) {
        e.preventDefault();
        e.stopPropagation();
        var active = document.activeElement;
        if (active && $(active).is('.blockr-view-rename-input')) active.blur();
        cancelConfirm();
        setManaging(el, false);
      });
      // Closing the menu leaves manage mode, so it always opens on the list.
      $(el).closest('.blockr-view-dropdown').on('hidden.bs.dropdown.viewBinding', function () {
        var active = document.activeElement;
        if (active && $(active).is('.blockr-view-rename-input')) active.blur();
        cancelConfirm();
        setManaging(el, false);
      });

      // Remove asks in place: the x turns the row into "Remove this view?"
      // with a Remove button; only that button sends the request, and the
      // server removes the page without a dialog. The question is a layer
      // (Blockr.layer), so Escape or a click anywhere outside its row takes
      // it back.
      var confirming = null;
      var cancelConfirm = function () {
        if (!confirming) return;
        confirming.layer.remove();
        $(confirming.item).removeClass('is-confirming').find('.blockr-view-confirm').remove();
        confirming = null;
      };

      $(el).on('click.viewBinding', '.blockr-view-remove-confirm', function (e) {
        e.stopPropagation();
        e.preventDefault();
        var $item = $(this).closest('.blockr-view-item');
        Shiny.setInputValue(el.id + '_remove', $item.attr('data-view-id'), {
          priority: 'event'
        });
      });

      $(el).on('click.viewBinding', '.blockr-view-remove', function (e) {
        e.stopPropagation();
        e.preventDefault();
        var $item = $(this).closest('.blockr-view-item');
        cancelConfirm();
        $item.addClass('is-confirming').append(
          $('<span>').addClass('blockr-view-confirm').append(
            $('<span>').addClass('blockr-view-confirm-text')
              .text('Remove \u201c' + itemName($item) + '\u201d?'),
            $('<button>').attr('type', 'button')
              .addClass('blockr-view-remove-confirm').text('Remove')
          )
        );
        confirming = {
          item: $item[0],
          layer: Blockr.layer($item[0], {
            escape: cancelConfirm,
            outside: cancelConfirm
          })
        };
        $item.find('.blockr-view-remove-confirm').trigger('focus');
      });

      // Add click: the server adds an empty "View N" and switches to it; the
      // page arrives through receiveMessage, its name open for renaming.
      $(el).on('click.viewBinding', '.blockr-view-add', function (e) {
        e.stopPropagation();
        e.preventDefault();
        Shiny.setInputValue(el.id + '_add', Date.now(), { priority: 'event' });
      });

      enableDrag(el);
    },

    unsubscribe: function (el) {
      $(el).off('.viewBinding');
      $(el).closest('.blockr-view-dropdown').off('.viewBinding');
      tabsOf(el).off('.viewBinding');
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
      }

      if (data.hasOwnProperty('tabs')) {
        showTabs(el, data.tabs === true);
      }

      if (data.hasOwnProperty('rename_new')) {
        renameNew.set(el, data.rename_new);
      }

      if (data.hasOwnProperty('add')) {
        var $new = $(data.add.html);
        $(el).find('.blockr-view-list').append($new);
        tabsOf(el).append(data.add.tab);
        var asked = renameNew.get(el) === itemName($new);
        if (asked) renameNew.delete(el);
        if (isManaging($(el))) {
          markEditable($new, true);
          if (asked) startRename($new);
        }

        // Deliberately not activated here. The server owns which view is
        // active: an add that means to navigate carries `active` in its delta
        // and lands as a `value` message a moment later. Activating on the
        // client would leave the nav pointing at a view the board never
        // switched to -- and with the echo below dropped, nothing corrects
        // it.
      }

      if (data.hasOwnProperty('remove')) {
        $(el)
          .find('.blockr-view-item[data-view-id="' + data.remove + '"]')
          .remove();
        tabOf(el, data.remove).remove();
      }

      if (data.hasOwnProperty('rename')) {
        var $target = $(el).find(
          '.blockr-view-item[data-view-id="' + data.rename.id + '"]'
        );
        $target.find('.blockr-view-item-name').text(data.rename.to);
        tabOf(el, data.rename.id).text(data.rename.to);

        if ($target.hasClass('active')) {
          setToggleLabel($(el), data.rename.to);
        }
      }

      if (data.hasOwnProperty('order')) {
        var $list = $(el).find('.blockr-view-list');
        var $tabs = tabsOf(el);
        // Re-append each item in the server's order; re-appending an existing
        // node moves it, so iterating in order lands the DOM in that order.
        data.order.forEach(function (viewId) {
          $list.append(
            $list.find('.blockr-view-item[data-view-id="' + viewId + '"]')
          );
          $tabs.append(tabOf(el, viewId));
        });
      }

      // Do NOT report the value back. The server drives the active view on
      // every path (a switch, an add, the removal of the active view), so it
      // already knows what it just pushed -- and an echo is not merely
      // redundant. With two switches in flight (a section clicked before the
      // previous one settled) the first push's echo lands after the second has
      // been applied, misses the server's `client_active` guard and is applied
      // as a fresh switch, whose push echoes in turn: the board then ping-pongs
      // between the visited views forever. Shiny's no-resend dedup does not
      // absorb it either, since the alternating values always differ.
      //
      // Forget the cached value instead, so a later real click on the view the
      // server pushed away from is still sent (the dedup would swallow it).
      Shiny.forgetLastInputValue(el.id);
    }
  });

  Shiny.inputBindings.register(viewBinding, 'blockr.view');

  // Custom message handler to switch the active dockview
  Shiny.addCustomMessageHandler('switch-view', function (m) {
    var activate = function () {
      $('.blockr-view-dock').removeClass('blockr-view-dock-active');
      $('#' + CSS.escape(m.id)).addClass('blockr-view-dock-active');
    };

    // Element may not exist yet (insertUI in same flush), retry briefly
    if (document.getElementById(m.id)) {
      activate();
    } else {
      var attempts = 0;
      var timer = setInterval(function () {
        attempts++;
        if (document.getElementById(m.id) || attempts >= 20) {
          clearInterval(timer);
          activate();
        }
      }, 50);
    }
  });
});
