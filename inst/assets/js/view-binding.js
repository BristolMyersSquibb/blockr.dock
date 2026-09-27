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

  // The small icons of a view row, the same as view_icons in R.
  var ICONS = {
    grip: '<svg width="8" height="12" viewBox="0 0 8 12" fill="currentColor"><circle cx="2" cy="2" r="1"></circle><circle cx="6" cy="2" r="1"></circle><circle cx="2" cy="6" r="1"></circle><circle cx="6" cy="6" r="1"></circle><circle cx="2" cy="10" r="1"></circle><circle cx="6" cy="10" r="1"></circle></svg>',
    check: '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><path d="M3.5 8.5l3 3 6-7"></path></svg>',
    x: '<svg width="10" height="10" viewBox="0 0 10 10" fill="none" stroke="currentColor" stroke-width="1" stroke-linecap="round"><path d="M2.5 2.5l5 5M7.5 2.5l-5 5"></path></svg>'
  };

  // A view row as view_item_ui() draws it.
  var buildItem = function (id, name, canCrud) {
    var $item = $('<div>')
      .addClass('dropdown-item blockr-menu__item blockr-view-item')
      .attr('data-view-id', id);
    if (canCrud) {
      $item.append(
        $('<span>').addClass('blockr-view-grip')
          .attr('aria-label', 'Drag to reorder').html(ICONS.grip)
      );
    }
    $item.append(
      $('<span>').addClass('blockr-view-item-name').text(name),
      $('<span>').addClass('blockr-menu__check').html(ICONS.check)
    );
    if (canCrud) {
      $item.append(
        $('<span>').addClass('blockr-view-action blockr-view-remove')
          .attr('role', 'button').attr('title', 'Remove view').html(ICONS.x)
      );
    }
    return $item;
  };

  var closeMenu = function ($el) {
    var toggle = $el.closest('.blockr-view-dropdown')
      .find('[data-bs-toggle="dropdown"]')[0];
    if (toggle) bootstrap.Dropdown.getOrCreateInstance(toggle).hide();
  };

  var isManaging = function ($el) {
    return $el.closest('.blockr-view-nav').hasClass('is-managing');
  };

  // In manage mode a view's name is edited in place with a click, so it
  // carries blockr.ui's editable marker: the text cursor and a "Click to
  // rename" tooltip. Outside the mode a click switches views, so it goes.
  var markEditable = function ($scope, on) {
    $scope.find('.blockr-view-item-name').each(function () {
      if (on) this.setAttribute('data-blockr-editable', 'Click to rename');
      else this.removeAttribute('data-blockr-editable');
    });
  };

  var setManaging = function (el, on) {
    $(el).toggleClass('is-managing', on);
    markEditable($(el), on);
  };

  // Swap a view's name for a field. Enter and blur commit, Escape restores.
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
    var restore = function (text) {
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
        var navId = $item.closest('.blockr-view-nav').attr('id');
        Shiny.setInputValue(navId + '_rename', {
          id: $item.attr('data-view-id'),
          to: newName
        }, { priority: 'event' });
      }
    };

    $input.on('click', function (e) { e.stopPropagation(); });
    $input.on('keydown', function (e) {
      e.stopPropagation();
      if (e.key === 'Enter') {
        e.preventDefault();
        commit();
      } else if (e.key === 'Escape') {
        e.preventDefault();
        committed = true;
        restore(currentName);
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
      $(el).find('.blockr-view-item').removeClass('active');
      var $item = $(el)
        .find('.blockr-view-item[data-view-id="' + value + '"]')
        .addClass('active');
      if ($item.length) {
        setToggleLabel($(el), itemName($item));
      }
    },

    subscribe: function (el, callback) {
      // A real DOM change event on the nav. Programmatic updates do NOT come
      // through here: receiveMessage no longer triggers 'change' (see there).
      $(el).on('change.viewBinding', function () {
        callback(true);
      });

      // View switch: a click on a view, unless the menu is managing views
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

        var $nav = $(el);
        $nav.find('.blockr-view-item').removeClass('active');
        $item.addClass('active');
        setToggleLabel($nav, itemName($item));
        callback(true);
        closeMenu($nav);
      });

      // Manage views: the same list becomes an editor, and back with Done.
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
        setManaging(el, false);
      });

      // Remove asks in place: the x turns the row into "Remove this view?"
      // with a Remove button; only that button sends the request, and the
      // server removes the view without a dialog. A click anywhere else in
      // the menu, or Escape, takes the question back.
      var cancelConfirm = function () {
        $(el).find('.blockr-view-item.is-confirming').each(function () {
          $(this).removeClass('is-confirming').find('.blockr-view-confirm').remove();
        });
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
        $item.find('.blockr-view-remove-confirm').trigger('focus');
      });

      $(el).on('click.viewBinding', function (e) {
        if (!$(e.target).closest('.blockr-view-confirm, .blockr-view-remove').length) {
          cancelConfirm();
        }
      });

      // Escape takes a pending question back instead of closing the menu.
      // Capture phase on the window: Bootstrap handles the dropdown's keys in
      // the capture phase on the document, and the window's runs first.
      if (!el._blockrConfirmEscape) {
        el._blockrConfirmEscape = function (e) {
          if (e.key !== 'Escape' || !$(el).find('.is-confirming').length) return;
          e.preventDefault();
          e.stopPropagation();
          cancelConfirm();
        };
        window.addEventListener('keydown', el._blockrConfirmEscape, true);
      }

      // Add click: the server adds an empty "View N" and switches to it; the
      // view arrives through receiveMessage, its name open for renaming.
      $(el).on('click.viewBinding', '.blockr-view-add', function (e) {
        e.stopPropagation();
        e.preventDefault();
        Shiny.setInputValue(el.id + '_add', Date.now(), { priority: 'event' });
      });

      enableDrag(el);
    },

    unsubscribe: function (el) {
      $(el).off('.viewBinding');
      if (el._blockrConfirmEscape) {
        window.removeEventListener('keydown', el._blockrConfirmEscape, true);
        el._blockrConfirmEscape = null;
      }
      $(el).closest('.blockr-view-dropdown').off('.viewBinding');
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
      }

      if (data.hasOwnProperty('add')) {
        var canCrud = data.canCrud !== false;
        var $new = buildItem(data.add.id, data.add.name, canCrud);
        $(el).find('.blockr-view-list').append($new);
        // "New view" in manage mode: the new row's name opens for renaming.
        if ($(el).hasClass('is-managing')) {
          markEditable($new, true);
          startRename($new);
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
      }

      if (data.hasOwnProperty('rename')) {
        var $target = $(el).find(
          '.blockr-view-item[data-view-id="' + data.rename.id + '"]'
        );
        $target.find('.blockr-view-item-name').text(data.rename.to);

        if ($target.hasClass('active')) {
          setToggleLabel($(el), data.rename.to);
        }
      }

      if (data.hasOwnProperty('order')) {
        var $list = $(el).find('.blockr-view-list');
        // Re-append each item in the server's order; re-appending an existing
        // node moves it, so iterating in order lands the DOM in that order.
        data.order.forEach(function (viewId) {
          $list.append(
            $list.find('.blockr-view-item[data-view-id="' + viewId + '"]')
          );
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
