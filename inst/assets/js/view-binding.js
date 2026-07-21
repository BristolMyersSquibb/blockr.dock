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
  var itemName = function ($item) {
    return $item.find('.blockr-view-item-name').text();
  };

  var setToggleLabel = function ($el, text) {
    $el
      .closest('.blockr-view-dropdown')
      .find('.blockr-view-toggle-label')
      .text(text);
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
      // Programmatic updates (receiveMessage) trigger 'change'
      $(el).on('change.viewBinding', function () {
        callback(true);
      });

      // View switch: click on item (but not on action buttons)
      $(el).on('click.viewBinding', '.blockr-view-item', function (e) {
        if ($(e.target).closest('.blockr-view-item-actions').length) {
          e.stopPropagation();
          return;
        }

        e.preventDefault();
        var $item = $(this);
        var $nav = $(el);

        $nav.find('.blockr-view-item').removeClass('active');
        $item.addClass('active');

        setToggleLabel($nav, itemName($item));

        callback(true);
      });

      // Edit click: swap name span for inline input
      $(el).on('click.viewBinding', '.blockr-view-edit', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $item = $(this).closest('.blockr-view-item');
        var $name = $item.find('.blockr-view-item-name');
        var currentName = $name.text();

        var $input = $('<input>')
          .addClass('blockr-view-rename-input')
          .val(currentName)
          .attr('type', 'text');

        $name.replaceWith($input);
        $input.focus().select();

        var committed = false;
        var commit = function (closeMenu) {
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
          var $newName = $('<span>')
            .addClass('blockr-view-item-name')
            .text(newName);
          $input.replaceWith($newName);

          if (newName !== currentName) {
            // The id is stable across a rename; only the label changes.
            if ($item.hasClass('active')) {
              setToggleLabel($item.closest('.blockr-view-dropdown'), newName);
            }

            var $nav = $item.closest('.blockr-view-nav');
            var navId = $nav.attr('id');
            Shiny.setInputValue(navId + '_rename', {
              id: $item.attr('data-view-id'),
              to: newName
            }, { priority: 'event' });
          }

          if (closeMenu) {
            var toggle = $item.closest('.blockr-view-dropdown')
              .find('[data-bs-toggle="dropdown"]')[0];
            if (toggle) {
              var dd = bootstrap.Dropdown.getOrCreateInstance(toggle);
              dd.hide();
            }
          }
        };

        $input.on('keydown', function (e) {
          if (e.key === 'Enter') {
            e.preventDefault();
            commit(true);
          } else if (e.key === 'Escape') {
            committed = true;
            var $newName = $('<span>')
              .addClass('blockr-view-item-name')
              .text(currentName);
            $input.replaceWith($newName);
          }
        });

        $input.on('blur', function () {
          commit(false);
        });
      });

      // Remove click
      $(el).on('click.viewBinding', '.blockr-view-remove', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $item = $(this).closest('.blockr-view-item');
        var viewId = $item.attr('data-view-id');
        var $nav = $item.closest('.blockr-view-nav');
        var navId = $nav.attr('id');

        Shiny.setInputValue(navId + '_remove', viewId, { priority: 'event' });
      });

      // Reorder click: view order is board state, so the gesture carries only a
      // relative move intent. The server applies it and pushes the settled
      // order back via receiveMessage; the DOM never moves optimistically.
      $(el).on('click.viewBinding', '.blockr-view-up, .blockr-view-down', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $item = $(this).closest('.blockr-view-item');
        var $nav = $item.closest('.blockr-view-nav');
        var navId = $nav.attr('id');
        var dir = $(this).hasClass('blockr-view-up') ? 'up' : 'down';

        Shiny.setInputValue(navId + '_reorder', {
          id: $item.attr('data-view-id'),
          dir: dir
        }, { priority: 'event' });
      });

      // Add click
      $(el).on('click.viewBinding', '.blockr-view-add', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $nav = $(this).closest('.blockr-view-nav');
        var navId = $nav.attr('id');

        Shiny.setInputValue(navId + '_add', Date.now(), { priority: 'event' });
      });
    },

    unsubscribe: function (el) {
      $(el).off('.viewBinding');
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
      }

      if (data.hasOwnProperty('add')) {
        var addId = data.add.id;
        var addName = data.add.name;
        var canCrud = data.canCrud !== false;
        var newItem = $('<div>')
          .addClass('dropdown-item blockr-view-item')
          .attr('data-view-id', addId)
          .append(
            $('<span>').addClass('blockr-view-item-name').text(addName)
          );

        if (canCrud) {
          var chevronUpSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-up" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path fill-rule="evenodd" d="M7.646 4.646a.5.5 0 0 1 .708 0l6 6a.5.5 0 0 1-.708.708L8 5.707l-5.646 5.647a.5.5 0 0 1-.708-.708l6-6z"></path></svg>';
          var chevronDownSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-down" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path fill-rule="evenodd" d="M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z"></path></svg>';
          var pencilSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-pencil" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M12.146.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1 0 .708l-10 10a.5.5 0 0 1-.168.11l-5 2a.5.5 0 0 1-.65-.65l2-5a.5.5 0 0 1 .11-.168l10-10zM11.207 2.5 13.5 4.793 14.793 3.5 12.5 1.207 11.207 2.5zm1.586 3L10.5 3.207 4 9.707V10h.5a.5.5 0 0 1 .5.5v.5h.5a.5.5 0 0 1 .5.5v.5h.293l6.5-6.5zm-9.761 5.175-.106.106-1.528 3.821 3.821-1.528.106-.106A.5.5 0 0 1 5 12.5V12h-.5a.5.5 0 0 1-.5-.5V11h-.5a.5.5 0 0 1-.468-.325z"></path></svg>';
          var xLgSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-x-lg" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8 2.146 2.854Z"></path></svg>';
          newItem.append(
            $('<span>')
              .addClass('blockr-view-item-actions')
              .append(
                $('<span>')
                  .addClass('blockr-view-action blockr-view-up')
                  .attr('role', 'button')
                  .attr('title', 'Move up')
                  .html(chevronUpSvg),
                $('<span>')
                  .addClass('blockr-view-action blockr-view-down')
                  .attr('role', 'button')
                  .attr('title', 'Move down')
                  .html(chevronDownSvg),
                $('<span>')
                  .addClass('blockr-view-action blockr-view-edit')
                  .attr('role', 'button')
                  .attr('title', 'Rename')
                  .html(pencilSvg),
                $('<span>')
                  .addClass('blockr-view-action blockr-view-remove')
                  .attr('role', 'button')
                  .attr('title', 'Remove')
                  .html(xLgSvg)
              )
          );
        }

        // Insert before the divider (if present) or at end
        var $divider = $(el).find('.dropdown-divider');
        if ($divider.length) {
          $divider.before(newItem);
        } else {
          $(el).append(newItem);
        }

        // Activate the new item and update toggle label
        $(el).find('.blockr-view-item').removeClass('active');
        newItem.addClass('active');
        setToggleLabel($(el), addName);
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
        var $nav = $(el);
        var $anchor = $nav.find('.dropdown-divider');
        // Re-append each item in the server's order; re-appending an existing
        // node moves it, so iterating in order lands the DOM in that order.
        data.order.forEach(function (viewId) {
          var $item = $nav.find(
            '.blockr-view-item[data-view-id="' + viewId + '"]'
          );
          if ($anchor.length) {
            $anchor.before($item);
          } else {
            $nav.append($item);
          }
        });
      }

      $(el).trigger('change');
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
