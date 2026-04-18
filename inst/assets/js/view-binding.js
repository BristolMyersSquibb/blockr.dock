$(function () {
  var showNotification = function (message, type, duration) {
    Shiny.notifications.show({
      html: message,
      type: type || 'warning',
      duration: duration != null ? duration : 3000
    });
  };

  var viewBinding = new Shiny.InputBinding();

  $.extend(viewBinding, {
    find: function (scope) {
      return $(scope).find('.blockr-view-nav');
    },

    getValue: function (el) {
      return $(el).find('.blockr-view-item.active').attr('data-view-name') || null;
    },

    setValue: function (el, value) {
      $(el).find('.blockr-view-item').removeClass('active');
      $(el)
        .find('.blockr-view-item[data-view-name="' + value + '"]')
        .addClass('active');
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

        var viewName = $item.attr('data-view-name');
        $nav
          .closest('.blockr-view-dropdown')
          .find('.blockr-view-toggle-label')
          .text(viewName);

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
          // Validate: non-empty, alphanumeric/spaces/hyphens/underscores only,
          // and not a duplicate of another view
          var errorMsg = null;
          if (rawName.length === 0) {
            errorMsg = 'View name cannot be empty.';
          } else if (!/^[a-zA-Z0-9 _-]+$/.test(rawName)) {
            errorMsg = 'Invalid name. Only letters, numbers, spaces, hyphens and underscores are allowed.';
          } else {
            var $siblings = $item.closest('.blockr-view-nav').find('.blockr-view-item');
            $siblings.each(function () {
              if (this !== $item[0] && $(this).attr('data-view-name') === rawName) {
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
            $item.attr('data-view-name', newName);

            if ($item.hasClass('active')) {
              $item
                .closest('.blockr-view-dropdown')
                .find('.blockr-view-toggle-label')
                .text(newName);
            }

            var $nav = $item.closest('.blockr-view-nav');
            var id = $nav.attr('id');
            Shiny.setInputValue(id + '_rename', {
              from: currentName,
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
        var viewName = $item.attr('data-view-name');
        var $nav = $item.closest('.blockr-view-nav');
        var id = $nav.attr('id');

        Shiny.setInputValue(id + '_remove', viewName, { priority: 'event' });
      });

      // Add click
      $(el).on('click.viewBinding', '.blockr-view-add', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $nav = $(this).closest('.blockr-view-nav');
        var id = $nav.attr('id');

        Shiny.setInputValue(id + '_add', Date.now(), { priority: 'event' });
      });
    },

    unsubscribe: function (el) {
      $(el).off('.viewBinding');
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
        $(el)
          .closest('.blockr-view-dropdown')
          .find('.blockr-view-toggle-label')
          .text(data.value);
      }

      if (data.hasOwnProperty('add')) {
        var name = data.add;
        var canCrud = data.canCrud !== false;
        var newItem = $('<div>')
          .addClass('dropdown-item blockr-view-item')
          .attr('data-view-name', name)
          .append(
            $('<span>').addClass('blockr-view-item-name').text(name)
          );

        if (canCrud) {
          var pencilSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-pencil" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M12.146.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1 0 .708l-10 10a.5.5 0 0 1-.168.11l-5 2a.5.5 0 0 1-.65-.65l2-5a.5.5 0 0 1 .11-.168l10-10zM11.207 2.5 13.5 4.793 14.793 3.5 12.5 1.207 11.207 2.5zm1.586 3L10.5 3.207 4 9.707V10h.5a.5.5 0 0 1 .5.5v.5h.5a.5.5 0 0 1 .5.5v.5h.293l6.5-6.5zm-9.761 5.175-.106.106-1.528 3.821 3.821-1.528.106-.106A.5.5 0 0 1 5 12.5V12h-.5a.5.5 0 0 1-.5-.5V11h-.5a.5.5 0 0 1-.468-.325z"></path></svg>';
          var xLgSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-x-lg" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8 2.146 2.854Z"></path></svg>';
          newItem.append(
            $('<span>')
              .addClass('blockr-view-item-actions')
              .append(
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
        $(el)
          .closest('.blockr-view-dropdown')
          .find('.blockr-view-toggle-label')
          .text(name);
      }

      if (data.hasOwnProperty('remove')) {
        $(el)
          .find('.blockr-view-item[data-view-name="' + data.remove + '"]')
          .remove();
      }

      if (data.hasOwnProperty('rename')) {
        var $target = $(el).find(
          '.blockr-view-item[data-view-name="' + data.rename.from + '"]'
        );
        $target.attr('data-view-name', data.rename.to);
        $target.find('.blockr-view-item-name').text(data.rename.to);

        if ($target.hasClass('active')) {
          $(el)
            .closest('.blockr-view-dropdown')
            .find('.blockr-view-toggle-label')
            .text(data.rename.to);
        }
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
