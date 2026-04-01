$(function () {
  var workspaceBinding = new Shiny.InputBinding();

  $.extend(workspaceBinding, {
    find: function (scope) {
      return $(scope).find('.blockr-ws-nav');
    },

    getValue: function (el) {
      return $(el).find('.blockr-ws-item.active').attr('data-ws-name') || null;
    },

    setValue: function (el, value) {
      $(el).find('.blockr-ws-item').removeClass('active');
      $(el)
        .find('.blockr-ws-item[data-ws-name="' + value + '"]')
        .addClass('active');
    },

    subscribe: function (el, callback) {
      // Workspace switch: click on item (but not on action buttons)
      $(el).on('click.workspaceBinding', '.blockr-ws-item', function (e) {
        if ($(e.target).closest('.blockr-ws-item-actions').length) {
          e.stopPropagation();
          return;
        }

        e.preventDefault();
        var $item = $(this);
        var $nav = $(el);

        $nav.find('.blockr-ws-item').removeClass('active');
        $item.addClass('active');

        var wsName = $item.attr('data-ws-name');
        $nav
          .closest('.blockr-ws-dropdown')
          .find('.blockr-ws-toggle-label')
          .text(wsName);

        callback(true);
      });

      // Edit click: swap name span for inline input
      $(el).on('click.workspaceBinding', '.blockr-ws-edit', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $item = $(this).closest('.blockr-ws-item');
        var $name = $item.find('.blockr-ws-item-name');
        var currentName = $name.text();

        var $input = $('<input>')
          .addClass('blockr-ws-rename-input')
          .val(currentName)
          .attr('type', 'text');

        $name.replaceWith($input);
        $input.focus().select();

        var committed = false;
        var commit = function (closeMenu) {
          if (committed) return;
          committed = true;

          var newName = $input.val().trim() || currentName;
          var $newName = $('<span>')
            .addClass('blockr-ws-item-name')
            .text(newName);
          $input.replaceWith($newName);

          if (newName !== currentName) {
            $item.attr('data-ws-name', newName);

            if ($item.hasClass('active')) {
              $item
                .closest('.blockr-ws-dropdown')
                .find('.blockr-ws-toggle-label')
                .text(newName);
            }

            var $nav = $item.closest('.blockr-ws-nav');
            var id = $nav.attr('id');
            Shiny.setInputValue(id + '_rename', {
              from: currentName,
              to: newName
            }, { priority: 'event' });
          }

          if (closeMenu) {
            var toggle = $item.closest('.blockr-ws-dropdown')
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
              .addClass('blockr-ws-item-name')
              .text(currentName);
            $input.replaceWith($newName);
          }
        });

        $input.on('blur', function () {
          commit(false);
        });
      });

      // Remove click
      $(el).on('click.workspaceBinding', '.blockr-ws-remove', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $item = $(this).closest('.blockr-ws-item');
        var wsName = $item.attr('data-ws-name');
        var $nav = $item.closest('.blockr-ws-nav');
        var id = $nav.attr('id');

        Shiny.setInputValue(id + '_remove', wsName, { priority: 'event' });
      });

      // Add click
      $(el).on('click.workspaceBinding', '.blockr-ws-add', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $nav = $(this).closest('.blockr-ws-nav');
        var id = $nav.attr('id');

        Shiny.setInputValue(id + '_add', Date.now(), { priority: 'event' });
      });
    },

    unsubscribe: function (el) {
      $(el).off('.workspaceBinding');
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
        $(el)
          .closest('.blockr-ws-dropdown')
          .find('.blockr-ws-toggle-label')
          .text(data.value);
      }

      if (data.hasOwnProperty('add')) {
        var name = data.add;
        var canCrud = data.canCrud !== false;
        var newItem = $('<div>')
          .addClass('dropdown-item blockr-ws-item')
          .attr('data-ws-name', name)
          .append(
            $('<span>').addClass('blockr-ws-item-name').text(name)
          );

        if (canCrud) {
          var pencilSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-pencil" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M12.146.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1 0 .708l-10 10a.5.5 0 0 1-.168.11l-5 2a.5.5 0 0 1-.65-.65l2-5a.5.5 0 0 1 .11-.168l10-10zM11.207 2.5 13.5 4.793 14.793 3.5 12.5 1.207 11.207 2.5zm1.586 3L10.5 3.207 4 9.707V10h.5a.5.5 0 0 1 .5.5v.5h.5a.5.5 0 0 1 .5.5v.5h.293l6.5-6.5zm-9.761 5.175-.106.106-1.528 3.821 3.821-1.528.106-.106A.5.5 0 0 1 5 12.5V12h-.5a.5.5 0 0 1-.5-.5V11h-.5a.5.5 0 0 1-.468-.325z"></path></svg>';
          var xLgSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-x-lg" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8 2.146 2.854Z"></path></svg>';
          newItem.append(
            $('<span>')
              .addClass('blockr-ws-item-actions')
              .append(
                $('<span>')
                  .addClass('blockr-ws-action blockr-ws-edit')
                  .attr('role', 'button')
                  .attr('title', 'Rename')
                  .html(pencilSvg),
                $('<span>')
                  .addClass('blockr-ws-action blockr-ws-remove')
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
        $(el).find('.blockr-ws-item').removeClass('active');
        newItem.addClass('active');
        $(el)
          .closest('.blockr-ws-dropdown')
          .find('.blockr-ws-toggle-label')
          .text(name);
      }

      if (data.hasOwnProperty('remove')) {
        $(el)
          .find('.blockr-ws-item[data-ws-name="' + data.remove + '"]')
          .remove();
      }

      if (data.hasOwnProperty('rename')) {
        var $target = $(el).find(
          '.blockr-ws-item[data-ws-name="' + data.rename.from + '"]'
        );
        $target.attr('data-ws-name', data.rename.to);
        $target.find('.blockr-ws-item-name').text(data.rename.to);

        if ($target.hasClass('active')) {
          $(el)
            .closest('.blockr-ws-dropdown')
            .find('.blockr-ws-toggle-label')
            .text(data.rename.to);
        }
      }

      $(el).trigger('change');
    }
  });

  Shiny.inputBindings.register(workspaceBinding, 'blockr.workspace');

  // Custom message handler to switch the active dockview
  Shiny.addCustomMessageHandler('switch-workspace', function (m) {
    var activate = function () {
      $('.blockr-ws-dock').removeClass('blockr-ws-dock-active');
      $('#' + CSS.escape(m.id)).addClass('blockr-ws-dock-active');
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
