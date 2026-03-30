$(function () {
  var workspaceBinding = new Shiny.InputBinding();

  $.extend(workspaceBinding, {
    find: function (scope) {
      return $(scope).find('.blockr-ws-nav');
    },

    getValue: function (el) {
      return $(el).find('.blockr-ws-item.active').data('ws-name') || null;
    },

    setValue: function (el, value) {
      $(el).find('.blockr-ws-item').removeClass('active');
      $(el)
        .find('.blockr-ws-item[data-ws-name="' + value + '"]')
        .addClass('active');
    },

    subscribe: function (el, callback) {
      $(el).on('click.workspaceBinding', '.blockr-ws-item', function (e) {
        e.preventDefault();
        var $item = $(this);
        var $nav = $(el);

        $nav.find('.blockr-ws-item').removeClass('active');
        $item.addClass('active');

        // Update dropdown toggle label
        $nav
          .closest('.blockr-ws-dropdown')
          .find('.blockr-ws-toggle-label')
          .text($item.data('ws-name'));

        callback(true);
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
        var newItem = $('<button>')
          .addClass('dropdown-item blockr-ws-item')
          .attr('data-ws-name', name)
          .append(
            $('<span>').addClass('blockr-ws-item-name').text(name)
          );

        if (canCrud) {
          newItem.append(
            $('<span>')
              .addClass('blockr-ws-item-actions')
              .append(
                $('<button>')
                  .addClass('blockr-ws-action blockr-ws-edit')
                  .attr('title', 'Rename')
                  .html('<i class="bi bi-pencil"></i>'),
                $('<button>')
                  .addClass('blockr-ws-action blockr-ws-remove')
                  .attr('title', 'Remove')
                  .html('<i class="bi bi-x-lg"></i>')
              )
          );
        }

        // Insert before the add button (if present) or at end
        var $addBtn = $(el).find('.blockr-ws-add');
        if ($addBtn.length) {
          $addBtn.before(newItem);
        } else {
          $(el).append(newItem);
        }
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

        // Update toggle label if this was the active item
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

  // Delegate edit click: swap name span for inline input
  $(document).on('click', '.blockr-ws-edit', function (e) {
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

    var commit = function () {
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

        // Send rename event to Shiny
        var $nav = $item.closest('.blockr-ws-nav');
        var id = $nav.attr('id');
        Shiny.setInputValue(id + '_rename', {
          from: currentName,
          to: newName
        }, { priority: 'event' });
      }
    };

    $input.on('keydown', function (e) {
      if (e.key === 'Enter') {
        e.preventDefault();
        commit();
      } else if (e.key === 'Escape') {
        var $newName = $('<span>')
          .addClass('blockr-ws-item-name')
          .text(currentName);
        $input.replaceWith($newName);
      }
    });

    $input.on('blur', commit);
  });

  // Delegate remove click: send remove event to Shiny
  $(document).on('click', '.blockr-ws-remove', function (e) {
    e.stopPropagation();
    e.preventDefault();

    var $item = $(this).closest('.blockr-ws-item');
    var wsName = $item.data('ws-name');
    var $nav = $item.closest('.blockr-ws-nav');
    var id = $nav.attr('id');

    Shiny.setInputValue(id + '_remove', wsName, { priority: 'event' });
  });

  // Delegate add click
  $(document).on('click', '.blockr-ws-add', function (e) {
    e.stopPropagation();
    e.preventDefault();

    var $nav = $(this).closest('.blockr-ws-nav');
    var id = $nav.attr('id');

    Shiny.setInputValue(id + '_add', Date.now(), { priority: 'event' });
  });

  // Custom message handler to switch the active dockview
  Shiny.addCustomMessageHandler('switch-workspace', function (m) {
    $('.blockr-ws-dock').removeClass('blockr-ws-dock-active');
    var target = '#' + m.id;
    $(target).addClass('blockr-ws-dock-active');
  });
});
