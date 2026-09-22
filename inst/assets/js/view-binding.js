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

  // The toggle (and, when the nav sits in the sidebar, the navbar
  // breadcrumb) states the active view's place: its chapter path ahead of its
  // name. Both are read off the item that was clicked, so a switch relabels
  // them without a server roundtrip. The chapter span collapses to nothing
  // when the view is ungrouped, which is every view on a board with no
  // chapters.
  var itemChapter = function ($item) {
    return $item.attr('data-view-chapter') || '';
  };

  var setToggleLabel = function ($el, text, chapter) {
    var $dd = $el.closest('.blockr-view-dropdown');
    $dd.find('.blockr-view-toggle-label').text(text);
    if (chapter !== undefined) {
      $dd.find('.blockr-view-toggle-chapter').text(chapter);
    }
    // Sidebar mode has no dropdown to label; the navbar crumb stands in for
    // it. Scoped to the document because the crumb is not inside the nav.
    var $crumb = $('.blockr-view-crumb');
    if ($crumb.length) {
      $crumb.find('.blockr-view-crumb-name').text(text);
      if (chapter !== undefined) {
        $crumb.find('.blockr-view-crumb-chapter').text(chapter);
      }
    }
  };

  // A chapter header is a sibling of the items it heads, not a wrapper, so
  // "the views under this chapter" is the run of following siblings up to the
  // next header at the same depth or shallower. Keeping the list flat is what
  // lets every other gesture -- switch, rename, remove, reorder -- keep
  // finding the same nodes it always did.
  var chapterMembers = function ($header) {
    var depth = parseInt($header.attr('data-chapter-depth'), 10);
    var members = [];
    $header.nextAll().each(function () {
      var $el = $(this);
      if ($el.hasClass('blockr-view-chapter')) {
        if (parseInt($el.attr('data-chapter-depth'), 10) <= depth) {
          return false;
        }
      } else if (!$el.hasClass('blockr-view-item')) {
        return false;
      }
      members.push(this);
    });
    return $(members);
  };

  var setChapterCollapsed = function ($header, collapsed) {
    $header.toggleClass('collapsed', collapsed);
    chapterMembers($header).toggleClass('blockr-view-hidden', collapsed);
    // A nested header that was collapsed in its own right stays collapsed, so
    // re-opening a parent must not re-show its children.
    if (!collapsed) {
      chapterMembers($header)
        .filter('.blockr-view-chapter.collapsed')
        .each(function () {
          chapterMembers($(this)).addClass('blockr-view-hidden');
        });
    }
  };

  // Mark the chapters holding the active view, so a collapsed one still says
  // where you are. Every header on the ancestor path is marked, not just the
  // innermost, because any of them may be the one that is collapsed.
  var markActiveChapters = function ($nav) {
    $nav.find('.blockr-view-chapter').removeClass('has-active');
    var $active = $nav.find('.blockr-view-item.active');
    if (!$active.length) return;
    var chapter = itemChapter($active);
    if (!chapter) return;
    $nav.find('.blockr-view-chapter').each(function () {
      var key = $(this).attr('data-chapter-key') || '';
      if (chapter === key || chapter.indexOf(key + ' / ') === 0) {
        $(this).addClass('has-active');
      }
    });
  };

  // The chapters currently on the board, in board order, read off the items
  // rather than off the headers: every item carries its full path, while the
  // dropdown heads only the top level, so the items are the one place both
  // surfaces agree on the whole set. No server roundtrip to list them.
  var chapterOptions = function ($nav) {
    var seen = {};
    var out = [];
    $nav.find('.blockr-view-item').each(function () {
      var path = itemChapter($(this));
      if (path && !seen[path]) {
        seen[path] = true;
        out.push(path);
      }
    });
    return out;
  };

  var closeChapterMenu = function () {
    $('.blockr-view-chapter-menu').remove();
  };

  var sendChapter = function ($item, to) {
    var $nav = $item.closest('.blockr-view-nav');
    Shiny.setInputValue($nav.attr('id') + '_chapter', {
      id: $item.attr('data-view-id'),
      to: to
    }, { priority: 'event' });
  };

  // The menu of places a view can go: the chapters that exist, a way out of
  // all of them, and a way to name a new one. Nothing is applied on the
  // client -- the pick is reported and the server's arrangement push is what
  // moves the row, so the nav never shows a grouping the board does not hold.
  var openChapterMenu = function ($item) {
    closeChapterMenu();

    var $nav = $item.closest('.blockr-view-nav');
    var current = itemChapter($item);
    var $menu = $('<div>').addClass('blockr-view-chapter-menu');

    var option = function (label, value, extraClass) {
      return $('<div>')
        .addClass('blockr-view-chapter-option ' + (extraClass || ''))
        .toggleClass('active', value === current)
        .attr('data-chapter', value)
        .text(label);
    };

    chapterOptions($nav).forEach(function (path) {
      $menu.append(option(path, path));
    });

    if (chapterOptions($nav).length) {
      $menu.append($('<hr>').addClass('blockr-view-chapter-menu-divider'));
    }

    $menu.append(option('No chapter', '', 'blockr-view-chapter-none'));
    $menu.append(
      $('<div>')
        .addClass('blockr-view-chapter-option blockr-view-chapter-new')
        .text('New chapter\u2026')
    );

    $item.append($menu);

    $menu.on('click', '.blockr-view-chapter-option', function (e) {
      e.stopPropagation();
      e.preventDefault();

      var $opt = $(this);

      // "New chapter" swaps the menu for an input rather than opening a
      // second surface. A path is typed with " / " between levels, the same
      // separator the nav prints, so a nested chapter needs no other gesture.
      if ($opt.hasClass('blockr-view-chapter-new')) {
        var $input = $('<input>')
          .addClass('blockr-view-chapter-input')
          .attr('type', 'text')
          .attr('placeholder', 'Chapter, or Parent / Child')
          .val(current);

        $menu.empty().append($input);
        $input.focus().select();

        $input.on('keydown', function (ev) {
          ev.stopPropagation();
          if (ev.key === 'Enter') {
            ev.preventDefault();
            var val = $input.val().trim();
            closeChapterMenu();
            if (val !== current) {
              sendChapter($item, val);
            }
          } else if (ev.key === 'Escape') {
            closeChapterMenu();
          }
        });

        $input.on('click', function (ev) {
          ev.stopPropagation();
        });

        $input.on('blur', closeChapterMenu);
        return;
      }

      var to = $opt.attr('data-chapter');
      closeChapterMenu();
      if (to !== current) {
        sendChapter($item, to);
      }
    });

    $menu.on('click', function (e) {
      e.stopPropagation();
    });
  };

  // Anywhere else closes it. Bound once on the document because the menu
  // lives inside the nav and a click on the nav's own rows must dismiss it
  // before that row's handler runs.
  $(document).on('click.viewChapterMenu', function (e) {
    if (!$(e.target).closest('.blockr-view-chapter-menu').length) {
      closeChapterMenu();
    }
  });

  // Inline icon markup, for the rows this binding builds itself: a view
  // added mid-session, and a chapter header the arrangement push creates.
  // The R renderer draws the same icons through bsicons; these are the
  // client-side copy, in one place rather than one per builder.
  var caretDownSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-caret-down-fill" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M7.247 11.14 2.451 5.658C1.885 5.013 2.345 4 3.204 4h9.592a1 1 0 0 1 .753 1.659l-4.796 5.48a1 1 0 0 1-1.506 0z"></path></svg>';
  var chevronUpSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-up" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path fill-rule="evenodd" d="M7.646 4.646a.5.5 0 0 1 .708 0l6 6a.5.5 0 0 1-.708.708L8 5.707l-5.646 5.647a.5.5 0 0 1-.708-.708l6-6z"></path></svg>';
  var chevronDownSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-down" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path fill-rule="evenodd" d="M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z"></path></svg>';
  var pencilSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-pencil" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M12.146.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1 0 .708l-10 10a.5.5 0 0 1-.168.11l-5 2a.5.5 0 0 1-.65-.65l2-5a.5.5 0 0 1 .11-.168l10-10zM11.207 2.5 13.5 4.793 14.793 3.5 12.5 1.207 11.207 2.5zm1.586 3L10.5 3.207 4 9.707V10h.5a.5.5 0 0 1 .5.5v.5h.5a.5.5 0 0 1 .5.5v.5h.293l6.5-6.5zm-9.761 5.175-.106.106-1.528 3.821 3.821-1.528.106-.106A.5.5 0 0 1 5 12.5V12h-.5a.5.5 0 0 1-.5-.5V11h-.5a.5.5 0 0 1-.468-.325z"></path></svg>';
  var folderSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-folder" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M.54 3.87.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3h3.982a2 2 0 0 1 1.992 2.181l-.637 7A2 2 0 0 1 13.174 14H2.826a2 2 0 0 1-1.991-1.819l-.637-7a1.99 1.99 0 0 1 .342-1.31zM2.19 4a1 1 0 0 0-.996 1.09l.637 7a1 1 0 0 0 .995.91h10.348a1 1 0 0 0 .995-.91l.637-7A1 1 0 0 0 13.81 4H2.19zm4.69-1.707A1 1 0 0 0 6.172 2H2.5a1 1 0 0 0-1 .981l.006.139q.323-.119.684-.12h5.396z"></path></svg>';
  var copySvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-copy" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path fill-rule="evenodd" d="M4 2a2 2 0 0 1 2-2h8a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V2Zm2-1a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h8a1 1 0 0 0 1-1V2a1 1 0 0 0-1-1H6ZM2 5a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h8a1 1 0 0 0 1-1v-1h1v1a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h1v1H2Z"></path></svg>';
  var xLgSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-x-lg" style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8 2.146 2.854Z"></path></svg>';

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
        setToggleLabel($(el), itemName($item), itemChapter($item));
      }
      markActiveChapters($(el));
    },

    subscribe: function (el, callback) {
      // A real DOM change event on the nav. Programmatic updates do NOT come
      // through here: receiveMessage no longer triggers 'change' (see there).
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

        setToggleLabel($nav, itemName($item), itemChapter($item));
        markActiveChapters($nav);

        callback(true);
      });

      // Collapse a chapter. Purely local: which chapters are open is a
      // property of this browser tab, not of the board, so it is never
      // reported and never persisted. A header in the dropdown is inert, so
      // the handler only fires where a twisty is rendered.
      $(el).on('click.viewBinding', '.blockr-view-chapter', function (e) {
        var $header = $(this);
        if ($(e.target).closest('.blockr-view-chapter-actions').length) {
          e.stopPropagation();
          return;
        }
        if (!$header.closest('.blockr-view-nav-sidebar').length) {
          return;
        }
        e.stopPropagation();
        e.preventDefault();
        setChapterCollapsed($header, !$header.hasClass('collapsed'));
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
              setToggleLabel(
                $item.closest('.blockr-view-dropdown'),
                newName,
                itemChapter($item)
              );
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

      // Rename a chapter: swap its label for an inline input, the way the
      // per-view rename does. What is sent is the chapter's whole path and
      // the new label for its last level -- the server finds the views to
      // rewrite, because a chapter is a label they share and the client's
      // copy of that set is not the authority.
      $(el).on('click.viewBinding', '.blockr-view-chapter-edit', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $header = $(this).closest('.blockr-view-chapter');
        var $label = $header.find('.blockr-view-chapter-label');
        var from = $header.attr('data-chapter-key') || '';
        var current = $label.text();

        if (!$label.length) {
          return;
        }

        var $input = $('<input>')
          .addClass('blockr-view-chapter-rename-input')
          .val(current)
          .attr('type', 'text');

        $label.replaceWith($input);
        $input.focus().select();

        var done = false;
        var restore = function (text) {
          if (done) return;
          done = true;
          $input.replaceWith(
            $('<span>').addClass('blockr-view-chapter-label').text(text)
          );
        };

        var commit = function () {
          if (done) return;
          var to = $input.val().trim();
          if (!to.length) {
            showNotification('Chapter name cannot be empty.');
            restore(current);
            return;
          }
          // Restore to the OLD label, not the new one: the server's
          // arrangement push is what relabels the header, so the nav never
          // shows a chapter the board does not hold. A rename that the
          // server rejects therefore leaves nothing stale behind.
          restore(current);
          if (to !== current) {
            var $nav = $header.closest('.blockr-view-nav');
            Shiny.setInputValue($nav.attr('id') + '_chapter_rename', {
              from: from,
              to: to
            }, { priority: 'event' });
          }
        };

        // The header is a collapse toggle in the sidebar, so keystrokes and
        // clicks inside the input must not reach it.
        $input.on('click', function (ev) {
          ev.stopPropagation();
        });

        $input.on('keydown', function (ev) {
          ev.stopPropagation();
          if (ev.key === 'Enter') {
            ev.preventDefault();
            commit();
          } else if (ev.key === 'Escape') {
            restore(current);
          }
        });

        $input.on('blur', commit);
      });

      // Move to chapter
      $(el).on('click.viewBinding', '.blockr-view-chapter-move', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $item = $(this).closest('.blockr-view-item');

        // A second click on the same action closes it, so the gesture is its
        // own way out.
        if ($item.find('.blockr-view-chapter-menu').length) {
          closeChapterMenu();
          return;
        }

        openChapterMenu($item);
      });

      // Duplicate. The gesture carries only the source id: what a copy is --
      // its members, its arrangement, its chapter, a free name -- is read off
      // the board, which is the only place that knows.
      $(el).on('click.viewBinding', '.blockr-view-duplicate', function (e) {
        e.stopPropagation();
        e.preventDefault();

        var $item = $(this).closest('.blockr-view-item');
        var $nav = $item.closest('.blockr-view-nav');

        Shiny.setInputValue($nav.attr('id') + '_duplicate',
                            $item.attr('data-view-id'),
                            { priority: 'event' });
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
        var addChapter = data.add.chapter || '';
        var newItem = $('<div>')
          .addClass('dropdown-item blockr-view-item')
          .attr('data-view-id', addId)
          .attr('data-view-chapter', addChapter)
          .attr('data-chapter-depth', addChapter ? addChapter.split(' / ').length : 0)
          .append(
            $('<span>').addClass('blockr-view-item-name').text(addName)
          );

        if (canCrud) {
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
                  .addClass('blockr-view-action blockr-view-chapter-move')
                  .attr('role', 'button')
                  .attr('title', 'Move to chapter')
                  .html(folderSvg),
                $('<span>')
                  .addClass('blockr-view-action blockr-view-duplicate')
                  .attr('role', 'button')
                  .attr('title', 'Duplicate')
                  .html(copySvg),
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
          setToggleLabel($(el), data.rename.to, itemChapter($target));
        }
      }

      // The nav's whole arrangement, restated: chapter headers and view items
      // in render order. Re-appending an existing node moves it, so walking
      // the sequence in order lands the DOM in that order. Headers are
      // created on demand and any header the sequence no longer names is
      // dropped, which is how a chapter that lost its last view disappears --
      // there was never an object to delete, only a label its views shared.
      if (data.hasOwnProperty('structure')) {
        var $nav = $(el);
        var $anchor = $nav.find('.dropdown-divider');
        var sidebar = $nav.hasClass('blockr-view-nav-sidebar');
        var seen = {};

        var place = function ($node) {
          if ($anchor.length) {
            $anchor.before($node);
          } else {
            $nav.append($node);
          }
        };

        data.structure.forEach(function (entry) {
          if (entry.kind === 'view') {
            var $item = $nav.find(
              '.blockr-view-item[data-view-id="' + entry.id + '"]'
            );
            $item
              .attr('data-view-chapter', entry.chapter || '')
              .attr('data-chapter-depth', entry.depth);
            place($item);
            return;
          }

          seen[entry.key] = true;
          var $header = $nav.find(
            '.blockr-view-chapter[data-chapter-key="' +
              entry.key.replace(/"/g, '\\"') + '"]'
          );

          if (!$header.length) {
            $header = $('<div>')
              .addClass('blockr-view-chapter')
              .attr('data-chapter-key', entry.key);
            if (sidebar) {
              $header.append(
                $('<span>')
                  .addClass('blockr-view-chapter-twisty')
                  .html(caretDownSvg)
              );
            }
            $header.append($('<span>').addClass('blockr-view-chapter-label'));
            // Built, not cloned from a sibling header: the FIRST chapter on a
            // board has no sibling to clone from, and a header without its
            // pencil cannot be renamed. CRUD is read off the view rows, which
            // the server already gates -- a nav with no item actions is a
            // locked or simplified board, and a chapter is no more editable
            // there than a view is.
            //
            // Order matters: beside the label, ahead of the count, so the
            // counts line up down the column whether or not a row is hovered.
            if ($nav.find('.blockr-view-item-actions').length) {
              $header.append(
                $('<span>')
                  .addClass('blockr-view-chapter-actions')
                  .append(
                    $('<span>')
                      .addClass('blockr-view-action blockr-view-chapter-edit')
                      .attr('role', 'button')
                      .attr('title', 'Rename chapter')
                      .html(pencilSvg)
                  )
              );
            }
            if (sidebar) {
              $header.append(
                $('<span>').addClass('blockr-view-chapter-count')
              );
            }
          }

          $header
            .attr('data-chapter-depth', entry.depth)
            .attr(
              'class',
              'blockr-view-chapter blockr-view-chapter-' + entry.depth +
                ($header.hasClass('collapsed') ? ' collapsed' : '')
            );
          $header.find('.blockr-view-chapter-label').text(entry.label);
          $header.find('.blockr-view-chapter-count').text(entry.count);
          place($header);
        });

        $nav.find('.blockr-view-chapter').each(function () {
          if (!seen[$(this).attr('data-chapter-key')]) {
            $(this).remove();
          }
        });

        // A regroup can move the active view under a different header, and a
        // collapsed chapter must not hide a view that has just been placed
        // outside it.
        $nav.find('.blockr-view-item').removeClass('blockr-view-hidden');
        $nav.find('.blockr-view-chapter.collapsed').each(function () {
          setChapterCollapsed($(this), true);
        });
        markActiveChapters($nav);

        var $active = $nav.find('.blockr-view-item.active');
        if ($active.length) {
          setToggleLabel($nav, itemName($active), itemChapter($active));
        }
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
