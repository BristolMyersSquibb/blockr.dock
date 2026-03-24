$(function () {
  // Bootstrap icon SVGs for dynamically created elements
  const bsIconPencil = '<svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" fill="currentColor" class="bi bi-pencil" viewBox="0 0 16 16"><path d="M12.146.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1 0 .708l-10 10a.5.5 0 0 1-.168.11l-5 2a.5.5 0 0 1-.65-.65l2-5a.5.5 0 0 1 .11-.168zM11.207 2.5 13.5 4.793 14.793 3.5 12.5 1.207zm1.586 3L10.5 3.207 4 9.707V10h.5a.5.5 0 0 1 .5.5v.5h.5a.5.5 0 0 1 .5.5v.5h.293zm-9.761 5.175-.106.106-1.528 3.821 3.821-1.528.106-.106A.5.5 0 0 1 5 12.5V12h-.5a.5.5 0 0 1-.5-.5V11h-.5a.5.5 0 0 1-.468-.325"/></svg>';
  const bsIconX = '<svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" fill="currentColor" class="bi bi-x" viewBox="0 0 16 16"><path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708"/></svg>';
  const bsIconPlus = '<svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" fill="currentColor" class="bi bi-plus" viewBox="0 0 16 16"><path d="M8 4a.5.5 0 0 1 .5.5v3h3a.5.5 0 0 1 0 1h-3v3a.5.5 0 0 1-1 0v-3h-3a.5.5 0 0 1 0-1h3v-3A.5.5 0 0 1 8 4"/></svg>';

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
      $(m.to).append($(m.from));
    }
  )

  Shiny.addCustomMessageHandler(
    'toggle-section', (m) => {
      if (m.show) { $('#' + m.id).show(); }
      else { $('#' + m.id).hide(); }
    }
  )

  Shiny.addCustomMessageHandler(
    'toggle-element-class', (m) => {
      if (m.add) { $('#' + m.id).addClass(m.className); }
      else { $('#' + m.id).removeClass(m.className); }
    }
  )

  // Switch workspace: toggle DockView containers + update tab states
  Shiny.addCustomMessageHandler(
    'switch-workspace', (m) => {
      // Toggle DockView containers
      const allWs = document.querySelectorAll('.blockr-workspace');
      allWs.forEach(ws => {
        ws.classList.toggle('active', ws.id === 'workspace-' + m.active);
      });

      if (m.parent) {
        // Child workspace: activate the parent tab, update child label
        document.querySelectorAll('.workspace-tab-parent').forEach(tab => {
          const isMatch = tab.dataset.parent === m.parent;
          tab.classList.toggle('active', isMatch);
          if (isMatch) {
            const childLabel = tab.querySelector('.ws-active-child');
            if (childLabel) childLabel.textContent = ' / ' + m.active;
          }
        });

        // Deactivate flat tabs
        document.querySelectorAll('.workspace-tab').forEach(tab => {
          tab.classList.remove('active');
        });

        // Mark active child in dropdown
        document.querySelectorAll('.workspace-child-item').forEach(item => {
          item.classList.toggle('active', item.dataset.workspace === m.active);
        });
      } else {
        // Flat workspace: activate the matching tab
        document.querySelectorAll('.workspace-tab').forEach(tab => {
          tab.classList.toggle('active', tab.dataset.workspace === m.active);
        });

        // Deactivate parent tabs (keep child label so user sees last active child)
        document.querySelectorAll('.workspace-tab-parent').forEach(tab => {
          tab.classList.remove('active');
        });
      }
    }
  )

  // Flat tab click
  $(document).on('click', '.workspace-tab', function(e) {
    e.preventDefault();
    if ($(this).hasClass('disabled')) return;
    const ws = $(this).data('workspace');
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    Shiny.setInputValue(ns + 'active_workspace', ws, { priority: 'event' });
  });

  // Dropdown child item click
  $(document).on('click', '.workspace-child-item', function(e) {
    e.preventDefault();
    if ($(this).hasClass('disabled')) return;
    const ws = $(this).data('workspace');
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    Shiny.setInputValue(ns + 'active_workspace', ws, { priority: 'event' });
  });

  // New workspace (top-level) — show type picker modal
  $(document).on('click', '.workspace-new-btn', function(e) {
    e.preventDefault();
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    const modalId = 'ws-new-type-modal';
    $('#' + modalId).remove();

    const modal = $(`
      <div class="modal fade" id="${modalId}" tabindex="-1">
        <div class="modal-dialog modal-sm modal-dialog-centered">
          <div class="modal-content">
            <div class="modal-body py-4 px-3">
              <p class="text-center fw-semibold mb-3">New workspace</p>
              <div class="d-grid gap-2">
                <button type="button" class="btn btn-outline-secondary btn-sm" id="ws-new-simple">
                  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-file-earmark me-2" viewBox="0 0 16 16"><path d="M14 4.5V14a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V2a2 2 0 0 1 2-2h5.5zm-3 0A1.5 1.5 0 0 1 9.5 3V1H4a1 1 0 0 0-1 1v12a1 1 0 0 0 1 1h8a1 1 0 0 0 1-1V4.5z"/></svg>
                  Simple workspace
                </button>
                <button type="button" class="btn btn-outline-secondary btn-sm" id="ws-new-parent">
                  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-folder me-2" viewBox="0 0 16 16"><path d="M.54 3.87.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3H13.5a2 2 0 0 1 2 2v1H8.741a2 2 0 0 0-1.6.8L6.5 7.607a1 1 0 0 1-.8.393H1.5v-1A2 2 0 0 1 .54 3.87M1.5 8.5h4.2a2 2 0 0 0 1.6-.8l.641-.8a1 1 0 0 1 .8-.4H15.5v6a2 2 0 0 1-2 2h-11a2 2 0 0 1-2-2z"/></svg>
                  Group with children
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    `);
    $('body').append(modal);
    const bsModal = new bootstrap.Modal(document.getElementById(modalId));
    bsModal.show();

    $('#ws-new-simple').one('click', function() {
      bsModal.hide();
      Shiny.setInputValue(ns + 'new_workspace',
        { ts: Date.now(), parent: null, type: 'simple' }, { priority: 'event' });
    });

    $('#ws-new-parent').one('click', function() {
      bsModal.hide();
      Shiny.setInputValue(ns + 'new_workspace',
        { ts: Date.now(), parent: null, type: 'parent' }, { priority: 'event' });
    });
  });

  // New child workspace inside a parent dropdown
  $(document).on('click', '.workspace-child-new-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    const parent = $(this).data('parent');
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    Shiny.setInputValue(ns + 'new_workspace',
      { ts: Date.now(), parent: parent }, { priority: 'event' });
  });

  // Delete workspace (leaf) — show confirmation modal
  $(document).on('click', '.ws-delete-icon:not(.ws-parent-delete-icon)', function(e) {
    e.preventDefault();
    e.stopPropagation();
    const ws = $(this).data('workspace');
    const ns = $(this).data('ns') || '';

    showDeleteModal(ws, ns, 'delete_workspace');
  });

  // Delete parent workspace — deletes parent and all children
  $(document).on('click', '.ws-parent-delete-icon', function(e) {
    e.preventDefault();
    e.stopPropagation();
    const ws = $(this).data('workspace');
    const ns = $(this).data('ns') || '';

    showDeleteModal(ws, ns, 'delete_parent_workspace');
  });

  function showDeleteModal(name, ns, inputName) {
    const modalId = 'ws-delete-confirm-modal';
    $('#' + modalId).remove();
    const modal = $(`
      <div class="modal fade" id="${modalId}" tabindex="-1">
        <div class="modal-dialog modal-sm modal-dialog-centered">
          <div class="modal-content">
            <div class="modal-body text-center py-4">
              <p class="mb-3">Delete <strong>${name}</strong>?</p>
              <div class="d-flex justify-content-center gap-2">
                <button type="button" class="btn btn-sm btn-secondary" data-bs-dismiss="modal">Cancel</button>
                <button type="button" class="btn btn-sm btn-danger" id="ws-delete-confirm">Delete</button>
              </div>
            </div>
          </div>
        </div>
      </div>
    `);
    $('body').append(modal);
    const bsModal = new bootstrap.Modal(document.getElementById(modalId));
    bsModal.show();

    $('#ws-delete-confirm').one('click', function() {
      bsModal.hide();
      Shiny.setInputValue(ns + inputName, name, { priority: 'event' });
    });
  }

  // Prevent edit/delete icon clicks from switching workspace or closing dropdown
  $(document).on('click', '.ws-edit-icon, .ws-delete-icon', function(e) {
    e.preventDefault();
    e.stopPropagation();
  });

  // Prevent dropdown from closing when editing workspace name
  let wsEditActive = false;
  $(document).on('hide.bs.dropdown', function(e) {
    if (wsEditActive) {
      e.preventDefault();
    }
  });

  // Inline rename: pencil click turns label into input
  $(document).on('click', '.ws-edit-icon:not(.ws-parent-edit-icon)', function(e) {
    const $tab = $(this).closest('.workspace-tab, .workspace-child-item');
    const $label = $tab.find('.ws-tab-label');
    if ($label.length === 0 || $label.is(':hidden')) return;

    const oldName = $tab.data('workspace');
    const ns = $tab.closest('.blockr-workspace-tabs').data('ns') || '';
    wsEditActive = true;

    const $input = $('<input>', {
      type: 'text',
      class: 'form-control form-control-sm ws-rename-input',
      value: oldName
    });

    $label.hide().after($input);
    $input.focus().select();

    function commitRename() {
      wsEditActive = false;
      const newName = $input.val().trim();
      $input.remove();
      $label.show();
      if (newName && newName !== oldName) {
        Shiny.setInputValue(ns + 'rename_workspace',
          { old: oldName, new: newName }, { priority: 'event' });
      }
    }

    $input.on('blur', commitRename);
    $input.on('keydown', function(ev) {
      if (ev.key === 'Enter') {
        ev.preventDefault();
        $input.off('blur');
        commitRename();
      } else if (ev.key === 'Escape') {
        $input.val(oldName);
        $input.off('blur');
        commitRename();
      }
    });
    // Prevent input clicks from bubbling to tab/dropdown
    $input.on('click', function(ev) { ev.stopPropagation(); });
  });

  // Inline rename for parent tabs
  $(document).on('click', '.ws-parent-edit-icon', function() {
    const $parent = $(this).closest('.nav-item');
    const $label = $parent.find('.ws-parent-label');
    if ($label.length === 0 || $label.is(':hidden')) return;

    const oldName = $label.text();
    const ns = $parent.closest('.blockr-workspace-tabs').data('ns') || '';

    const $input = $('<input>', {
      type: 'text',
      class: 'form-control form-control-sm ws-rename-input',
      value: oldName
    });

    $label.hide().after($input);
    $input.focus().select();

    function commitRename() {
      const newName = $input.val().trim();
      $input.remove();
      $label.show();
      if (newName && newName !== oldName) {
        Shiny.setInputValue(ns + 'rename_parent_workspace',
          { old: oldName, new: newName }, { priority: 'event' });
      }
    }

    $input.on('blur', commitRename);
    $input.on('keydown', function(ev) {
      if (ev.key === 'Enter') {
        ev.preventDefault();
        $input.off('blur');
        commitRename();
      } else if (ev.key === 'Escape') {
        $input.val(oldName);
        $input.off('blur');
        commitRename();
      }
    });
    $input.on('click', function(ev) { ev.stopPropagation(); });
  });

  // R-initiated: add workspace tab (flat or as child of parent)
  Shiny.addCustomMessageHandler('add-workspace-tab', (m) => {
    const tabs = document.querySelector('.blockr-workspace-tabs');

    if (m.parent) {
      // Add as child inside parent dropdown
      const menu = tabs.querySelector(
        `.workspace-child-menu[data-parent="${m.parent}"]`
      );
      if (menu) {
        const addBtn = menu.querySelector('.workspace-child-new-btn')?.closest('li');
        const li = document.createElement('li');
        li.innerHTML = `
          <a href="#" class="dropdown-item workspace-child-item"
             data-workspace="${m.name}" data-parent="${m.parent}">
            <span class="ws-tab-label">${m.name}</span>
          </a>
        `;
        if (addBtn) {
          menu.insertBefore(li, addBtn);
        } else {
          menu.appendChild(li);
        }
      }
    } else {
      // Add as flat top-level tab
      const newBtn = tabs.querySelector('.workspace-new-btn')?.closest('.nav-item');
      const li = document.createElement('li');
      li.className = 'nav-item';
      li.innerHTML = `
        <a href="#" class="nav-link workspace-tab" data-workspace="${m.name}">
          <span class="ws-tab-label">${m.name}</span>
        </a>
      `;
      if (newBtn) {
        tabs.insertBefore(li, newBtn);
      } else {
        tabs.appendChild(li);
      }
    }
  });

  // R-initiated: add a new parent tab with dropdown and one initial child
  Shiny.addCustomMessageHandler('add-parent-workspace-tab', (m) => {
    const tabs = document.querySelector('.blockr-workspace-tabs');
    const newBtn = tabs.querySelector('.workspace-new-btn')?.closest('.nav-item');

    const li = document.createElement('li');
    li.className = 'nav-item dropdown';
    li.innerHTML = `
      <a class="nav-link dropdown-toggle workspace-tab-parent"
         href="#" data-bs-toggle="dropdown" role="button"
         aria-expanded="false" data-parent="${m.parent}">
        <span class="ws-parent-label">${m.parent}</span>
        <span class="ws-active-child"> / ${m.child}</span>
        <span class="ws-tab-actions ws-parent-actions">
          <span class="ws-edit-icon ws-parent-edit-icon">${bsIconPencil}</span>
          <span class="ws-delete-icon ws-parent-delete-icon" tabindex="0"
                data-workspace="${m.parent}" data-ns="${m.ns}">${bsIconX}</span>
        </span>
      </a>
      <ul class="dropdown-menu workspace-child-menu" data-parent="${m.parent}">
        <li>
          <a href="#" class="dropdown-item workspace-child-item active"
             data-workspace="${m.child}" data-parent="${m.parent}">
            <span class="ws-tab-label">${m.child}</span>
          </a>
        </li>
        <li>
          <a href="#" class="dropdown-item workspace-child-new-btn"
             data-parent="${m.parent}">${bsIconPlus}</a>
        </li>
      </ul>
    `;

    if (newBtn) {
      tabs.insertBefore(li, newBtn);
    } else {
      tabs.appendChild(li);
    }
  });

  // R-initiated: remove workspace DockView container from DOM
  // Uses sendCustomMessage (same channel as move-element) to guarantee
  // ordering: all move-element calls complete before the container is destroyed.
  Shiny.addCustomMessageHandler('remove-workspace-container', (m) => {
    const el = document.getElementById(m.id);
    if (el) {
      // Unbind Shiny outputs/inputs before removing to avoid orphaned bindings
      Shiny.unbindAll(el);
      el.remove();
    }
  });

  // R-initiated: remove workspace tab
  Shiny.addCustomMessageHandler('remove-workspace-tab', (m) => {
    // Remove flat tab
    const tab = document.querySelector(
      `.workspace-tab[data-workspace="${m.name}"]`
    );
    if (tab) tab.closest('.nav-item').remove();

    // Remove child item from parent dropdown (keep parent even if empty)
    const childItem = document.querySelector(
      `.workspace-child-item[data-workspace="${m.name}"]`
    );
    if (childItem) {
      childItem.closest('li').remove();
    }
  });

  // R-initiated: remove parent tab and all its children
  Shiny.addCustomMessageHandler('remove-parent-workspace-tab', (m) => {
    const menu = document.querySelector(
      `.workspace-child-menu[data-parent="${m.name}"]`
    );
    if (menu) menu.closest('.nav-item').remove();
  });

  // R-initiated: rename parent tab
  Shiny.addCustomMessageHandler('rename-parent-workspace-tab', (m) => {
    const menu = document.querySelector(
      `.workspace-child-menu[data-parent="${m.old}"]`
    );
    if (menu) {
      menu.setAttribute('data-parent', m.new);
      const navItem = menu.closest('.nav-item');
      // Update parent label
      const label = navItem.querySelector('.ws-parent-label');
      if (label) label.textContent = m.new;
      // Update data-parent on the toggle link
      const toggle = navItem.querySelector('.workspace-tab-parent');
      if (toggle) toggle.setAttribute('data-parent', m.new);
      // Update data-parent on all children
      menu.querySelectorAll('.workspace-child-item').forEach(child => {
        child.setAttribute('data-parent', m.new);
        $(child).data('parent', m.new);
      });
      // Update data-parent on child new button
      const addBtn = menu.querySelector('.workspace-child-new-btn');
      if (addBtn) {
        addBtn.setAttribute('data-parent', m.new);
        $(addBtn).data('parent', m.new);
      }
      // Update delete icon data attribute
      const delIcon = navItem.querySelector('.ws-parent-delete-icon');
      if (delIcon) {
        delIcon.setAttribute('data-workspace', m.new);
        $(delIcon).data('workspace', m.new);
      }
    }
  });

  // R-initiated: rename workspace tab
  Shiny.addCustomMessageHandler('rename-workspace-tab', (m) => {
    // Update flat tabs — use jQuery .data() to update cache (not just dataset)
    // because click handlers read via $.data('workspace')
    const tab = document.querySelector(
      `.workspace-tab[data-workspace="${m.old}"]`
    );
    if (tab) {
      $(tab).data('workspace', m.new);
      tab.setAttribute('data-workspace', m.new);
      const label = tab.querySelector('.ws-tab-label');
      if (label) label.textContent = m.new;
    }

    // Update child items in dropdowns
    const childItem = document.querySelector(
      `.workspace-child-item[data-workspace="${m.old}"]`
    );
    if (childItem) {
      $(childItem).data('workspace', m.new);
      childItem.setAttribute('data-workspace', m.new);
      const label = childItem.querySelector('.ws-tab-label');
      if (label) label.textContent = m.new;

      // Update the active child label on parent tab if this child is active
      const parent = childItem.dataset.parent;
      if (parent) {
        const parentTab = document.querySelector(
          `.workspace-tab-parent[data-parent="${parent}"]`
        );
        if (parentTab) {
          const activeChild = parentTab.querySelector('.ws-active-child');
          if (activeChild && activeChild.textContent.includes(m.old)) {
            activeChild.textContent = ' / ' + m.new;
          }
        }
      }
    }

    // Update workspace container ID
    const container = document.getElementById('workspace-' + m.old);
    if (container) container.id = 'workspace-' + m.new;

    // Update delete icons with the old workspace name
    document.querySelectorAll(
      `.ws-delete-icon[data-workspace="${m.old}"]`
    ).forEach(icon => {
      $(icon).data('workspace', m.new);
      icon.setAttribute('data-workspace', m.new);
    });
  });
})
