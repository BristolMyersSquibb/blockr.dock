$(function () {
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
      document.querySelectorAll('.blockr-workspace').forEach(ws => {
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

        // Deactivate parent tabs
        document.querySelectorAll('.workspace-tab-parent').forEach(tab => {
          tab.classList.remove('active');
          const childLabel = tab.querySelector('.ws-active-child');
          if (childLabel) childLabel.textContent = '';
        });
      }
    }
  )

  // Flat tab click
  $(document).on('click', '.workspace-tab', function(e) {
    e.preventDefault();
    const ws = $(this).data('workspace');
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    Shiny.setInputValue(ns + 'active_workspace', ws, { priority: 'event' });
  });

  // Dropdown child item click
  $(document).on('click', '.workspace-child-item', function(e) {
    e.preventDefault();
    const ws = $(this).data('workspace');
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    Shiny.setInputValue(ns + 'active_workspace', ws, { priority: 'event' });
  });

  // New workspace
  $(document).on('click', '.workspace-new-btn', function(e) {
    e.preventDefault();
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    Shiny.setInputValue(ns + 'new_workspace', Date.now(), { priority: 'event' });
  });

  // Delete workspace — show confirmation modal
  $(document).on('click', '.ws-delete-icon', function(e) {
    e.preventDefault();
    e.stopPropagation();
    const ws = $(this).data('workspace');
    const ns = $(this).data('ns') || '';

    const modalId = 'ws-delete-confirm-modal';
    $('#' + modalId).remove();
    const modal = $(`
      <div class="modal fade" id="${modalId}" tabindex="-1">
        <div class="modal-dialog modal-sm modal-dialog-centered">
          <div class="modal-content">
            <div class="modal-body text-center py-4">
              <p class="mb-3">Delete workspace <strong>${ws}</strong>?</p>
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
      Shiny.setInputValue(ns + 'delete_workspace', ws, { priority: 'event' });
    });
  });

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
  $(document).on('click', '.ws-edit-icon', function(e) {
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

  // R-initiated: add workspace tab
  Shiny.addCustomMessageHandler('add-workspace-tab', (m) => {
    const tabs = document.querySelector('.blockr-workspace-tabs');
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
  });

  // R-initiated: remove workspace tab
  Shiny.addCustomMessageHandler('remove-workspace-tab', (m) => {
    const tab = document.querySelector(
      `.workspace-tab[data-workspace="${m.name}"]`
    );
    if (tab) tab.closest('.nav-item').remove();
  });

  // R-initiated: rename workspace tab
  Shiny.addCustomMessageHandler('rename-workspace-tab', (m) => {
    // Update flat tabs
    const tab = document.querySelector(
      `.workspace-tab[data-workspace="${m.old}"]`
    );
    if (tab) {
      tab.dataset.workspace = m.new;
      const label = tab.querySelector('.ws-tab-label');
      if (label) label.textContent = m.new;
    }

    // Update child items in dropdowns
    const childItem = document.querySelector(
      `.workspace-child-item[data-workspace="${m.old}"]`
    );
    if (childItem) {
      childItem.dataset.workspace = m.new;
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
      icon.dataset.workspace = m.new;
    });
  });
})
