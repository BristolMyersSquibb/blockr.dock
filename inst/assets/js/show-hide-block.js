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

  // Delete workspace
  $(document).on('click', '.workspace-delete-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    const ws = $(this).data('workspace');
    const ns = $(this).closest('.blockr-workspace-tabs').data('ns');
    Shiny.setInputValue(ns + 'delete_workspace', ws, { priority: 'event' });
  });

  // Rename workspace (double-click on workspace tab)
  $(document).on('dblclick', '.workspace-tab', function(e) {
    e.preventDefault();
    e.stopPropagation();
    const $item = $(this);
    const oldName = $item.data('workspace');
    const ns = $item.closest('.blockr-workspace-tabs').data('ns');

    const $input = $('<input>', {
      type: 'text',
      class: 'form-control form-control-sm workspace-rename-input',
      value: oldName
    });

    $item.replaceWith($input);
    $input.focus().select();

    function commitRename() {
      const newName = $input.val().trim();
      if (newName && newName !== oldName) {
        Shiny.setInputValue(ns + 'rename_workspace',
          { old: oldName, new: newName }, { priority: 'event' });
      } else {
        // Restore original tab if no change
        const $restored = $('<a>', {
          href: '#',
          class: 'nav-link workspace-tab active',
          'data-workspace': oldName,
          text: oldName
        });
        $input.replaceWith($restored);
      }
    }

    $input.on('blur', commitRename);
    $input.on('keydown', function(ev) {
      if (ev.key === 'Enter') {
        ev.preventDefault();
        commitRename();
      } else if (ev.key === 'Escape') {
        const $restored = $('<a>', {
          href: '#',
          class: 'nav-link workspace-tab active',
          'data-workspace': oldName,
          text: oldName
        });
        $input.replaceWith($restored);
      }
    });
  });

  // R-initiated: add workspace tab
  Shiny.addCustomMessageHandler('add-workspace-tab', (m) => {
    const tabs = document.querySelector('.blockr-workspace-tabs');
    const newBtn = tabs.querySelector('.workspace-new-btn')?.closest('.nav-item');
    const li = document.createElement('li');
    li.className = 'nav-item';
    li.innerHTML = `
      <a href="#" class="nav-link workspace-tab" data-workspace="${m.name}">
        ${m.name}
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
    const tab = document.querySelector(
      `.workspace-tab[data-workspace="${m.old}"]`
    );
    if (tab) {
      tab.dataset.workspace = m.new;
      tab.textContent = m.new;
    }

    // Update workspace container ID
    const container = document.getElementById('workspace-' + m.old);
    if (container) container.id = 'workspace-' + m.new;
  });
})
