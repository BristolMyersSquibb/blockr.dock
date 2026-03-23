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
})
