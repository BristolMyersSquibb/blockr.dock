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

  Shiny.addCustomMessageHandler(
    'switch-workspace', (m) => {
      document.querySelectorAll('.blockr-workspace').forEach(ws => {
        ws.classList.toggle('active', ws.id === 'workspace-' + m.active);
      });
      document.querySelectorAll('.workspace-item').forEach(item => {
        item.classList.toggle('active', item.dataset.workspace === m.active);
      });
      const label = document.querySelector('.active-ws-label');
      if (label) label.textContent = m.active;
    }
  )

  $(document).on('click', '.workspace-item', function(e) {
    e.preventDefault();
    const ws = $(this).data('workspace');
    const ns = $(this).closest('.blockr-workspace-dropdown').data('ns');
    Shiny.setInputValue(ns + 'active_workspace', ws, { priority: 'event' });
  });
})
