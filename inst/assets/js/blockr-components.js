/**
 * blockr-components.js
 * Bootstrap-free component handlers for blockr.dock
 */

// Shim: Fake Bootstrap 5 presence for bslib components (e.g., input_switch, popover)
if (!window.bootstrap) {
  // Minimal Popover constructor that bslib expects
  function Popover(element, options) {
    this.element = element;
    this.options = options || {};
  }
  Popover.VERSION = '5.3.0';
  Popover.prototype.show = function() {};
  Popover.prototype.hide = function() {};
  Popover.prototype.toggle = function() {};
  Popover.prototype.dispose = function() {};
  Popover.prototype.disconnect = function() {};
  Popover.getInstance = function() { return null; };
  Popover.getOrCreateInstance = function(el, opts) { return new Popover(el, opts); };

  // Minimal Tooltip constructor
  function Tooltip(element, options) {
    this.element = element;
    this.options = options || {};
  }
  Tooltip.VERSION = '5.3.0';
  Tooltip.prototype.show = function() {};
  Tooltip.prototype.hide = function() {};
  Tooltip.prototype.toggle = function() {};
  Tooltip.prototype.dispose = function() {};
  Tooltip.getInstance = function() { return null; };
  Tooltip.getOrCreateInstance = function(el, opts) { return new Tooltip(el, opts); };

  window.bootstrap = {
    Tab: { VERSION: '5.3.0' },
    Modal: { VERSION: '5.3.0' },
    Dropdown: { VERSION: '5.3.0' },
    Offcanvas: { VERSION: '5.3.0' },
    Collapse: { VERSION: '5.3.0' },
    Popover: Popover,
    Tooltip: Tooltip
  };
}

(function() {
  'use strict';

  /* ===========================================================================
     OFFCANVAS
     =========================================================================== */

  document.addEventListener('click', function(e) {
    if (!e.target || typeof e.target.closest !== 'function') return;

    // Toggle button
    var toggle = e.target.closest('[data-bs-toggle="offcanvas"]');
    if (toggle) {
      e.preventDefault();
      var target = document.querySelector(toggle.getAttribute('data-bs-target'));
      if (target) target.classList.toggle('show');
      return;
    }

    // Close button
    var close = e.target.closest('[data-bs-dismiss="offcanvas"]');
    if (close) {
      e.preventDefault();
      var offcanvas = close.closest('.offcanvas');
      if (offcanvas) offcanvas.classList.remove('show');
    }
  });

  /* ===========================================================================
     ACCORDION
     =========================================================================== */

  document.addEventListener('click', function(e) {
    if (!e.target || typeof e.target.closest !== 'function') return;

    var btn = e.target.closest('.accordion-button');
    if (!btn) return;

    e.preventDefault();

    var targetId = btn.getAttribute('data-bs-target');
    var target = targetId ? document.querySelector(targetId) : null;
    if (!target) return;

    var isOpen = target.classList.contains('show');
    var accordion = btn.closest('.accordion');

    // Close others (unless multiple allowed)
    if (accordion && !accordion.hasAttribute('data-bs-always-open')) {
      accordion.querySelectorAll('.accordion-collapse.show').forEach(function(el) {
        if (el !== target) {
          el.classList.remove('show');
          var otherBtn = accordion.querySelector('[data-bs-target="#' + el.id + '"]');
          if (otherBtn) otherBtn.classList.add('collapsed');
        }
      });
    }

    // Toggle current
    target.classList.toggle('show', !isOpen);
    btn.classList.toggle('collapsed', isOpen);
  });

  /* ===========================================================================
     COLLAPSE (generic)
     =========================================================================== */

  document.addEventListener('click', function(e) {
    if (!e.target || typeof e.target.closest !== 'function') return;

    var toggle = e.target.closest('[data-bs-toggle="collapse"]:not(.accordion-button)');
    if (!toggle) return;

    e.preventDefault();
    var targetSel = toggle.getAttribute('data-bs-target') || toggle.getAttribute('href');
    var target = targetSel ? document.querySelector(targetSel) : null;
    if (target) target.classList.toggle('show');
  });

  /* ===========================================================================
     MODAL (Shiny's built-in, just needs close handling)
     =========================================================================== */

  document.addEventListener('click', function(e) {
    if (!e.target || typeof e.target.closest !== 'function') return;

    var close = e.target.closest('[data-bs-dismiss="modal"], [data-dismiss="modal"]');
    if (close) {
      e.preventDefault();
      var modal = document.getElementById('shiny-modal');
      if (modal && typeof Shiny !== 'undefined') {
        Shiny.unbindAll(modal);
        modal.innerHTML = '';
        document.body.classList.remove('modal-open');
      }
    }
  });

  // Modal backdrop click
  document.addEventListener('click', function(e) {
    var modal = document.getElementById('shiny-modal');
    if (modal && e.target === modal) {
      var dialog = modal.querySelector('.modal-dialog');
      if (dialog && !dialog.contains(e.target) && typeof Shiny !== 'undefined') {
        Shiny.unbindAll(modal);
        modal.innerHTML = '';
        document.body.classList.remove('modal-open');
      }
    }
  });

  // Escape key closes modal/offcanvas
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') {
      // Close modal
      var modal = document.getElementById('shiny-modal');
      if (modal && modal.children.length > 0 && typeof Shiny !== 'undefined') {
        Shiny.unbindAll(modal);
        modal.innerHTML = '';
        document.body.classList.remove('modal-open');
        return;
      }
      // Close offcanvas
      var offcanvas = document.querySelector('.offcanvas.show');
      if (offcanvas) offcanvas.classList.remove('show');
    }
  });

  // Modal visibility observer
  var modalObserver = new MutationObserver(function() {
    var modal = document.getElementById('shiny-modal');
    if (modal) {
      if (modal.children.length > 0) {
        modal.style.display = 'block';
        document.body.classList.add('modal-open');
      } else {
        modal.style.display = '';
        document.body.classList.remove('modal-open');
      }
    }
  });

  function initModalObserver() {
    var modal = document.getElementById('shiny-modal');
    if (modal) modalObserver.observe(modal, { childList: true });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initModalObserver);
  } else {
    initModalObserver();
  }

  /* ===========================================================================
     G6 DEBUG - Finding the root cause
     =========================================================================== */

  var g6CheckInterval = setInterval(function() {
    document.querySelectorAll('.g6').forEach(function(el) {
      if (el.dataset.checked) return;

      var widget = window.HTMLWidgets && HTMLWidgets.find('#' + el.id);
      if (widget && widget.getWidget) {
        var graph = widget.getWidget();
        if (graph) {
          el.dataset.checked = 'true';

          // THE KEY INFO - G6's internal size vs actual container
          var graphSize = graph.getSize ? graph.getSize() : [0, 0];
          var containerRect = el.getBoundingClientRect();
          var viewportCenter = graph.getViewportCenter ? graph.getViewportCenter() : [0, 0, 0];

          console.log('========================================');
          console.log('G6 MISMATCH DEBUG');
          console.log('========================================');
          console.log('G6 internal size:    ', graphSize[0], 'x', graphSize[1]);
          console.log('Container actual:    ', Math.round(containerRect.width), 'x', Math.round(containerRect.height));
          console.log('Viewport center:     ', viewportCenter[0], ',', viewportCenter[1]);
          console.log('Expected center:     ', Math.round(containerRect.width/2), ',', Math.round(containerRect.height/2));

          // Check the canvas - this is what G6 actually renders to
          var canvas = el.querySelector('canvas');
          if (canvas) {
            console.log('Canvas size:         ', canvas.width, 'x', canvas.height);
            console.log('Canvas CSS size:     ', canvas.style.width, ',', canvas.style.height);
          }

          // What element does G6 think is its container?
          console.log('');
          console.log('Container chain sizes:');
          var p = el;
          for (var i = 0; i < 5 && p; i++) {
            console.log('  [' + i + '] ' + (p.className || p.tagName).substring(0, 40) + ': ' +
                        p.offsetWidth + 'x' + p.offsetHeight);
            p = p.parentElement;
          }

          // Check if there's a size mismatch and fix it
          if (Math.abs(graphSize[0] - containerRect.width) > 50) {
            console.log('');
            console.log('SIZE MISMATCH! G6 thinks:', graphSize[0], 'but container is:', containerRect.width);
            console.log('Attempting to resize G6...');

            // Try to resize G6 to match container
            if (graph.setSize) {
              graph.setSize(containerRect.width, containerRect.height);
              console.log('Called setSize');
            } else if (graph.changeSize) {
              graph.changeSize(containerRect.width, containerRect.height);
              console.log('Called changeSize');
            }

            // Check new size
            var newSize = graph.getSize ? graph.getSize() : [0, 0];
            console.log('New G6 size:', newSize[0], 'x', newSize[1]);
          }

          // Fix viewport if wrong
          var newViewport = graph.getViewportCenter ? graph.getViewportCenter() : [0, 0, 0];
          if (Math.abs(newViewport[0] - containerRect.width/2) > 50) {
            if (graph.fitCenter) graph.fitCenter();
          }
        }
      }
    });
  }, 300);

  setTimeout(function() { clearInterval(g6CheckInterval); }, 10000);

  /* ===========================================================================
     DROPDOWN
     =========================================================================== */

  document.addEventListener('click', function(e) {
    if (!e.target || typeof e.target.closest !== 'function') return;

    var toggle = e.target.closest('[data-bs-toggle="dropdown"]');
    if (toggle) {
      e.preventDefault();
      e.stopPropagation();
      var dropdown = toggle.closest('.dropdown');
      var menu = dropdown ? dropdown.querySelector('.dropdown-menu') : null;

      // Close other open dropdowns first
      document.querySelectorAll('.dropdown-menu.show').forEach(function(m) {
        if (m !== menu) m.classList.remove('show');
      });

      if (menu) menu.classList.toggle('show');
      return;
    }

    // Click outside closes dropdown
    if (!e.target.closest('.dropdown-menu')) {
      document.querySelectorAll('.dropdown-menu.show').forEach(function(m) {
        m.classList.remove('show');
      });
    }
  });

})();
