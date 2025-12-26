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

  // Minimal Modal constructor for Shiny modals
  // We don't actually use this - visibility is handled by MutationObserver on #shiny-modal
  function Modal(element, options) {
    this.element = element;
    this.options = options || {};
  }
  Modal.VERSION = '5.3.0';
  Modal.prototype.show = function() {
    // No-op: MutationObserver handles showing
  };
  Modal.prototype.hide = function() {
    // No-op: MutationObserver handles hiding
  };
  Modal.prototype.toggle = function() {};
  Modal.prototype.dispose = function() {};
  Modal.getInstance = function(el) { return null; };
  Modal.getOrCreateInstance = function(el, opts) { return new Modal(el, opts); };

  window.bootstrap = {
    Tab: { VERSION: '5.3.0' },
    Modal: Modal,
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
    var allowMultiple = accordion && accordion.getAttribute('data-multiple') === 'true';

    // Close others (unless multiple allowed)
    if (accordion && !allowMultiple) {
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

  // Shiny handler for programmatic accordion control
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('blockr-accordion-set', function(msg) {
      var accordion = document.getElementById(msg.id);
      if (!accordion) return;

      accordion.querySelectorAll('.accordion-item').forEach(function(item) {
        var value = item.getAttribute('data-value');
        var shouldBeOpen = msg.values.indexOf(value) !== -1;
        var collapse = item.querySelector('.accordion-collapse');
        var button = item.querySelector('.accordion-button');

        if (collapse) {
          collapse.classList.toggle('show', shouldBeOpen);
        }
        if (button) {
          button.classList.toggle('collapsed', !shouldBeOpen);
        }
      });
    });
  }

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

  // Modal visibility observer - shows/hides #shiny-modal based on content
  var modalObserver = new MutationObserver(function(mutations) {
    var modal = document.getElementById('shiny-modal');
    if (modal) {
      if (modal.children.length > 0) {
        modal.style.display = 'block';
        modal.classList.add('show');
        document.body.classList.add('modal-open');
      } else {
        modal.style.display = '';
        modal.classList.remove('show');
        document.body.classList.remove('modal-open');
      }
    }
  });

  function initModalObserver() {
    var modal = document.getElementById('shiny-modal');
    if (modal) {
      modalObserver.observe(modal, { childList: true });
    } else {
      setTimeout(initModalObserver, 500);
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initModalObserver);
  } else {
    initModalObserver();
  }

  /* ===========================================================================
     G6 Graph Fix - Correct size/viewport mismatch after initialization
     =========================================================================== */

  var g6CheckInterval = setInterval(function() {
    document.querySelectorAll('.g6').forEach(function(el) {
      if (el.dataset.checked) return;

      var widget = window.HTMLWidgets && HTMLWidgets.find('#' + el.id);
      if (widget && widget.getWidget) {
        var graph = widget.getWidget();
        if (graph) {
          el.dataset.checked = 'true';

          var graphSize = graph.getSize ? graph.getSize() : [0, 0];
          var containerRect = el.getBoundingClientRect();

          // Fix size mismatch (G6 may have measured wrong parent)
          if (Math.abs(graphSize[0] - containerRect.width) > 50) {
            if (graph.setSize) {
              graph.setSize(containerRect.width, containerRect.height);
            } else if (graph.changeSize) {
              graph.changeSize(containerRect.width, containerRect.height);
            }
          }

          // Fix viewport center if wrong
          var viewportCenter = graph.getViewportCenter ? graph.getViewportCenter() : [0, 0, 0];
          if (Math.abs(viewportCenter[0] - containerRect.width/2) > 50) {
            if (graph.fitCenter) graph.fitCenter();
          }
        }
      }
    });
  }, 300);

  setTimeout(function() { clearInterval(g6CheckInterval); }, 10000);

  /* ===========================================================================
     TOGGLE BUTTONS INPUT BINDING (replacement for shinyWidgets::checkboxGroupButtons)
     =========================================================================== */

  if (typeof Shiny !== 'undefined') {
    var toggleButtonsBinding = new Shiny.InputBinding();

    $.extend(toggleButtonsBinding, {
      find: function(scope) {
        return $(scope).find('.blockr-toggle-buttons');
      },

      getValue: function(el) {
        var selected = [];
        $(el).find('input[type="checkbox"]:checked').each(function() {
          selected.push($(this).val());
        });
        return selected.length > 0 ? selected : null;
      },

      setValue: function(el, value) {
        $(el).find('input[type="checkbox"]').each(function() {
          var isSelected = value && value.indexOf($(this).val()) !== -1;
          $(this).prop('checked', isSelected);
          $(this).closest('label').toggleClass('active', isSelected);
        });
      },

      subscribe: function(el, callback) {
        $(el).on('change.toggleButtons', 'input[type="checkbox"]', function(e) {
          // Toggle active class on label
          $(this).closest('label').toggleClass('active', this.checked);
          callback();
        });
      },

      unsubscribe: function(el) {
        $(el).off('.toggleButtons');
      }
    });

    Shiny.inputBindings.register(toggleButtonsBinding, 'blockr.toggleButtons');
  }

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
