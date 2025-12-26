/**
 * blockr-components.js
 * Bootstrap-free component handlers for blockr.dock
 */

// Shim: Fake Bootstrap 5 presence for bslib components (e.g., input_switch)
if (!window.bootstrap) {
  window.bootstrap = {
    Tab: { VERSION: '5.3.0' },
    Modal: { VERSION: '5.3.0' },
    Dropdown: { VERSION: '5.3.0' },
    Offcanvas: { VERSION: '5.3.0' },
    Collapse: { VERSION: '5.3.0' }
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
