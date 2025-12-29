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

  /* ===========================================================================
     MOBILE LAYOUT - Vertical card stack for mobile devices
     Each dockview panel becomes a card, panels with multiple tabs get internal tabs
     =========================================================================== */

  var MOBILE_BREAKPOINT = 900;
  var mobileInitialized = false;
  var lastRenderedCardState = '';
  var renderCardsTimeout = null;
  var isRenderingCards = false;

  // Get all block cards from offcanvas (the source of truth)
  function getBlockCards() {
    var offcanvas = document.querySelector('[id$="blocks_offcanvas"]');
    if (!offcanvas) return [];

    var cards = offcanvas.querySelectorAll('.card[id*="block_handle"]');
    return Array.from(cards);
  }

  // Get extension panels (DAG, etc.) from the extensions offcanvas
  function getExtensionPanels() {
    var offcanvas = document.querySelector('[id$="exts_offcanvas"]');
    if (!offcanvas) return [];

    // Find extension content divs
    var extPanels = offcanvas.querySelectorAll('[class*="ext_panel-"]');
    return Array.from(extPanels);
  }

  // Get block info from a card element
  function getBlockInfo(card) {
    var id = card.id;
    var match = id.match(/block_handle[-_](.+)$/);
    var blockId = match ? match[1] : id;

    var title = null;

    // Method 1: Look for the uiOutput element that contains the block name
    var nameOut = card.querySelector('[id$="block_name_out"]');
    if (nameOut) {
      var h5 = nameOut.querySelector('h5, h6, .block-name, span');
      if (h5) {
        title = h5.textContent.trim();
      } else {
        title = nameOut.textContent.trim();
      }
    }

    // Method 2: Look for the hidden text input that stores the name
    if (!title) {
      var nameInput = card.querySelector('[id$="block_name_in"]');
      if (nameInput && nameInput.value) {
        title = nameInput.value.trim();
      }
    }

    // Method 3: Look for .card-title content
    if (!title) {
      var cardTitle = card.querySelector('.card-title h5, .card-title h6');
      if (cardTitle) {
        title = cardTitle.textContent.trim();
      }
    }

    // Final fallback: prettify the block ID
    if (!title || title.length > 50) {
      title = blockId.replace(/_/g, ' ').replace(/\b\w/g, function(c) { return c.toUpperCase(); });
    }

    return { id: blockId, fullId: id, title: title };
  }

  // Render all blocks and extensions as vertically stacked cards
  function renderMobileCards() {
    var cardStack = document.querySelector('.mobile-card-stack');
    if (!cardStack) return;

    var blockCards = getBlockCards();
    var extPanels = getExtensionPanels();

    // Check if anything changed
    var currentState = blockCards.map(function(c) { return c.id; }).join(',') +
                       '|' + extPanels.length;
    if (currentState === lastRenderedCardState && cardStack.children.length > 0) {
      return; // Nothing changed
    }
    lastRenderedCardState = currentState;

    // Unbind Shiny before clearing
    if (typeof Shiny !== 'undefined' && Shiny.unbindAll) {
      Shiny.unbindAll(cardStack);
    }

    cardStack.innerHTML = '';

    // Show empty state if no blocks
    if (blockCards.length === 0 && extPanels.length === 0) {
      cardStack.innerHTML = '<div class="mobile-empty-state">' +
        '<div class="mobile-empty-state-icon">📦</div>' +
        '<div class="mobile-empty-state-text">No blocks yet</div>' +
        '<div class="mobile-empty-state-hint">Tap + to add a block</div>' +
        '</div>';
      return;
    }

    // Render each block as a card
    blockCards.forEach(function(originalCard) {
      var info = getBlockInfo(originalCard);

      // Create wrapper card
      var mobileCard = document.createElement('div');
      mobileCard.className = 'mobile-block-card';
      mobileCard.setAttribute('data-block-id', info.fullId);

      // Card header with title
      var header = document.createElement('div');
      header.className = 'mobile-card-header';
      header.innerHTML = '<span class="mobile-card-title">' + escapeHtml(info.title) + '</span>';
      mobileCard.appendChild(header);

      // Card body with cloned content
      var body = document.createElement('div');
      body.className = 'mobile-card-body';

      // Clone the original card content
      var clone = originalCard.cloneNode(true);
      clone.style.display = 'block';
      clone.classList.add('mobile-cloned-block');
      body.appendChild(clone);

      mobileCard.appendChild(body);
      cardStack.appendChild(mobileCard);
    });

    // Render extension panels as cards (e.g., DAG view)
    extPanels.forEach(function(extPanel, index) {
      var mobileCard = document.createElement('div');
      mobileCard.className = 'mobile-block-card mobile-extension-card';

      // Try to get extension title
      var title = 'Extension';
      var extClass = Array.from(extPanel.classList).find(function(c) {
        return c.indexOf('ext_panel-') !== -1;
      });
      if (extClass) {
        // Extract name from class like "ext_panel-dag_extension"
        var extName = extClass.replace('ext_panel-', '').replace(/_/g, ' ');
        title = extName.replace(/\b\w/g, function(c) { return c.toUpperCase(); });
      }

      // Card header
      var header = document.createElement('div');
      header.className = 'mobile-card-header';
      header.innerHTML = '<span class="mobile-card-title">' + escapeHtml(title) + '</span>';
      mobileCard.appendChild(header);

      // Card body with cloned extension content
      var body = document.createElement('div');
      body.className = 'mobile-card-body mobile-extension-body';

      var clone = extPanel.cloneNode(true);
      clone.style.display = 'block';
      body.appendChild(clone);

      mobileCard.appendChild(body);
      cardStack.appendChild(mobileCard);
    });

    // Rebind Shiny
    if (typeof Shiny !== 'undefined' && Shiny.bindAll) {
      Shiny.bindAll(cardStack);
    }
  }

  // Helper to escape HTML
  function escapeHtml(text) {
    var div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  // Handle add block button on mobile
  function setupMobileAddButton() {
    var addBtn = document.querySelector('.mobile-add-btn');
    if (!addBtn || addBtn.dataset.mobileSetup) return;
    addBtn.dataset.mobileSetup = 'true';

    addBtn.addEventListener('click', function() {
      var panel = document.querySelector('.blockr-block-panel');

      if (panel) {
        panel.classList.remove('blockr-block-panel-hidden');
        if (typeof Shiny !== 'undefined') {
          var stateInput = panel.id.replace('block_panel', 'block_panel_state');
          Shiny.setInputValue(stateInput, {
            mode: 'add',
            source_block: null,
            timestamp: Date.now()
          }, {priority: 'event'});
        }
        setTimeout(function() {
          var searchInput = panel.querySelector('.blockr-block-panel-search-input');
          if (searchInput) searchInput.focus();
        }, 100);
      }
    });
  }

  // Initialize mobile layout
  function initMobileLayout() {
    if (mobileInitialized) return;
    if (window.innerWidth > MOBILE_BREAKPOINT) return;

    var checkReady = setInterval(function() {
      var cardStack = document.querySelector('.mobile-card-stack');
      var offcanvas = document.querySelector('[id$="blocks_offcanvas"]');

      if (cardStack && offcanvas) {
        clearInterval(checkReady);
        renderMobileCards();
        setupMobileAddButton();
        mobileInitialized = true;
      }
    }, 100);

    setTimeout(function() { clearInterval(checkReady); }, 10000);
  }

  // Watch for block changes and update cards
  function observeBlockChanges() {
    var observer = new MutationObserver(function(mutations) {
      if (window.innerWidth > MOBILE_BREAKPOINT) return;
      if (isRenderingCards) return;

      // Check if any mutation involves block cards (not mobile UI elements)
      var blockChanged = mutations.some(function(m) {
        return Array.from(m.addedNodes).concat(Array.from(m.removedNodes)).some(function(n) {
          if (n.nodeType !== 1) return false;
          if (n.classList && (n.classList.contains('mobile-block-card') ||
              n.classList.contains('mobile-card-stack'))) {
            return false;
          }
          return n.id && n.id.indexOf('block_handle') !== -1;
        });
      });

      if (blockChanged) {
        clearTimeout(renderCardsTimeout);
        renderCardsTimeout = setTimeout(function() {
          isRenderingCards = true;
          renderMobileCards();
          setTimeout(function() { isRenderingCards = false; }, 200);
        }, 300);
      }
    });

    observer.observe(document.body, { childList: true, subtree: true });
  }

  // Initialize
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() {
      initMobileLayout();
      observeBlockChanges();
    });
  } else {
    initMobileLayout();
    observeBlockChanges();
  }

  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:connected', function() {
      setTimeout(function() {
        initMobileLayout();
        observeBlockChanges();
      }, 300);
    });
  }

  // Handle resize
  var resizeTimeout;
  var wasMobile = window.innerWidth <= MOBILE_BREAKPOINT;

  window.addEventListener('resize', function() {
    clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(function() {
      var isMobile = window.innerWidth <= MOBILE_BREAKPOINT;

      if (isMobile && !wasMobile) {
        mobileInitialized = false;
        lastRenderedCardState = '';
        initMobileLayout();
      } else if (isMobile) {
        renderMobileCards();
      }

      wasMobile = isMobile;
    }, 150);
  });

  // Shiny handler to update mobile cards when blocks change
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('blockr-update-mobile-tabs', function(msg) {
      if (window.innerWidth <= MOBILE_BREAKPOINT) {
        renderMobileCards();
      }
    });
  }

})();
