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
     MOBILE LAYOUT - Separate tab-based UI for mobile devices
     =========================================================================== */

  var MOBILE_BREAKPOINT = 900;
  var mobileActiveTab = null;
  var mobileInitialized = false;

  // Get all block cards from anywhere (offcanvas, dockview panels, or mobile content)
  function getBlockCards() {
    // Find all block cards in the document
    var allCards = document.querySelectorAll('.card[id*="block_handle"]');
    if (allCards.length === 0) return [];

    // Deduplicate by ID (in case of clones)
    var seen = {};
    var unique = [];
    allCards.forEach(function(card) {
      if (!seen[card.id]) {
        seen[card.id] = true;
        unique.push(card);
      }
    });
    return unique;
  }

  // Get block info from a card element
  function getBlockInfo(card) {
    var id = card.id;
    // Extract block ID from handle ID (format: ns-block_handle-xxx or ns-block_handle_xxx)
    var match = id.match(/block_handle[-_](.+)$/);
    var blockId = match ? match[1] : id;

    // Try to find the block name from the editable title input
    var title = null;
    var titleInput = card.querySelector('.inline-editable input[type="text"]');
    if (titleInput && titleInput.value) {
      title = titleInput.value.trim();
    }

    // Fallback: look for the displayed title text
    if (!title) {
      var titleSpan = card.querySelector('.inline-editable .editable-text, .inline-editable span:not(.edit-icon)');
      if (titleSpan) {
        title = titleSpan.textContent.trim();
      }
    }

    // Final fallback: use a clean version of the block ID
    if (!title || title.length > 50) {
      // Convert block_id like "deep_fireant" to "Deep Fireant"
      title = blockId.replace(/_/g, ' ').replace(/\b\w/g, function(c) { return c.toUpperCase(); });
    }

    return { id: blockId, fullId: id, title: title };
  }

  // Track last rendered state to avoid unnecessary re-renders
  var lastRenderedBlockIds = '';

  // Render tabs for all blocks
  function renderMobileTabs() {
    var tabBar = document.querySelector('.mobile-tab-bar');
    if (!tabBar) return;

    var cards = getBlockCards();

    // Check if anything changed to avoid flickering
    var currentIds = cards.map(function(c) { return c.id; }).join(',');
    if (currentIds === lastRenderedBlockIds && tabBar.children.length > 0) {
      return; // Nothing changed, skip re-render
    }
    lastRenderedBlockIds = currentIds;

    // Clear existing tabs
    tabBar.innerHTML = '';

    if (cards.length === 0) {
      // Show empty state
      var contentArea = document.querySelector('.mobile-content-area');
      if (contentArea) {
        contentArea.innerHTML = '<div class="mobile-empty-state">' +
          '<div class="mobile-empty-state-icon">📦</div>' +
          '<div class="mobile-empty-state-text">No blocks yet</div>' +
          '<div class="mobile-empty-state-hint">Tap + to add a block</div>' +
          '</div>';
      }
      return;
    }

    cards.forEach(function(card, index) {
      var info = getBlockInfo(card);
      var tab = document.createElement('button');
      tab.className = 'mobile-tab' + (index === 0 ? ' active' : '');
      tab.setAttribute('data-block-id', info.fullId);
      tab.textContent = info.title;

      tab.addEventListener('click', function() {
        selectMobileTab(info.fullId);
      });

      tabBar.appendChild(tab);
    });

    // Select first tab by default
    if (cards.length > 0 && !mobileActiveTab) {
      selectMobileTab(cards[0].id);
    } else if (mobileActiveTab) {
      // Re-select the active tab if it still exists
      var exists = cards.some(function(c) { return c.id === mobileActiveTab; });
      if (exists) {
        selectMobileTab(mobileActiveTab);
      } else {
        selectMobileTab(cards[0].id);
      }
    }
  }

  // Select a tab and show its content
  function selectMobileTab(blockId) {
    mobileActiveTab = blockId;

    // Update tab active state
    var tabs = document.querySelectorAll('.mobile-tab');
    tabs.forEach(function(tab) {
      tab.classList.toggle('active', tab.getAttribute('data-block-id') === blockId);
    });

    // Move the selected block to content area
    var contentArea = document.querySelector('.mobile-content-area');
    var card = document.getElementById(blockId);

    if (contentArea && card) {
      // Clear content area first
      contentArea.innerHTML = '';
      // Clone or move the card
      var clone = card.cloneNode(true);
      clone.style.display = 'block';
      contentArea.appendChild(clone);

      // Re-bind Shiny if needed
      if (typeof Shiny !== 'undefined' && Shiny.bindAll) {
        Shiny.bindAll(contentArea);
      }
    }
  }

  // Handle add block button on mobile
  function setupMobileAddButton() {
    var addBtn = document.querySelector('.mobile-add-btn');
    if (!addBtn) return;

    addBtn.addEventListener('click', function() {
      // Find and show the block panel directly
      var panel = document.querySelector('.blockr-block-panel');

      if (panel) {
        panel.classList.remove('blockr-block-panel-hidden');
        // Set the panel state for Shiny
        if (typeof Shiny !== 'undefined') {
          // Find the state input ID from the panel
          var stateInput = panel.id.replace('block_panel', 'block_panel_state');
          Shiny.setInputValue(stateInput, {
            mode: 'add',
            source_block: null,
            timestamp: Date.now()
          }, {priority: 'event'});
        }
        // Focus search input
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

    // Only initialize if we're on mobile
    if (window.innerWidth > MOBILE_BREAKPOINT) return;

    // Wait for DOM to be ready
    var checkReady = setInterval(function() {
      var tabBar = document.querySelector('.mobile-tab-bar');
      var offcanvas = document.querySelector('[id$="blocks_offcanvas"]');

      if (tabBar && offcanvas) {
        clearInterval(checkReady);
        renderMobileTabs();
        setupMobileAddButton();
        mobileInitialized = true;
      }
    }, 100);

    // Timeout after 10 seconds
    setTimeout(function() { clearInterval(checkReady); }, 10000);
  }

  // Watch for block changes and update tabs
  var renderTabsTimeout = null;
  var isRenderingTabs = false;

  function observeBlockChanges() {
    // Observe the entire body for block card changes
    var observer = new MutationObserver(function(mutations) {
      if (window.innerWidth > MOBILE_BREAKPOINT) return;
      if (isRenderingTabs) return; // Prevent re-entrancy

      // Check if any mutation involves block cards (not mobile UI elements)
      var blockChanged = mutations.some(function(m) {
        return Array.from(m.addedNodes).concat(Array.from(m.removedNodes)).some(function(n) {
          if (n.nodeType !== 1) return false;
          // Only trigger for actual block cards, not mobile UI
          if (n.classList && (n.classList.contains('mobile-tab') ||
              n.classList.contains('mobile-tab-bar') ||
              n.classList.contains('mobile-content-area'))) {
            return false;
          }
          return n.id && n.id.indexOf('block_handle') !== -1;
        });
      });

      if (blockChanged) {
        // Debounce with longer delay to prevent flickering
        clearTimeout(renderTabsTimeout);
        renderTabsTimeout = setTimeout(function() {
          isRenderingTabs = true;
          renderMobileTabs();
          setTimeout(function() { isRenderingTabs = false; }, 200);
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

  // Handle resize - reinitialize mobile when switching from desktop
  var resizeTimeout;
  var wasMobile = window.innerWidth <= MOBILE_BREAKPOINT;

  window.addEventListener('resize', function() {
    clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(function() {
      var isMobile = window.innerWidth <= MOBILE_BREAKPOINT;

      if (isMobile && !wasMobile) {
        // Switched from desktop to mobile - reinitialize
        mobileInitialized = false;
        mobileActiveTab = null;
        initMobileLayout();
      } else if (isMobile) {
        // Already mobile, just refresh tabs
        renderMobileTabs();
      }

      wasMobile = isMobile;
    }, 150);
  });

  // Shiny handler to update mobile tabs when blocks change
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('blockr-update-mobile-tabs', function(msg) {
      if (window.innerWidth <= MOBILE_BREAKPOINT) {
        renderMobileTabs();
      }
    });
  }

})();
