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

  // Get all block cards from anywhere (offcanvas, dockview panels, etc.)
  function getBlockCards() {
    // Find all block cards in the document
    var allCards = document.querySelectorAll('.card[id*="block_handle"]');
    if (allCards.length === 0) return [];

    // Deduplicate by ID and exclude clones in mobile card stack
    var seen = {};
    var unique = [];
    allCards.forEach(function(card) {
      // Skip cards that are inside the mobile card stack (our clones)
      if (card.closest('.mobile-card-stack')) return;
      // Skip cards that have the mobile-cloned-block class
      if (card.classList.contains('mobile-cloned-block')) return;

      if (!seen[card.id]) {
        seen[card.id] = true;
        unique.push(card);
      }
    });
    return unique;
  }

  // Get extension panels (DAG, etc.) from the extensions offcanvas or dockview
  function getExtensionPanels() {
    var extPanels = [];

    // Method 1: Look in extensions offcanvas
    var offcanvas = document.querySelector('[id$="exts_offcanvas"]');
    if (offcanvas) {
      var panels = offcanvas.querySelectorAll('[class*="ext_panel-"]');
      extPanels = Array.from(panels);
      console.log('[Mobile] Found', extPanels.length, 'extensions in offcanvas');
    }

    // Method 2: Look for G6 graph containers (DAG view)
    if (extPanels.length === 0) {
      var g6Containers = document.querySelectorAll('.g6');
      g6Containers.forEach(function(g6) {
        // Don't include if already in mobile card stack
        if (!g6.closest('.mobile-card-stack')) {
          var parent = g6.closest('[class*="ext_panel-"]') || g6.parentElement;
          if (parent && extPanels.indexOf(parent) === -1) {
            extPanels.push(parent);
          }
        }
      });
      console.log('[Mobile] Found', extPanels.length, 'extensions via G6');
    }

    return extPanels;
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

  // Get panel groups from dockview DOM structure
  function getDockviewGroups() {
    var groups = [];

    // First find the widget container, then look for the internal dockview structure
    var widgetContainer = document.querySelector('.dockview.html-widget') ||
                          document.querySelector('[class*="dockview"]');

    // Now find the actual dockview internal structure
    var dockview = null;
    if (widgetContainer) {
      dockview = widgetContainer.querySelector('.dv-dockview') ||
                 widgetContainer.querySelector('[class*="dv-"][class*="dockview"]');
    }

    // Fallback: search entire document
    if (!dockview) {
      dockview = document.querySelector('.dv-dockview') ||
                 document.querySelector('.blockr-board-view');
    }

    if (!dockview) {
      console.log('[Mobile] No dockview internal structure found');
      return groups;
    }

    console.log('[Mobile] Found dockview internal:', dockview.className);

    // Find all group containers
    var groupElements = dockview.querySelectorAll('.groupview, .dv-groupview');

    // If still none, search entire document for groupviews
    if (groupElements.length === 0) {
      groupElements = document.querySelectorAll('.groupview, .dv-groupview');
    }

    console.log('[Mobile] Found', groupElements.length, 'dockview groups');

    groupElements.forEach(function(groupEl, gIdx) {
      var group = { tabs: [], activeTabId: null };
      var seenTitles = {};

      // Find tabs container - try multiple selectors
      var tabsContainer = groupEl.querySelector('.tabs-container, .dv-tabs, [class*="tabs-container"], [class*="tabscontainer"]');

      // If no container, look for tabs directly in the group
      var tabs;
      if (tabsContainer) {
        tabs = Array.from(tabsContainer.querySelectorAll('.tab, .dv-tab')).filter(function(tab) {
          return tab.parentElement === tabsContainer;
        });
      } else {
        // Search for any tab-like elements in the group
        tabs = Array.from(groupEl.querySelectorAll('.tab, .dv-tab, [class*="tab"]:not([class*="content"]):not([class*="container"])')).filter(function(tab) {
          // Must have text content and not be nested too deep
          return tab.textContent.trim().length > 0 && tab.textContent.trim().length < 50;
        });
      }

      console.log('[Mobile] Group', gIdx, 'tabsContainer:', tabsContainer ? 'yes' : 'no', 'tabs:', tabs.length);

      // Debug: log group's class names and children
      if (tabs.length === 0) {
        console.log('[Mobile] Group', gIdx, 'classes:', groupEl.className);
        var children = Array.from(groupEl.children).map(function(c) { return c.className; });
        console.log('[Mobile] Group', gIdx, 'children:', children.join(', '));
      }

      tabs.forEach(function(tab) {
        var tabId = tab.getAttribute('data-panel-id') || '';
        var isActive = tab.classList.contains('active-tab');

        // Get tab title - look for the content span
        var titleEl = tab.querySelector('.dv-default-tab-content');
        var title = titleEl ? titleEl.textContent.trim() : '';

        // Skip tabs with empty titles or close buttons
        if (!title || title === '×' || title === 'x') return;

        // Skip duplicate titles
        if (seenTitles[title]) return;
        seenTitles[title] = true;

        console.log('[Mobile] Group', gIdx, 'tab:', title, 'id:', tabId, 'active:', isActive);

        group.tabs.push({
          id: tabId,
          title: title,
          isActive: isActive
        });

        if (isActive) {
          group.activeTabId = tabId;
        }
      });

      // If no active tab detected, default to first
      if (group.tabs.length > 0 && !group.activeTabId) {
        group.tabs[0].isActive = true;
        group.activeTabId = group.tabs[0].id;
      }

      if (group.tabs.length > 0) {
        groups.push(group);
      }
    });

    console.log('[Mobile] Total groups with tabs:', groups.length);
    return groups;
  }

  // Find block card by matching panel/tab ID or title
  function findBlockCardForTab(tabId, tabTitle, blockCards) {
    console.log('[Mobile] Finding block for tab:', tabId, 'title:', tabTitle);

    for (var i = 0; i < blockCards.length; i++) {
      var card = blockCards[i];
      var info = getBlockInfo(card);

      // Match by block ID (the tab ID often contains the block ID)
      var idMatch = tabId && (
        tabId.indexOf(info.id) !== -1 ||
        info.id.indexOf(tabId) !== -1 ||
        tabId === info.id ||
        tabId === info.fullId
      );

      // Match by title
      var titleMatch = tabTitle && info.title &&
        tabTitle.toLowerCase() === info.title.toLowerCase();

      if (idMatch || titleMatch) {
        console.log('[Mobile] Matched block:', info.title, 'via', idMatch ? 'ID' : 'title');
        return { card: card, info: info };
      }
    }

    console.log('[Mobile] No match found for tab:', tabId, tabTitle);
    return null;
  }

  // Render all panels as vertically stacked cards (mirrors dockview structure)
  function renderMobileCards() {
    var cardStack = document.querySelector('.mobile-card-stack');
    if (!cardStack) return;

    var blockCards = getBlockCards();
    var extPanels = getExtensionPanels();
    var dockGroups = getDockviewGroups();

    // Check if anything changed
    var currentState = blockCards.map(function(c) { return c.id; }).join(',') +
                       '|' + extPanels.length + '|' + dockGroups.length;
    if (currentState === lastRenderedCardState && cardStack.children.length > 0) {
      return; // Nothing changed
    }
    lastRenderedCardState = currentState;

    // Unbind Shiny before clearing
    if (typeof Shiny !== 'undefined' && Shiny.unbindAll) {
      Shiny.unbindAll(cardStack);
    }

    cardStack.innerHTML = '';

    // Show empty state if no blocks and no extensions
    if (blockCards.length === 0 && extPanels.length === 0) {
      cardStack.innerHTML = '<div class="mobile-empty-state">' +
        '<div class="mobile-empty-state-icon">📦</div>' +
        '<div class="mobile-empty-state-text">No blocks yet</div>' +
        '<div class="mobile-empty-state-hint">Tap + to add a block</div>' +
        '</div>';
      return;
    }

    // Track which blocks have been rendered (to handle ungrouped blocks)
    var renderedBlockIds = {};

    // Render each dockview group as a card
    dockGroups.forEach(function(group, groupIndex) {
      // Skip groups that only contain extensions (like "Workflow")
      var isExtensionGroup = group.tabs.some(function(t) {
        return t.title.toLowerCase() === 'workflow' || t.id.toLowerCase().indexOf('workflow') !== -1;
      });
      if (isExtensionGroup) return; // Extensions handled separately

      var mobileCard = document.createElement('div');
      mobileCard.className = 'mobile-block-card';
      mobileCard.setAttribute('data-group-index', groupIndex);

      // If group has multiple tabs, add tab bar
      if (group.tabs.length > 1) {
        var tabBar = document.createElement('div');
        tabBar.className = 'mobile-card-tabs';

        group.tabs.forEach(function(tab, tabIndex) {
          var tabBtn = document.createElement('button');
          tabBtn.className = 'mobile-card-tab' + (tab.isActive ? ' active' : '');
          tabBtn.setAttribute('data-tab-id', tab.id || tab.title);
          tabBtn.textContent = tab.title;
          tabBtn.addEventListener('click', function() {
            switchCardTab(mobileCard, tab.id || tab.title);
          });
          tabBar.appendChild(tabBtn);
        });

        mobileCard.appendChild(tabBar);
      } else if (group.tabs.length === 1) {
        // Single tab - show header with title
        var header = document.createElement('div');
        header.className = 'mobile-card-header';
        header.innerHTML = '<span class="mobile-card-title">' + escapeHtml(group.tabs[0].title) + '</span>';
        mobileCard.appendChild(header);
      }

      // Card body with content for each tab
      var body = document.createElement('div');
      body.className = 'mobile-card-body';

      group.tabs.forEach(function(tab) {
        var match = findBlockCardForTab(tab.id, tab.title, blockCards);
        if (match) {
          renderedBlockIds[match.card.id] = true;

          var tabContent = document.createElement('div');
          tabContent.className = 'mobile-card-tab-content' + (tab.isActive ? ' active' : '');
          tabContent.setAttribute('data-tab-id', tab.id || tab.title);

          var clone = match.card.cloneNode(true);
          clone.style.display = 'block';
          clone.classList.add('mobile-cloned-block');
          tabContent.appendChild(clone);

          body.appendChild(tabContent);
        }
      });

      mobileCard.appendChild(body);

      if (body.children.length > 0) {
        cardStack.appendChild(mobileCard);
      }
    });

    // Render any blocks not in dockview groups (fallback)
    blockCards.forEach(function(originalCard) {
      if (renderedBlockIds[originalCard.id]) return;

      var info = getBlockInfo(originalCard);

      var mobileCard = document.createElement('div');
      mobileCard.className = 'mobile-block-card';
      mobileCard.setAttribute('data-block-id', info.fullId);

      var header = document.createElement('div');
      header.className = 'mobile-card-header';
      header.innerHTML = '<span class="mobile-card-title">' + escapeHtml(info.title) + '</span>';
      mobileCard.appendChild(header);

      var body = document.createElement('div');
      body.className = 'mobile-card-body';

      var clone = originalCard.cloneNode(true);
      clone.style.display = 'block';
      clone.classList.add('mobile-cloned-block');
      body.appendChild(clone);

      mobileCard.appendChild(body);
      cardStack.appendChild(mobileCard);
    });

    // Render extension panels as cards (e.g., DAG/Workflow view)
    extPanels.forEach(function(extPanel, index) {
      var mobileCard = document.createElement('div');
      mobileCard.className = 'mobile-block-card mobile-extension-card';

      var title = 'Workflow';
      var extClass = Array.from(extPanel.classList).find(function(c) {
        return c.indexOf('ext_panel-') !== -1;
      });
      if (extClass) {
        var extName = extClass.replace('ext_panel-', '').replace(/_/g, ' ');
        title = extName.replace(/\b\w/g, function(c) { return c.toUpperCase(); });
      }

      var header = document.createElement('div');
      header.className = 'mobile-card-header';
      header.innerHTML = '<span class="mobile-card-title">' + escapeHtml(title) + '</span>';
      mobileCard.appendChild(header);

      var body = document.createElement('div');
      body.className = 'mobile-card-body mobile-extension-body';

      var clone = extPanel.cloneNode(true);
      clone.style.display = 'block';
      body.appendChild(clone);

      // Add view-only notice
      var notice = document.createElement('div');
      notice.className = 'mobile-extension-notice';
      notice.textContent = 'View only on mobile';
      body.appendChild(notice);

      mobileCard.appendChild(body);
      cardStack.appendChild(mobileCard);
    });

    // Rebind Shiny
    if (typeof Shiny !== 'undefined' && Shiny.bindAll) {
      Shiny.bindAll(cardStack);
    }
  }

  // Switch between tabs within a card
  function switchCardTab(card, tabId) {
    // Update tab buttons
    card.querySelectorAll('.mobile-card-tab').forEach(function(btn) {
      btn.classList.toggle('active', btn.getAttribute('data-tab-id') === tabId);
    });

    // Update tab content visibility
    card.querySelectorAll('.mobile-card-tab-content').forEach(function(content) {
      content.classList.toggle('active', content.getAttribute('data-tab-id') === tabId);
    });
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
