// =============================================================================
// Blockr Sidebar Module
// =============================================================================
// Single sidebar container with dynamic S3-dispatched content.
// show_sidebar(id) triggers renderUI which dispatches to sidebar_content_ui.*

(function() {
  'use strict';

  // ===========================================================================
  // Global Sidebar Controller
  // ===========================================================================

  window.blockrSidebar = {
    /**
     * Show the sidebar
     * Content is rendered dynamically via Shiny renderUI
     */
    show: function() {
      var sidebar = document.querySelector('.blockr-sidebar');
      if (!sidebar) return;

      // Show the sidebar
      sidebar.classList.remove('blockr-sidebar-hidden');

      // Trigger input binding update
      $(document).trigger('blockr-sidebar-change');
    },

    /**
     * Hide the sidebar
     */
    hide: function() {
      var sidebar = document.querySelector('.blockr-sidebar');
      if (sidebar) {
        sidebar.classList.add('blockr-sidebar-hidden');
      }

      // Clear keyboard selection tracking
      selectedBlockId = null;

      $(document).trigger('blockr-sidebar-change');
    },

    /**
     * Update sidebar metadata (title, subtitle, search visibility)
     * Called from R via session$sendCustomMessage
     */
    updateMeta: function(data) {
      var sidebar = document.querySelector('.blockr-sidebar');
      if (!sidebar) return;

      // Update title
      var titleEl = sidebar.querySelector('.blockr-sidebar-title');
      if (titleEl) {
        titleEl.textContent = data.title || '';
      }

      // Update subtitle
      var subtitleEl = sidebar.querySelector('.blockr-sidebar-subtitle');
      if (subtitleEl) {
        subtitleEl.textContent = data.subtitle || '';
        subtitleEl.style.display = data.subtitle ? '' : 'none';
      }

      // Show/hide search
      var searchContainer = sidebar.querySelector('[id$="sidebar_search_container"]');
      if (searchContainer) {
        var showSearch = data.show_search === true;
        searchContainer.style.display = showSearch ? '' : 'none';

        // Clear and focus search if shown
        if (showSearch) {
          var searchInput = searchContainer.querySelector('.blockr-sidebar-search-input');
          if (searchInput) {
            searchInput.value = '';
            // Trigger input event to clear any filters
            searchInput.dispatchEvent(new Event('input', { bubbles: true }));
            setTimeout(function() {
              searchInput.focus();
            }, 100);
          }
        }
      }
    },

    /**
     * Check if sidebar is open
     * @returns {boolean}
     */
    isOpen: function() {
      var sidebar = document.querySelector('.blockr-sidebar');
      return sidebar && !sidebar.classList.contains('blockr-sidebar-hidden');
    }
  };

  // ===========================================================================
  // Keyboard Navigation State
  // ===========================================================================

  var selectedBlockId = null;

  // ===========================================================================
  // Keyboard Navigation (consolidated handler)
  // ===========================================================================

  document.addEventListener('keydown', function(e) {
    // Escape closes sidebar from anywhere
    if (e.key === 'Escape' && blockrSidebar.isOpen()) {
      blockrSidebar.hide();
      return;
    }

    // Arrow keys and Enter only work when focused on search input
    if (!e.target.classList.contains('blockr-sidebar-search-input')) return;

    var sidebar = e.target.closest('.blockr-sidebar');
    if (!sidebar) return;

    var cards = Array.from(
      sidebar.querySelectorAll('.blockr-block-card:not([style*="display: none"])')
    );
    if (cards.length === 0) return;

    var selected = sidebar.querySelector('.blockr-block-card.blockr-card-selected');
    var selectedIndex = selected ? cards.indexOf(selected) : -1;
    var newCard = null;

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (selected) selected.classList.remove('blockr-card-selected');
      var nextIndex = selectedIndex < cards.length - 1 ? selectedIndex + 1 : 0;
      newCard = cards[nextIndex];
      newCard.classList.add('blockr-card-selected');
      newCard.scrollIntoView({ block: 'nearest' });
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (selected) selected.classList.remove('blockr-card-selected');
      var prevIndex = selectedIndex > 0 ? selectedIndex - 1 : cards.length - 1;
      newCard = cards[prevIndex];
      newCard.classList.add('blockr-card-selected');
      newCard.scrollIntoView({ block: 'nearest' });
    } else if (e.key === 'Enter') {
      e.preventDefault();
      var card = selected || cards[0];
      if (card) {
        var main = card.querySelector('.blockr-block-card-main');
        if (main) {
          main.click();
        } else {
          card.click();
        }
      }
    }

    // Update selectedBlockId for arrow key navigation
    if (newCard) {
      selectedBlockId = newCard.dataset.blockId || null;
    }
  });

  // ===========================================================================
  // MutationObserver for Content Changes
  // ===========================================================================

  // Track observed elements for cleanup
  var observedContents = new WeakSet();

  var contentObserver = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      var sidebar = mutation.target.closest('.blockr-sidebar');
      if (sidebar && !sidebar.classList.contains('blockr-sidebar-hidden')) {
        // Try to restore previous selection by block ID
        if (selectedBlockId) {
          var card = sidebar.querySelector(
            '.blockr-block-card[data-block-id="' + selectedBlockId + '"]'
          );
          if (card && !card.style.display.includes('none')) {
            card.classList.add('blockr-card-selected');
            return;
          }
        }

        // Auto-select first card if none selected
        var existing = sidebar.querySelector('.blockr-card-selected');
        if (!existing) {
          var firstCard = sidebar.querySelector(
            '.blockr-block-card:not([style*="display: none"])'
          );
          if (firstCard) {
            firstCard.classList.add('blockr-card-selected');
            selectedBlockId = firstCard.dataset.blockId || null;
          }
        }
      }
    });
  });

  function observeSidebarContent(content) {
    if (!observedContents.has(content)) {
      contentObserver.observe(content, { childList: true, subtree: true });
      observedContents.add(content);
    }
  }

  function observeSidebarContents() {
    document.querySelectorAll('.blockr-sidebar-content').forEach(observeSidebarContent);
  }

  var bodyObserver = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      // Handle added nodes - observe new sidebar content elements
      mutation.addedNodes.forEach(function(node) {
        if (node.nodeType === 1) {
          var contents = node.querySelectorAll ?
            node.querySelectorAll('.blockr-sidebar-content') : [];
          contents.forEach(observeSidebarContent);
        }
      });

      // Handle removed nodes - disconnect is automatic via WeakSet
      // (contentObserver stops observing elements that are garbage collected)
    });
  });

  // Cleanup function for Shiny session end
  function cleanupObservers() {
    contentObserver.disconnect();
    bodyObserver.disconnect();
  }

  $(function() {
    observeSidebarContents();
    bodyObserver.observe(document.body, { childList: true, subtree: true });
  });

  // Cleanup on Shiny session end
  $(document).on('shiny:disconnected', cleanupObservers);

  // ===========================================================================
  // Event Delegation for Sidebar Actions
  // ===========================================================================
  // All interactive elements use data-action attributes instead of inline onclick.
  // This centralizes JS logic and enables keyboard navigation.

  document.addEventListener('click', function(e) {
    // Find the element with data-action (might be the target or an ancestor)
    var actionEl = e.target.closest('[data-action]');
    if (!actionEl) return;

    // Only handle actions within the sidebar
    var sidebar = actionEl.closest('.blockr-sidebar');
    if (!sidebar) return;

    var action = actionEl.dataset.action;

    switch (action) {
      // Card click - sends input value with data fields
      case 'card-click':
        handleCardClick(actionEl);
        break;

      // Accordion confirm button - sends input value with data fields
      case 'card-confirm':
        handleCardClick(actionEl);
        break;

      // Toggle accordion open/close
      case 'toggle-accordion':
        e.stopPropagation();
        var cardId = actionEl.dataset.cardId;
        var card = document.getElementById(cardId);
        if (card) {
          var isOpen = card.classList.toggle('is-open');
          // Update aria-expanded
          actionEl.setAttribute('aria-expanded', isOpen ? 'true' : 'false');
        }
        break;

      // Toggle class on self (e.g., stack footer options, settings categories)
      case 'toggle-class':
        var toggleClass = actionEl.dataset.toggleClass || 'is-open';
        var targetSelector = actionEl.dataset.target;
        var target = targetSelector
          ? actionEl.closest(targetSelector)
          : actionEl;
        if (target) {
          var wasOpen = target.classList.toggle(toggleClass);
          // Update aria-expanded if present
          if (actionEl.hasAttribute('aria-expanded')) {
            actionEl.setAttribute('aria-expanded', wasOpen ? 'true' : 'false');
          }
        }
        break;

      // Stack card multi-select toggle
      case 'stack-card-toggle':
        handleStackCardToggle(actionEl);
        break;

      // Simple confirm button (just sends timestamp)
      case 'confirm':
        var inputId = actionEl.dataset.inputId;
        if (inputId) {
          Shiny.setInputValue(inputId, Date.now(), {priority: 'event'});
        }
        break;
    }
  });

  /**
   * Handle card click - collects all data-* attributes and sends to Shiny
   */
  function handleCardClick(el) {
    var inputId = el.dataset.inputId;
    if (!inputId) return;

    // Collect all data-* attributes except action and inputId
    var data = {timestamp: Date.now()};
    for (var key in el.dataset) {
      if (key !== 'action' && key !== 'inputId') {
        data[key] = el.dataset[key];
      }
    }

    Shiny.setInputValue(inputId, data, {priority: 'event'});
  }

  /**
   * Handle stack card multi-select toggle
   */
  function handleStackCardToggle(card) {
    var inputId = card.dataset.inputId;
    var blockId = card.dataset.blockId;
    if (!inputId || !blockId) return;

    // Find the hidden input that stores selected IDs
    var input = document.getElementById(inputId);
    if (!input) return;

    // Parse current selection
    var ids = input.value ? input.value.split(',').filter(function(x) {
      return x;
    }) : [];

    // Toggle this block
    var idx = ids.indexOf(blockId);
    if (idx > -1) {
      ids.splice(idx, 1);
      card.classList.remove('is-selected');
    } else {
      ids.push(blockId);
      card.classList.add('is-selected');
    }

    // Update hidden input and notify Shiny
    input.value = ids.join(',');
    Shiny.setInputValue(inputId, ids, {priority: 'event'});
  }

  // ===========================================================================
  // Shiny Custom Message Handlers
  // ===========================================================================

  Shiny.addCustomMessageHandler('blockr-sidebar', function(message) {
    if (message.action === 'show') {
      blockrSidebar.show();
    } else if (message.action === 'hide') {
      blockrSidebar.hide();
    }
  });

  // Handler for updating sidebar metadata (title, subtitle, search)
  Shiny.addCustomMessageHandler('blockr-sidebar-meta', function(message) {
    blockrSidebar.updateMeta(message);
  });

  // ===========================================================================
  // Shiny Input Binding
  // ===========================================================================

  var sidebarBinding = new Shiny.InputBinding();

  $.extend(sidebarBinding, {
    find: function(scope) {
      return $(scope).find('.blockr-sidebar-container');
    },

    initialize: function(el) {
      // Initial state (sidebar is closed)
    },

    getValue: function(el) {
      return blockrSidebar.isOpen() ? 'open' : null;
    },

    setValue: function(el, value) {
      if (value) {
        blockrSidebar.show();
      } else {
        blockrSidebar.hide();
      }
    },

    subscribe: function(el, callback) {
      $(document).on('blockr-sidebar-change.sidebarBinding', callback);
    },

    unsubscribe: function(el) {
      $(document).off('.sidebarBinding');
    }
  });

  Shiny.inputBindings.register(sidebarBinding, 'blockr.sidebar');

})();
