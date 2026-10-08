(function () {
  "use strict";

  // The block browser is a Shiny input: its value is the block the user
  // chose to add. Adding is an event (click a card, or the in-card add
  // button), so the value carries a monotonically increasing `nonce`
  // that guarantees Shiny sees a change and re-fires even when the same
  // block is added twice. R reads it as `input[[<root id>]]` and ignores
  // the nonce, as it does for a pick from the block actions' menu, which
  // writes the same input.
  var COMMIT_EVENT = "blockr-block-browser:commit";
  var commitSeq = 0;

  // Card-list helpers shared by the block browser and the link and stack
  // menus. All three render the same `.blockr-block-browser-card` markup
  // and the same `data-name` / `data-description` / `data-package` /
  // `data-category` search contract, so the filter and the card-iteration
  // helper live on a tiny `window.BlockrDock.cardSearch` namespace. The
  // menus depend on `block_browser_dep()` being attached first (which it
  // is wherever `link_menu_ui()` or `stack_menu_ui()` is rendered) and
  // just call into this API. Keep the surface deliberately small.
  var BlockrDock = window.BlockrDock = window.BlockrDock || {};
  BlockrDock.cardSearch = BlockrDock.cardSearch || {
    getCards: function (root) {
      return Array.prototype.slice.call(
        root.querySelectorAll(".blockr-block-browser-card")
      );
    },
    applySearch: function (root, query) {
      var q = (query || "").trim().toLowerCase();
      var anyVisible = false;
      BlockrDock.cardSearch.getCards(root).forEach(function (card) {
        if (q.length === 0) {
          card.classList.remove("hidden");
          anyVisible = true;
          return;
        }
        var haystack = [
          card.getAttribute("data-name") || "",
          card.getAttribute("data-description") || "",
          card.getAttribute("data-package") || "",
          card.getAttribute("data-category") || ""
        ]
          .join(" ")
          .toLowerCase();
        var hit = haystack.indexOf(q) !== -1;
        card.classList.toggle("hidden", !hit);
        if (hit) anyVisible = true;
      });
      root.classList.toggle("is-empty", !anyVisible);
    }
  };

  // Shared structural reconciler for the instance-backed menus (stack,
  // link). Given a `.blockr-block-browser-categories` container and the
  // full desired card set for it (`[{ id, html }, ...]`, each `html` the
  // server-rendered markup), it removes cards no longer desired, inserts
  // desired cards not yet in the DOM into the matching category section
  // (creating the section when absent), and drops emptied category
  // sections. It does NOT touch eligibility / selection / search /
  // empty-state - callers retune those after, so a board change never
  // disturbs scroll, expansion, or in-progress input. The link menu
  // calls it once per direction container; the stack menu has a single
  // container.
  function cssEscapeAttr(s) {
    return String(s).replace(/["\\]/g, "\\$&");
  }
  function parseCardHtml(html) {
    var tmp = document.createElement("div");
    tmp.innerHTML = String(html).trim();
    return tmp.firstElementChild;
  }
  function insertCardNode(cats, category, node) {
    var sec = cats.querySelector(
      '.blockr-block-browser-category[data-category="' +
        cssEscapeAttr(category) + '"]'
    );
    if (sec) {
      (sec.querySelector(".blockr-block-browser-cards") || sec)
        .appendChild(node);
      return;
    }
    sec = document.createElement("div");
    sec.className = "blockr-block-browser-category";
    sec.setAttribute("data-category", category);
    var h = document.createElement("h3");
    h.textContent = category;
    var list = document.createElement("div");
    list.className = "blockr-block-browser-cards";
    list.appendChild(node);
    sec.appendChild(h);
    sec.appendChild(list);
    cats.appendChild(sec);
  }
  BlockrDock.cardSync = BlockrDock.cardSync || function (cats, cards) {
    if (!cats) return;
    // `sendInputMessage` auto-unboxes a length-1 list to a scalar.
    if (!cards) cards = [];
    if (!Array.isArray(cards)) cards = [cards];

    var desired = {};
    cards.forEach(function (c) {
      if (c && c.id != null) desired[c.id] = c;
    });

    Array.prototype.slice
      .call(cats.querySelectorAll(".blockr-block-browser-card"))
      .forEach(function (card) {
        var id = card.getAttribute("data-block-type");
        if (!Object.prototype.hasOwnProperty.call(desired, id)) {
          if (card.parentNode) card.parentNode.removeChild(card);
        }
      });

    cards.forEach(function (c) {
      if (!c || c.id == null || !c.html) return;
      var sel = '.blockr-block-browser-card[data-block-type="' +
        cssEscapeAttr(c.id) + '"]';
      if (cats.querySelector(sel)) return;
      var node = parseCardHtml(c.html);
      if (node) {
        insertCardNode(cats, node.getAttribute("data-category") || "", node);
      }
    });

    Array.prototype.slice
      .call(cats.querySelectorAll(".blockr-block-browser-category"))
      .forEach(function (sec) {
        if (!sec.querySelector(".blockr-block-browser-card")) {
          if (sec.parentNode) sec.parentNode.removeChild(sec);
        }
      });
  };

  // Cards currently shown (not filtered out by search).
  function visibleCards(root) {
    return BlockrDock.cardSearch.getCards(root).filter(function (card) {
      return !card.classList.contains("hidden");
    });
  }

  // Move the keyboard-selection highlight to `card` (or clear it when
  // null) and scroll it into view. Selection is a purely visual marker
  // - `.card-selected` - distinct from the chevron's `.card-expanded`.
  function selectCard(root, card) {
    BlockrDock.cardSearch.getCards(root).forEach(function (c) {
      c.classList.toggle("card-selected", c === card);
    });
    if (card) card.scrollIntoView({ block: "nearest" });
  }

  // Fields are rendered only for the flows that use them, so an absent
  // field simply means "not applicable" -> null. An empty value is also
  // reported as null.
  function getFieldValue(card, fieldClass) {
    var el = card.querySelector(
      "." + fieldClass + " input, ." + fieldClass + " select"
    );
    if (!el) return null;
    return el.value === "" ? null : el.value;
  }

  function gatherSpec(card) {
    var field = function (name) {
      return getFieldValue(card, "blockr-block-browser-field-" + name);
    };
    return {
      type: card.getAttribute("data-block-type"),
      id: field("id"),
      title: field("title"),
      link_id: field("link-id"),
      near_link_id: field("near-link-id"),
      far_link_id: field("far-link-id"),
      block_input: field("block-input"),
      target_input: field("target-input"),
      nonce: ++commitSeq
    };
  }

  // Record the chosen block on the root and fire the commit event; the
  // binding's `subscribe` callback then has Shiny read `getValue`.
  function commitCard(root, card) {
    root._blockrBrowserValue = gatherSpec(card);
    root.dispatchEvent(new CustomEvent(COMMIT_EVENT));
  }

  // Unfold or fold a card's form.
  function toggleCard(card) {
    var open = card.classList.toggle("card-expanded");
    var chevron = card.querySelector(".blockr-block-browser-card-chevron");
    if (chevron) chevron.setAttribute("aria-expanded", open ? "true" : "false");
  }

  // Wire the search box and the delegated card-area click handler.
  // Idempotent: safe to call from both initialize() and subscribe()
  // (Shiny may invoke either first).
  function initBrowser(root) {
    if (root.dataset.blockrBlockBrowserInit === "1") return;
    root.dataset.blockrBlockBrowserInit = "1";

    var search = root.querySelector(".blockr-block-browser-search");
    if (search) {
      var filter = function () {
        BlockrDock.cardSearch.applySearch(root, search.value);
        // Keep a valid card highlighted so Enter adds the top hit; drop
        // the highlight if the previously selected card was filtered out.
        var current = root.querySelector(
          ".blockr-block-browser-card.card-selected"
        );
        if (!current || current.classList.contains("hidden")) {
          selectCard(root, visibleCards(root)[0] || null);
        }
      };
      search.addEventListener("input", filter);
      // What was typed in the menu the browser was opened from.
      if (search.value) filter();

      // Arrow keys move the highlight, Enter adds it. Gated to the search
      // box so arrow keys still edit text inside an expanded card's form.
      root.addEventListener("keydown", function (event) {
        if (event.target !== search) return;

        var cards = visibleCards(root);
        if (cards.length === 0) return;

        var current = root.querySelector(
          ".blockr-block-browser-card.card-selected"
        );
        var idx = current ? cards.indexOf(current) : -1;

        if (event.key === "ArrowDown") {
          event.preventDefault();
          selectCard(root, cards[idx < cards.length - 1 ? idx + 1 : 0]);
        } else if (event.key === "ArrowUp") {
          event.preventDefault();
          selectCard(root, cards[idx > 0 ? idx - 1 : cards.length - 1]);
        } else if (event.key === "Enter") {
          event.preventDefault();
          commitCard(root, current || cards[0]);
        }
      });
    }

    var cardsArea = root.querySelector(".blockr-block-browser-categories");
    if (cardsArea) {
      cardsArea.addEventListener("click", function (event) {
        var card = event.target.closest(".blockr-block-browser-card");
        if (!card || !root.contains(card)) return;

        if (event.target.closest(".blockr-block-browser-card-chevron")) {
          toggleCard(card);
          event.preventDefault();
          return;
        }
        if (event.target.closest(".blockr-block-browser-card-add")) {
          commitCard(root, card);
          event.preventDefault();
          return;
        }
        if (event.target.closest(".blockr-block-browser-card-advanced")) {
          return;
        }
        commitCard(root, card);
      });
    }
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-block-browser");
    },
    initialize: function (el) {
      initBrowser(el);
    },
    getValue: function (el) {
      return el._blockrBrowserValue || null;
    },
    subscribe: function (el, callback) {
      initBrowser(el);
      // Shiny's callback takes no args; addEventListener passes the
      // Event, which breaks the value-update path - wrap to drop it.
      var handler = function () { callback(); };
      el._blockrBrowserHandler = handler;
      el.addEventListener(COMMIT_EVENT, handler);
    },
    unsubscribe: function (el) {
      if (el._blockrBrowserHandler) {
        el.removeEventListener(COMMIT_EVENT, el._blockrBrowserHandler);
        el._blockrBrowserHandler = null;
      }
    }
  });

  Shiny.inputBindings.register(binding, "blockr.dock.blockBrowser");
})();
