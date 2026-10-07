(function () {
  "use strict";

  // Card-list helpers shared by the link and stack menus. Both render
  // the same `.blockr-block-browser-card` markup and the same
  // `data-name` / `data-description` / `data-package` / `data-category`
  // search contract, so the filter and the card-iteration helper live
  // on a tiny `window.BlockrDock.cardSearch` namespace. Both menus
  // depend on `block_browser_dep()` being attached first (which it is
  // wherever `link_menu_ui()` or `stack_menu_ui()` is rendered) and
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
})();
