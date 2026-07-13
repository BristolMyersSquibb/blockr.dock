(function () {
  "use strict";

  // The link menu is a Shiny input: its value is the link the user
  // chose to add (one click = one link). The anchor block lives on
  // the root via `data-anchor`; each card carries `data-direction`
  // ("outgoing" or "incoming"); the JS composes source / target on
  // commit so the consumer doesn't have to know the orientation.
  // The binding also accepts a `pool-update` `receiveMessage` so the
  // consumer can keep the menu open across multiple commits and push
  // live eligibility updates without re-rendering.
  var COMMIT_EVENT = "blockr-link-menu:commit";
  var commitSeq = 0;
  var seedCounter = 0;

  // Reuse the search helper shipped by blockr-block-browser.js.
  // `link_menu_ui()` always attaches block_browser_dep() before
  // link_menu_dep(), so the namespace is in scope by the time this
  // binding runs.
  var cardSearch = window.BlockrUI.cardSearch;
  var cardSync = window.BlockrUI.cardSync;

  function getField(card, fieldClass) {
    return card.querySelector(
      "." + fieldClass + " input, ." + fieldClass + " select"
    );
  }

  // Absent fields (mode-inapplicable, e.g. block_input on an arity-1
  // target) are reported as null so the R side can fall back.
  function getFieldValue(card, fieldClass) {
    var el = getField(card, fieldClass);
    if (!el) return null;
    var v = el.value;
    return v === "" ? null : v;
  }

  function gatherSpec(root, card) {
    var direction = card.getAttribute("data-direction");
    var anchor = root.getAttribute("data-anchor");
    var other = card.getAttribute("data-block-type");
    var spec = {
      source: direction === "outgoing" ? anchor : other,
      target: direction === "outgoing" ? other : anchor,
      link_id: getFieldValue(card, "blockr-block-browser-field-link-id"),
      block_input: getFieldValue(
        card, "blockr-block-browser-field-block-input"
      ),
      nonce: ++commitSeq
    };
    return spec;
  }

  function commitCard(root, card) {
    root._blockrLinkMenuValue = gatherSpec(root, card);
    root.dispatchEvent(new CustomEvent(COMMIT_EVENT));
  }

  // Live pool update: hide cards whose data-block-type is no longer
  // in the eligible pool for their data-direction; refresh each
  // visible card's block-input <select> options from the per-target
  // free_inputs map; recompute the panel's `is-empty` state; re-seed
  // per-card link-id defaults from `link_id_seed`. Card-expansion
  // state, scroll position, and the search input value are
  // intentionally untouched.
  // Shiny's `sendInputMessage` auto-unboxes length-1 R character
  // vectors to JSON scalars ("y" rather than ["y"]), which breaks the
  // array iteration / membership checks below. Normalize defensively.
  function asArray(v) {
    if (v === null || v === undefined) return [];
    if (Array.isArray(v)) return v;
    return [v];
  }

  function applyPoolUpdate(root, payload) {
    var elig = payload.eligible || {};
    var outgoing = asArray(elig.outgoing);
    var incoming = asArray(elig.incoming);
    var freeInputs = payload.free_inputs || {};
    var anchor = root.getAttribute("data-anchor");
    var anyVisible = false;
    cardSearch.getCards(root).forEach(function (card) {
      var dir = card.getAttribute("data-direction");
      var id = card.getAttribute("data-block-type");
      var pool = dir === "outgoing" ? outgoing : incoming;
      var keep = pool.indexOf(id) !== -1;
      card.classList.toggle("hidden", !keep);
      if (keep) anyVisible = true;

      // Refresh the card's block-input <select> options. For
      // OUTGOING the target is the card; for INCOMING it's the
      // anchor. If the target has 0 or 1 free named ports the picker
      // is hidden / forced and the select element isn't present in
      // the DOM at all - nothing to refresh in that case.
      var targetId = dir === "outgoing" ? id : anchor;
      if (Object.prototype.hasOwnProperty.call(freeInputs, targetId)) {
        refreshBlockInputSelect(card, asArray(freeInputs[targetId]));
      }
    });
    root.classList.toggle("is-empty", !anyVisible);

    if (payload.link_id_seed) {
      reseedLinkIds(root, payload.link_id_seed);
    }
  }

  // Rebuild the card's block-input <select> options from `inputs`,
  // then hide the field entirely when the post-update pool collapses
  // to <= 1 options - a single-option dropdown is useless chrome.
  // The select still carries the (sole) free port so a card-body
  // click commits the right value via `getFieldValue`. No-op when
  // the card doesn't render a picker (the field wrapper was omitted
  // at render time for single-input / variadic targets).
  function refreshBlockInputSelect(card, inputs) {
    var field = card.querySelector(
      ".blockr-block-browser-field-block-input"
    );
    if (!field) return;
    var select = field.querySelector("select");
    if (select) {
      var prev = select.value;
      select.innerHTML = "";
      inputs.forEach(function (opt) {
        var el = document.createElement("option");
        el.value = opt;
        el.textContent = opt;
        select.appendChild(el);
      });
      if (inputs.length > 0) {
        if (inputs.indexOf(prev) !== -1) {
          select.value = prev;
        } else {
          select.value = inputs[0];
        }
      }
    }
    field.style.display = inputs.length > 1 ? "" : "none";
  }

  // Re-seed per-card link-id defaults from a fresh rand_names() value
  // pushed by the server. Skip fields the user has edited (marked via
  // a `data-edited` attribute set on the first user-driven input
  // event). Append a monotonic suffix so two cards reading from the
  // same seed propose distinct ids.
  function reseedLinkIds(root, seed) {
    var inputs = root.querySelectorAll(
      ".blockr-block-browser-field-link-id input"
    );
    Array.prototype.forEach.call(inputs, function (el) {
      if (el.dataset.edited === "1") return;
      seedCounter += 1;
      el.value = seed + "_" + seedCounter;
    });
  }

  // `menu:sync` is the board-driven superset of `pool-update`: where
  // pool-update only toggles cards already in the DOM, menu:sync can
  // also ADD cards (a block that became eligible after a link / block
  // was removed elsewhere) and REMOVE ones that vanished, then refresh
  // ports + re-seed ids. The full desired card set (with server-rendered
  // `html`) arrives in `payload.cards`, each tagged with its direction.
  // Card markup is authored once in R; the JS only places it.
  function ensureDirectionCats(wrap, dir, hasCards) {
    var sec = wrap.querySelector(
      '.blockr-link-menu-direction[data-direction="' + dir + '"]'
    );
    if (sec) return sec.querySelector(".blockr-block-browser-categories");
    if (!hasCards) return null;
    sec = document.createElement("div");
    sec.className = "blockr-link-menu-direction";
    sec.setAttribute("data-direction", dir);
    var h = document.createElement("h4");
    h.className = "blockr-link-menu-section-header";
    h.textContent = dir === "incoming" ? "Input from" : "Output to";
    var cats = document.createElement("div");
    cats.className = "blockr-block-browser-categories";
    sec.appendChild(h);
    sec.appendChild(cats);
    // INCOMING renders above OUTGOING (left-to-right data flow).
    if (dir === "incoming") {
      wrap.insertBefore(sec, wrap.firstChild);
    } else {
      wrap.appendChild(sec);
    }
    return cats;
  }

  function applyMenuSync(root, payload) {
    var cards = asArray(payload.cards);
    var wrap = root.querySelector(".blockr-link-menu-directions");
    if (!wrap) return;

    ["incoming", "outgoing"].forEach(function (dir) {
      var dirCards = cards.filter(function (c) {
        return c && c.direction === dir;
      });
      var cats = ensureDirectionCats(wrap, dir, dirCards.length > 0);
      if (!cats) return;
      cardSync(cats, dirCards);
      // Drop the whole direction section once its last card is gone.
      if (!cats.querySelector(".blockr-block-browser-card")) {
        var sec = cats.closest(".blockr-link-menu-direction");
        if (sec && sec.parentNode) sec.parentNode.removeChild(sec);
      }
    });

    var freeInputs = payload.free_inputs || {};
    var anchor = root.getAttribute("data-anchor");
    cardSearch.getCards(root).forEach(function (card) {
      var dir = card.getAttribute("data-direction");
      var id = card.getAttribute("data-block-type");
      var targetId = dir === "outgoing" ? id : anchor;
      if (Object.prototype.hasOwnProperty.call(freeInputs, targetId)) {
        refreshBlockInputSelect(card, asArray(freeInputs[targetId]));
      }
    });

    if (payload.link_id_seed) {
      reseedLinkIds(root, payload.link_id_seed);
    }

    var search = root.querySelector(".blockr-block-browser-search");
    cardSearch.applySearch(root, search ? search.value : "");
  }

  // Mark a link-id input as user-edited so subsequent pool-update
  // pushes leave it alone.
  function markEditedOnInput(root) {
    root.addEventListener("input", function (event) {
      var t = event.target;
      if (!t) return;
      if (!t.closest || !t.closest(".blockr-block-browser-field-link-id")) {
        return;
      }
      t.dataset.edited = "1";
    });
  }

  function initMenu(root) {
    if (root.dataset.blockrLinkMenuInit === "1") return;
    root.dataset.blockrLinkMenuInit = "1";

    var search = root.querySelector(".blockr-block-browser-search");
    if (search) {
      search.addEventListener("input", function () {
        cardSearch.applySearch(root, search.value);
      });
    }

    var cardsArea = root.querySelector(".blockr-link-menu-directions");
    if (cardsArea) {
      cardsArea.addEventListener("click", function (event) {
        var card = event.target.closest(".blockr-block-browser-card");
        if (!card || !root.contains(card)) return;

        if (event.target.closest(".blockr-block-browser-card-chevron")) {
          card.classList.toggle("card-expanded");
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

    markEditedOnInput(root);
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-link-menu");
    },
    initialize: function (el) {
      initMenu(el);
    },
    getValue: function (el) {
      return el._blockrLinkMenuValue || null;
    },
    subscribe: function (el, callback) {
      initMenu(el);
      // Shiny's callback takes no args; addEventListener passes the
      // Event, which breaks the value-update path - wrap to drop it.
      var handler = function () { callback(); };
      el._blockrLinkMenuHandler = handler;
      el.addEventListener(COMMIT_EVENT, handler);
    },
    unsubscribe: function (el) {
      if (el._blockrLinkMenuHandler) {
        el.removeEventListener(COMMIT_EVENT, el._blockrLinkMenuHandler);
        el._blockrLinkMenuHandler = null;
      }
    },
    receiveMessage: function (el, data) {
      if (!data) return;
      if (data.type === "menu:sync") {
        applyMenuSync(el, data);
      } else if (data.type === "pool-update") {
        applyPoolUpdate(el, data);
      }
    }
  });

  Shiny.inputBindings.register(binding, "blockr.ui.linkMenu");
})();
