(function () {
  "use strict";

  // The stack menu is a Shiny input: its value is the selection set the
  // user committed via the confirm button. Selecting is multi-toggle
  // (click a card to add / remove), so the commit value carries a
  // monotonically increasing `nonce` to guarantee Shiny re-fires even
  // when the same set is confirmed twice. The panel-level name / color
  // / id fields are normal Shiny inputs - the binding does NOT read
  // them; the R side composes the full spec from input$commit + the
  // panel inputs. `receiveMessage` is reserved.
  var COMMIT_EVENT = "blockr-stack-menu:commit";
  var commitSeq = 0;

  // Shared card-list helpers live on `window.BlockrDock.cardSearch`, set
  // up by `blockr-block-browser.js`. `stack_menu_ui()` always attaches
  // `block_browser_dep()` before `stack_menu_dep()`, so the namespace
  // is in scope by the time this binding runs.
  var cardSearch = window.BlockrDock.cardSearch;
  var cardSync = window.BlockrDock.cardSync;

  // The selection set lives on the root element. We keep it as an
  // ordered list of `data-block-type` strings (board block ids in this
  // module's repurposing) so the publish has a stable JSON shape.
  function getSelection(root) {
    if (!root._blockrStackMenuSelection) {
      root._blockrStackMenuSelection = [];
    }
    return root._blockrStackMenuSelection;
  }

  function setCardSelected(card, selected) {
    card.classList.toggle("card-selected", !!selected);
    if (selected) {
      card.setAttribute("data-selected", "true");
    } else {
      card.removeAttribute("data-selected");
    }
  }

  function toggleCard(root, card) {
    var sel = getSelection(root);
    var id = card.getAttribute("data-block-type");
    var idx = sel.indexOf(id);
    if (idx === -1) {
      sel.push(id);
      setCardSelected(card, true);
    } else {
      sel.splice(idx, 1);
      setCardSelected(card, false);
    }
  }

  function commitSelection(root) {
    var sel = getSelection(root).slice();
    root._blockrStackMenuValue = {
      blocks: sel,
      nonce: ++commitSeq
    };
    root.dispatchEvent(new CustomEvent(COMMIT_EVENT));
  }

  // Apply a `menu:sync` diff pushed from R when the board changes:
  // structurally reconcile the cards (shared helper), then reconcile the
  // selection (drop removed cards, adopt any newly-inserted card that
  // arrives pre-selected, re-render selected classes from the
  // authoritative list) and re-run the search so visibility +
  // empty-state stay consistent. Scroll, expansion, and the panel-level
  // name / colour / id inputs are deliberately left untouched.
  function applyMenuSync(root, data) {
    cardSync(root.querySelector(".blockr-block-browser-categories"), data.cards);

    var sel = getSelection(root);
    for (var i = sel.length - 1; i >= 0; i--) {
      if (!root.querySelector(
        '[data-block-type="' + sel[i].replace(/["\\]/g, "\\$&") + '"]'
      )) {
        sel.splice(i, 1);
      }
    }
    cardSearch.getCards(root).forEach(function (card) {
      var id = card.getAttribute("data-block-type");
      if (card.getAttribute("data-selected") === "true" &&
          sel.indexOf(id) === -1) {
        sel.push(id);
      }
      setCardSelected(card, sel.indexOf(id) !== -1);
    });

    var search = root.querySelector(".blockr-block-browser-search");
    cardSearch.applySearch(root, search ? search.value : "");
  }

  // Colour field: a hex text input - the canonical value, and the only
  // one Shiny binds - beside a native `<input type="color">`. R renders
  // both from the same value, so there is nothing to seed; they only
  // have to follow each other.
  //
  // The sync is delegated on the panel root, which outlives the form:
  // the form is server-rendered into a `uiOutput` so it tracks the board,
  // and per-element listeners would die with each render.

  // `<input type="color">` takes only the 6-digit form; the R side
  // accepts the 3-digit shorthand too. Returns null for anything the
  // colour input cannot hold.
  function expandHex(value) {
    var v = (value || "").trim();
    if (/^#[0-9a-fA-F]{3}$/.test(v)) {
      return "#" + v.slice(1).replace(/./g, "$&$&");
    }
    return /^#[0-9a-fA-F]{6}$/.test(v) ? v : null;
  }

  function syncColorFields(root, event) {
    var hex = root.querySelector(".blockr-stack-menu-hex");
    var swatch = root.querySelector(".blockr-stack-menu-swatch");
    if (!hex || !swatch) return;

    if (event.target === swatch) {
      hex.value = swatch.value;
      // Only once the picker commits: it fires "input" throughout a drag,
      // and Shiny's text binding sends "change" immediately.
      if (event.type === "change") {
        hex.dispatchEvent(new Event("change", { bubbles: true }));
      }
    } else if (event.target === hex) {
      var expanded = expandHex(hex.value);
      if (expanded) {
        swatch.value = expanded;
      }
    }
  }

  function initColorPicker(root) {
    var handler = function (event) { syncColorFields(root, event); };
    root.addEventListener("input", handler);
    root.addEventListener("change", handler);
  }

  // Edit mode caps the cards container so exactly four cards are
  // visible; the rest scroll. Done in JS (not CSS) because card
  // heights vary with the consuming app's font-size (the dock renders
  // cards taller than plain Shiny), so a hard CSS height can't be
  // calibrated for both at once. Measurement is deferred via two
  // animation-frame ticks so the sidebar transition + initial layout
  // are settled before we read bounding rects.
  function capCardsToFour(root) {
    if (root.getAttribute("data-mode") !== "edit") return;
    var measure = function () {
      var cats = root.querySelector(".blockr-block-browser-categories");
      if (!cats) return;
      var cards = cats.querySelectorAll(".blockr-block-browser-card");
      if (cards.length <= 4) return;
      var fourth = cards[3];
      var catsTop = cats.getBoundingClientRect().top;
      var fourthBottom = fourth.getBoundingClientRect().bottom;
      // +4px so the 4th card isn't clipped by sub-pixel rounding.
      var h = Math.ceil(fourthBottom - catsTop + 4);
      cats.style.height = h + "px";
      cats.style.maxHeight = h + "px";
    };
    requestAnimationFrame(function () { requestAnimationFrame(measure); });
  }

  function initMenu(root) {
    if (root.dataset.blockrStackMenuInit === "1") return;
    root.dataset.blockrStackMenuInit = "1";

    // Seed selection from any cards that already carry data-selected.
    cardSearch.getCards(root).forEach(function (card) {
      if (card.getAttribute("data-selected") === "true") {
        var id = card.getAttribute("data-block-type");
        getSelection(root).push(id);
        card.classList.add("card-selected");
      }
    });

    var search = root.querySelector(".blockr-block-browser-search");
    if (search) {
      search.addEventListener("input", function () {
        cardSearch.applySearch(root, search.value);
      });
    }

    var cardsArea = root.querySelector(".blockr-block-browser-categories");
    if (cardsArea) {
      cardsArea.addEventListener("click", function (event) {
        var card = event.target.closest(".blockr-block-browser-card");
        if (!card || !root.contains(card)) return;
        event.preventDefault();
        toggleCard(root, card);
      });
    }

    var confirm = root.querySelector(".blockr-stack-menu-confirm");
    if (confirm) {
      confirm.addEventListener("click", function (event) {
        event.preventDefault();
        commitSelection(root);
      });
    }

    initColorPicker(root);
    capCardsToFour(root);
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-stack-menu");
    },
    initialize: function (el) {
      initMenu(el);
    },
    getValue: function (el) {
      return el._blockrStackMenuValue || null;
    },
    subscribe: function (el, callback) {
      initMenu(el);
      var handler = function () { callback(); };
      el._blockrStackMenuHandler = handler;
      el.addEventListener(COMMIT_EVENT, handler);
    },
    unsubscribe: function (el) {
      if (el._blockrStackMenuHandler) {
        el.removeEventListener(COMMIT_EVENT, el._blockrStackMenuHandler);
        el._blockrStackMenuHandler = null;
      }
    },
    receiveMessage: function (el, data) {
      if (data && data.type === "menu:sync") {
        applyMenuSync(el, data);
      }
    }
  });

  Shiny.inputBindings.register(binding, "blockr.dock.stackMenu");
})();
