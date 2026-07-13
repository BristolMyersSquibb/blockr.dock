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

  // Inline colour picker: hue + lightness sliders + hex text input.
  // The hex `<input>` is the canonical value carrier (a normal Shiny
  // text input). Slider input recomputes HSL -> hex and writes the hex
  // field, dispatching a native "input" event so Shiny's built-in
  // text-input binding picks the change up - we never call
  // Shiny.setInputValue ourselves. Typing in the hex field goes the
  // other way: hex -> HSL -> slider positions + preview swatch.

  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  // HSL with S fixed at 60% (matches the dock's default chroma feel)
  // and L variable. h in [0, 360], l in [0, 100].
  function hslToHex(h, l) {
    var s = 60;
    h = ((h % 360) + 360) % 360;
    l = clamp(l, 0, 100);
    var sN = s / 100;
    var lN = l / 100;
    var c = (1 - Math.abs(2 * lN - 1)) * sN;
    var x = c * (1 - Math.abs(((h / 60) % 2) - 1));
    var m = lN - c / 2;
    var r1 = 0, g1 = 0, b1 = 0;
    if (h < 60)        { r1 = c; g1 = x; b1 = 0; }
    else if (h < 120)  { r1 = x; g1 = c; b1 = 0; }
    else if (h < 180)  { r1 = 0; g1 = c; b1 = x; }
    else if (h < 240)  { r1 = 0; g1 = x; b1 = c; }
    else if (h < 300)  { r1 = x; g1 = 0; b1 = c; }
    else               { r1 = c; g1 = 0; b1 = x; }
    var to255 = function (v) {
      var n = Math.round((v + m) * 255);
      var s = clamp(n, 0, 255).toString(16);
      return s.length === 1 ? "0" + s : s;
    };
    return "#" + to255(r1) + to255(g1) + to255(b1);
  }

  function hexToHsl(hex) {
    if (!/^#[0-9a-fA-F]{6}$/.test(hex)) return null;
    var r = parseInt(hex.slice(1, 3), 16) / 255;
    var g = parseInt(hex.slice(3, 5), 16) / 255;
    var b = parseInt(hex.slice(5, 7), 16) / 255;
    var max = Math.max(r, g, b);
    var min = Math.min(r, g, b);
    var l = (max + min) / 2;
    var h = 0, s = 0;
    if (max !== min) {
      var d = max - min;
      s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
      switch (max) {
        case r: h = ((g - b) / d) + (g < b ? 6 : 0); break;
        case g: h = ((b - r) / d) + 2; break;
        case b: h = ((r - g) / d) + 4; break;
      }
      h *= 60;
    }
    return { h: Math.round(h), s: Math.round(s * 100), l: Math.round(l * 100) };
  }

  function updatePreview(root) {
    var hex = root.querySelector(".blockr-stack-menu-hex");
    var sw = root.querySelector(".blockr-stack-menu-color-swatch");
    if (!hex || !sw) return;
    var v = (hex.value || "").trim();
    if (/^#[0-9a-fA-F]{6}$/.test(v)) {
      sw.style.background = v;
    }
  }

  function initColorPicker(root) {
    var hex = root.querySelector(".blockr-stack-menu-hex");
    var hue = root.querySelector(".blockr-stack-menu-hue");
    var lit = root.querySelector(".blockr-stack-menu-lightness");
    if (!hex || !hue || !lit) return;

    // Seed slider positions from the initial hex value (so an edit
    // flow with a pre-filled colour shows the correct slider state).
    var initial = hexToHsl(hex.value || "");
    if (initial) {
      hue.value = String(initial.h);
      lit.value = String(initial.l);
    }
    updatePreview(root);

    var onSlider = function () {
      var h = parseInt(hue.value, 10);
      var l = parseInt(lit.value, 10);
      hex.value = hslToHex(h, l);
      hex.dispatchEvent(new Event("input", { bubbles: true }));
      hex.dispatchEvent(new Event("change", { bubbles: true }));
      updatePreview(root);
    };
    hue.addEventListener("input", onSlider);
    lit.addEventListener("input", onSlider);

    hex.addEventListener("input", function () {
      var parsed = hexToHsl((hex.value || "").trim());
      if (parsed) {
        hue.value = String(parsed.h);
        lit.value = String(parsed.l);
      }
      updatePreview(root);
    });
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
