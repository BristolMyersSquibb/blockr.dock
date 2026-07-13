(function () {
  "use strict";

  var FOCUSABLE_SELECTOR = [
    "a[href]",
    "button:not([disabled])",
    "input:not([disabled]):not([type=\"hidden\"])",
    "select:not([disabled])",
    "textarea:not([disabled])",
    "[tabindex]:not([tabindex=\"-1\"])"
  ].join(", ");

  var STATE_EVENT = "blockr-sidebar:state";
  var INIT_FLAG = "blockrSidebarInit";

  function getBody(panel) {
    return panel.querySelector(".blockr-sidebar-body");
  }

  function getTitle(panel) {
    return panel.querySelector(".blockr-sidebar-title");
  }

  function getFocusables(scope) {
    return Array.prototype.slice
      .call(scope.querySelectorAll(FOCUSABLE_SELECTOR))
      .filter(function (el) {
        return !el.hasAttribute("disabled") && el.offsetParent !== null;
      });
  }

  function dispatchState(panel) {
    panel.dispatchEvent(new CustomEvent(STATE_EVENT));
  }

  function isOpen(panel) {
    return panel.classList.contains("blockr-sidebar-open");
  }

  function isPinned(panel) {
    return panel.classList.contains("blockr-sidebar-pinned");
  }

  function refreshBodyReflow() {
    // A panel "docks" (shifts the page content aside via a class + CSS
    // variable on `<html>`) when it is open AND either:
    //   * it is a push-mode panel (`data-mode="push"`): always pushes
    //     while open, regardless of pin; or
    //   * it is pinned: an overlay-mode panel floats above the board by
    //     default and only docks once the user pins it.
    // So the rule is "open AND (mode=push OR pinned)". We apply to
    // `<html>` (rather than `<body>`) because bslib's page-fill layouts
    // pin body to 100% of html and zero its padding inline - body-level
    // padding / margin no longer constrains the visible viewport. Padding
    // on html shrinks html's content area, and body (sized 100%) follows.
    var root = document.documentElement;
    root.classList.remove(
      "blockr-html-pushed-left",
      "blockr-html-pushed-right"
    );

    // Both sides may be open simultaneously; track each width separately
    // so the page content is constrained between them rather than hidden
    // under whichever sidebar was opened second. The two-selector union
    // is "(open AND push)" OR "(open AND pinned)".
    var pushedOpen = document.querySelectorAll(
      ".blockr-sidebar.blockr-sidebar-open[data-mode=\"push\"], " +
        ".blockr-sidebar.blockr-sidebar-open.blockr-sidebar-pinned"
    );
    var widths = { left: 0, right: 0 };
    for (var i = 0; i < pushedOpen.length; i++) {
      var panel = pushedOpen[i];
      var side = panel.dataset.side === "left" ? "left" : "right";
      widths[side] = panel.getBoundingClientRect().width;
      root.classList.add("blockr-html-pushed-" + side);
    }
    root.style.setProperty(
      "--blockr-sidebar-width-left", widths.left + "px"
    );
    root.style.setProperty(
      "--blockr-sidebar-width-right", widths.right + "px"
    );
    // Back-compat: keep the singular var pointing at the open side when
    // only one is open. With both open consumers should switch to the
    // side-specific vars.
    var legacy = widths.right || widths.left || 0;
    root.style.setProperty("--blockr-sidebar-width", legacy + "px");
  }

  function trapFocus(panel, event) {
    if (event.key !== "Tab") return;
    var focusables = getFocusables(panel);
    if (focusables.length === 0) {
      event.preventDefault();
      panel.focus();
      return;
    }
    var first = focusables[0];
    var last = focusables[focusables.length - 1];
    var active = document.activeElement;
    if (event.shiftKey && active === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && active === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function attachOutsideClick(panel) {
    if (panel._blockrSidebarOutsideHandler) return;
    var handler = function (event) {
      if (!isOpen(panel) || isPinned(panel)) return;
      if (panel.contains(event.target)) return;
      hidePanel(panel);
    };
    panel._blockrSidebarOutsideHandler = handler;
    // Defer one tick so the click that opened the panel does not
    // immediately close it.
    setTimeout(function () {
      document.addEventListener("mousedown", handler, true);
    }, 0);
  }

  function detachOutsideClick(panel) {
    var handler = panel._blockrSidebarOutsideHandler;
    if (!handler) return;
    document.removeEventListener("mousedown", handler, true);
    panel._blockrSidebarOutsideHandler = null;
  }

  // Swap the body content with new HTML + deps. Used by `show_sidebar(id,
  // ui = ...)` from R; skipped on open-only shows and on JS-driven opens
  // (`data-blockr-sidebar-target`), where the body was either pre-rendered
  // at `sidebar_ui(ui = ...)` time or left in place by an earlier show.
  function swapBody(body, data) {
    Shiny.unbindAll(body);
    if (data.dependencies && data.dependencies.length) {
      Shiny.renderDependencies(data.dependencies);
    }
    body.replaceChildren.apply(
      body,
      window.$ ? $.parseHTML(data.html || "", document, true) : []
    );
    Shiny.initializeInputs(body);
    Shiny.bindAll(body);
  }

  // Visual / a11y / focus side of opening a panel. Reused by:
  //   * `receiveMessage` after a body-swap show
  //   * `receiveMessage` on open-only shows (no `data.html`)
  //   * the document-level `[data-blockr-sidebar-target]` click trigger
  function openPanel(panel) {
    var body = getBody(panel);
    if (!body) return;

    // Re-bind the body's inputs/outputs on open. `hidePanel` unbinds on
    // close so a hidden panel doesn't hold stale bindings, but a
    // pre-rendered panel opened via `show_sidebar(id)` with no `ui` never
    // swaps its body (the only other place that re-binds), so after its
    // first close its bindings would stay dead and it would silently stop
    // emitting (e.g. the add / append block browser committing only once).
    // `bindAll` skips already-bound nodes, so this is a no-op on the first
    // open and right after a body-swap show.
    Shiny.bindAll(body);

    // Remember previously-focused element so close can restore it.
    panel._blockrSidebarPreviousActive = document.activeElement;

    panel.classList.add("blockr-sidebar-open");
    panel.setAttribute("aria-hidden", "false");

    refreshBodyReflow();
    attachOutsideClick(panel);
    dispatchState(panel);

    // Move focus to first focusable element inside the body.
    var focusables = getFocusables(body);
    if (focusables.length) {
      var first = focusables[0];
      first.focus();
      // Selectize-aware: if the first focusable belongs to a selectize
      // control, also call the selectize API so the dropdown pops open.
      // Plain `.focus()` lands on the visible `.selectize-input` div but
      // does not trigger the open behaviour, so consumers used to wire
      // `$('#id')[0].selectize.focus()` to `shown.bs.modal`. Doing it
      // here means every sidebar consumer benefits without per-form opt-in.
      var ctrl = first.closest && first.closest(".selectize-control");
      if (ctrl) {
        var select = ctrl.querySelector("select.selectized");
        if (
          select &&
          select.selectize &&
          typeof select.selectize.focus === "function"
        ) {
          select.selectize.focus();
        }
      }
    } else {
      panel.focus();
    }
  }

  function showPanel(panel, data) {
    var body = getBody(panel);
    if (!body) return;

    // Body-swap branch: only when the R helper shipped new content
    // (`show_sidebar(id, ui = <tags>)`). `show_sidebar(id)` with no `ui`
    // omits `data.html` entirely so we leave the existing body alone.
    if (typeof data.html !== "undefined") {
      swapBody(body, data);
    }

    // Title is independently optional: only update when the payload
    // explicitly carries one. Absent `title` leaves whatever was set at
    // `sidebar_ui(title = ...)` time (or by a previous show) untouched.
    if (typeof data.title !== "undefined") {
      var titleEl = getTitle(panel);
      if (titleEl) {
        titleEl.textContent = data.title == null ? "" : String(data.title);
      }
    }

    openPanel(panel);
  }

  // Document-level click handler: any element with
  // `data-blockr-sidebar-target="<id>"` toggles the matching panel
  // without an R round-trip. Lives at document scope so triggers placed
  // anywhere in the page (including nested DOM swaps) just work.
  function handleTriggerClick(event) {
    if (!event.target || !event.target.closest) return;
    var trigger = event.target.closest("[data-blockr-sidebar-target]");
    if (!trigger) return;
    var targetId = trigger.getAttribute("data-blockr-sidebar-target");
    if (!targetId) return;
    var panel = document.getElementById(targetId);
    if (!panel || !panel.classList.contains("blockr-sidebar")) return;
    event.preventDefault();
    initPanel(panel);
    if (isOpen(panel)) {
      hidePanel(panel);
    } else {
      openPanel(panel);
    }
  }

  function hidePanel(panel) {
    var body = getBody(panel);
    if (body) Shiny.unbindAll(body);

    panel.classList.remove("blockr-sidebar-open");
    // Closing is an explicit user/server action; pin opts in to "stay open
    // while I work elsewhere", so clearing pin on close prevents a stale pin
    // bleeding into the next show.
    panel.classList.remove("blockr-sidebar-pinned");
    panel.setAttribute("aria-hidden", "true");

    detachOutsideClick(panel);
    refreshBodyReflow();
    dispatchState(panel);

    // Restore focus to previously-focused element.
    var prev = panel._blockrSidebarPreviousActive;
    if (prev && typeof prev.focus === "function") {
      prev.focus();
    }
    panel._blockrSidebarPreviousActive = null;
  }

  function togglePinned(panel) {
    // Pin governs both dismissal (Esc / outside click) and reflow: a
    // pinned push-capable panel docks beside the content, an unpinned one
    // floats as an overlay. Refresh the reflow so toggling pin pushes /
    // releases the board immediately.
    panel.classList.toggle("blockr-sidebar-pinned");
    refreshBodyReflow();
    dispatchState(panel);
  }

  function initPanel(panel) {
    if (panel.dataset[INIT_FLAG] === "true") return;
    panel.dataset[INIT_FLAG] = "true";

    var pinBtn = panel.querySelector(".blockr-sidebar-pin");
    if (pinBtn) {
      pinBtn.addEventListener("click", function (event) {
        event.preventDefault();
        togglePinned(panel);
      });
    }

    var closeBtn = panel.querySelector(".blockr-sidebar-close");
    if (closeBtn) {
      closeBtn.addEventListener("click", function (event) {
        event.preventDefault();
        hidePanel(panel);
      });
    }

    panel.addEventListener("keydown", function (event) {
      if (!isOpen(panel)) return;
      if (event.key === "Escape" && !isPinned(panel)) {
        event.preventDefault();
        hidePanel(panel);
        return;
      }
      trapFocus(panel, event);
    });
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-sidebar");
    },
    initialize: function (el) {
      initPanel(el);
    },
    getValue: function (el) {
      return {
        open: el.classList.contains("blockr-sidebar-open"),
        pinned: el.classList.contains("blockr-sidebar-pinned")
      };
    },
    receiveMessage: function (el, data) {
      if (!data) return;
      initPanel(el);
      if (data.action === "show") {
        showPanel(el, data);
      } else if (data.action === "hide") {
        hidePanel(el);
      }
    },
    subscribe: function (el, callback) {
      initPanel(el);
      // Shiny's input-binding callback expects to be invoked with no args.
      // addEventListener passes the Event object, which silently breaks the
      // value-update path inside Shiny - so wrap to drop it.
      var handler = function () { callback(); };
      el._blockrSidebarHandler = handler;
      el.addEventListener(STATE_EVENT, handler);
    },
    unsubscribe: function (el) {
      if (el._blockrSidebarHandler) {
        el.removeEventListener(STATE_EVENT, el._blockrSidebarHandler);
        el._blockrSidebarHandler = null;
      }
    }
  });

  Shiny.inputBindings.register(binding, "blockr.dock.sidebar");

  // Register the document-level trigger listener exactly once, even if
  // the bundle is loaded multiple times (e.g. via repeated dependency
  // injection across renderUI swaps). Guarded by a flag on the document.
  if (!document._blockrSidebarTriggerInit) {
    document.addEventListener("click", handleTriggerClick);
    document._blockrSidebarTriggerInit = true;
  }
})();
