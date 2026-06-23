(function () {
  "use strict";

  // The panel navigator (variant B). A single Shiny input whose value is
  // multiplexed by `kind`:
  //   - "toggle":       the switch shows / hides a block on the view
  //   - "assign":       grip-drag a block to another stack, or reorder
  //   - "add_stack":    create a new (empty) stack
  //   - "rename":       rename a block
  //   - "rename_stack": rename a stack
  // The value carries a monotonically increasing `nonce` so Shiny re-fires.
  var EVT = "blockr-panel-navigator:event";
  var seq = 0;

  function commit(root, value) {
    value.nonce = ++seq;
    root._navValue = value;
    root.dispatchEvent(new CustomEvent(EVT));
  }

  // ---- search ----------------------------------------------------------
  function applySearch(root, query) {
    var q = (query || "").trim().toLowerCase();
    var any = false;
    root.querySelectorAll(".blockr-panel-nav-row").forEach(function (row) {
      var hit = q.length === 0 ||
        (row.getAttribute("data-search") || "").indexOf(q) !== -1;
      row.classList.toggle("hidden", !hit);
      if (hit) any = true;
    });
    root.querySelectorAll(".blockr-panel-nav-grp").forEach(function (grp) {
      var vis = grp.querySelectorAll(
        ".blockr-panel-nav-row:not(.hidden)"
      ).length;
      // While searching, hide empty groups and open the matching ones so
      // hits inside collapsed stacks are visible.
      grp.classList.toggle("hidden", q.length > 0 && vis === 0);
      if (q.length > 0 && vis > 0) grp.classList.add("open");
    });
    root.classList.toggle("is-empty", !any && q.length > 0);
  }

  // ---- visibility toggle (the switch is the ONLY toggle) ---------------
  function updateShown(row) {
    var grp = row.closest(".blockr-panel-nav-grp");
    if (!grp) return;
    var n = grp.querySelectorAll(".blockr-panel-nav-row.on").length;
    var vis = grp.querySelector(".blockr-panel-nav-gvis");
    if (vis) vis.textContent = n + " shown";
  }

  function commitToggle(root, row) {
    var on = row.getAttribute("data-on-view") === "true";
    row.setAttribute("data-on-view", on ? "false" : "true");
    row.classList.toggle("on", !on);
    row.classList.toggle("off", on);
    var sw = row.querySelector(".blockr-panel-nav-switch");
    if (sw) sw.setAttribute("aria-checked", on ? "false" : "true");
    updateShown(row);
    commit(root, {
      kind: "toggle",
      id: row.getAttribute("data-panel-id"),
      type: row.getAttribute("data-panel-type"),
      to: on ? "remove" : "add"
    });
  }

  function toggleCollapse(ghd) {
    var grp = ghd.closest(".blockr-panel-nav-grp");
    var open = grp.classList.toggle("open");
    ghd.setAttribute("aria-expanded", open ? "true" : "false");
  }

  // ---- inline rename (canonical pattern; see nav_rename() in R) --------
  function enterEdit(wrapper) {
    if (wrapper.classList.contains("editing")) return;
    var textEl = wrapper.querySelector(".blockr-panel-nav-rename-text");
    var input = wrapper.querySelector(".blockr-panel-nav-rename-input");
    if (!textEl || !input) return;
    var host = wrapper.closest('[draggable="true"]');
    if (host) { wrapper._navHost = host; host.setAttribute("draggable", "false"); }
    input.value = textEl.textContent;
    wrapper.classList.add("editing");
    input.focus();
    input.select();
  }

  function exitEdit(wrapper) {
    wrapper.classList.remove("editing");
    if (wrapper._navHost) {
      wrapper._navHost.setAttribute("draggable", "true");
      wrapper._navHost = null;
    }
  }

  function commitRename(root, wrapper) {
    var textEl = wrapper.querySelector(".blockr-panel-nav-rename-text");
    var input = wrapper.querySelector(".blockr-panel-nav-rename-input");
    var name = (input.value || "").trim();
    exitEdit(wrapper);
    if (!name || name === textEl.textContent) return;
    textEl.textContent = name; // optimistic
    if (wrapper.getAttribute("data-rename-kind") === "stack") {
      var grp = wrapper.closest(".blockr-panel-nav-grp");
      commit(root, {
        kind: "rename_stack",
        stack: grp ? grp.getAttribute("data-stack-id") || "" : "",
        name: name
      });
    } else {
      var row = wrapper.closest(".blockr-panel-nav-row");
      if (row) {
        row.setAttribute(
          "data-search",
          (name + " " + (row.getAttribute("data-search") || "")).toLowerCase()
        );
      }
      commit(root, {
        kind: "rename",
        type: "block",
        id: row ? row.getAttribute("data-panel-id") : null,
        name: name
      });
    }
  }

  function wireRenameInput(root, wrapper) {
    if (wrapper.dataset.navRenameWired === "1") return;
    wrapper.dataset.navRenameWired = "1";
    var input = wrapper.querySelector(".blockr-panel-nav-rename-input");
    if (!input) return;
    input.addEventListener("keydown", function (event) {
      if (event.key === "Enter") {
        event.preventDefault();
        input.blur();
      } else if (event.key === "Escape") {
        event.preventDefault();
        wrapper._navCancel = true;
        input.blur();
      }
      event.stopPropagation();
    });
    input.addEventListener("blur", function () {
      if (wrapper._navCancel) { wrapper._navCancel = false; exitEdit(wrapper); return; }
      commitRename(root, wrapper);
    });
    input.addEventListener("click", function (e) { e.stopPropagation(); });
    input.addEventListener("dblclick", function (e) { e.stopPropagation(); });
  }

  function startEdit(root, wrapper) {
    wireRenameInput(root, wrapper);
    enterEdit(wrapper);
  }

  // ---- grip drag: move between stacks (mostly) / reorder within --------
  function clearDropMarks(root) {
    root.querySelectorAll(".blockr-panel-nav-dropover").forEach(function (g) {
      g.classList.remove("blockr-panel-nav-dropover");
    });
    root.querySelectorAll(".blockr-panel-nav-row.dropline").forEach(function (r) {
      r.classList.remove("dropline");
    });
  }

  function initNavigator(root) {
    if (root.dataset.blockrPanelNavInit === "1") return;
    root.dataset.blockrPanelNavInit = "1";

    var search = root.querySelector(".blockr-panel-nav-search");
    if (search) {
      search.addEventListener("input", function () {
        applySearch(root, search.value);
      });
    }

    var body = root.querySelector(".blockr-panel-nav-body");
    if (body) {
      body.addEventListener("click", function (event) {
        // The switch is the ONLY visibility toggle.
        var sw = event.target.closest(".blockr-panel-nav-switch");
        if (sw) {
          var srow = sw.closest(".blockr-panel-nav-row");
          if (srow) commitToggle(root, srow);
          event.preventDefault();
          return;
        }
        // The stack bar collapses — except its rename label (rename zone).
        var ghd = event.target.closest(".blockr-panel-nav-ghd");
        if (ghd) {
          if (event.target.closest(".blockr-panel-nav-rename-text")) return;
          toggleCollapse(ghd);
          event.preventDefault();
          return;
        }
        // Click a row body (not the name, which is a rename zone) -> reveal:
        // bring a VISIBLE block's tab to the front. Never changes visibility
        // (the switch owns that); clicking a hidden row is inert.
        var frow = event.target.closest(".blockr-panel-nav-row");
        if (frow &&
            !event.target.closest(".blockr-panel-nav-rename-text") &&
            !event.target.closest(".blockr-panel-nav-grip") &&
            frow.getAttribute("data-on-view") === "true") {
          commit(root, {
            kind: "focus",
            id: frow.getAttribute("data-panel-id"),
            type: frow.getAttribute("data-panel-type")
          });
        }
      });

      body.addEventListener("dblclick", function (event) {
        var rt = event.target.closest(".blockr-panel-nav-rename-text");
        if (!rt) return;
        event.preventDefault();
        startEdit(root, rt.closest(".blockr-panel-nav-rename"));
      });

      body.addEventListener("keydown", function (event) {
        var sw = event.target.closest(".blockr-panel-nav-switch");
        if (sw && (event.key === "Enter" || event.key === " ")) {
          event.preventDefault();
          var srow = sw.closest(".blockr-panel-nav-row");
          if (srow) commitToggle(root, srow);
          return;
        }
        if (event.key === "F2") {
          var hostRow = event.target.closest(
            ".blockr-panel-nav-row, .blockr-panel-nav-ghd"
          );
          if (hostRow) {
            var w = hostRow.querySelector(".blockr-panel-nav-rename");
            if (w) { event.preventDefault(); startEdit(root, w); }
          }
          return;
        }
        var ghd = event.target.closest(".blockr-panel-nav-ghd");
        if (ghd && event.target === ghd &&
            (event.key === "Enter" || event.key === " ")) {
          event.preventDefault();
          toggleCollapse(ghd);
        }
      });

      body.addEventListener("dragstart", function (event) {
        var row = event.target.closest(".blockr-panel-nav-row");
        if (!row || row.getAttribute("draggable") !== "true") return;
        root._navDragId = row.getAttribute("data-panel-id");
        root._navDragRow = row;
        row.classList.add("dragging");
        // Reveal an empty Ungrouped as a drop target for the drag.
        root.classList.add("is-dragging");
        if (event.dataTransfer) {
          event.dataTransfer.effectAllowed = "move";
          event.dataTransfer.setData("text/plain", root._navDragId);
        }
      });

      body.addEventListener("dragend", function () {
        if (root._navDragRow) root._navDragRow.classList.remove("dragging");
        root.classList.remove("is-dragging");
        clearDropMarks(root);
        root._navDragId = null;
        root._navDragRow = null;
        root._navDropStack = null;
        root._navDropBeforeId = null;
      });

      body.addEventListener("dragover", function (event) {
        if (root._navDragId == null) return;
        var grp = event.target.closest(".blockr-panel-nav-dropzone");
        if (!grp) return;
        event.preventDefault();
        if (event.dataTransfer) event.dataTransfer.dropEffect = "move";
        clearDropMarks(root);
        grp.classList.add("blockr-panel-nav-dropover");
        root._navDropStack = grp.getAttribute("data-stack-id") || "";
        // Insert-before = the first row whose midpoint is below the cursor.
        var rows = Array.prototype.slice
          .call(grp.querySelectorAll(".blockr-panel-nav-row"))
          .filter(function (r) { return r !== root._navDragRow; });
        var beforeId = null;
        for (var i = 0; i < rows.length; i++) {
          var rect = rows[i].getBoundingClientRect();
          if (event.clientY < rect.top + rect.height / 2) {
            beforeId = rows[i].getAttribute("data-panel-id");
            rows[i].classList.add("dropline");
            break;
          }
        }
        root._navDropBeforeId = beforeId;
      });

      body.addEventListener("drop", function (event) {
        if (root._navDragId == null) return;
        var grp = event.target.closest(".blockr-panel-nav-dropzone");
        if (!grp) { clearDropMarks(root); return; }
        event.preventDefault();
        var id = root._navDragId;
        var stack = root._navDropStack != null ? root._navDropStack
          : (grp.getAttribute("data-stack-id") || "");
        var before = root._navDropBeforeId || null;
        clearDropMarks(root);
        commit(root, { kind: "assign", id: id, stack: stack, before: before });
      });
    }

    // ---- add-stack control ------------------------------------------
    var addWrap = root.querySelector(".blockr-panel-nav-addstack");
    if (addWrap) {
      var btn = addWrap.querySelector(".blockr-panel-nav-addstack-btn");
      var input = addWrap.querySelector(".blockr-panel-nav-addstack-input");
      if (btn && input) {
        btn.addEventListener("click", function () {
          if (addWrap.classList.toggle("is-open")) input.focus();
        });
        input.addEventListener("keydown", function (event) {
          if (event.key === "Enter") {
            event.preventDefault();
            var name = input.value.trim();
            if (!name) return;
            commit(root, { kind: "add_stack", name: name });
            input.value = "";
            addWrap.classList.remove("is-open");
          } else if (event.key === "Escape") {
            input.value = "";
            addWrap.classList.remove("is-open");
          }
        });
      }
    }
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-panel-navigator");
    },
    initialize: function (el) { initNavigator(el); },
    getValue: function (el) { return el._navValue || null; },
    subscribe: function (el, callback) {
      initNavigator(el);
      var handler = function () { callback(); };
      el._navHandler = handler;
      el.addEventListener(EVT, handler);
    },
    unsubscribe: function (el) {
      if (el._navHandler) {
        el.removeEventListener(EVT, el._navHandler);
        el._navHandler = null;
      }
    }
  });

  Shiny.inputBindings.register(binding, "blockr.ui.panelNavigator");
})();
