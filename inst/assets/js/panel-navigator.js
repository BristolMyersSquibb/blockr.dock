/* Panel navigator: client behaviour for the dock's "Blocks" sidebar.
 *
 * The body is injected dynamically (via blockr.ui::show_sidebar), so all
 * handlers are delegated on `document` and scoped to the navigator root
 * (`.blockr-panel-nav`). The root's `data-input-id` is the Shiny input the
 * navigator's server observer reads (a multiplexed {kind, id, to, nonce}).
 *
 * Eye toggles are optimistic: the row's shown/hidden class is flipped (and
 * the group count updated) immediately, then the event is sent for the
 * server to apply via show/hide_block_panel(). Search reuses the shared
 * card filter shipped by block_browser_dep() (window.BlockrUI.cardSearch).
 */
(function () {
  "use strict";

  var seq = 0;

  function navRoot(el) {
    return el && el.closest ? el.closest(".blockr-panel-nav") : null;
  }

  function send(root, payload) {
    var id = root.getAttribute("data-input-id");
    if (!id || !window.Shiny) return;
    payload.nonce = ++seq;
    Shiny.setInputValue(id, payload, { priority: "event" });
  }

  function updateCount(card) {
    var group = card.closest(".blockr-panel-nav-group");
    if (!group) return;
    var total = group.querySelectorAll(".blockr-panel-nav-card").length;
    var shown = group.querySelectorAll(
      ".blockr-panel-nav-card.pn-shown"
    ).length;
    var count = group.querySelector(".blockr-panel-nav-count");
    if (count) count.textContent = shown + "/" + total + " shown";
  }

  document.addEventListener("click", function (event) {
    var root = navRoot(event.target);
    if (!root) return;

    var card = event.target.closest(".blockr-panel-nav-card");
    if (!card) return;

    var bid = card.getAttribute("data-block-id");

    if (event.target.closest(".blockr-panel-nav-eye")) {
      var wasShown = card.classList.contains("pn-shown");
      card.classList.toggle("pn-shown", !wasShown);
      card.classList.toggle("pn-hidden", wasShown);
      updateCount(card);
      send(root, { kind: "toggle", id: bid, to: wasShown ? "remove" : "add" });
      return;
    }

    // Row body click: reveal (bring the panel to front) when it is shown.
    if (card.classList.contains("pn-shown")) {
      send(root, { kind: "focus", id: bid });
    }
  });

  document.addEventListener("input", function (event) {
    var root = navRoot(event.target);
    if (!root) return;
    if (!event.target.classList.contains("blockr-panel-nav-search")) return;

    if (window.BlockrUI && window.BlockrUI.cardSearch) {
      window.BlockrUI.cardSearch.applySearch(root, event.target.value);
    }
  });

  // ---- "+ New stack" ---------------------------------------------------

  document.addEventListener("click", function (event) {
    var root = navRoot(event.target);
    if (!root) return;
    if (event.target.closest(".blockr-panel-nav-add-stack")) {
      send(root, { kind: "add_stack" });
    }
  });

  // ---- drag a card onto a stack heading to reassign --------------------

  function dropGroup(el) {
    return el && el.closest ? el.closest(".blockr-panel-nav-group") : null;
  }

  document.addEventListener("dragstart", function (event) {
    var card = event.target.closest(".blockr-panel-nav-card");
    if (!card || !navRoot(card)) return;
    // The eye and an in-progress rename must not start a drag.
    if (
      card.getAttribute("data-editing") === "true" ||
      event.target.closest(".blockr-panel-nav-eye")
    ) {
      event.preventDefault();
      return;
    }
    event.dataTransfer.setData("text/plain", card.getAttribute("data-block-id"));
    event.dataTransfer.effectAllowed = "move";
    card.classList.add("pn-dragging");
  });

  document.addEventListener("dragend", function (event) {
    var card = event.target.closest(".blockr-panel-nav-card");
    if (card) card.classList.remove("pn-dragging");
    var roots = document.querySelectorAll(".pn-drop-target");
    Array.prototype.forEach.call(roots, function (el) {
      el.classList.remove("pn-drop-target");
    });
  });

  document.addEventListener("dragover", function (event) {
    var grp = dropGroup(event.target);
    if (!grp || !navRoot(grp)) return;
    event.preventDefault();
    event.dataTransfer.dropEffect = "move";
  });

  document.addEventListener("dragenter", function (event) {
    var grp = dropGroup(event.target);
    if (grp && navRoot(grp)) grp.classList.add("pn-drop-target");
  });

  document.addEventListener("dragleave", function (event) {
    var grp = dropGroup(event.target);
    if (grp && !grp.contains(event.relatedTarget)) {
      grp.classList.remove("pn-drop-target");
    }
  });

  document.addEventListener("drop", function (event) {
    var grp = dropGroup(event.target);
    if (!grp || !navRoot(grp)) return;
    event.preventDefault();
    grp.classList.remove("pn-drop-target");
    var bid = event.dataTransfer.getData("text/plain");
    if (!bid) return;
    // `data-stack-id` is the target stack ("" = Ungrouped / out of stack).
    send(navRoot(grp), {
      kind: "assign",
      id: bid,
      to: grp.getAttribute("data-stack-id") || ""
    });
  });

  // ---- inline rename (double-click a block name or stack heading) ------

  document.addEventListener("dblclick", function (event) {
    var root = navRoot(event.target);
    if (!root) return;

    var nameEl = event.target.closest(".blockr-block-browser-card-name");
    if (nameEl) {
      var card = nameEl.closest(".blockr-panel-nav-card");
      beginEdit(root, nameEl, "rename", card.getAttribute("data-block-id"), card);
      return;
    }

    var headEl = event.target.closest(".blockr-panel-nav-group-head h3");
    if (headEl) {
      var grp = headEl.closest(".blockr-panel-nav-group");
      var sid = grp.getAttribute("data-stack-id");
      // Ungrouped (no id) is not a real stack and can't be renamed.
      if (sid) beginEdit(root, headEl, "rename_stack", sid, null);
    }
  });

  function beginEdit(root, el, kind, id, card) {
    if (el.getAttribute("contenteditable") === "true") return;
    var orig = el.textContent;
    var done = false;

    if (card) {
      card.setAttribute("data-editing", "true");
      card.setAttribute("draggable", "false");
    }
    el.setAttribute("contenteditable", "true");
    el.classList.add("pn-editing");
    el.focus();

    var range = document.createRange();
    range.selectNodeContents(el);
    var sel = window.getSelection();
    sel.removeAllRanges();
    sel.addRange(range);

    function finish(commit) {
      if (done) return;
      done = true;
      el.removeAttribute("contenteditable");
      el.classList.remove("pn-editing");
      if (card) {
        card.removeAttribute("data-editing");
        card.setAttribute("draggable", "true");
      }
      el.removeEventListener("keydown", onKey);
      el.removeEventListener("blur", onBlur);

      var val = el.textContent.trim();
      if (commit && val && val !== orig) {
        send(root, { kind: kind, id: id, value: val });
      } else {
        el.textContent = orig;
      }
    }

    function onKey(ev) {
      if (ev.key === "Enter") {
        ev.preventDefault();
        finish(true);
      } else if (ev.key === "Escape") {
        ev.preventDefault();
        finish(false);
      }
    }
    function onBlur() {
      finish(true);
    }

    el.addEventListener("keydown", onKey);
    el.addEventListener("blur", onBlur);
  }
})();
