(function () {
  "use strict";

  // The edit-inputs menu is a Shiny input whose value is the command the
  // user just issued against a block's input list: a `reorder` (the new
  // slot order, as link ids), a `rename` (a slot's new name), or a
  // `remove` (a slot's link id). One binding on the list root captures
  // all three via delegated listeners, so the re-rendered row `uiOutput`
  // inside carries no per-row Shiny inputs of its own. The R side owns
  // validation and turns the command into a board update.
  var EVENT = "blockr-inputs-menu:commit";
  var seq = 0;

  function isVariadic(root) {
    return root.getAttribute("data-variadic") === "true";
  }

  function commit(root, payload) {
    payload.nonce = ++seq;
    root._blockrInputsValue = payload;
    root.dispatchEvent(new CustomEvent(EVENT));
  }

  function rowOrder(root) {
    var order = [];
    root.querySelectorAll(".blockr-inputs-row[data-link-id]").forEach(
      function (row) {
        order.push(row.getAttribute("data-link-id"));
      }
    );
    return order;
  }

  // Native HTML5 drag-to-reorder. A row is only draggable while the
  // pointer is on its handle (set on mousedown, cleared on dragend), so
  // selecting text in the name field never starts a drag. `dragover`
  // moves the dragged row before / after the row under the pointer;
  // `dragend` reads the settled DOM order and commits it.
  function initDrag(root) {
    var dragging = null;

    root.addEventListener("mousedown", function (event) {
      var row = event.target.closest(".blockr-inputs-row[data-link-id]");
      if (!row || !root.contains(row)) return;
      if (event.target.closest(".blockr-inputs-drag-handle")) {
        row.setAttribute("draggable", "true");
      } else {
        row.removeAttribute("draggable");
      }
    });

    root.addEventListener("dragstart", function (event) {
      var row = event.target.closest(".blockr-inputs-row[data-link-id]");
      if (!row || !root.contains(row)) return;
      dragging = row;
      row.classList.add("is-dragging");
      event.dataTransfer.effectAllowed = "move";
      // Firefox does not start a drag without data being set.
      try {
        event.dataTransfer.setData(
          "text/plain", row.getAttribute("data-link-id") || ""
        );
      } catch (ignore) {
        // ignore
      }
    });

    root.addEventListener("dragover", function (event) {
      if (!dragging) return;
      event.preventDefault();
      var over = event.target.closest(".blockr-inputs-row[data-link-id]");
      if (!over || over === dragging || !root.contains(over)) return;
      var rect = over.getBoundingClientRect();
      var after = event.clientY - rect.top > rect.height / 2;
      var list = dragging.parentNode;
      list.insertBefore(dragging, after ? over.nextSibling : over);
    });

    root.addEventListener("drop", function (event) {
      if (dragging) event.preventDefault();
    });

    root.addEventListener("dragend", function () {
      if (!dragging) return;
      dragging.classList.remove("is-dragging");
      dragging.removeAttribute("draggable");
      dragging = null;
      if (isVariadic(root)) {
        commit(root, { action: "reorder", order: rowOrder(root) });
      }
    });
  }

  function initRemove(root) {
    root.addEventListener("click", function (event) {
      var btn = event.target.closest(".blockr-inputs-remove");
      if (!btn || !root.contains(btn)) return;
      var row = btn.closest(".blockr-inputs-row[data-link-id]");
      if (!row) return;
      commit(root, {
        action: "remove",
        link_id: row.getAttribute("data-link-id")
      });
    });
  }

  // Rename commits on `change` (blur / Enter). Enter also blurs so a user
  // who never leaves the field still commits.
  function initRename(root) {
    root.addEventListener("change", function (event) {
      var el = event.target;
      if (!el.classList) return;

      if (el.classList.contains("blockr-inputs-name-input")) {
        var row = el.closest(".blockr-inputs-row[data-link-id]");
        if (row) {
          commit(root, {
            action: "rename",
            link_id: row.getAttribute("data-link-id"),
            name: el.value
          });
        }
      }
    });

    root.addEventListener("keydown", function (event) {
      if (event.key !== "Enter") return;
      var input = event.target;
      if (input.classList &&
          input.classList.contains("blockr-inputs-name-input")) {
        event.preventDefault();
        input.blur();
      }
    });
  }

  // Finite port pickers are block-browser selectize inputs. Selectize fires
  // a jQuery `change` on the (hidden) underlying <select>, which native
  // listeners do not catch, so delegate through jQuery. An empty value
  // (the picker was cleared) disconnects the port.
  function initSourceSelect(root) {
    $(root).on("change", ".blockr-inputs-source-picker select", function () {
      var val = this.value;

      // A finite port picker redirects / disconnects that port.
      var portRow = this.closest(".blockr-inputs-row[data-port]");
      if (portRow) {
        commit(root, {
          action: "redirect",
          port: portRow.getAttribute("data-port"),
          source: val
        });
        return;
      }

      // The variadic "Add input" section appends a slot, then clears the
      // picker so it is ready for the next add.
      if (this.closest(".blockr-inputs-add-section") && val) {
        commit(root, { action: "add", source: val });
        if (this.selectize) {
          this.selectize.clear(true);
        }
      }
    });
  }

  function initMenu(root) {
    if (root.dataset.blockrInputsInit === "1") return;
    root.dataset.blockrInputsInit = "1";
    initDrag(root);
    initRemove(root);
    initRename(root);
    initSourceSelect(root);
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-inputs-menu");
    },
    initialize: function (el) {
      initMenu(el);
    },
    getValue: function (el) {
      return el._blockrInputsValue || null;
    },
    subscribe: function (el, callback) {
      initMenu(el);
      // Shiny's callback takes no args; addEventListener passes the Event,
      // which breaks the value-update path - wrap to drop it.
      var handler = function () { callback(); };
      el._blockrInputsHandler = handler;
      el.addEventListener(EVENT, handler);
    },
    unsubscribe: function (el) {
      if (el._blockrInputsHandler) {
        el.removeEventListener(EVENT, el._blockrInputsHandler);
        el._blockrInputsHandler = null;
      }
    }
  });

  Shiny.inputBindings.register(binding, "blockr.dock.inputsMenu");
})();
