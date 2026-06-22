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
})();
