// Keeps a block card's `data-open` attribute (the open sections, as painted
// from R) current as the section toggles open and close them. The stylesheet
// reads it for what one panel cannot see about its sibling: the rule above
// the preview shows only while controls are open above it. Bootstrap's
// collapse events bubble, so a block's own nested accordions are filtered out
// by requiring the item to sit directly in the card's accordion.
(function () {
  function update(e, open) {
    var el = e.target;
    if (!el.matches('.blockr-block-accordion > .accordion-item > .accordion-collapse')) {
      return;
    }
    var item = el.parentElement;
    var acc = item.parentElement;
    var value = item.getAttribute('data-value');
    var vals = (acc.getAttribute('data-open') || '').split(' ').filter(function (v) {
      return v && v !== value;
    });
    if (open) vals.push(value);
    acc.setAttribute('data-open', vals.join(' '));
  }

  document.addEventListener('show.bs.collapse', function (e) { update(e, true); });
  document.addEventListener('hide.bs.collapse', function (e) { update(e, false); });

  // The preview lip (plugin-block.R) flips the card's hidden "outputs"
  // checkbox, the same input the "…" menu row clicks, so the server sees one
  // input whichever toggle was used.
  document.addEventListener('click', function (e) {
    var btn = e.target instanceof Element ? e.target.closest('.blockr-preview-lip-btn') : null;
    if (!btn) return;
    var group = document.getElementById(btn.getAttribute('data-blockr-sections'));
    var input = group && group.querySelector('input[value="outputs"]');
    if (input) input.click();
  });
})();
