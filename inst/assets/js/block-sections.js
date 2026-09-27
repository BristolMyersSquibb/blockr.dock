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
})();
