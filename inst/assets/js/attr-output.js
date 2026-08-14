$(function () {
  // An output whose value is a set of attributes to stamp onto its own
  // element. Shiny's HTML output path (`renderContent`) unbinds, initializes
  // and re-binds the rendered scope, and the unbind and the bind each book a
  // clientdata walk of every bound output on the page -- so a card that only
  // needs a `style` or a `data-status` written pays for two of those. Writing
  // the attributes directly costs nothing beyond the write, while the element
  // stays a real Shiny output: it suspends while hidden and re-renders when a
  // deferred card is inserted, which a custom message would not.
  var attrOutput = new Shiny.OutputBinding();

  $.extend(attrOutput, {
    find: function (scope) {
      return $(scope).find('.blockr-attr-output');
    },

    // An empty value clears the attribute rather than writing an empty one,
    // so the "no status" state leaves the element as bare as it was built.
    renderValue: function (el, attrs) {
      $.each(attrs || {}, function (name, value) {
        if (value === null || value === '') {
          el.removeAttribute(name);
        } else {
          el.setAttribute(name, value);
        }
      });
    }
  });

  Shiny.outputBindings.register(attrOutput, 'blockr.dock.attrOutput');
});
