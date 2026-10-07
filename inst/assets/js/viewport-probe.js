$(function () {
  // The viewport width, as a startup-only Shiny input. `getValue()` runs during
  // input initialisation, so the width rides the session's first input batch
  // and the server can take the narrow decision before it inserts a dock --
  // there is no dock output at page load to read a width from.
  //
  // There is deliberately no `subscribe()`: the decision is taken once per
  // session and reflowing takes a reload, so a resize must not re-fire this.
  var viewportBinding = new Shiny.InputBinding();

  $.extend(viewportBinding, {
    find: function (scope) {
      return $(scope).find('.blockr-viewport-probe');
    },

    getValue: function () {
      return window.innerWidth;
    },

    subscribe: function () {},

    unsubscribe: function () {}
  });

  Shiny.inputBindings.register(viewportBinding, 'blockr.viewportProbe');
});
