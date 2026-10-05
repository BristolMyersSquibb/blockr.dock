// The dark-mode checkbox of the board options (dark_mode_ui() in R). It has
// no Shiny binding of its own: core's dark_mode option takes "light" or
// "dark", so this reports that word under the option's input id, and writes
// the scheme onto <html> as data-bs-theme, which the design tokens read. A
// box rendered with data-mode="auto" follows the system setting.
//
// Core's option server answers a new or restored value with bslib's
// `toggle_dark_mode()`, which sends "bslib.toggle-dark-mode". bslib's own
// handler comes with its dark-mode toggle, which the dock does not draw, so
// the message is handled here.
(function () {
  'use strict';

  function apply(dark) {
    document.documentElement.setAttribute('data-bs-theme', dark ? 'dark' : 'light');
  }

  function systemDark() {
    return !!(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
  }

  function report(box) {
    Shiny.setInputValue(box.getAttribute('data-input'), box.checked ? 'dark' : 'light');
  }

  function boxes() {
    return document.querySelectorAll('input.blockr-dark-mode');
  }

  document.addEventListener('change', function (e) {
    var box = e.target;
    if (!(box instanceof HTMLInputElement) || !box.matches('input.blockr-dark-mode')) return;
    apply(box.checked);
    report(box);
  });

  $(document).on('shiny:connected', function () {
    boxes().forEach(function (box) {
      if (box.getAttribute('data-mode') === 'auto') box.checked = systemDark();
      apply(box.checked);
      report(box);
    });

    Shiny.addCustomMessageHandler('bslib.toggle-dark-mode', function (data) {
      // A value sets the scheme; none flips it, as bslib's handler does.
      var first = boxes()[0];
      var dark = data && data.value ? data.value === 'dark' : !(first && first.checked);
      boxes().forEach(function (box) { box.checked = dark; });
      apply(dark);
    });
  });
})();
