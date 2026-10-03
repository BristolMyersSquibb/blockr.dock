// A card's open sections (block_card_toggles() in R), as the Shiny input the
// server opens and closes the card's panels from. The set lives in
// `data-sections`. A button toggles its own section; the controls have no
// button, and the card's "…" menu toggles them with a `blockr-section:toggle`
// event carrying the section.
(function () {
  'use strict';

  function sections(el) {
    return el.getAttribute('data-sections').split(' ').filter(Boolean);
  }

  function toggle(el, section) {
    var open = sections(el);
    var at = open.indexOf(section);
    if (at < 0) open.push(section);
    else open.splice(at, 1);
    el.setAttribute('data-sections', open.join(' '));
    el.querySelectorAll('[data-section]').forEach(function (btn) {
      var on = open.indexOf(btn.getAttribute('data-section')) >= 0;
      btn.classList.toggle('active', on);
      btn.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
  }

  var binding = new Shiny.InputBinding();

  $.extend(binding, {
    find: function (scope) {
      return $(scope).find('.blockr-section-toggle');
    },
    // Every section closed reads as NULL, as a checkbox group reports it.
    getValue: function (el) {
      var open = sections(el);
      return open.length ? open : null;
    },
    subscribe: function (el, callback) {
      $(el).on('click.blockrSections', '[data-section]', function () {
        toggle(el, this.getAttribute('data-section'));
        callback();
      });
      $(el).on('blockr-section:toggle.blockrSections', function (e) {
        toggle(el, e.originalEvent.detail);
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off('.blockrSections');
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.dock.sections');
})();
