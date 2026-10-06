// A card's open sections (block_card_toggles() in R), as the Shiny input the
// server saves the set from. The set lives in `data-sections`. A button
// toggles its own section; the controls have no button, and the card's "…"
// menu toggles them with a `blockr-section:toggle` event carrying the
// section.
//
// The card does not wait for the server: the section folds on the click. A
// closed section carries `hidden`, so Shiny suspends the outputs in it. The
// fold animates the height, then hides; the unfold shows, then animates. A
// closing section carries `is-closing` from the start of the fold, so the
// stylesheet, which reads the sections' own state, drops the rule above the
// preview as the fold begins.
(function () {
  'use strict';

  var DURATION = 220;

  function sections(el) {
    return el.getAttribute('data-sections').split(' ').filter(Boolean);
  }

  function sectionsOf(el) {
    var card = el.closest('.blockr-block-card-body');
    return card && card.querySelector(':scope > .blockr-block-sections');
  }

  function finish(section) {
    section.classList.remove('is-folding', 'is-closing');
    section.style.height = '';
  }

  function fold(section, open) {
    var reduce = window.matchMedia &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    if (section._blockrFold) {
      clearTimeout(section._blockrFold);
      section._blockrFold = null;
      finish(section);
    }

    if (open === !section.hidden) return;

    if (reduce) {
      section.hidden = !open;
      $(section).trigger(open ? 'shown' : 'hidden');
      return;
    }

    if (open) {
      section.hidden = false;
      var h = section.scrollHeight;
      section.style.height = '0px';
      section.classList.add('is-folding');
      // Shiny re-checks visibility on `shown`, so outputs in the section
      // resume and render.
      $(section).trigger('shown');
      section.getBoundingClientRect();
      section.style.height = h + 'px';
      section._blockrFold = setTimeout(function () {
        section._blockrFold = null;
        finish(section);
      }, DURATION);
    } else {
      section.style.height = section.scrollHeight + 'px';
      section.classList.add('is-folding', 'is-closing');
      section.getBoundingClientRect();
      section.style.height = '0px';
      section._blockrFold = setTimeout(function () {
        section._blockrFold = null;
        finish(section);
        section.hidden = true;
        $(section).trigger('hidden');
      }, DURATION);
    }
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
    var box = sectionsOf(el);
    var target = box && box.querySelector(
      ':scope > .blockr-block-section[data-value="' + CSS.escape(section) + '"]'
    );
    if (target) fold(target, at < 0);
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
