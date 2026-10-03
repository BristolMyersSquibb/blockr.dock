// Marks <html> with `blockr-computing` while Shiny is busy and an output in
// the view container is recalculating, which is when the navbar spinner turns.
//
// The CSS used to read this off `html.shiny-busy:has(.blockr-view-container
// .recalculating)`. A `:has()` anchored on <html> makes Chrome search the
// whole page each time something inside it changes, which on a large board
// was most of the style work of typing into a field. A class written here
// costs one query per Shiny event instead.
(function () {
  var root = document.documentElement;
  var queued = false;

  function update() {
    queued = false;
    var on = root.classList.contains('shiny-busy') &&
      document.querySelector('.blockr-view-container .recalculating') !== null;
    if (root.classList.contains('blockr-computing') !== on) {
      root.classList.toggle('blockr-computing', on);
    }
  }

  function schedule() {
    if (queued) return;
    queued = true;
    requestAnimationFrame(update);
  }

  $(document).on(
    'shiny:busy shiny:idle shiny:recalculating shiny:recalculated ' +
      'shiny:value shiny:error shiny:outputinvalidated',
    schedule
  );
})();
