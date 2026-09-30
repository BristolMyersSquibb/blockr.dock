// Renames a block in place, for every card from one set of listeners on the
// document, whenever the card was inserted. A title marked
// `data-blockr-editable` starts editing on a double-click (the block menu's
// "Rename" sends one). The field is the card's Shiny text input, so a name
// reaches the server the way a keystroke does. Enter and a click elsewhere
// commit, Escape restores the name editing began with; an empty name is
// refused on Enter and dropped on blur.
(function () {
  var FIELD = '.blockr-inline-edit > .blockr-title-edit input';

  function parts(input) {
    var root = $(input).closest('.blockr-inline-edit');
    return {
      wrap: root.children('.blockr-title-edit'),
      display: root.children('.blockr-title-display')
    };
  }

  $(document).on(
    'dblclick',
    '.blockr-inline-edit > .blockr-title-display[data-blockr-editable]',
    function () {
      var wrap = $(this).siblings('.blockr-title-edit');
      var input = wrap.find('input');
      input.data('before', input.val());
      wrap.removeClass('is-invalid');
      // Hiding with `visibility` keeps the name's box, so the row keeps its
      // height and the field, positioned against it, lands on the name.
      this.style.visibility = 'hidden';
      wrap.show();
      input[0].focus();
      input[0].select();
    }
  );

  $(document).on('focusout', FIELD, function () {
    var p = parts(this);
    if (!$.trim(this.value)) {
      $(this).val($(this).data('before')).trigger('change');
    }
    p.wrap.removeClass('is-invalid').hide();
    p.display.css('visibility', '');
  });

  $(document).on('keydown', FIELD, function (e) {
    if (e.key === 'Enter') {
      if (!$.trim(this.value)) {
        parts(this).wrap.addClass('is-invalid');
        return;
      }
      this.blur();
    } else if (e.key === 'Escape') {
      $(this).val($(this).data('before')).trigger('change');
      this.blur();
    }
  });

  $(document).on('input change', FIELD, function () {
    var p = parts(this);
    if ($.trim(this.value)) p.wrap.removeClass('is-invalid');
    p.display.find('.blockr-title').text(this.value);
  });
})();
