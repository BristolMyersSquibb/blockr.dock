// The "Font" board option: data-blockr-font on the root picks the body face
// (blockr.ui's blockr-font-inter.css applies Inter under "inter").
(function () {
  'use strict';
  Shiny.addCustomMessageHandler('blockr-font', function (font) {
    document.documentElement.setAttribute('data-blockr-font', font || 'open-sans');
  });
})();
