// Lightweight input mask for text inputs with a data-mask attribute, e.g.
// data-mask="00/00/0000" (date) or data-mask="(00) 0000-0000" (phone).
// "0" in the mask is a digit placeholder; every other character is a
// literal that gets inserted automatically as the user types digits.
(function () {
  "use strict";

  function applyMask(el) {
    var mask = el.getAttribute("data-mask");
    if (!mask) return;

    var maxDigits = 0;
    for (var m = 0; m < mask.length; m++) {
      if (mask[m] === "0") maxDigits++;
    }

    var digits = el.value.replace(/\D/g, "").slice(0, maxDigits);
    var out = "";
    var di = 0;
    for (var i = 0; i < mask.length && di < digits.length; i++) {
      if (mask[i] === "0") {
        out += digits[di];
        di++;
      } else {
        out += mask[i];
      }
    }

    if (out === el.value) return;

    var atEnd = el.selectionStart === el.value.length && el.selectionEnd === el.value.length;
    el.value = out;
    if (atEnd && el.setSelectionRange) {
      el.setSelectionRange(out.length, out.length);
    }
  }

  // Capture-phase listener on the document runs before any listener bound
  // directly on the input (e.g. tabler-reactive.js's change handler), so the
  // value is already reformatted by the time it gets read/sent elsewhere.
  document.addEventListener(
    "input",
    function (e) {
      var el = e.target;
      if (el && el.getAttribute && el.getAttribute("data-mask")) {
        applyMask(el);
      }
    },
    true
  );

  // Normalize any pre-filled values (initial render or dynamically-injected
  // markup) so they match the mask immediately, without waiting for input.
  function scan(root) {
    if (root.nodeType !== 1 && root.nodeType !== 9) return;
    if (root.matches && root.matches("[data-mask]")) applyMask(root);
    if (root.querySelectorAll) {
      var els = root.querySelectorAll("[data-mask]");
      for (var i = 0; i < els.length; i++) applyMask(els[i]);
    }
  }

  document.addEventListener("DOMContentLoaded", function () {
    scan(document);
  });

  new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i++) {
      var added = mutations[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        scan(added[j]);
      }
    }
  }).observe(document.documentElement, { childList: true, subtree: true });
})();
