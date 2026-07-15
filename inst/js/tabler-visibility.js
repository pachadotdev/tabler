/**
 * tabler-visibility.js
 * show()/hide()/toggle() DOM handling for tablerApp — the browser-side
 * counterpart of R/visibility.R.
 *
 * Listens for the "tabler:message" CustomEvent dispatched by
 * tabler-reactive.js whenever the R session calls session$sendCustomMessage()
 * with type "tabler-show", "tabler-hide", or "tabler-toggle".
 *
 * No framework dependencies — plain ES5-compatible JavaScript.
 */
(function () {
  "use strict";

  // R's `list(id = NULL, ...)` is serialised by jsonlite as `{}` (an empty
  // object), not JSON null. Treat both "" / undefined / an object as "absent".
  function isStr(v) {
    return typeof v === "string" && v.length > 0;
  }

  function getElements(params) {
    var els = [];
    if (isStr(params.id)) {
      var el = document.getElementById(params.id);
      if (el) els.push(el);
    } else if (isStr(params.selector)) {
      var found = document.querySelectorAll(params.selector);
      for (var i = 0; i < found.length; i++) els.push(found[i]);
    }
    return els;
  }

  function isHidden(el) {
    return window.getComputedStyle(el).display === "none";
  }

  // Force a reflow so a subsequently-set CSS property animates instead of
  // jumping straight to its final value.
  function reflow(el) {
    /* eslint-disable-next-line no-unused-expressions */
    void el.offsetWidth;
  }

  function fade(el, doShow, ms) {
    el.style.transition = "opacity " + ms + "ms";
    if (doShow) {
      el.style.display = "";
      el.style.opacity = "0";
      reflow(el);
      el.style.opacity = "1";
    } else {
      el.style.opacity = "1";
      reflow(el);
      el.style.opacity = "0";
      window.setTimeout(function () {
        el.style.display = "none";
      }, ms);
    }
  }

  function slide(el, doShow, ms) {
    el.style.transition = "max-height " + ms + "ms ease";
    el.style.overflow = "hidden";
    if (doShow) {
      el.style.display = "";
      el.style.maxHeight = "0px";
      reflow(el);
      el.style.maxHeight = el.scrollHeight + "px";
      window.setTimeout(function () {
        el.style.maxHeight = "";
        el.style.overflow = "";
        el.style.transition = "";
      }, ms);
    } else {
      el.style.maxHeight = el.scrollHeight + "px";
      reflow(el);
      el.style.maxHeight = "0px";
      window.setTimeout(function () {
        el.style.display = "none";
        el.style.maxHeight = "";
        el.style.overflow = "";
        el.style.transition = "";
      }, ms);
    }
  }

  function setVisibility(el, doShow, params) {
    var ms = (params.time || 0) * 1000;
    if (!params.anim) {
      el.style.display = doShow ? "" : "none";
    } else if (params.animType === "fade") {
      fade(el, doShow, ms);
    } else {
      slide(el, doShow, ms);
    }
  }

  function showHide(doShow, params) {
    var els = getElements(params);
    for (var i = 0; i < els.length; i++) {
      setVisibility(els[i], doShow, params);
    }
  }

  function toggle(params) {
    // R's jsonlite serialises `condition = NULL` as `{}`, so only a real
    // boolean means the caller supplied an explicit condition.
    if (typeof params.condition === "boolean") {
      showHide(params.condition, params);
      return;
    }
    var els = getElements(params);
    for (var i = 0; i < els.length; i++) {
      setVisibility(els[i], isHidden(els[i]), params);
    }
  }

  function handleMessage(e) {
    var detail = e.detail || {};
    var msg = detail.message || {};
    if (detail.type === "tabler-show") {
      showHide(true, msg);
    } else if (detail.type === "tabler-hide") {
      showHide(false, msg);
    } else if (detail.type === "tabler-toggle") {
      toggle(msg);
    }
  }

  document.addEventListener("tabler:message", handleMessage);
}());
