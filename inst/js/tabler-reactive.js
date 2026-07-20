/**
 * tabler-reactive.js
 * Browser-side reactive bridge for tablerApp (standalone httpuv server).
 * No framework dependencies — plain ES5-compatible JavaScript.
 */
(function () {
  "use strict";

  /* -----------------------------------------------------------------------
   * WebSocket connection
   * --------------------------------------------------------------------- */
  var wsUrl = (location.protocol === "https:" ? "wss://" : "ws://") +
              location.host + "/ws";
  var ws;
  var reconnectDelay  = 1000;
  var everConnected   = false;  // guard: only show overlay after first success
  var stopped         = false;  // set when server sends {type:"stop"}

  /* -----------------------------------------------------------------------
   * URL sync state
   * --------------------------------------------------------------------- */
  var urlSync = { enabled: false, exclude: [] };

  /* -----------------------------------------------------------------------
   * Disconnect overlay — uses <dialog showModal()> so the browser places it
   * in the top layer, above every z-index and every native control.
   * --------------------------------------------------------------------- */
  function showOverlay() {
    var el = document.getElementById("tabler-disconnect-overlay");
    if (el && !el.open) el.showModal();
  }

  function hideOverlay() {
    var el = document.getElementById("tabler-disconnect-overlay");
    if (el && el.open) el.close();
  }

  function createOverlay() {
    // Style the ::backdrop that showModal() creates automatically
    var style = document.createElement("style");
    style.textContent =
      "#tabler-disconnect-overlay::backdrop { background: rgba(0,0,0,0.55); }";
    document.head.appendChild(style);

    var dialog = document.createElement("dialog");
    dialog.id = "tabler-disconnect-overlay";
    dialog.setAttribute("style",
      "border:none;" +
      "border-radius:8px;" +
      "padding:2rem 3rem;" +
      "box-shadow:0 4px 24px rgba(0,0,0,0.3);" +
      "text-align:center;"
    );
    dialog.innerHTML =
      "<div style='font-size:1.5rem;font-weight:600;color:#333;'>App stopped</div>" +
      "<div style='font-size:0.9rem;color:#888;margin-top:0.5rem;'>" +
      "Reload the page once the app is restarted.</div>";
    document.body.appendChild(dialog);
  }

  function connect() {
    ws = new WebSocket(wsUrl);

    ws.onopen = function () {
      everConnected  = true;
      reconnectDelay = 1000;
      hideOverlay();
      sendCurrentInputs();
    };

    ws.onmessage = function (event) {
      try {
        var msg = JSON.parse(event.data);
        handleMessage(msg);
      } catch (e) {
        console.error("[tabler-reactive] bad message:", event.data, e);
      }
    };

    ws.onclose = function () {
      if (stopped) return;   // intentional stop: overlay already shown, no reconnect
      if (everConnected) {
        // Connection was alive and just dropped — show overlay immediately.
        showOverlay();
      }
      setTimeout(connect, reconnectDelay);
      reconnectDelay = Math.min(reconnectDelay * 2, 10000);
    };

    ws.onerror = function () { /* onclose will fire after onerror */ };
  }

  function sendMsg(obj) {
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify(obj));
    }
  }

  /* -----------------------------------------------------------------------
   * Output handling — update DOM when the server sends a value
   * --------------------------------------------------------------------- */

  // Replacing an output's innerHTML wholesale on every update destroys any
  // existing <img> (e.g. from plotOutput()) and creates a brand new one with
  // no image data yet - it renders blank/broken until the new resource
  // finishes loading over a fresh HTTP request, causing a visible flash on
  // every re-render. If both the current and incoming content are a single
  // <img>, update the existing element's attributes in place instead: the
  // browser keeps showing the last decoded frame until the new .src has
  // fully loaded, then swaps it in a single step with no flicker.
  function updateOutputHtml(el, html) {
    var existingImg = el.children.length === 1 && el.children[0].tagName === "IMG"
      ? el.children[0] : null;
    if (existingImg) {
      var tmp = document.createElement("div");
      tmp.innerHTML = html;
      var newImg = tmp.children.length === 1 ? tmp.children[0] : null;
      if (newImg && newImg.tagName === "IMG") {
        for (var i = 0; i < newImg.attributes.length; i++) {
          var attr = newImg.attributes[i];
          existingImg.setAttribute(attr.name, attr.value);
        }
        return;
      }
    }
    el.innerHTML = html;
  }

  function handleMessage(msg) {
    if (msg.type === "reload") {
      location.reload();
      return;
    } else if (msg.type === "stop") {
      stopped = true;
      showOverlay();
      return;
    } else if (msg.type === "output") {
      var el = document.getElementById(msg.id);
      if (el) {
        updateOutputHtml(el, msg.html);
        bindInputs(el);   // re-bind any newly-injected inputs (uiOutput)
      }
    } else if (msg.type === "custom") {
      if (msg.messageType === "tablerSyncUrl") {
        urlSync.exclude = (msg.message && msg.message.exclude) ? msg.message.exclude : [];
        urlSync.enabled = true;
        applyUrlParams();
        var qs = buildQueryString(collectUrlParams());
        history.replaceState(null, "", location.pathname + (qs ? "?" + qs : ""));
      } else {
        var ev = new CustomEvent("tabler:message", {
          detail: { type: msg.messageType, message: msg.message }
        });
        document.dispatchEvent(ev);
      }
    }
  }

  /* -----------------------------------------------------------------------
   * URL parameter helpers
   * --------------------------------------------------------------------- */

  // Read URL query, set matching DOM inputs, and send values to R.
  function applyUrlParams() {
    if (!location.search) return;
    var pairs = location.search.slice(1).split("&");
    for (var i = 0; i < pairs.length; i++) {
      if (!pairs[i]) continue;
      var idx = pairs[i].indexOf("=");
      if (idx < 0) continue;
      var key = decodeURIComponent(pairs[i].slice(0, idx));
      var raw = decodeURIComponent(pairs[i].slice(idx + 1));
      applyParam(key, raw);
    }
  }

  // Apply a single URL param to the matching input element(s) and send to R.
  function applyParam(id, raw) {
    var els = document.querySelectorAll('[data-tabler-input="' + id + '"]');
    if (els.length === 0) return;
    var type = els[0].getAttribute("data-tabler-type") || "text";
    var val;

    if (type === "range2") {
      // Preferred format is dash-separated (e.g. "2013-2020"), an unreserved
      // URI character that never needs percent-encoding. Still accept the
      // legacy comma-separated format for old bookmarked/shared URLs.
      var m = /^(-?[\d.]+)-(-?[\d.]+)$/.exec(raw);
      var parts = m ? [m[1], m[2]] : raw.split(",");
      var lo = els[0].querySelector('[data-tabler-range-role="lo"]');
      var hi = els[0].querySelector('[data-tabler-range-role="hi"]');
      var loV = parseFloat(parts[0]);
      var hiV = parseFloat(parts[1]);
      if (lo && !isNaN(loV)) lo.value = loV;
      if (hi && !isNaN(hiV)) hi.value = isNaN(hiV) ? loV : hiV;
      var badge2 = document.getElementById(id + "_val");
      if (badge2) badge2.textContent = loV + " - " + (isNaN(hiV) ? loV : hiV);
      val = [loV, isNaN(hiV) ? loV : hiV];
    } else if (type === "checkbox") {
      var checked = raw === "true";
      els[0].checked = checked;
      val = checked;
    } else if (type === "range" || type === "number") {
      var n = parseFloat(raw);
      els[0].value = raw;
      val = isNaN(n) ? raw : n;
      var badge = document.getElementById(id + "_val");
      if (badge) badge.textContent = raw;
    } else if (type === "radio") {
      for (var i = 0; i < els.length; i++) els[i].checked = els[i].value === raw;
      val = raw;
    } else if (type === "checkbox-group") {
      var vals = raw.split(",");
      for (var j = 0; j < els.length; j++) els[j].checked = vals.indexOf(els[j].value) !== -1;
      val = vals;
    } else {
      els[0].value = raw;
      val = raw;
    }

    sendMsg({ type: "input", name: id, value: val });
  }

  // Collect current values for all non-excluded, non-button inputs.
  function collectUrlParams() {
    var els = document.querySelectorAll("[data-tabler-input]");
    var params = {};
    var seen = {};
    for (var i = 0; i < els.length; i++) {
      var el   = els[i];
      var id   = el.getAttribute("data-tabler-input");
      var type = el.getAttribute("data-tabler-type") || "text";
      if (type === "button") continue;
      if (urlSync.exclude.indexOf(id) !== -1) continue;
      if (seen[id]) continue;
      seen[id] = true;

      if (type === "range2") {
        var pair = readRange2(el);
        params[id] = pair.lo + "-" + pair.hi;
      } else if (type === "checkbox") {
        params[id] = el.checked ? "true" : "false";
      } else if (type === "checkbox-group") {
        var all = document.querySelectorAll('[data-tabler-input="' + id + '"][data-tabler-type="checkbox-group"]');
        var checked = [];
        for (var k = 0; k < all.length; k++) if (all[k].checked) checked.push(all[k].value);
        if (checked.length) params[id] = checked.join(",");
      } else if (type === "radio") {
        var sel = document.querySelector('[data-tabler-input="' + id + '"]:checked');
        if (sel) params[id] = sel.value;
      } else {
        params[id] = el.value;
      }
    }
    return params;
  }

  // Build a query string from a plain object (no quotes, keys sorted).
  function buildQueryString(params) {
    var parts = [];
    var keys  = Object.keys(params).sort();
    for (var i = 0; i < keys.length; i++) {
      var k = keys[i], v = params[k];
      if (v !== undefined && v !== "") {
        parts.push(encodeURIComponent(k) + "=" + encodeURIComponent(v));
      }
    }
    return parts.join("&");
  }

  // Update a range/range2 slider's on-screen value badge immediately, without
  // waiting for the server round-trip - so the label the user sees while
  // dragging never lags behind the thumb.
  function updateRangeBadge(el) {
    var id   = el.getAttribute("data-tabler-input");
    var type = el.getAttribute("data-tabler-type") || "text";
    var badge = document.getElementById(id + "_val");
    if (!badge) return;
    if (type === "range2") {
      var pair = readRange2(el);
      badge.textContent = pair.lo + " - " + pair.hi;
    } else {
      badge.textContent = el.value;
    }
  }

  /* -----------------------------------------------------------------------
   * Input binding — send value to R whenever an input changes
   * --------------------------------------------------------------------- */
  function sendInputValue(el) {
    var id   = el.getAttribute("data-tabler-input");
    var type = el.getAttribute("data-tabler-type") || "text";
    var val;

    if (type === "range2") {
      var pair = readRange2(el);
      val = [pair.lo, pair.hi];
    } else if (type === "checkbox") {
      val = el.checked;
    } else if (type === "checkbox-group") {
      // Collect all checked values for this group
      var all = document.querySelectorAll(
        '[data-tabler-input="' + id + '"][data-tabler-type="checkbox-group"]'
      );
      val = [];
      for (var i = 0; i < all.length; i++) {
        if (all[i].checked) val.push(all[i].value);
      }
    } else if (type === "radio") {
      val = el.value;
    } else if (type === "range" || type === "number") {
      val = parseFloat(el.value);
      if (isNaN(val)) val = el.value;
    } else if (type === "button") {
      var n = parseInt(el.getAttribute("data-click-count") || "0", 10) + 1;
      el.setAttribute("data-click-count", String(n));
      val = n;
    } else {
      val = el.value;
    }

    sendMsg({ type: "input", name: id, value: val });

    // Update sibling value-display badge for sliders
    if (type === "range") {
      var badge = document.getElementById(id + "_val");
      if (badge) badge.textContent = el.value;
    } else if (type === "range2") {
      var badge2 = document.getElementById(id + "_val");
      if (badge2) badge2.textContent = val[0] + " - " + val[1];
    }

    // Mirror to URL if sync is enabled
    if (urlSync.enabled && type !== "button" && urlSync.exclude.indexOf(id) === -1) {
      var qs = buildQueryString(collectUrlParams());
      history.replaceState(null, "", location.pathname + (qs ? "?" + qs : ""));
    }
  }

  // Read/clamp a range2 (dual-thumb) widget's current lo/hi values. If the
  // thumb being dragged has crossed the other one, the other thumb is
  // pushed to match it so lo never exceeds hi.
  function readRange2(wrapper) {
    var lo = wrapper.querySelector('[data-tabler-range-role="lo"]');
    var hi = wrapper.querySelector('[data-tabler-range-role="hi"]');
    var loV = parseFloat(lo.value);
    var hiV = parseFloat(hi.value);
    if (loV > hiV) {
      if (document.activeElement === hi) { loV = hiV; lo.value = loV; }
      else { hiV = loV; hi.value = hiV; }
    }
    return { lo: loV, hi: hiV };
  }

  // Bind all data-tabler-input elements inside `root` (or document)
  //
  // Elements bound at page load (before the WebSocket connects) get their
  // initial value pushed to the server via sendCurrentInputs() once the
  // socket opens - see connect()'s ws.onopen. But elements injected LATER
  // by a renderUI()/uiOutput() update (e.g. a <select> inside a downloadable
  // section that only appears after a button click) are bound here, with
  // the socket already open, and sendCurrentInputs() never runs again for
  // them. Without an explicit initial send, such an input's default value
  // (e.g. the first <option selected>) is visible in the browser but the
  // corresponding input$xxx stays NULL on the server until the user
  // manually interacts with it - causing anything that reads it immediately
  // (e.g. a downloadHandler's filename()) to fail. So every newly-bound,
  // non-button input has its current value sent right after binding.
  function bindInputs(root) {
    var container = root || document;
    var els = container.querySelectorAll("[data-tabler-input]");
    for (var i = 0; i < els.length; i++) {
      (function (el) {
        if (el._tablerBound) return;   // already bound
        el._tablerBound = true;

        var type = el.getAttribute("data-tabler-type") || "text";
        var eventName;

        if (type === "range2") {
          // Native "input" events fire continuously while dragging (bubbling
          // up from either thumb) - only update the badge for those, so the
          // label tracks the thumb live without triggering a server re-render
          // per pixel moved. "change" fires once, when the drag/keypress
          // commits (e.g. on mouseup), which is when the value is sent.
          el.addEventListener("input", function () {
            updateRangeBadge(el);
          });
          el.addEventListener("change", function () {
            sendInputValue(el);
          });
          sendInputValue(el);   // push initial lo/hi values
          return;
        } else if (type === "select") {
          eventName = "change";
        } else if (type === "range") {
          eventName = "change";
        } else if (type === "checkbox" || type === "checkbox-group" || type === "radio") {
          eventName = "change";
        } else if (type === "button") {
          eventName = "click";
        } else {
          eventName = "input";   // text, number, ...
        }

        if (type === "range") {
          // Same reasoning as range2 above: live badge on "input", actual
          // send only on "change" (drag release / keypress commit).
          el.addEventListener("input", function () {
            updateRangeBadge(el);
          });
        }

        el.addEventListener(eventName, function () {
          sendInputValue(el);
        });

        // Buttons' value is a click counter that must only advance on a
        // real click (see sendCurrentInputs()'s own comment) - never send
        // an initial value for those.
        if (type !== "button") {
          sendInputValue(el);
        }
      })(els[i]);
    }
  }

  // Collect and send the current value of every bound input
  function sendCurrentInputs() {
    var els = document.querySelectorAll("[data-tabler-input]");
    var seen = {};
    for (var i = 0; i < els.length; i++) {
      var el   = els[i];
      var id   = el.getAttribute("data-tabler-input");
      var type = el.getAttribute("data-tabler-type") || "text";

      // Buttons (actionButton) have no "current value" to resend - their
      // value is a click counter that must only advance on a real click.
      // Resending it here would fake a click on every page load/reconnect,
      // immediately firing any observeEvent()/eventReactive()/bindEvent()
      // gated on the button before the user ever interacts with it.
      if (type === "button") continue;

      // For checkbox-group and radio, send once per group
      if ((type === "checkbox-group" || type === "radio") && seen[id]) continue;
      seen[id] = true;

      sendInputValue(el);
    }
  }

  /* -----------------------------------------------------------------------
   * Initialise
   * --------------------------------------------------------------------- */
  function init() {
    try { createOverlay(); } catch (e) {
      console.warn("[tabler] overlay init failed:", e);
    }
    bindInputs();
    connect();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
}());
