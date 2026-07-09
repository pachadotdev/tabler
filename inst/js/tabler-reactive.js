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
  function handleMessage(msg) {
    if (msg.type === "stop") {
      stopped = true;
      showOverlay();
      return;
    } else if (msg.type === "output") {
      var el = document.getElementById(msg.id);
      if (el) {
        el.innerHTML = msg.html;
        bindInputs(el);   // re-bind any newly-injected inputs (uiOutput)
      }
    } else if (msg.type === "custom") {
      var ev = new CustomEvent("tabler:message", {
        detail: { type: msg.messageType, message: msg.message }
      });
      document.dispatchEvent(ev);
    }
  }

  /* -----------------------------------------------------------------------
   * Input binding — send value to R whenever an input changes
   * --------------------------------------------------------------------- */
  function sendInputValue(el) {
    var id   = el.getAttribute("data-tabler-input");
    var type = el.getAttribute("data-tabler-type") || "text";
    var val;

    if (type === "checkbox") {
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
    }
  }

  // Bind all data-tabler-input elements inside `root` (or document)
  function bindInputs(root) {
    var container = root || document;
    var els = container.querySelectorAll("[data-tabler-input]");
    for (var i = 0; i < els.length; i++) {
      (function (el) {
        if (el._tablerBound) return;   // already bound
        el._tablerBound = true;

        var type = el.getAttribute("data-tabler-type") || "text";
        var eventName;

        if (type === "select") {
          eventName = "change";
        } else if (type === "range") {
          eventName = "input";
        } else if (type === "checkbox" || type === "checkbox-group" || type === "radio") {
          eventName = "change";
        } else if (type === "button") {
          eventName = "click";
        } else {
          eventName = "input";   // text, number, ...
        }

        el.addEventListener(eventName, function () {
          sendInputValue(el);
        });
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
