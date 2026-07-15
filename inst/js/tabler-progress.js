/**
 * tabler-progress.js
 * Full-page loading overlay driven by showProgress()/hideProgress() server
 * calls. Listens for the generic "tabler:message" CustomEvent dispatched by
 * tabler-reactive.js for any custom message type other than "tablerSyncUrl".
 */
(function () {
  "use strict";

  var progressInterval = null;

  function ensureOverlay() {
    var overlay = document.getElementById("tabler-progress-overlay");
    if (overlay) return overlay;

    overlay = document.createElement("div");
    overlay.id = "tabler-progress-overlay";
    overlay.innerHTML =
      '<div class="tabler-progress-container">' +
        '<div class="tabler-progress-text" id="tabler-progress-text"></div>' +
        '<div class="tabler-progress-bar-wrapper">' +
          '<div class="tabler-progress-bar" id="tabler-progress-bar"></div>' +
        "</div>" +
      "</div>";
    document.body.appendChild(overlay);
    return overlay;
  }

  function showProgress(message) {
    var overlay = ensureOverlay();
    var text = document.getElementById("tabler-progress-text");
    var bar  = document.getElementById("tabler-progress-bar");

    text.textContent = (message && message.text) || "Loading...";
    bar.style.width  = "0%";
    overlay.style.display = "flex";

    if (progressInterval) clearInterval(progressInterval);
    var width = 0;
    progressInterval = setInterval(function () {
      if (width >= 90) {
        clearInterval(progressInterval);
        progressInterval = null;
      } else {
        width += Math.random() * 10;
        if (width > 90) width = 90;
        bar.style.width = width + "%";
      }
    }, 200);
  }

  function hideProgress() {
    var overlay = document.getElementById("tabler-progress-overlay");
    if (!overlay) return;
    if (progressInterval) {
      clearInterval(progressInterval);
      progressInterval = null;
    }
    var bar = document.getElementById("tabler-progress-bar");
    if (bar) bar.style.width = "100%";
    setTimeout(function () {
      overlay.style.display = "none";
    }, 300);
  }

  document.addEventListener("tabler:message", function (e) {
    var detail = e.detail || {};
    if (detail.type === "tabler-showProgress") {
      showProgress(detail.message);
    } else if (detail.type === "tabler-hideProgress") {
      hideProgress();
    }
  });
})();
