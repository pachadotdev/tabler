/**
 * tabler-login.js
 * Handles the "tabler-logout" custom message sent by logout() (see login.R).
 * Real logout only ever happens server-side (via the /logout endpoint,
 * which clears the signed session cookie) - this just navigates the
 * browser there, the same way tabler-progress.js reacts to
 * showProgress()/hideProgress().
 */
(function () {
  "use strict";

  document.addEventListener("tabler:message", function (e) {
    var detail = e.detail || {};
    if (detail.type === "tabler-logout") {
      window.location.href = "/logout";
    }
  });
})();
