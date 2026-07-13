// Tab switching functionality for Tabler dashboard.
// Vanilla JS — no jQuery or framework dependency.
(function () {
  "use strict";

  var TAB_SELECTOR = 'a[data-toggle="tab"], a[data-bs-toggle="tab"]';

  function switchTab(targetId) {
    if (!targetId) return;

    // Remove active from all tab links
    document.querySelectorAll(TAB_SELECTOR).forEach(function (el) {
      el.classList.remove("active");
    });

    // Activate the link(s) pointing at targetId
    var linkSel = 'a[data-target="' + targetId + '"], a[data-bs-target="' + targetId + '"]';
    document.querySelectorAll(linkSel).forEach(function (el) {
      el.classList.add("active");
    });

    // Hide all tab panes
    document.querySelectorAll(".tab-pane").forEach(function (el) {
      el.classList.remove("show", "active");
    });

    // Show the target pane
    var pane = document.querySelector(targetId);
    if (pane) pane.classList.add("show", "active");

    // Let charts/widgets resize
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
    }, 50);
  }

  document.addEventListener("DOMContentLoaded", function () {
    // Delegated click handler for tab links
    document.addEventListener("click", function (e) {
      var link = e.target.closest(TAB_SELECTOR);
      if (!link) return;
      e.preventDefault();
      var targetId = link.getAttribute("data-target") ||
                     link.getAttribute("data-bs-target") ||
                     link.getAttribute("href");
      switchTab(targetId);
    });

    // Activate first tab by default if none are already active
    setTimeout(function () {
      if (document.querySelector(".tab-pane.active")) return;
      var first = document.querySelector(".tab-pane");
      if (!first) return;
      var tabId = first.getAttribute("id");
      first.classList.add("show", "active");
      var sel = 'a[data-target="#' + tabId + '"], a[data-bs-target="#' + tabId + '"]';
      document.querySelectorAll(sel).forEach(function (el) {
        el.classList.add("active");
      });
      setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 100);
    }, 100);
  });
}());
