// Inline month calendar for dateInput(inline = TRUE). Vanilla JS, no
// external calendar library - renders one month at a time with prev/next
// navigation, and reports the selected day as a "yyyy-mm-dd" string via the
// element's data-value attribute + a bubbling "input" event, which
// tabler-reactive.js's generic input binding (data-tabler-type="date-inline")
// picks up and sends to the server.
(function () {
  "use strict";

  var MONTH_NAMES = [
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  ];
  var WEEKDAYS = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];

  function pad2(n) {
    return n < 10 ? "0" + n : String(n);
  }

  function toIso(year, month, day) {
    return year + "-" + pad2(month + 1) + "-" + pad2(day);
  }

  function todayIso() {
    var d = new Date();
    return toIso(d.getFullYear(), d.getMonth(), d.getDate());
  }

  function parseIso(str) {
    if (!str) return null;
    var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(str);
    if (!m) return null;
    return { year: parseInt(m[1], 10), month: parseInt(m[2], 10) - 1, day: parseInt(m[3], 10) };
  }

  function render(el) {
    var view = el._tablerDpView;
    var selected = el.getAttribute("data-value") || "";
    var min = el.getAttribute("data-min") || "";
    var max = el.getAttribute("data-max") || "";
    var today = todayIso();

    var firstOfMonth = new Date(view.year, view.month, 1);
    var daysInMonth = new Date(view.year, view.month + 1, 0).getDate();
    var leading = (firstOfMonth.getDay() + 6) % 7; // Monday-first index

    var html = "";
    html += '<div class="tabler-datepicker-header">';
    html += '<button type="button" class="btn btn-icon button-prev-month" aria-label="Previous month"><i class="ti ti-chevron-left"></i></button>';
    html += '<span class="tabler-datepicker-title">' + MONTH_NAMES[view.month] + " " + view.year + "</span>";
    html += '<button type="button" class="btn btn-icon button-next-month" aria-label="Next month"><i class="ti ti-chevron-right"></i></button>';
    html += "</div>";

    html += '<div class="tabler-datepicker-weekdays">';
    for (var w = 0; w < 7; w++) html += "<div>" + WEEKDAYS[w] + "</div>";
    html += "</div>";

    html += '<div class="tabler-datepicker-days">';
    for (var b = 0; b < leading; b++) html += "<div></div>";
    for (var day = 1; day <= daysInMonth; day++) {
      var iso = toIso(view.year, view.month, day);
      var classes = "day-item";
      if (iso === today) classes += " is-today";
      if (iso === selected) classes += " is-selected";
      if ((min && iso < min) || (max && iso > max)) classes += " is-disabled";
      html += '<div class="' + classes + '" data-date="' + iso + '" tabindex="0">' + day + "</div>";
    }
    html += "</div>";

    el.innerHTML = html;
  }

  function shiftMonth(el, delta) {
    var view = el._tablerDpView;
    var m = view.month + delta;
    var y = view.year + Math.floor(m / 12);
    m = ((m % 12) + 12) % 12;
    el._tablerDpView = { year: y, month: m };
    render(el);
  }

  function selectDay(el, iso) {
    el.setAttribute("data-value", iso);
    render(el);
    el.dispatchEvent(new Event("input", { bubbles: true }));
  }

  function init(el) {
    if (el._tablerDpBound) return;
    el._tablerDpBound = true;

    var parsed = parseIso(el.getAttribute("data-value")) || parseIso(todayIso());
    el._tablerDpView = { year: parsed.year, month: parsed.month };
    render(el);

    el.addEventListener("click", function (e) {
      var prevBtn = e.target.closest(".button-prev-month");
      var nextBtn = e.target.closest(".button-next-month");
      var dayEl = e.target.closest(".day-item");

      if (prevBtn) {
        shiftMonth(el, -1);
      } else if (nextBtn) {
        shiftMonth(el, 1);
      } else if (dayEl && dayEl.classList.contains("is-disabled") === false && dayEl.getAttribute("data-date")) {
        selectDay(el, dayEl.getAttribute("data-date"));
      }
    });
  }

  function scan(root) {
    if (root.nodeType !== 1 && root.nodeType !== 9) return;
    if (root.matches && root.matches(".tabler-datepicker-inline")) init(root);
    if (root.querySelectorAll) {
      var els = root.querySelectorAll(".tabler-datepicker-inline");
      for (var i = 0; i < els.length; i++) init(els[i]);
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
