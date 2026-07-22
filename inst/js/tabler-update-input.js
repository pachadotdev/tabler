/**
 * tabler-update-input.js
 * Handles server -> browser "tabler-updateInput" messages, mirroring
 * shiny's updateSelectInput()/updateSelectizeInput()/updateSliderInput().
 *
 * After mutating the DOM, it dispatches the same event type
 * tabler-reactive.js's bindInputs() listens for, so the new value is picked
 * up and sent back to the server automatically (and any slider value-badge
 * is refreshed) without duplicating that logic here.
 */
(function () {
  "use strict";

  function escapeHtml(text) {
    return String(text)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function updateInput(p) {
    if (!p || !p.id) return;
    var els = document.querySelectorAll('[data-tabler-input="' + p.id + '"]');
    if (els.length === 0) return;
    var el   = els[0];
    var type = el.getAttribute("data-tabler-type") || "text";

    if ((type === "select" || type === "select-multiple") && p.choices) {
      var selectedSet = null;
      if (p.selected !== undefined && p.selected !== null) {
        var selectedArr = Array.isArray(p.selected) ? p.selected : [p.selected];
        selectedSet = {};
        for (var s = 0; s < selectedArr.length; s++) selectedSet[String(selectedArr[s])] = true;
      }
      var html       = "";
      var currentGrp = null;
      var inGroup    = false;
      for (var i = 0; i < p.choices.length; i++) {
        var choice     = p.choices[i];
        var isSelected = selectedSet
          ? !!selectedSet[String(choice.value)]
          : (type === "select" && p.selected !== undefined && p.selected !== null &&
             String(choice.value) === String(p.selected));
        var optionHtml = '<option value="' + escapeHtml(choice.value) + '"' +
                          (isSelected ? " selected" : "") + ">" +
                          escapeHtml(choice.label) + "</option>";

        if (choice.group) {
          if (currentGrp !== choice.group) {
            if (inGroup) html += "</optgroup>";
            html += '<optgroup label="' + escapeHtml(choice.group) + '">';
            currentGrp = choice.group;
            inGroup    = true;
          }
          html += optionHtml;
        } else {
          if (inGroup) { html += "</optgroup>"; inGroup = false; currentGrp = null; }
          html += optionHtml;
        }
      }
      if (inGroup) html += "</optgroup>";
      el.innerHTML = html;
    } else if (type === "range") {
      if (p.min !== undefined && p.min !== null) el.min = p.min;
      if (p.max !== undefined && p.max !== null) el.max = p.max;
      if (p.step !== undefined && p.step !== null) el.step = p.step;
      if (p.value !== undefined && p.value !== null) {
        el.value = Array.isArray(p.value) ? p.value[0] : p.value;
      }
    } else if (type === "range2") {
      var lo = el.querySelector('[data-tabler-range-role="lo"]');
      var hi = el.querySelector('[data-tabler-range-role="hi"]');
      if (p.min !== undefined && p.min !== null) { lo.min = p.min; hi.min = p.min; }
      if (p.max !== undefined && p.max !== null) { lo.max = p.max; hi.max = p.max; }
      if (p.step !== undefined && p.step !== null) { lo.step = p.step; hi.step = p.step; }
      if (p.value !== undefined && p.value !== null) {
        var vals = Array.isArray(p.value) ? p.value : [p.value, p.value];
        lo.value = vals[0];
        hi.value = vals.length > 1 ? vals[1] : vals[0];
      }
    } else if (p.value !== undefined && p.value !== null) {
      el.value = p.value;
    } else if (p.selected !== undefined && p.selected !== null) {
      el.value = p.selected;
    }

    var eventName = (type === "range" || type === "range2") ? "input" : "change";
    el.dispatchEvent(new Event(eventName, { bubbles: true }));
  }

  document.addEventListener("tabler:message", function (e) {
    var detail = e.detail || {};
    if (detail.type === "tabler-updateInput") {
      updateInput(detail.message);
    }
  });
})();
