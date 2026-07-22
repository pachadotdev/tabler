/**
 * tabler-multi-select.js
 * Upgrades selectMultipleInput()'s ".tabler-multi-select" wrapper into a
 * searchable, tag-based multi-select combobox, mirroring
 * tabler-searchable-select.js's approach for the single-select case: the
 * underlying native <select multiple> (bound via data-tabler-input, same
 * as any other input) stays the single source of truth for the reactive
 * value - this file only drives visible tags + a text box + a dropdown on
 * top of it and keeps them in sync, so tabler-reactive.js's input binding
 * and tabler-update-input.js's updateSelectInput()/updateSelectizeInput()
 * handling keep working completely unmodified.
 *
 * No framework dependencies — plain ES5-compatible JavaScript.
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

  // Read the current <option>/<optgroup> structure straight from the
  // native <select>, so it always reflects the latest choices even after
  // updateSelectInput() has rewritten its innerHTML.
  function readOptions(select) {
    var items    = [];
    var children = select.children;
    for (var i = 0; i < children.length; i++) {
      var node = children[i];
      if (node.tagName === "OPTGROUP") {
        var opts = node.children;
        for (var j = 0; j < opts.length; j++) {
          items.push({ value: opts[j].value, label: opts[j].textContent, group: node.label });
        }
      } else if (node.tagName === "OPTION") {
        items.push({ value: node.value, label: node.textContent, group: null });
      }
    }
    return items;
  }

  function selectedValues(select) {
    var out  = [];
    var opts = select.selectedOptions || select.querySelectorAll("option:checked");
    for (var i = 0; i < opts.length; i++) out.push(opts[i].value);
    return out;
  }

  function init(wrapper) {
    if (wrapper._tablerMultiSelectBound) return;
    wrapper._tablerMultiSelectBound = true;

    var input  = wrapper.querySelector("[data-tabler-multi-select-search]");
    var menu   = wrapper.querySelector(".tabler-multi-select-menu");
    var select = wrapper.querySelector("select[data-tabler-input]");
    var tags   = wrapper.querySelector(".tabler-multi-select-tags");
    var ctrl   = wrapper.querySelector(".tabler-multi-select-control");
    if (!input || !menu || !select || !tags) return;

    var highlighted = -1;

    function visibleItems() {
      return menu.querySelectorAll(".dropdown-item[data-value]");
    }

    function renderTags() {
      var items   = readOptions(select);
      var byValue = {};
      for (var i = 0; i < items.length; i++) byValue[items[i].value] = items[i].label;

      var vals = selectedValues(select);
      var html = "";
      for (var j = 0; j < vals.length; j++) {
        var lbl = byValue.hasOwnProperty(vals[j]) ? byValue[vals[j]] : vals[j];
        html += '<span class="tabler-multi-select-tag" data-value="' + escapeHtml(vals[j]) + '">' +
          escapeHtml(lbl) +
          '<button type="button" class="tabler-multi-select-tag-remove" data-value="' +
          escapeHtml(vals[j]) + '" aria-label="Remove ' + escapeHtml(lbl) + '">' +
          '<i class="ti ti-x"></i></button></span>';
      }
      tags.innerHTML = html;
    }

    function renderMenu(filterText) {
      var items    = readOptions(select);
      var selected = selectedValues(select);
      var q        = (filterText || "").toLowerCase();
      var filtered = [];
      for (var i = 0; i < items.length; i++) {
        if (selected.indexOf(items[i].value) !== -1) continue;
        if (q === "" || items[i].label.toLowerCase().indexOf(q) !== -1) filtered.push(items[i]);
      }

      var html         = "";
      var currentGroup = "\u0000"; // sentinel that never matches a real group name
      if (filtered.length === 0) {
        html = '<li><span class="dropdown-item disabled">No matches</span></li>';
      } else {
        for (var k = 0; k < filtered.length; k++) {
          var it = filtered[k];
          if (it.group !== currentGroup) {
            if (it.group) html += '<li><h6 class="dropdown-header">' + escapeHtml(it.group) + "</h6></li>";
            currentGroup = it.group;
          }
          html += '<li><a class="dropdown-item" href="#" data-value="' +
            escapeHtml(it.value) + '">' + escapeHtml(it.label) + "</a></li>";
        }
      }
      menu.innerHTML = html;
      highlighted = -1;
    }

    function openMenu() {
      renderMenu(input.value);
      menu.classList.add("show");
      input.setAttribute("aria-expanded", "true");
    }

    function closeMenu() {
      menu.classList.remove("show");
      input.setAttribute("aria-expanded", "false");
      highlighted = -1;
    }

    function setOptionSelected(value, isSelected) {
      var opts = select.querySelectorAll("option");
      for (var i = 0; i < opts.length; i++) {
        if (opts[i].value === value) opts[i].selected = isSelected;
      }
      select.dispatchEvent(new Event("change", { bubbles: true }));
    }

    function addValue(value) {
      setOptionSelected(value, true);
      input.value = "";
      renderTags();
      renderMenu("");
    }

    function removeValue(value) {
      setOptionSelected(value, false);
      renderTags();
      if (menu.classList.contains("show")) renderMenu(input.value);
    }

    function highlight(idx) {
      var els = visibleItems();
      for (var i = 0; i < els.length; i++) els[i].classList.toggle("tabler-select-search-hl", i === idx);
      if (idx >= 0 && els[idx] && els[idx].scrollIntoView) els[idx].scrollIntoView({ block: "nearest" });
      highlighted = idx;
    }

    input.addEventListener("focus", openMenu);

    input.addEventListener("input", function () {
      renderMenu(input.value);
      openMenu();
    });

    input.addEventListener("keydown", function (e) {
      if (e.key === "ArrowDown") {
        e.preventDefault();
        if (!menu.classList.contains("show")) openMenu();
        highlight(Math.min(highlighted + 1, visibleItems().length - 1));
      } else if (e.key === "ArrowUp") {
        e.preventDefault();
        highlight(Math.max(highlighted - 1, 0));
      } else if (e.key === "Enter") {
        if (menu.classList.contains("show")) {
          e.preventDefault();
          var els = visibleItems();
          if (highlighted >= 0 && els[highlighted]) els[highlighted].click();
        }
      } else if (e.key === "Backspace" && input.value === "") {
        var vals = selectedValues(select);
        if (vals.length) removeValue(vals[vals.length - 1]);
      } else if (e.key === "Escape") {
        input.value = "";
        closeMenu();
      }
    });

    // Use mousedown (fires before the input's blur) so a click on an item
    // registers before renderMenu()/closeMenu() would otherwise run.
    menu.addEventListener("mousedown", function (e) {
      var target = e.target.closest ? e.target.closest(".dropdown-item[data-value]") : null;
      if (!target) return;
      e.preventDefault();
      addValue(target.getAttribute("data-value"));
    });

    tags.addEventListener("click", function (e) {
      var btn = e.target.closest ? e.target.closest(".tabler-multi-select-tag-remove") : null;
      if (!btn) return;
      removeValue(btn.getAttribute("data-value"));
    });

    // Clicking anywhere in the control (e.g. empty space beside the tags)
    // focuses the search box, like a real tag input.
    if (ctrl) {
      ctrl.addEventListener("mousedown", function (e) {
        if (e.target === input) return;
        e.preventDefault();
        input.focus();
      });
    }

    input.addEventListener("blur", function () {
      // Delay so the mousedown handlers above can run first.
      setTimeout(closeMenu, 150);
    });

    // Keep tags in sync when the server updates the underlying <select>
    // via updateSelectInput()/updateSelectizeInput() (see
    // tabler-update-input.js), which rewrites its <option>s and dispatches
    // a "change" event on it.
    select.addEventListener("change", function () {
      renderTags();
    });

    renderTags();
  }

  function scan(root) {
    var container = root || document;
    if (container.classList && container.classList.contains("tabler-multi-select")) init(container);
    var wrappers = container.querySelectorAll
      ? container.querySelectorAll(".tabler-multi-select")
      : [];
    for (var i = 0; i < wrappers.length; i++) init(wrappers[i]);
  }

  document.addEventListener("DOMContentLoaded", function () { scan(document); });

  // Pick up wrappers injected later by renderUI()/uiOutput() updates or any
  // other dynamic DOM insertion, without depending on tabler-reactive.js
  // internals.
  new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i++) {
      var added = mutations[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        if (added[j].nodeType === 1) scan(added[j]);
      }
    }
  }).observe(document.documentElement, { childList: true, subtree: true });
})();
