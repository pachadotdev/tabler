/**
 * tabler-searchable-select.js
 * Upgrades selectInput()'s ".tabler-select-search" wrapper with type-to-
 * filter behavior: typing anywhere in an option's label filters the
 * dropdown to matching choices (substring match, not just a prefix), e.g.
 * typing "Korea" narrows the list to "South Korea", "North Korea", etc.
 *
 * The underlying native <select> (bound via data-tabler-input, same as any
 * other selectInput()) stays the single source of truth for the reactive
 * value - this file only drives a visible text box + dropdown menu on top
 * of it and keeps them in sync, so tabler-reactive.js's input binding and
 * tabler-update-input.js's updateSelectInput()/updateSelectizeInput()
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

  function currentLabel(select) {
    var opt = select.options[select.selectedIndex];
    return opt ? opt.textContent : "";
  }

  function init(wrapper) {
    if (wrapper._tablerSelectSearchBound) return;
    wrapper._tablerSelectSearchBound = true;

    var input  = wrapper.querySelector("[data-tabler-select-search]");
    var menu   = wrapper.querySelector(".tabler-select-search-menu");
    var select = wrapper.querySelector("select[data-tabler-input]");
    if (!input || !menu || !select) return;

    var highlighted = -1;

    function visibleItems() {
      return menu.querySelectorAll(".dropdown-item[data-value]");
    }

    function renderMenu(filterText) {
      var items = readOptions(select);
      var q     = (filterText || "").toLowerCase();
      var filtered = [];
      for (var i = 0; i < items.length; i++) {
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
          html += '<li><a class="dropdown-item' +
            (String(it.value) === String(select.value) ? " active" : "") +
            '" href="#" data-value="' + escapeHtml(it.value) + '">' +
            escapeHtml(it.label) + "</a></li>";
        }
      }
      menu.innerHTML = html;
      highlighted = -1;
    }

    function openMenu() {
      menu.classList.add("show");
      input.setAttribute("aria-expanded", "true");
    }

    function closeMenu() {
      menu.classList.remove("show");
      input.setAttribute("aria-expanded", "false");
      highlighted = -1;
    }

    function selectValue(value, label) {
      select.value = value;
      input.value  = label;
      select.dispatchEvent(new Event("change", { bubbles: true }));
      closeMenu();
    }

    function highlight(idx) {
      var els = visibleItems();
      for (var i = 0; i < els.length; i++) els[i].classList.toggle("tabler-select-search-hl", i === idx);
      if (idx >= 0 && els[idx] && els[idx].scrollIntoView) els[idx].scrollIntoView({ block: "nearest" });
      highlighted = idx;
    }

    input.addEventListener("focus", function () {
      renderMenu(input.value === currentLabel(select) ? "" : input.value);
      openMenu();
    });

    input.addEventListener("input", function () {
      renderMenu(input.value);
      openMenu();
    });

    input.addEventListener("keydown", function (e) {
      if (e.key === "ArrowDown") {
        e.preventDefault();
        if (!menu.classList.contains("show")) renderMenu(input.value);
        openMenu();
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
      } else if (e.key === "Escape") {
        input.value = currentLabel(select);
        closeMenu();
      }
    });

    // Use mousedown (fires before the input's blur) so a click on an item
    // registers before renderMenu()/closeMenu() would otherwise run.
    menu.addEventListener("mousedown", function (e) {
      var target = e.target.closest ? e.target.closest(".dropdown-item[data-value]") : null;
      if (!target) return;
      e.preventDefault();
      selectValue(target.getAttribute("data-value"), target.textContent);
    });

    input.addEventListener("blur", function () {
      // Delay so the mousedown handler above can run first.
      setTimeout(function () {
        input.value = currentLabel(select);
        closeMenu();
      }, 150);
    });

    // Keep the visible label in sync when the server updates the
    // underlying <select> via updateSelectInput()/updateSelectizeInput()
    // (see tabler-update-input.js), which rewrites its <option>s and
    // dispatches a "change" event on it.
    select.addEventListener("change", function () {
      if (document.activeElement !== input) input.value = currentLabel(select);
    });

    input.value = currentLabel(select);
  }

  function scan(root) {
    var container = root || document;
    if (container.classList && container.classList.contains("tabler-select-search")) init(container);
    var wrappers = container.querySelectorAll
      ? container.querySelectorAll(".tabler-select-search")
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
