#' @title Theme Settings Customizer Panel
#' @description Builds a floating action button plus an offcanvas panel that
#'   lets the end-user customize the color mode, color scheme, font family,
#'   theme base (gray shade) and corner radius, mirroring the customizer found
#'   on <https://tabler.io/admin-template/preview>. Choices are persisted to
#'   `localStorage` (under the same `tabler-*` keys used by the bundled
#'   `tabler-theme.min.js`) and applied immediately via `data-bs-*` attributes
#'   on `<html>`.
#' @param color Initially-selected color scheme (default \code{"blue"}).
#' @return A `tabler.tag` tagList to be inserted into the page body.
#' @keywords internal
#' @noRd
theme_settings_panel <- function(color = "blue") {
  colors <- c(
    "blue", "azure", "indigo", "purple", "pink", "red",
    "orange", "yellow", "lime", "green", "teal", "cyan"
  )
  fonts <- c("sans-serif", "serif", "monospace", "comic")
  bases <- c("slate", "gray", "zinc", "neutral", "stone")
  radiuses <- c("0", "0.5", "1", "1.5", "2")

  capitalize <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

  radio_section <- function(name, options, selected, label_fn = capitalize) {
    tagList(lapply(options, function(opt) {
      tags$label(
        class = "form-check",
        div(
          class = "form-selectgroup-item",
          tags$input(
            type = "radio", name = name, value = opt,
            class = "form-check-input", checked = identical(opt, selected)
          ),
          div(class = "form-check-label", label_fn(opt))
        )
      )
    }))
  }

  color_section <- function(selected) {
    div(
      class = "row g-2",
      lapply(colors, function(col) {
        div(
          class = "col-auto",
          tags$label(
            class = "form-colorinput",
            tags$input(
              type = "radio", name = "theme-primary", value = col,
              class = "form-colorinput-input", checked = identical(col, selected)
            ),
            span(class = paste0("form-colorinput-color bg-", col))
          )
        )
      })
    )
  }

  section <- function(title, hint, content) {
    div(
      class = "mb-4",
      tags$label(class = "form-label", title),
      tags$p(class = "form-hint", hint),
      content
    )
  }

  div(
    class = "settings",
    a(
      href = "#", class = "btn btn-floating btn-icon btn-primary",
      `data-bs-toggle` = "offcanvas", `data-bs-target` = "#offcanvas-settings",
      `aria-controls` = "offcanvas-settings", `aria-label` = "Theme Settings",
      tags$i(class = "ti ti-brush")
    ),
    tags$form(
      class = "offcanvas offcanvas-start offcanvas-narrow", tabindex = "-1",
      id = "offcanvas-settings", role = "dialog", `aria-modal` = "true",
      `aria-labelledby` = "offcanvas-settings-title",
      div(
        class = "offcanvas-header",
        tags$h2(class = "offcanvas-title", id = "offcanvas-settings-title", "Theme Settings"),
        tags$button(
          type = "button", class = "btn-close", `data-bs-dismiss` = "offcanvas",
          `aria-label` = "Close"
        )
      ),
      div(
        class = "offcanvas-body d-flex flex-column",
        div(
          section("Color mode", "Choose the color mode for your app.", radio_section("theme", c("light", "dark"), "light")),
          section("Color scheme", "The perfect color mode for your app.", color_section(color)),
          section("Font family", "Choose the font family that fits your app.", radio_section("theme-font", fonts, "sans-serif")),
          section("Theme base", "Choose the gray shade for your app.", radio_section("theme-base", bases, "gray")),
          section("Corner Radius", "Choose the border radius factor for your app.", radio_section("theme-radius", radiuses, "1", label_fn = identity))
        ),
        div(
          class = "mt-auto space-y",
          tags$button(
            type = "button", class = "btn w-100", id = "reset-theme-settings",
            tags$i(class = "ti ti-rotate me-1"), "Reset changes"
          ),
          a(href = "#", class = "btn btn-primary w-100", `data-bs-dismiss` = "offcanvas", "Save")
        )
      )
    ),
    tags$style(HTML(
      ".settings{position:fixed;bottom:1rem;left:1rem;z-index:1030}"
    )),
    tags$script(HTML(theme_settings_script(defaults = list(
      theme = "light", `theme-base` = "gray", `theme-font` = "sans-serif",
      `theme-primary` = color, `theme-radius` = "1"
    ))))
  )
}

#' @title Theme Settings Panel Client-Side Script
#' @description Builds the inline `<script>` body that wires the settings
#'   panel's radio inputs to `localStorage`/`data-bs-*` attributes on
#'   `<html>`, mirroring the behavior of the bundled `tabler-theme.min.js`
#'   (which only runs once, on initial page load).
#' @param defaults Named list of default values, one per settings key.
#' @return A single JS string.
#' @keywords internal
#' @noRd
theme_settings_script <- function(defaults) {
  defaults_json <- paste0(
    "{", paste(sprintf("'%s':'%s'", names(defaults), unlist(defaults)), collapse = ","), "}"
  )
  sprintf("(function(){
    try{
      var defaults=%s;
      var form=document.getElementById('offcanvas-settings');
      var resetBtn=document.getElementById('reset-theme-settings');
      if(!form) return;
      var checkItems=function(){
        for(var key in defaults){
          var value=localStorage.getItem('tabler-'+key)||document.documentElement.getAttribute('data-bs-'+key)||defaults[key];
          var radios=form.querySelectorAll('[name=\"'+key+'\"]');
          radios.forEach(function(r){r.checked=(r.value===value)});
        }
      };
      form.addEventListener('change',function(e){
        var t=e.target,name=t.name,value=t.value;
        if(!(name in defaults)) return;
        localStorage.setItem('tabler-'+name,value);
        document.documentElement.setAttribute('data-bs-'+name,value);
      });
      if(resetBtn){
        resetBtn.addEventListener('click',function(){
          for(var key in defaults){
            localStorage.removeItem('tabler-'+key);
            document.documentElement.setAttribute('data-bs-'+key,defaults[key]);
          }
          checkItems();
        });
      }
      checkItems();
    }catch(e){}
  })();", defaults_json)
}
