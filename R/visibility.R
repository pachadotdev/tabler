#' Show/Hide an element
#'
#' Show or hide an HTML element in a running \code{\link{tabler_app}}.\cr\cr
#' \strong{\code{show}} makes an element visible, \strong{\code{hide}} makes
#' an element invisible, \strong{\code{toggle}} displays the element if it is
#' hidden and hides it if it is visible.\cr\cr
#' \strong{\code{show_element}}, \strong{\code{hide_element}}, and
#' \strong{\code{toggle_element}} are synonyms for \code{show}/\code{hide}/
#' \code{toggle}.\cr\cr
#' If \code{condition} is given to \code{toggle}, that condition is used to
#' decide whether to show or hide the element: the element is shown when the
#' condition evaluates to \code{TRUE} and hidden otherwise.
#'
#' @details
#' This is a dependency-free adaptation of \pkg{shinyjs}'s
#' \code{show}/\code{hide}/\code{toggle} functions for \pkg{tabler}, which does
#' not use or depend on \pkg{shiny}. Instead of relying on a Shiny session,
#' these functions use the plain-list \code{session} object created by
#' \code{\link{tabler_app}} and passed as the third argument to the
#' \code{server} function.
#'
#' @param session The \code{session} object passed by \code{\link{tabler_app}}
#'   to the server function.
#' @param id The id of the element/HTML tag.
#' @param anim If \code{TRUE} then animate the behaviour.
#' @param anim_type The type of animation to use, either \code{"slide"} or \code{"fade"}.
#' @param time The number of seconds to make the animation last.
#' @param selector CSS selector of the elements to show/hide. Ignored if the
#' \code{id} argument is given. For example, to select all span elements with
#' class x, use \code{selector = "span.x"}.
#' @param condition An optional argument to \code{toggle}, see 'Details' below.
#' @seealso \code{\link{tabler_app}}
#' @examples
#' if (interactive()) {
#'   ui <- page(
#'     title = "Show/Hide Example",
#'     body = body(
#'       action_button("btn", "Click me"),
#'       div(id = "panel", "Watch what happens to me")
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observe_event(input$btn, {
#'       # Change the following line for more examples
#'       toggle(session, "panel")
#'     })
#'   }
#'
#'   tabler_app(ui, server)
#' }
#' \dontrun{
#' # The function call in the above app can be replaced by any of the
#' # following examples to produce similar apps
#' toggle(session, id = "panel")
#' toggle(session, "panel", TRUE)
#' toggle(session, "panel", TRUE, "fade", 2)
#' toggle(session, id = "panel", time = 1, anim = TRUE, anim_type = "slide")
#' show(session, "panel")
#' show(session, id = "panel", anim = TRUE)
#' hide(session, "panel")
#' hide(session, id = "panel", anim = TRUE)
#' }
#'
#' ## toggle can be given an optional `condition` argument, which
#' ## determines if to show or hide the element
#' if (interactive()) {
#'   ui <- page(
#'     title = "Conditional Toggle",
#'     body = body(
#'       checkbox_input("checkbox", "Show the text", TRUE),
#'       div(id = "element", "Watch what happens to me")
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observe({
#'       toggle(session, id = "element", condition = input$checkbox)
#'     })
#'   }
#'
#'   tabler_app(ui, server)
#' }
#' @name visibilityFuncs
NULL

# Send a show/hide/toggle message to all connected browsers via the
# tabler_app session object (see R/app.R: session$sendCustomMessage).
.visibilityMessage <- function(session, fxn, params) {
  if (!is.list(session) || !is.function(session[["sendCustomMessage"]])) {
    warning(fxn, "() requires a tabler_app session object - ignoring", call. = FALSE)
    return(invisible(NULL))
  }
  # Respect module namespacing: within module_server(), session$ns() prefixes
  # ids with the module's namespace so id = "panel" resolves to the actual
  # DOM id (e.g. "co-panel") created by the module's ns() call in the UI.
  if (!is.null(params[["id"]]) && is.function(session[["ns"]])) {
    params[["id"]] <- session$ns(params[["id"]])
  }
  session$sendCustomMessage(paste0("tabler-", fxn), params)
  invisible(NULL)
}

#' @export
#' @rdname visibilityFuncs
show <- function(session = get_default_reactive_domain(), id = NULL, anim = FALSE,
                 anim_type = "slide", time = 0.5, selector = NULL) {
  params <- list(
    id = id, anim = anim, anim_type = anim_type,
    time = time, selector = selector
  )
  .visibilityMessage(session, "show", params)
}

#' @export
#' @rdname visibilityFuncs
show_element <- show

#' @export
#' @rdname visibilityFuncs
hide <- function(session = get_default_reactive_domain(), id = NULL, anim = FALSE,
                 anim_type = "slide", time = 0.5, selector = NULL) {
  params <- list(
    id = id, anim = anim, anim_type = anim_type,
    time = time, selector = selector
  )
  .visibilityMessage(session, "hide", params)
}

#' @export
#' @rdname visibilityFuncs
hide_element <- hide

#' @export
#' @rdname visibilityFuncs
toggle <- function(session = get_default_reactive_domain(), id = NULL, anim = FALSE,
                   anim_type = "slide", time = 0.5, selector = NULL, condition = NULL) {
  params <- list(
    id = id, anim = anim, anim_type = anim_type,
    time = time, selector = selector, condition = condition
  )
  .visibilityMessage(session, "toggle", params)
}

#' @export
#' @rdname visibilityFuncs
toggle_element <- toggle

#' @title Initialize a Tag as Hidden
#' @description Create a tag (or tag list) that is invisible when the page
#'   first loads. It can be made visible later with \code{\link{toggle}} or
#'   \code{\link{show}}.
#' @param ... Tag (or tag_list or list of tags) to make invisible.
#' @return The tag (or tags) that were given as an argument, in a hidden state.
#' @seealso \code{\link{show}}, \code{\link{hide}}, \code{\link{toggle}}
#' @examples
#' if (interactive()) {
#'   ui <- page(
#'     title = "Hidden Example",
#'     body = body(
#'       action_button("btn", "Click me"),
#'       hidden(
#'         p(id = "element", "I was born invisible")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observe_event(input$btn, {
#'       show(session, "element")
#'     })
#'   }
#'
#'   tabler_app(ui, server)
#' }
#' @export
hidden <- function(...) {
  items <- list(...)
  hidden <- lapply(items, .hideTag)
  if (length(hidden) == 1L) hidden[[1L]] else structure(hidden, class = c("tabler.tag.list", "list"))
}

# Recursively add `display:none;` to the `style` attribute of every tag
.hideTag <- function(x) {
  if (inherits(x, "tabler.tag")) {
    style <- x$attribs[["style"]]
    x$attribs[["style"]] <- paste0(if (!is.null(style) && nzchar(style)) paste0(style, ";") else "", "display:none;")
    x
  } else if (inherits(x, "tabler.tag.list") || (is.list(x) && !inherits(x, "tabler.tag"))) {
    structure(lapply(x, .hideTag), class = class(x))
  } else {
    x
  }
}
