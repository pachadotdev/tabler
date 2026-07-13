# Output placeholder tags and render functions for tablerApp

# ---------------------------------------------------------------------------
# Placeholder tags (UI side)
# ---------------------------------------------------------------------------

#' @title Text Output Placeholder
#' @description Places a \code{<span>} in the UI whose content is updated by
#'   \code{renderText} in the server.
#' @param outputId The output identifier (must match the server-side name).
#' @param inline   If \code{TRUE}, use \code{<span>}; otherwise \code{<div>}.
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
textOutput <- function(outputId, inline = FALSE) {
  if (inline) {
    span(id = outputId, class = "tabler-out-text")
  } else {
    div(id = outputId, class = "tabler-out-text")
  }
}

#' @title Verbatim Text Output Placeholder
#' @description Places a \code{<pre>} in the UI for monospace/printed output.
#' @param outputId The output identifier.
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
verbatimTextOutput <- function(outputId) {
  tags$pre(id = outputId, class = "tabler-out-verbatim bg-dark p-2 rounded")
}

#' @title UI Output Placeholder
#' @description Places a \code{<div>} whose inner HTML is replaced wholesale by
#'   \code{renderUI} output.
#' @param outputId The output identifier.
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
uiOutput <- function(outputId) {
  div(id = outputId, class = "tabler-out-ui")
}

#' @aliases htmlOutput
#' @rdname tabler-outputs
#' @export
htmlOutput <- uiOutput

# ---------------------------------------------------------------------------
# Render functions (server side)
# ---------------------------------------------------------------------------

# All render functions return a tabler_render object: a list with $expr
# (the quoted expression), $env (the caller's environment), and $type
# (a hint to the output serialiser).

.render <- function(expr_q, env, type) {
  structure(list(expr = expr_q, env = env, type = type), class = "tabler_render")
}

#' @title Render Text
#' @description Returns a character string that is HTML-escaped and injected as
#'   the inner HTML of the matching \code{textOutput} placeholder.
#' @param expr Expression that evaluates to a character vector.
#' @return A \code{tabler_render} object for use with \code{tablerApp}.
#' @rdname tabler-outputs
#' @export
renderText <- function(expr) {
  .render(substitute(expr), parent.frame(), "text")
}

#' @title Render Printed Output
#' @description Captures the \code{print()} representation of the result and
#'   shows it verbatim (HTML-escaped).
#' @param expr Expression to evaluate and print.
#' @return A \code{tabler_render} object.
#' @rdname tabler-outputs
#' @export
renderPrint <- function(expr) {
  .render(substitute(expr), parent.frame(), "print")
}

#' @title Render UI
#' @description Returns an HTML tag tree that replaces the inner HTML of the
#'   matching \code{uiOutput} placeholder.
#' @param expr Expression that returns a tag or tagList.
#' @return A \code{tabler_render} object.
#' @rdname tabler-outputs
#' @export
renderUI <- function(expr) {
  .render(substitute(expr), parent.frame(), "ui")
}

#' @title Plot Output Placeholder
#' @description Places a \code{<div>} in the UI whose content is replaced by a
#'   base-R plot rendered server-side via \code{renderPlot}.
#' @param outputId The output identifier.
#' @param width    CSS width string (default \code{"100\%"}).
#' @param height   CSS height string (default \code{"400px"}).
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
plotOutput <- function(outputId, width = "100%", height = "400px") {
  div(id    = outputId,
      class = "tabler-out-ui",
      style = paste0("width:", width, ";min-height:", height, ";"))
}

#' @title Render a Plot
#' @description Evaluates \code{expr} inside an SVG graphics device, and
#'   injects the resulting inline SVG into the matching \code{plotOutput}
#'   placeholder.  Works with base-R graphics, \pkg{tinyplot}, \pkg{ggplot2},
#'   \pkg{lattice}, and any other graphics system that honours the active
#'   device.
#' @param expr   Expression that draws a plot (e.g. via \code{hist}, \code{plot}).
#' @param width  Device width in pixels (default \code{800}).
#' @param height Device height in pixels (default \code{400}).
#' @return A \code{tabler_render} object.
#' @rdname tabler-outputs
#' @export
renderPlot <- function(expr, width = 800, height = 400) {
  structure(
    list(expr = substitute(expr), env = parent.frame(), type = "plot",
         width = width, height = height),
    class = "tabler_render"
  )
}

#' @title HTML Widget Output Placeholder
#' @description Places a \code{<div>} in the UI whose content is replaced by an
#'   htmlwidget (e.g. from \pkg{d3po}, \pkg{leaflet}, \pkg{plotly}) rendered by
#'   a matching server-side call.  The widget is served inside a sandboxed
#'   \code{<iframe>} so its own JS/CSS cannot conflict with the page.
#' @param outputId The output identifier (must match the server-side name).
#' @param width    CSS width string (default \code{"100\%"}).
#' @param height   CSS height string (default \code{"400px"}).
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
widgetOutput <- function(outputId, width = "100%", height = "400px") {
  div(id    = outputId,
      class = "tabler-out-ui",
      style = paste0("width:", width, ";min-height:", height, ";"))
}

#' @title Render an HTML Widget
#' @description Generic render function for any \pkg{htmlwidgets}-based widget.
#'   Captures the expression \emph{without} evaluating it and stores the calling
#'   environment, so \code{tablerApp} can evaluate it inside a reactive context
#'   without requiring a live Shiny session.  Reactive values referenced inside
#'   \code{expr} are tracked automatically.
#'
#' @details
#' Package authors who ship their own \code{render_*()} helpers can make them
#' tabler-compatible by attaching the same two attributes:
#' \preformatted{
#'   attr(fn, "tabler_expr") <- substitute(expr)
#'   attr(fn, "tabler_env")  <- parent.frame()
#' }
#' This is exactly what \code{render_d3po()} in \pkg{d3po} already does.
#'
#' @param expr Expression that returns an \code{htmlwidget} object (e.g.
#'   \code{d3po(...)}, \code{leaflet(...)}).
#' @return A zero-argument function with \code{tabler_expr} and
#'   \code{tabler_env} attributes recognised by \code{tablerApp}.
#' @rdname tabler-outputs
#' @export
renderWidget <- function(expr) {
  e   <- substitute(expr)
  env <- parent.frame()
  fn  <- function() eval(e, env)
  attr(fn, "tabler_expr") <- e
  attr(fn, "tabler_env")  <- env
  fn
}

# Serialise a render result to an HTML string
.serialise_output <- function(val, type) {
  switch(type,
    text  = html_escape(paste(as.character(val), collapse = " ")),
    print = {
      txt <- paste(utils::capture.output(print(val)), collapse = "\n")
      html_escape(txt)
    },
    ui     = render_html(val),
    html   = as.character(val),          # already-escaped error strings
    html_escape(as.character(val))
  )
}
