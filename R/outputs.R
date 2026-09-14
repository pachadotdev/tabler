# Output placeholder tags and render functions for tabler_app

# ---------------------------------------------------------------------------
# Placeholder tags (UI side)
# ---------------------------------------------------------------------------

#' @title Text Output Placeholder
#' @description Places a \code{<span>} in the UI whose content is updated by
#'   \code{render_text} in the server.
#' @param output_id The output identifier (must match the server-side name).
#' @param inline   If \code{TRUE}, use \code{<span>}; otherwise \code{<div>}.
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
text_output <- function(output_id, inline = FALSE) {
  if (inline) {
    span(id = output_id, class = "tabler-out-text")
  } else {
    div(id = output_id, class = "tabler-out-text")
  }
}

#' @title Verbatim Text Output Placeholder
#' @description Places a \code{<pre>} in the UI for monospace/printed output.
#' @param output_id The output identifier.
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
verbatim_text_output <- function(output_id) {
  tags$pre(id = output_id, class = "tabler-out-verbatim bg-dark p-2 rounded")
}

#' @title UI Output Placeholder
#' @description Places a tag (by default a \code{<div>}) whose inner HTML is
#'   replaced wholesale by \code{render_ui} output.
#' @param output_id The output identifier.
#' @param container A tag-building function used to create the placeholder
#'   element, e.g. \code{tags$h1}. Defaults to \code{\link{div}}.
#' @param ... Additional arguments passed to \code{container}.
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
ui_output <- function(output_id, container = div, ...) {
  container(id = output_id, class = "tabler-out-ui", ...)
}

#' @aliases html_output
#' @rdname tabler-outputs
#' @export
html_output <- ui_output

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
#'   the inner HTML of the matching \code{text_output} placeholder.
#' @param expr Expression that evaluates to a character vector.
#' @return A \code{tabler_render} object for use with \code{tabler_app}.
#' @rdname tabler-outputs
#' @export
render_text <- function(expr) {
  .render(substitute(expr), parent.frame(), "text")
}

#' @title Render Printed Output
#' @description Captures the \code{print()} representation of the result and
#'   shows it verbatim (HTML-escaped).
#' @param expr Expression to evaluate and print.
#' @return A \code{tabler_render} object.
#' @rdname tabler-outputs
#' @export
render_print <- function(expr) {
  .render(substitute(expr), parent.frame(), "print")
}

#' @title Render UI
#' @description Returns an HTML tag tree that replaces the inner HTML of the
#'   matching \code{ui_output} placeholder.
#' @param expr Expression that returns a tag or tag_list.
#' @return A \code{tabler_render} object.
#' @rdname tabler-outputs
#' @export
render_ui <- function(expr) {
  .render(substitute(expr), parent.frame(), "ui")
}

#' @title Plot Output Placeholder
#' @description Places a \code{<div>} in the UI whose content is replaced by a
#'   base-R plot rendered server-side via \code{render_plot}.
#' @param output_id The output identifier.
#' @param width    CSS width string (default \code{"100\%"}).
#' @param height   CSS height string (default \code{"400px"}).
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
plot_output <- function(output_id, width = "100%", height = "400px") {
  div(
    id = output_id,
    class = "tabler-out-ui",
    style = paste0("width:", width, ";min-height:", height, ";")
  )
}

#' @title Render a Plot
#' @description Evaluates \code{expr} inside an SVG graphics device, and
#'   injects the resulting inline SVG into the matching \code{plot_output}
#'   placeholder.  Works with base-R graphics, \pkg{tinyplot}, \pkg{ggplot2},
#'   \pkg{lattice}, and any other graphics system that honours the active
#'   device.
#' @param expr   Expression that draws a plot (e.g. via \code{hist}, \code{plot}).
#' @param width  Device width in pixels (default \code{800}).
#' @param height Device height in pixels (default \code{400}).
#' @return A \code{tabler_render} object.
#' @rdname tabler-outputs
#' @export
render_plot <- function(expr, width = 800, height = 400) {
  structure(
    list(
      expr = substitute(expr), env = parent.frame(), type = "plot",
      width = width, height = height
    ),
    class = "tabler_render"
  )
}

#' @title HTML Widget Output Placeholder
#' @description Places a \code{<div>} in the UI whose content is replaced by an
#'   htmlwidget (e.g. from \pkg{d3po}, \pkg{leaflet}, \pkg{plotly}) rendered by
#'   a matching server-side call.  The widget is served inside a sandboxed
#'   \code{<iframe>} so its own JS/CSS cannot conflict with the page.
#' @param output_id The output identifier (must match the server-side name).
#' @param width    CSS width string (default \code{"100\%"}).
#' @param height   CSS height string (default \code{"400px"}).
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
widget_output <- function(output_id, width = "100%", height = "400px") {
  div(
    id = output_id,
    class = "tabler-out-ui",
    style = paste0("width:", width, ";min-height:", height, ";")
  )
}

#' @title Render an HTML Widget
#' @description Generic render function for any \pkg{htmlwidgets}-based widget.
#'   Captures the expression \emph{without} evaluating it and stores the calling
#'   environment, so \code{tabler_app} can evaluate it inside a reactive context
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
#'   \code{tabler_env} attributes recognised by \code{tabler_app}.
#' @rdname tabler-outputs
#' @export
render_widget <- function(expr) {
  e <- substitute(expr)
  env <- parent.frame()
  fn <- function() eval(e, env)
  attr(fn, "tabler_expr") <- e
  attr(fn, "tabler_env") <- env
  fn
}

#' @title Create a Download Handler
#' @description Assigned to \code{output$id}, serves a file for download when
#'   the browser requests the matching \code{\link{download_button}}'s link,
#'   similar to \code{shiny::download_handler()}. Unlike other outputs, the
#'   file is generated fresh (not cached/pushed) each time it is requested.
#' @param filename A string, or a zero-argument function returning a string,
#'   giving the filename offered to the browser (e.g. \code{"data.csv"}).
#' @param content A one-argument function \code{function(file) {...}} that
#'   writes the file's contents to the path given by \code{file}.
#' @param content_type Optional MIME type string. If \code{NULL} (default), it
#'   is guessed from the filename's extension.
#' @return A \code{tabler_render} object for use with \code{tabler_app}.
#' @rdname tabler-outputs
#' @export
download_handler <- function(filename, content, content_type = NULL) {
  structure(
    list(filename = filename, content = content, content_type = content_type, type = "download"),
    class = "tabler_render"
  )
}

#' @title Download Button/Link
#' @description Creates a link that triggers a file download from the
#'   matching \code{\link{download_handler}}, similar to
#'   \code{shiny::download_button()}/\code{shiny::download_link()}.
#' @param output_id The output identifier (must match the server-side
#'   \code{download_handler} assigned to \code{output[[output_id]]}).
#' @param label Link/button text.
#' @param class Additional CSS classes (default \code{"btn-primary"} for
#'   \code{download_button}, none for \code{download_link}).
#' @param icon Optional icon name to prepend.
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-outputs
#' @export
download_button <- function(output_id, label = "Download", class = "btn-primary", icon = "download", ...) {
  icon_tag <- if (!is.null(icon)) tags$i(class = paste0("ti ti-", icon, " me-1"))
  tags$a(
    id = output_id,
    href = paste0("/downloads/", output_id),
    class = paste("btn", class),
    `data-tabler-download` = output_id,
    icon_tag,
    label,
    ...
  )
}

#' @rdname tabler-outputs
#' @export
download_link <- function(output_id, label = "Download", class = NULL, icon = NULL, ...) {
  icon_tag <- if (!is.null(icon)) tags$i(class = paste0("ti ti-", icon, " me-1"))
  tags$a(
    id = output_id,
    href = paste0("/downloads/", output_id),
    class = class,
    `data-tabler-download` = output_id,
    icon_tag,
    label,
    ...
  )
}

# Serialise a render result to an HTML string
.serialise_output <- function(val, type) {
  switch(type,
    text = html_escape(paste(as.character(val), collapse = " ")),
    # `val` is already the captured printed text (see .observe_output).
    print = html_escape(as.character(val)),
    ui = render_html(val),
    html = as.character(val), # already-escaped error strings
    html_escape(as.character(val))
  )
}
