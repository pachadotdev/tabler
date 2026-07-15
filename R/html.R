# HTML tag system — pure R, zero external dependencies
# Tags carry class "tabler.tag" so all existing layout code that checks
# inherits(x, "tabler.tag") continues to work unchanged.

.void_tags <- c(
  "area", "base", "br", "col", "embed", "hr", "img", "input",
  "link", "meta", "param", "source", "track", "wbr"
)

# Escape HTML special characters in text content / attribute values
html_escape <- function(text) {
  text <- as.character(text)
  text <- gsub("&",  "&amp;",  text, fixed = TRUE)
  text <- gsub("<",  "&lt;",   text, fixed = TRUE)
  text <- gsub(">",  "&gt;",   text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text
}

# Internal tag constructor ---------------------------------------------------
.new_tag <- function(elt_name, ...) {
  args <- list(...)
  nms  <- names(args)
  if (is.null(nms)) nms <- character(length(args))

  attribs  <- list()
  children <- list()

  for (i in seq_along(args)) {
    v  <- args[[i]]
    nm <- nms[i]
    if (is.null(v)) next
    if (nzchar(nm)) {
      attribs[[nm]] <- v
    } else if (is.list(v) && !inherits(v, c("tabler.tag", "tabler.tag.list", "tabler.html"))) {
      children <- c(children, v)   # flatten bare lists
    } else {
      children <- c(children, list(v))
    }
  }

  structure(
    list(name = elt_name, attribs = attribs, children = children),
    class = "tabler.tag"
  )
}

# Render helpers ------------------------------------------------------------
.render_attribs <- function(attribs) {
  if (length(attribs) == 0L) return("")
  nms   <- names(attribs)
  parts <- character(length(attribs))
  for (i in seq_along(attribs)) {
    v  <- attribs[[i]]
    k  <- nms[i]
    if (isTRUE(v)) {
      parts[i] <- k
    } else if (isFALSE(v) || is.null(v)) {
      parts[i] <- ""
    } else {
      parts[i] <- sprintf('%s="%s"', k, html_escape(paste(as.character(v), collapse = " ")))
    }
  }
  paste(parts[nzchar(parts)], collapse = " ")
}

#' Render a Tag Tree to an HTML String
#' @param x A tag, tagList, HTML literal, or character vector.
#' @return A single character string of HTML.
#' @keywords internal
#' @noRd
render_html <- function(x) {
  if (is.null(x))                              return("")
  if (inherits(x, "tabler.html"))              return(x$html)
  if (is.logical(x) || is.numeric(x))         return(html_escape(as.character(x)))
  if (is.character(x))                         return(html_escape(x))
  if (inherits(x, "tabler.tag.list")) {
    return(paste(vapply(x, render_html, character(1L)), collapse = ""))
  }
  # htmltools tags from external packages (e.g. htmlwidgets output placeholders).
  # Use their own as.character() which is registered by htmltools.
  if (is.list(x) && "shiny.tag" %in% class(x)) {
    return(paste(as.character(x), collapse = ""))
  }
  if (is.list(x) && "shiny.tag.list" %in% class(x)) {
    return(paste(vapply(x, function(el) paste(as.character(el), collapse = ""), character(1L)), collapse = ""))
  }
  if (is.list(x) && !inherits(x, "tabler.tag")) {
    return(paste(vapply(x, render_html, character(1L)), collapse = ""))
  }
  if (!inherits(x, "tabler.tag"))               return(paste(as.character(x), collapse = ""))

  nm    <- x$name
  attrs <- .render_attribs(x$attribs)
  if (nm %in% .void_tags) {
    if (nchar(attrs) > 0L) return(sprintf("<%s %s>", nm, attrs))
    return(sprintf("<%s>", nm))
  }
  kids <- paste(vapply(x$children, render_html, character(1L)), collapse = "")
  if (nchar(attrs) > 0L) return(sprintf("<%s %s>%s</%s>", nm, attrs, kids, nm))
  sprintf("<%s>%s</%s>", nm, kids, nm)
}


# Public API ----------------------------------------------------------------

#' Append Attributes to an Existing Tag
#' @param tag A tabler.tag object.
#' @param ... Named attributes to add/overwrite. The \code{class} attribute is
#'   appended (space-separated) rather than replaced.
#' @return The modified tag.
#' @export
tagAppendAttributes <- function(tag, ...) {
  new_attribs <- list(...)
  for (nm in names(new_attribs)) {
    if (nm == "class" && !is.null(tag$attribs[["class"]])) {
      tag$attribs[["class"]] <- paste(tag$attribs[["class"]], new_attribs[[nm]])
    } else {
      tag$attribs[[nm]] <- new_attribs[[nm]]
    }
  }
  tag
}

#' Create an Ordered Collection of Tags
#' @param ... Tags or other HTML content.
#' @return A tabler.tag.list object.
#' @export
tagList <- function(...) {
  structure(list(...), class = c("tabler.tag.list", "list"))
}

#' Mark a String as Raw HTML (Do Not Escape)
#' @param text Character string of literal HTML.
#' @return An object of class \code{"tabler.html"}.
#' @export
HTML <- function(text) {
  structure(list(html = text), class = "tabler.html")
}

#' @exportS3Method as.character tabler.html
as.character.tabler.html <- function(x, ...) x$html

# S3 methods for tabler.tag and tabler.tag.list — registered via registerS3method()
# in .onLoad() so tinydev/roxygen2 never see dot-separated names as S3 methods.
#' @noRd
.as_char_tag <- function(x, ...) render_html(x)

#' @noRd
.as_char_tag_list <- function(x, ...) {
  paste(vapply(x, render_html, character(1L)), collapse = "\n")
}

# Convenience tag builders (most common ones as first-class functions)
#' HTML Tag Builder Functions
#'
#' Convenience functions for building common HTML elements. Each function
#' accepts child content and named attributes as \code{...} arguments and
#' returns a \code{tabler.tag} object that can be nested, rendered with
#' \code{as.character()}, or passed to any tabler layout function.
#'
#' @param ... Child elements (strings, other tags) and/or named HTML
#'   attributes (e.g. \code{class = "foo"}, \code{href = "#"}).
#' @return A \code{tabler.tag} object.
#' @seealso \code{\link{tags}} for the full HTML5 tag namespace.
#' @name html-tags
#' @export
div  <- function(...) .new_tag("div",   ...)
#' @rdname html-tags
#' @export
span <- function(...) .new_tag("span",  ...)
#' @rdname html-tags
#' @export
p    <- function(...) .new_tag("p",     ...)
#' @rdname html-tags
#' @export
a    <- function(...) .new_tag("a",     ...)
#' @rdname html-tags
#' @export
img  <- function(...) .new_tag("img",   ...)
#' @rdname html-tags
#' @export
ul   <- function(...) .new_tag("ul",    ...)
#' @rdname html-tags
#' @export
li   <- function(...) .new_tag("li",    ...)
#' @rdname html-tags
#' @export
aside <- function(...) .new_tag("aside", ...)
#' @rdname html-tags
#' @export
h1   <- function(...) .new_tag("h1",    ...)
#' @rdname html-tags
#' @export
h2   <- function(...) .new_tag("h2",    ...)
#' @rdname html-tags
#' @export
h3   <- function(...) .new_tag("h3",    ...)
#' @rdname html-tags
#' @export
h4   <- function(...) .new_tag("h4",    ...)
#' @rdname html-tags
#' @export
h5   <- function(...) .new_tag("h5",    ...)
#' @rdname html-tags
#' @export
h6   <- function(...) .new_tag("h6",    ...)
#' @rdname html-tags
#' @export
br   <- function(...) .new_tag("br",    ...)

#' HTML Tag Namespace
#' @description A named list of tag-builder functions covering the full HTML5
#'   element set.  Usage: \code{tags$section(class = "foo", "content")}.
#' @export
tags <- list(
  a           = function(...) .new_tag("a",           ...),
  abbr        = function(...) .new_tag("abbr",        ...),
  address     = function(...) .new_tag("address",     ...),
  article     = function(...) .new_tag("article",     ...),
  aside       = function(...) .new_tag("aside",       ...),
  b           = function(...) .new_tag("b",           ...),
  blockquote  = function(...) .new_tag("blockquote",  ...),
  body        = function(...) .new_tag("body",        ...),
  br          = function(...) .new_tag("br",          ...),
  button      = function(...) .new_tag("button",      ...),
  caption     = function(...) .new_tag("caption",     ...),
  cite        = function(...) .new_tag("cite",        ...),
  code        = function(...) .new_tag("code",        ...),
  col         = function(...) .new_tag("col",         ...),
  colgroup    = function(...) .new_tag("colgroup",    ...),
  dd          = function(...) .new_tag("dd",          ...),
  del         = function(...) .new_tag("del",         ...),
  details     = function(...) .new_tag("details",     ...),
  dfn         = function(...) .new_tag("dfn",         ...),
  dialog      = function(...) .new_tag("dialog",      ...),
  div         = function(...) .new_tag("div",         ...),
  dl          = function(...) .new_tag("dl",          ...),
  dt          = function(...) .new_tag("dt",          ...),
  em          = function(...) .new_tag("em",          ...),
  embed       = function(...) .new_tag("embed",       ...),
  fieldset    = function(...) .new_tag("fieldset",    ...),
  figcaption  = function(...) .new_tag("figcaption",  ...),
  figure      = function(...) .new_tag("figure",      ...),
  footer      = function(...) .new_tag("footer",      ...),
  form        = function(...) .new_tag("form",        ...),
  h1          = function(...) .new_tag("h1",          ...),
  h2          = function(...) .new_tag("h2",          ...),
  h3          = function(...) .new_tag("h3",          ...),
  h4          = function(...) .new_tag("h4",          ...),
  h5          = function(...) .new_tag("h5",          ...),
  h6          = function(...) .new_tag("h6",          ...),
  head        = function(...) .new_tag("head",        ...),
  header      = function(...) .new_tag("header",      ...),
  hr          = function(...) .new_tag("hr",          ...),
  href        = function(...) .new_tag("href",        ...),
  i           = function(...) .new_tag("i",           ...),
  iframe      = function(...) .new_tag("iframe",      ...),
  img         = function(...) .new_tag("img",         ...),
  input       = function(...) .new_tag("input",       ...),
  ins         = function(...) .new_tag("ins",         ...),
  kbd         = function(...) .new_tag("kbd",         ...),
  label       = function(...) .new_tag("label",       ...),
  legend      = function(...) .new_tag("legend",      ...),
  li          = function(...) .new_tag("li",          ...),
  link        = function(...) .new_tag("link",        ...),
  main        = function(...) .new_tag("main",        ...),
  mark        = function(...) .new_tag("mark",        ...),
  meta        = function(...) .new_tag("meta",        ...),
  nav         = function(...) .new_tag("nav",         ...),
  noscript    = function(...) .new_tag("noscript",    ...),
  ol          = function(...) .new_tag("ol",          ...),
  optgroup    = function(...) .new_tag("optgroup",    ...),
  option      = function(...) .new_tag("option",      ...),
  p           = function(...) .new_tag("p",           ...),
  picture     = function(...) .new_tag("picture",     ...),
  pre         = function(...) .new_tag("pre",         ...),
  progress    = function(...) .new_tag("progress",    ...),
  script      = function(...) .new_tag("script",      ...),
  section     = function(...) .new_tag("section",     ...),
  select      = function(...) .new_tag("select",      ...),
  small       = function(...) .new_tag("small",       ...),
  source      = function(...) .new_tag("source",      ...),
  span        = function(...) .new_tag("span",        ...),
  strong      = function(...) .new_tag("strong",      ...),
  style       = function(...) .new_tag("style",       ...),
  sub         = function(...) .new_tag("sub",         ...),
  summary     = function(...) .new_tag("summary",     ...),
  sup         = function(...) .new_tag("sup",         ...),
  table       = function(...) .new_tag("table",       ...),
  tbody       = function(...) .new_tag("tbody",       ...),
  td          = function(...) .new_tag("td",          ...),
  textarea    = function(...) .new_tag("textarea",    ...),
  tfoot       = function(...) .new_tag("tfoot",       ...),
  th          = function(...) .new_tag("th",          ...),
  thead       = function(...) .new_tag("thead",       ...),
  time        = function(...) .new_tag("time",        ...),
  title       = function(...) .new_tag("title",       ...),
  tr          = function(...) .new_tag("tr",          ...),
  u           = function(...) .new_tag("u",           ...),
  ul          = function(...) .new_tag("ul",          ...),
  `var`       = function(...) .new_tag("var",         ...),
  video       = function(...) .new_tag("video",       ...)
)
