# HTML rendering helpers for use in reports and shiny apps. Not part of jsdiff
# itself, but a thin convenience layer over the change objects.

#' Render change objects as an HTML string
#'
#' Wraps additions in `<span class="jsdiff-added">` and deletions in
#' `<span class="jsdiff-removed">`. Suitable for embedding in a 'shiny' app via
#' [shiny::renderUI()] / [shiny::HTML()] or in an R Markdown document. Use
#' [diff_css_default()] to obtain matching CSS.
#'
#' @param changes A `jsdiff_changes` object (from a string-based diff).
#' @param wrap If `TRUE`, wrap the output in a `<div class="jsdiff">...</div>`.
#' @param pre If `TRUE` (default), wrap the content in `<pre>` so whitespace and
#'   newlines are preserved (recommended for line diffs).
#' @return A length-1 character string of HTML.
#' @examples
#' diff_to_html(diff_words("the cat", "the dog"))
#' @export
diff_to_html <- function(changes, wrap = TRUE, pre = TRUE) {
  if (identical(attr(changes, "jsdiffr_value_type"), "list")) {
    stop("diff_to_html() requires a string-based diff, not an array diff.")
  }
  val <- changes$value
  pieces <- vapply(seq_len(nrow(changes)), function(i) {
    v <- escape_html(val[i])
    if (changes$added[i]) paste0("<span class=\"jsdiff-added\">", v, "</span>")
    else if (changes$removed[i]) paste0("<span class=\"jsdiff-removed\">", v, "</span>")
    else paste0("<span class=\"jsdiff-context\">", v, "</span>")
  }, character(1))
  body <- paste0(pieces, collapse = "")
  if (pre) body <- paste0("<pre class=\"jsdiff-pre\">", body, "</pre>")
  if (wrap) body <- paste0("<div class=\"jsdiff\">", body, "</div>")
  body
}

#' Show a vector diff as HTML in the browser
#'
#' Compares `vec_1` and `vec_2` element-by-element, rendering one row of
#' inline highlighted diff per pair. Uses [diff_chars()] (character-level) by
#' default. Opens the result in the system browser.
#'
#' @param vec_1,vec_2 Character vectors of equal length to compare.
#' @param method Diff function applied per element pair. Defaults to [diff_chars].
#' @param view If `TRUE`, write to a temp file and open in the browser.
#' @return The HTML string, invisibly when `view = TRUE`.
#' @examples
#' diff_html(c("a", "b", "c"), c("a", "B", "c"))
#' @export
diff_html <- function(vec_1, vec_2, method = diff_chars, view = TRUE) {
  stopifnot(length(vec_1) == length(vec_2))
  rows <- vapply(seq_along(vec_1), function(i) {
    diff_to_html(method(vec_1[i], vec_2[i]), wrap = FALSE, pre = FALSE)
  }, character(1))
  body <- paste0('<div class="diff-row">', rows, "</div>", collapse = "\n")
  css <- paste0(
    diff_css_default(),
    "\n.diff-row{padding:2px 6px;border-bottom:1px solid #eee;font-family:ui-monospace,monospace;white-space:pre-wrap}"
  )
  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><style>', css,
    "</style></head><body>", body, "</body></html>"
  )
  if (view) {
    tmp <- tempfile(fileext = ".html")
    writeLines(html, tmp)
    browseURL(tmp)
    invisible(html)
  } else {
    html
  }
}

#' Default CSS for [diff_to_html()] output
#'
#' @return A length-1 character string of CSS rules.
#' @export
diff_css_default <- function() {
  paste(
    ".jsdiff .jsdiff-pre { margin: 0; font-family: ui-monospace, monospace; white-space: pre-wrap; }",
    ".jsdiff-added { background-color: #e6ffed; color: #033a16; }",
    ".jsdiff-removed { background-color: #ffebe9; color: #82071e; text-decoration: line-through; }",
    ".jsdiff-context { color: #24292f; }",
    sep = "\n"
  )
}
