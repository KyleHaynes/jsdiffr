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

# Internal: render one side of a changes object as an HTML fragment.
# side = "left"  -> context + removed (additions suppressed)
# side = "right" -> context + added   (removals suppressed)
.side_html <- function(changes, side) {
  val <- changes$value
  pieces <- vapply(seq_len(nrow(changes)), function(i) {
    v <- escape_html(val[i])
    if (side == "left") {
      if (changes$added[i])   return("")
      if (changes$removed[i]) paste0('<span class="jsdiff-removed">', v, "</span>")
      else                    paste0('<span class="jsdiff-context">',  v, "</span>")
    } else {
      if (changes$removed[i]) return("")
      if (changes$added[i])   paste0('<span class="jsdiff-added">',   v, "</span>")
      else                    paste0('<span class="jsdiff-context">',  v, "</span>")
    }
  }, character(1))
  paste0(pieces, collapse = "")
}

#' Show a vector diff as a three-column HTML table in the browser
#'
#' Compares `vec_1` and `vec_2` element-by-element, rendering one table row
#' per pair with **Left** (original), **Right** (new), and **Changes**
#' (inline combined diff) columns. Uses [diff_chars()] (character-level) by
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
    ch     <- method(vec_1[i], vec_2[i])
    left   <- .side_html(ch, "left")
    right  <- .side_html(ch, "right")
    inline <- diff_to_html(ch, wrap = FALSE, pre = FALSE)
    paste0("<tr><td>", left, "</td><td>", right, "</td><td>", inline, "</td></tr>")
  }, character(1))

  thead <- "<thead><tr><th>Left</th><th>Right</th><th>Changes</th></tr></thead>"
  table <- paste0(
    '<table class="jsdiff-table">', thead,
    "<tbody>", paste0(rows, collapse = ""), "</tbody></table>"
  )

  css <- paste0(
    diff_css_default(), "\n",
    "body{margin:16px;font-family:ui-monospace,monospace;font-size:13px}\n",
    ".jsdiff-table{width:100%;border-collapse:collapse}\n",
    ".jsdiff-table th,.jsdiff-table td{padding:4px 8px;border:1px solid #d0d7de;vertical-align:top;white-space:pre-wrap}\n",
    ".jsdiff-table th{background:#f6f8fa;font-weight:600;text-align:left}\n",
    ".jsdiff-table td:nth-child(1){background:#fff8f8}\n",
    ".jsdiff-table td:nth-child(2){background:#f8fff8}\n",
    ".jsdiff-table td:nth-child(3){background:#fafafa}"
  )

  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><style>', css,
    "</style></head><body>", table, "</body></html>"
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
