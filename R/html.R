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

# Internal: TRUE if every non-whitespace token in `text` is in `ignore_lc`.
.all_ignored <- function(text, ignore_lc) {
  words <- unlist(regmatches(text, gregexpr("\\S+", text, perl = TRUE)))
  length(words) > 0L && all(tolower(words) %in% ignore_lc)
}

# Internal: render one side of a changes object as an HTML fragment.
# side = "left"  -> context + removed (additions suppressed)
# side = "right" -> context + added   (removals suppressed)
# When ignore_lc is non-empty, changes consisting entirely of ignored words are
# rendered as plain context on their respective side instead of highlighted.
.side_html <- function(changes, side, ignore_lc = character(0)) {
  val <- changes$value
  pieces <- vapply(seq_len(nrow(changes)), function(i) {
    v  <- escape_html(val[i])
    ig <- length(ignore_lc) > 0L && .all_ignored(val[i], ignore_lc)
    if (side == "left") {
      if (changes$added[i])   return("")
      if (changes$removed[i]) {
        if (ig) paste0('<span class="jsdiff-context">', v, "</span>")
        else    paste0('<span class="jsdiff-removed">', v, "</span>")
      } else {
        paste0('<span class="jsdiff-context">', v, "</span>")
      }
    } else {
      if (changes$removed[i]) return("")
      if (changes$added[i]) {
        if (ig) paste0('<span class="jsdiff-context">', v, "</span>")
        else    paste0('<span class="jsdiff-added">',   v, "</span>")
      } else {
        paste0('<span class="jsdiff-context">', v, "</span>")
      }
    }
  }, character(1))
  paste0(pieces, collapse = "")
}

# Internal: render the inline Changes column HTML fragment.
# Ignored-word removals are skipped; the paired addition shows as plain context.
.inline_html <- function(changes, ignore_lc = character(0)) {
  val <- changes$value
  pieces <- vapply(seq_len(nrow(changes)), function(i) {
    v  <- escape_html(val[i])
    ig <- length(ignore_lc) > 0L && .all_ignored(val[i], ignore_lc)
    if (changes$removed[i]) {
      if (ig) return("")  # skip; the added side renders as context below
      paste0('<span class="jsdiff-removed">', v, "</span>")
    } else if (changes$added[i]) {
      if (ig) paste0('<span class="jsdiff-context">', v, "</span>")
      else    paste0('<span class="jsdiff-added">',   v, "</span>")
    } else {
      paste0('<span class="jsdiff-context">', v, "</span>")
    }
  }, character(1))
  paste0(pieces, collapse = "")
}

#' Show a vector diff as a three-column HTML table
#'
#' Compares `vec_1` and `vec_2` element-by-element, rendering one table row
#' per pair with **Left** (original), **Right** (new), and **Changes**
#' (inline combined diff) columns. Uses [diff_chars()] (character-level) by
#' default.
#'
#' @param vec_1,vec_2 Character vectors of equal length to compare.
#' @param method Diff function applied per element pair. Defaults to [diff_chars].
#' @param ignore_words Optional character vector of words to suppress from diff
#'   highlighting. Changes consisting entirely of these words (case-insensitive)
#'   are rendered as plain context rather than additions/deletions. Most useful
#'   with `method = diff_words`.
#' @param view If `TRUE`, write to a temp file and display it (see `viewer`).
#' @param viewer Function used to display the temp file when `view = TRUE`.
#'   Defaults to `NULL`, which auto-detects an in-editor viewer at call time:
#'   `getOption("viewer")` (set by RStudio and by the VS Code / Positron R
#'   extensions when their session watcher is attached), then
#'   `rstudioapi::viewer` if the 'rstudioapi' package reports one is available.
#'   Falls back to [utils::browseURL()] (with a one-line [message()] explaining
#'   why) when neither is found. Pass `utils::browseURL` explicitly to force
#'   the system browser.
#' @return The HTML string, invisibly when `view = TRUE`.
#' @examples
#' # Return the HTML as a string (use view = TRUE to display it).
#' diff_html(c("a", "b", "c"), c("a", "B", "c"), view = FALSE)
#'
#' # Ignore specific words when using word-level diff
#' diff_html(
#'   c("The big cat sat", "foo bar"),
#'   c("The small cat sat", "foo baz"),
#'   method = diff_words,
#'   ignore_words = c("big", "small"),
#'   view = FALSE
#' )
#' \dontrun{
#' # Opens the diff table in RStudio's/VS Code's Viewer pane if available,
#' # otherwise the system browser:
#' diff_html(c("a", "b", "c"), c("a", "B", "c"))
#'
#' # Force the system browser even inside an IDE:
#' diff_html(c("a", "b", "c"), c("a", "B", "c"), viewer = utils::browseURL)
#' }
#' @export
diff_html <- function(vec_1, vec_2, method = diff_chars, ignore_words = NULL,
                      view = TRUE, viewer = NULL) {
  stopifnot(length(vec_1) == length(vec_2))
  ignore_lc <- tolower(ignore_words %||% character(0))

  rows <- vapply(seq_along(vec_1), function(i) {
    ch     <- method(vec_1[i], vec_2[i])
    left   <- .side_html(ch, "left",  ignore_lc)
    right  <- .side_html(ch, "right", ignore_lc)
    inline <- .inline_html(ch, ignore_lc)
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
    .display_html(tmp, viewer)
    invisible(html)
  } else {
    html
  }
}

# Internal: find an in-editor viewer, checked fresh on every call because IDEs
# attach/detach their session-watcher hooks (e.g. vscode-R's "R: Attach Active
# Terminal") after the R session has already started, so a cached value could
# go stale mid-session.
.resolve_viewer <- function() {
  opt <- getOption("viewer")
  if (is.function(opt)) return(opt)
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    return(rstudioapi::viewer)
  }
  NULL
}

# Internal: display `path` (a local HTML file) with `viewer` if supplied,
# else auto-detect one, else fall back to the system browser with a message
# explaining why (so a silent "wrong window opened" doesn't look mysterious).
.display_html <- function(path, viewer = NULL) {
  if (!is.function(viewer)) viewer <- .resolve_viewer()
  if (is.function(viewer)) {
    viewer(path)
  } else {
    message(
      "jsdiffr: no IDE viewer detected (getOption(\"viewer\") is unset and ",
      "rstudioapi reports unavailable) - opening in the system browser. In ",
      "VS Code, check the R extension shows the terminal as attached ",
      "(Command Palette > \"R: Attach Active Terminal\" if not)."
    )
    browseURL(path)
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
