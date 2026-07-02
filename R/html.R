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

# Internal: TRUE if `side` ("left" -> removed, "right" -> added) has at least
# one non-ignored piece, i.e. the row actually differs on that side and its
# table cell should get diff shading.
.has_diff <- function(changes, side, ignore_lc = character(0)) {
  val <- changes$value
  flag <- if (side == "left") changes$removed else changes$added
  any(vapply(seq_len(nrow(changes)), function(i) {
    if (!flag[i]) return(FALSE)
    !(length(ignore_lc) > 0L && .all_ignored(val[i], ignore_lc))
  }, logical(1)))
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

# Internal: per-row rendering pieces shared by diff_html() and
# diff_html_select() so the two stay in sync.
.diff_row_pieces <- function(vec_1, vec_2, method, ignore_lc) {
  lapply(seq_along(vec_1), function(i) {
    ch <- method(vec_1[i], vec_2[i])
    list(
      left        = .side_html(ch, "left",  ignore_lc),
      right       = .side_html(ch, "right", ignore_lc),
      inline      = .inline_html(ch, ignore_lc),
      left_class  = if (.has_diff(ch, "left",  ignore_lc)) ' class="jsdiff-cell-removed"' else "",
      right_class = if (.has_diff(ch, "right", ignore_lc)) ' class="jsdiff-cell-added"'   else ""
    )
  })
}

# Internal: write `html` to a temp file and display it per `view`/`viewer`/
# `viewer_row_limit`, or just return it when view = FALSE. Shared by
# diff_html() and diff_html_select().
.view_html <- function(html, view, viewer, viewer_row_limit, n_rows) {
  if (!view) return(html)
  tmp <- tempfile(fileext = ".html")
  writeLines(html, tmp)
  if (is.null(viewer) && n_rows > viewer_row_limit) {
    message(
      "jsdiffr: ", n_rows, " rows exceeds viewer_row_limit (",
      viewer_row_limit, ") - opening in the system browser instead of the ",
      "in-editor viewer, which is slow to render large diff tables. Raise ",
      "viewer_row_limit to force the in-editor viewer anyway, or pass a ",
      "viewer= function to skip this check entirely."
    )
    browseURL(tmp)
  } else {
    .display_html(tmp, viewer)
  }
  invisible(html)
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
#'   the system browser (or skip the row-count check below).
#' @param viewer_row_limit When `viewer` is left at its default `NULL`, diffs
#'   with more than this many rows skip the in-editor viewer (RStudio/VS
#'   Code's webview panel is slow to open for large tables) and open in the
#'   system browser instead, which handles large static HTML files fine. Has
#'   no effect if `viewer` is supplied explicitly. Set to `Inf` to always use
#'   the in-editor viewer.
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
                      view = TRUE, viewer = NULL, viewer_row_limit = 300) {
  stopifnot(length(vec_1) == length(vec_2))
  ignore_lc <- tolower(ignore_words %||% character(0))
  pieces <- .diff_row_pieces(vec_1, vec_2, method, ignore_lc)

  rows <- vapply(pieces, function(p) {
    paste0(
      "<tr><td", p$left_class, ">", p$left, "</td><td", p$right_class, ">", p$right,
      "</td><td>", p$inline, "</td></tr>"
    )
  }, character(1))

  thead <- "<thead><tr><th>Left</th><th>Right</th><th>Changes</th></tr></thead>"
  table <- paste0(
    '<table class="jsdiff-table">', thead,
    "<tbody>", paste0(rows, collapse = ""), "</tbody></table>"
  )

  css <- paste0(
    diff_css_default(), "\n",
    "body{margin:16px;font-family:ui-monospace,monospace;font-size:13px}\n",
    ".jsdiff-table{width:100%;border-collapse:collapse;table-layout:fixed}\n",
    ".jsdiff-table th,.jsdiff-table td{padding:4px 8px;border:1px solid #d0d7de;vertical-align:top;white-space:pre-wrap;overflow-wrap:anywhere}\n",
    ".jsdiff-table th{background:#f6f8fa;font-weight:600;text-align:left}\n",
    ".jsdiff-table td:nth-child(1),.jsdiff-table td:nth-child(2){background:#fff}\n",
    ".jsdiff-table td.jsdiff-cell-removed{background:#fff8f8}\n",
    ".jsdiff-table td.jsdiff-cell-added{background:#f8fff8}\n",
    ".jsdiff-table td:nth-child(3){background:#fafafa}"
  )

  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><style>', css,
    "</style></head><body>", table, "</body></html>"
  )

  .view_html(html, view, viewer, viewer_row_limit, length(vec_1))
}

#' Show a vector diff as a row-selectable HTML table
#'
#' Like [diff_html()], but adds a checkbox beside the Left column and another
#' beside the Right column so individual rows can be marked. A floating
#' overlay tracks the checked rows live and displays them as an R vector
#' literal (e.g. `c(2, 22, 33)`) for each side, with a Copy button, so the
#' selection can be pasted straight into subsetting code such as
#' `vec_1[c(2, 22, 33)]`. Row numbers are 1-based and match positions in
#' `vec_1`/`vec_2`. A checkbox in each header column selects/clears all rows
#' on that side at once.
#'
#' @inheritParams diff_html
#' @return The HTML string, invisibly when `view = TRUE`.
#' @examples
#' diff_html_select(c("a", "b", "c"), c("a", "B", "c"), view = FALSE)
#' @export
diff_html_select <- function(vec_1, vec_2, method = diff_chars, ignore_words = NULL,
                             view = TRUE, viewer = NULL, viewer_row_limit = 300) {
  stopifnot(length(vec_1) == length(vec_2))
  ignore_lc <- tolower(ignore_words %||% character(0))
  pieces <- .diff_row_pieces(vec_1, vec_2, method, ignore_lc)

  rows <- vapply(seq_along(pieces), function(i) {
    p <- pieces[[i]]
    left_extra  <- if (nzchar(p$left_class))  " jsdiff-cell-removed" else ""
    right_extra <- if (nzchar(p$right_class)) " jsdiff-cell-added"   else ""
    paste0(
      "<tr>",
      '<td class="jsdiff-chk-cell jsdiff-col-left"><input type="checkbox" class="jsdiff-chk-left" data-idx="', i, '"></td>',
      '<td class="jsdiff-col-left', left_extra, '">', p$left, "</td>",
      '<td class="jsdiff-chk-cell jsdiff-col-right"><input type="checkbox" class="jsdiff-chk-right" data-idx="', i, '"></td>',
      '<td class="jsdiff-col-right', right_extra, '">', p$right, "</td>",
      "<td>", p$inline, "</td>",
      "</tr>"
    )
  }, character(1))

  thead <- paste0(
    "<thead><tr>",
    '<th class="jsdiff-chk-cell"><input type="checkbox" id="jsdiff-selall-left" title="Select all (Left)"></th>',
    "<th>Left</th>",
    '<th class="jsdiff-chk-cell"><input type="checkbox" id="jsdiff-selall-right" title="Select all (Right)"></th>',
    "<th>Right</th>",
    "<th>Changes</th>",
    "</tr></thead>"
  )
  table <- paste0(
    '<table class="jsdiff-table">', thead,
    "<tbody>", paste0(rows, collapse = ""), "</tbody></table>"
  )

  css <- paste0(
    diff_css_default(), "\n",
    "body{margin:16px;font-family:ui-monospace,monospace;font-size:13px}\n",
    ".jsdiff-table{width:100%;border-collapse:collapse;table-layout:fixed}\n",
    ".jsdiff-table th,.jsdiff-table td{padding:4px 8px;border:1px solid #d0d7de;vertical-align:top;white-space:pre-wrap;overflow-wrap:anywhere}\n",
    ".jsdiff-table th{background:#f6f8fa;font-weight:600;text-align:left}\n",
    ".jsdiff-table th.jsdiff-chk-cell,.jsdiff-table td.jsdiff-chk-cell{width:28px;text-align:center;white-space:nowrap}\n",
    ".jsdiff-table td.jsdiff-chk-cell{background:#fff}\n",
    ".jsdiff-table td:nth-child(2),.jsdiff-table td:nth-child(4){background:#fff}\n",
    ".jsdiff-table td.jsdiff-cell-removed{background:#fff8f8}\n",
    ".jsdiff-table td.jsdiff-cell-added{background:#f8fff8}\n",
    ".jsdiff-table td:nth-child(5){background:#fafafa}\n",
    ".jsdiff-table td.jsdiff-col-left,.jsdiff-table td.jsdiff-col-right{cursor:pointer}\n",
    ".jsdiff-table td.jsdiff-col-left:hover,.jsdiff-table td.jsdiff-col-right:hover{filter:brightness(0.96)}\n",
    ".jsdiff-table td.jsdiff-row-checked{outline:2px solid #0969da;outline-offset:-2px}\n",
    ".jsdiff-select-overlay{position:fixed;top:12px;right:12px;display:none;",
    "background:#fff;border:1px solid #d0d7de;border-radius:6px;",
    "box-shadow:0 4px 12px rgba(0,0,0,.15);padding:10px 12px;",
    "font-family:ui-monospace,monospace;font-size:12px;max-width:360px;z-index:1000}\n",
    ".jsdiff-select-overlay h4{margin:0 0 6px;font-size:12px;font-weight:600}\n",
    ".jsdiff-select-row{display:flex;align-items:center;gap:6px;margin:4px 0}\n",
    ".jsdiff-select-row code{flex:1;overflow-wrap:anywhere;background:#f6f8fa;padding:2px 4px;border-radius:4px}\n",
    ".jsdiff-select-row button{cursor:pointer;border:1px solid #d0d7de;background:#f6f8fa;",
    "border-radius:4px;padding:2px 6px;font-size:11px}\n",
    ".jsdiff-select-row button:hover{background:#eaeef2}"
  )

  overlay <- paste0(
    '<div id="jsdiff-select-overlay" class="jsdiff-select-overlay">',
    "<h4>Selected rows</h4>",
    '<div class="jsdiff-select-row">L <code id="jsdiff-select-left">c()</code>',
    '<button type="button" id="jsdiff-copy-left">Copy</button></div>',
    '<div class="jsdiff-select-row">R <code id="jsdiff-select-right">c()</code>',
    '<button type="button" id="jsdiff-copy-right">Copy</button></div>',
    "</div>"
  )

  script <- paste0(
    "<script>\n",
    "(function () {\n",
    "  function collect(sel) {\n",
    "    var idx = [];\n",
    "    document.querySelectorAll(sel).forEach(function (b) {\n",
    "      if (b.checked) idx.push(parseInt(b.getAttribute('data-idx'), 10));\n",
    "    });\n",
    "    idx.sort(function (a, b) { return a - b; });\n",
    "    return idx;\n",
    "  }\n",
    "  function fmt(idx) {\n",
    "    return idx.length ? 'c(' + idx.join(', ') + ')' : 'c()';\n",
    "  }\n",
    "  function fallbackCopy(text) {\n",
    "    var ta = document.createElement('textarea');\n",
    "    ta.value = text;\n",
    "    ta.style.position = 'fixed';\n",
    "    ta.style.opacity = '0';\n",
    "    document.body.appendChild(ta);\n",
    "    ta.focus();\n",
    "    ta.select();\n",
    "    try { document.execCommand('copy'); } catch (e) {}\n",
    "    document.body.removeChild(ta);\n",
    "  }\n",
    "  function copy(text) {\n",
    "    if (navigator.clipboard && window.isSecureContext) {\n",
    "      navigator.clipboard.writeText(text).catch(function () { fallbackCopy(text); });\n",
    "    } else {\n",
    "      fallbackCopy(text);\n",
    "    }\n",
    "  }\n",
    "\n",
    "  var overlay = document.getElementById('jsdiff-select-overlay');\n",
    "  var leftText = document.getElementById('jsdiff-select-left');\n",
    "  var rightText = document.getElementById('jsdiff-select-right');\n",
    "  var selAllLeft = document.getElementById('jsdiff-selall-left');\n",
    "  var selAllRight = document.getElementById('jsdiff-selall-right');\n",
    "\n",
    "  function paintSide(prefix, cellIndex) {\n",
    "    document.querySelectorAll('.jsdiff-chk-' + prefix).forEach(function (b) {\n",
    "      var cell = b.closest('tr').cells[cellIndex];\n",
    "      if (cell) cell.classList.toggle('jsdiff-row-checked', b.checked);\n",
    "    });\n",
    "  }\n",
    "\n",
    "  function update() {\n",
    "    var l = collect('.jsdiff-chk-left');\n",
    "    var r = collect('.jsdiff-chk-right');\n",
    "    leftText.textContent = fmt(l);\n",
    "    rightText.textContent = fmt(r);\n",
    "    overlay.style.display = (l.length || r.length) ? 'block' : 'none';\n",
    "    paintSide('left', 1);\n",
    "    paintSide('right', 3);\n",
    "  }\n",
    "\n",
    "  document.querySelectorAll('.jsdiff-chk-left, .jsdiff-chk-right').forEach(function (b) {\n",
    "    b.addEventListener('change', update);\n",
    "  });\n",
    "  selAllLeft.addEventListener('change', function () {\n",
    "    document.querySelectorAll('.jsdiff-chk-left').forEach(function (b) { b.checked = selAllLeft.checked; });\n",
    "    update();\n",
    "  });\n",
    "  selAllRight.addEventListener('change', function () {\n",
    "    document.querySelectorAll('.jsdiff-chk-right').forEach(function (b) { b.checked = selAllRight.checked; });\n",
    "    update();\n",
    "  });\n",
    "  document.getElementById('jsdiff-copy-left').addEventListener('click', function () { copy(leftText.textContent); });\n",
    "  document.getElementById('jsdiff-copy-right').addEventListener('click', function () { copy(rightText.textContent); });\n",
    "\n",
    "  document.querySelector('.jsdiff-table tbody').addEventListener('click', function (e) {\n",
    "    if (e.target.tagName === 'INPUT') return;\n",
    "    if (window.getSelection().toString().length > 0) return;\n",
    "    var cell = e.target.closest('td');\n",
    "    if (!cell) return;\n",
    "    var side = cell.classList.contains('jsdiff-col-left') ? 'left'\n",
    "      : cell.classList.contains('jsdiff-col-right') ? 'right' : null;\n",
    "    if (!side) return;\n",
    "    var row = cell.closest('tr');\n",
    "    var box = row.querySelector('.jsdiff-chk-' + side);\n",
    "    if (!box) return;\n",
    "    box.checked = !box.checked;\n",
    "    update();\n",
    "  });\n",
    "\n",
    "  update();\n",
    "})();\n",
    "</script>"
  )

  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><style>', css,
    "</style></head><body>", table, overlay, script, "</body></html>"
  )

  .view_html(html, view, viewer, viewer_row_limit, length(vec_1))
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
