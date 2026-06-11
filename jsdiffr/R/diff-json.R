# JSON diff, ported from jsdiff's src/diff/json.ts. Serialises each input to
# canonical, pretty-printed JSON (object keys sorted) and then diffs line by
# line, treating a dangling trailing comma as insignificant when matching lines.

#' Diff two JSON-serialisable values
#'
#' Each input is serialised to pretty-printed JSON with object keys sorted, then
#' compared line by line. A character-scalar input is treated as pre-serialised
#' JSON and used as-is. Lines that differ only by a trailing comma are
#' considered equal (matching jsdiff).
#'
#' @param old_val,new_val R objects to serialise and compare, or length-1
#'   character strings of pre-serialised JSON.
#' @param undefined_replacement Value substituted for `NULL`/`NA` leaves during
#'   serialisation (jsdiff's `undefinedReplacement`). Defaults to JSON `null`.
#' @inheritParams diff_chars
#' @return A [jsdiff_changes] object.
#' @examples
#' diff_json(list(a = 1, b = 2), list(a = 1, b = 3))
#' @export
diff_json <- function(old_val, new_val, undefined_replacement = NULL,
                      strip_trailing_cr = FALSE, max_edit_length = NULL,
                      timeout = NULL, one_change_per_token = FALSE) {
  cast <- function(v) {
    if (is.character(v) && length(v) == 1L) v
    else canonical_json(v, undefined_replacement = undefined_replacement)
  }
  old_str <- cast(old_val)
  new_str <- cast(new_val)

  tok <- function(s) tokenize_lines(s, newline_is_token = FALSE,
                                    strip_trailing_cr = strip_trailing_cr)
  key_fun <- function(t) gsub(",([\r\n])", "\\1", t, perl = TRUE)

  run_string_diff(tok(old_str), tok(new_str), key_fun, use_longest = TRUE,
                  mode = "lines", max_edit_length = max_edit_length,
                  timeout = timeout, one_change_per_token = one_change_per_token)
}

# Serialise an R object to canonical pretty JSON, mimicking
# JSON.stringify(canonicalize(obj), null, '  '): two-space indentation with
# object keys sorted alphabetically. Leaf scalars are delegated to jsonlite for
# correct escaping and number formatting.
canonical_json <- function(x, indent = 0L, undefined_replacement = NULL) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    if (is.null(undefined_replacement)) return("null")
    return(canonical_json(undefined_replacement, indent))
  }
  pad <- strrep("  ", indent)
  pad1 <- strrep("  ", indent + 1L)
  nms <- names(x)
  is_object <- !is.null(nms) && all(nzchar(nms))
  is_container <- is.list(x) || length(x) != 1L

  if (is_container && is_object) {
    if (length(x) == 0L) return("{}")
    ord <- order(nms)
    items <- vapply(ord, function(i) {
      key <- jsonlite::toJSON(nms[i], auto_unbox = TRUE)
      paste0(pad1, key, ": ", canonical_json(x[[i]], indent + 1L, undefined_replacement))
    }, character(1))
    return(paste0("{\n", paste(items, collapse = ",\n"), "\n", pad, "}"))
  }
  if (is_container) {
    if (length(x) == 0L) return("[]")
    items <- vapply(seq_along(x), function(i) {
      paste0(pad1, canonical_json(x[[i]], indent + 1L, undefined_replacement))
    }, character(1))
    return(paste0("[\n", paste(items, collapse = ",\n"), "\n", pad, "]"))
  }
  # Leaf scalar.
  jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null", na = "null")
}
