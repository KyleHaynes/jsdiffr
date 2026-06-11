# Converters, ported from jsdiff's src/convert/xml.ts and src/convert/dmp.ts.

escape_html <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s
}

#' Convert change objects to a serialized XML string
#'
#' Additions are wrapped in `<ins>` and deletions in `<del>`.
#'
#' @param changes A `jsdiff_changes` object (from a string-based diff).
#' @return A length-1 character string.
#' @export
convert_changes_to_xml <- function(changes) {
  val <- changes$value
  pieces <- vapply(seq_len(nrow(changes)), function(i) {
    v <- escape_html(val[i])
    if (changes$added[i]) paste0("<ins>", v, "</ins>")
    else if (changes$removed[i]) paste0("<del>", v, "</del>")
    else v
  }, character(1))
  paste0(pieces, collapse = "")
}

#' Convert change objects to the Google diff-match-patch format
#'
#' Returns a list of `c(operation, value)` pairs, where operation is `1`
#' (insertion), `-1` (deletion), or `0` (equality).
#'
#' @param changes A `jsdiff_changes` object.
#' @return A list of length-2 lists `list(operation, value)`.
#' @export
convert_changes_to_dmp <- function(changes) {
  val <- changes$value
  lapply(seq_len(nrow(changes)), function(i) {
    op <- if (changes$added[i]) 1L else if (changes$removed[i]) -1L else 0L
    list(operation = op, value = val[i])
  })
}
