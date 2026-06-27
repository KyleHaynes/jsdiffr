# Patch reversal, ported from jsdiff's src/patch/reverse.ts.

swap_prefix <- function(file_name) {
  if (is.null(file_name) || identical(file_name, "/dev/null")) return(file_name)
  if (startsWith(file_name, "a/")) return(paste0("b/", substring(file_name, 3L)))
  if (startsWith(file_name, "b/")) return(paste0("a/", substring(file_name, 3L)))
  file_name
}

#' Reverse a structured patch
#'
#' Returns a patch that undoes the supplied patch.
#'
#' @param patch A `jsdiff_patch` object or a list of them.
#' @return A reversed `jsdiff_patch` (or list of them).
#' @export
reverse_patch <- function(patch) {
  if (is.list(patch) && !is_patch(patch)) {
    return(rev(lapply(patch, reverse_patch)))
  }
  is_git <- isTRUE(patch$is_git)
  reversed <- patch
  reversed$old_file_name <- if (is_git) swap_prefix(patch$new_file_name) else patch$new_file_name
  reversed$old_header <- patch$new_header
  reversed$new_file_name <- if (is_git) swap_prefix(patch$old_file_name) else patch$old_file_name
  reversed$new_header <- patch$old_header
  reversed$old_mode <- patch$new_mode
  reversed$new_mode <- patch$old_mode
  reversed$is_create <- patch$is_delete
  reversed$is_delete <- patch$is_create
  reversed$hunks <- lapply(patch$hunks, function(hunk) {
    list(
      old_lines = hunk$new_lines,
      old_start = hunk$new_start,
      new_lines = hunk$old_lines,
      new_start = hunk$old_start,
      lines = vapply(hunk$lines, function(l) {
        if (startsWith(l, "-")) paste0("+", substring(l, 2L))
        else if (startsWith(l, "+")) paste0("-", substring(l, 2L))
        else l
      }, character(1), USE.NAMES = FALSE)
    )
  })

  if (isTRUE(patch$is_copy)) {
    reversed$new_file_name <- "/dev/null"
    reversed$new_header <- NULL
    reversed$is_delete <- TRUE
    reversed$is_create <- NULL
    reversed$is_copy <- NULL
    reversed$is_rename <- NULL
    reversed$hunks <- list()
  }
  new_patch(reversed)
}
