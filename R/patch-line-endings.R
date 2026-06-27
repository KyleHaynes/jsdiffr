# Line-ending conversion for patches, ported from jsdiff's
# src/patch/line-endings.ts.

map_hunk_lines <- function(patch, f) {
  patch$hunks <- lapply(patch$hunks, function(hunk) {
    hunk$lines <- f(hunk$lines)
    hunk
  })
  patch
}

#' Convert a patch's line endings from Unix to Windows
#' @param patch A `jsdiff_patch` or list of them.
#' @return The converted patch.
#' @export
unix_to_win <- function(patch) {
  if (is.list(patch) && !is_patch(patch)) {
    return(lapply(patch, unix_to_win))
  }
  patch$hunks <- lapply(patch$hunks, function(hunk) {
    lines <- hunk$lines
    n <- length(lines)
    hunk$lines <- vapply(seq_len(n), function(i) {
      line <- lines[i]
      nxt <- if (i < n) lines[i + 1L] else NA_character_
      if (startsWith(line, "\\") || endsWith(line, "\r") ||
          (!is.na(nxt) && startsWith(nxt, "\\"))) {
        line
      } else {
        paste0(line, "\r")
      }
    }, character(1), USE.NAMES = FALSE)
    hunk
  })
  new_patch(patch)
}

#' Convert a patch's line endings from Windows to Unix
#' @param patch A `jsdiff_patch` or list of them.
#' @return The converted patch.
#' @export
win_to_unix <- function(patch) {
  if (is.list(patch) && !is_patch(patch)) {
    return(lapply(patch, win_to_unix))
  }
  patch <- map_hunk_lines(patch, function(lines) {
    vapply(lines, function(line) {
      if (endsWith(line, "\r")) substr(line, 1L, nchar(line) - 1L) else line
    }, character(1), USE.NAMES = FALSE)
  })
  new_patch(patch)
}

#' Does a patch consistently use Unix line endings?
#' @param patch A `jsdiff_patch` or list of them.
#' @return A logical scalar.
#' @export
is_unix <- function(patch) {
  if (is_patch(patch)) patch <- list(patch)
  !any(vapply(patch, function(index) {
    any(vapply(index$hunks, function(hunk) {
      any(!startsWith(hunk$lines, "\\") & endsWith(hunk$lines, "\r"))
    }, logical(1)))
  }, logical(1)))
}

#' Does a patch consistently use Windows line endings?
#' @param patch A `jsdiff_patch` or list of them.
#' @return A logical scalar.
#' @export
is_win <- function(patch) {
  if (is_patch(patch)) patch <- list(patch)
  any_cr <- any(vapply(patch, function(index) {
    any(vapply(index$hunks, function(hunk) any(endsWith(hunk$lines, "\r")), logical(1)))
  }, logical(1)))
  if (!any_cr) return(FALSE)
  all(vapply(patch, function(index) {
    all(vapply(index$hunks, function(hunk) {
      lines <- hunk$lines
      n <- length(lines)
      all(vapply(seq_len(n), function(i) {
        line <- lines[i]
        nxt <- if (i < n) lines[i + 1L] else NA_character_
        startsWith(line, "\\") || endsWith(line, "\r") ||
          (!is.na(nxt) && startsWith(nxt, "\\"))
      }, logical(1)))
    }, logical(1)))
  }, logical(1)))
}
