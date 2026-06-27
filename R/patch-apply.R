# Unified-diff patch application, ported from jsdiff's src/patch/apply.ts,
# including the fuzzy-matching hunk fitter and the distance iterator.

# Iterator over [min_line, max_line] stepping outward from `start`
# (src/util/distance-iterator.ts). Returns NULL when exhausted.
distance_iterator <- function(start, min_line, max_line) {
  want_forward <- TRUE
  backward_exhausted <- FALSE
  forward_exhausted <- FALSE
  local_offset <- 1L
  iter <- function() {
    if (want_forward && !forward_exhausted) {
      if (backward_exhausted) {
        local_offset <<- local_offset + 1L
      } else {
        want_forward <<- FALSE
      }
      if (start + local_offset <= max_line) {
        return(start + local_offset)
      }
      forward_exhausted <<- TRUE
    }
    if (!backward_exhausted) {
      if (!forward_exhausted) want_forward <<- TRUE
      if (min_line <= start - local_offset) {
        v <- start - local_offset
        local_offset <<- local_offset + 1L
        return(v)
      }
      backward_exhausted <<- TRUE
      return(iter())
    }
    NULL
  }
  iter
}

#' Apply a unified diff patch to a string
#'
#' @param source The original text to patch.
#' @param patch A unified diff string, a `jsdiff_patch`, or a list containing a
#'   single `jsdiff_patch`.
#' @param fuzz_factor Maximum number of mismatched context lines tolerated when
#'   fitting a hunk. Non-negative integer.
#' @param auto_convert_line_endings If `TRUE` (default), automatically convert
#'   the patch between Unix and Windows line endings to match `source`.
#' @param compare_line Optional `function(line_number, line, operation,
#'   patch_content)` returning `TRUE` when a source line matches patch content.
#' @return The patched string, or `FALSE` if the patch could not be applied.
#' @export
apply_patch <- function(source, patch, fuzz_factor = 0,
                        auto_convert_line_endings = TRUE, compare_line = NULL) {
  if (is.character(patch)) {
    patches <- parse_patch(patch)
  } else if (is_patch(patch)) {
    patches <- list(patch)
  } else if (is.list(patch)) {
    patches <- patch
  } else {
    stop("Unsupported patch type")
  }
  if (length(patches) > 1L) {
    stop("apply_patch only works with a single input.")
  }
  apply_structured_patch(source, patches[[1L]], fuzz_factor,
                         auto_convert_line_endings, compare_line)
}

apply_structured_patch <- function(source, patch, fuzz_factor = 0,
                                   auto_convert_line_endings = TRUE,
                                   compare_line = NULL) {
  if (auto_convert_line_endings) {
    if (has_only_win_line_endings(source) && is_unix(patch)) {
      patch <- unix_to_win(patch)
    } else if (has_only_unix_line_endings(source) && is_win(patch)) {
      patch <- win_to_unix(patch)
    }
  }
  if (is.null(compare_line)) {
    compare_line <- function(line_number, line, operation, patch_content) {
      identical(line, patch_content)
    }
  }
  if (fuzz_factor < 0 || fuzz_factor != as.integer(fuzz_factor)) {
    stop("fuzz_factor must be a non-negative integer")
  }
  fuzz_factor <- as.integer(fuzz_factor)

  lines <- js_split_nl(source)
  hunks <- patch$hunks
  if (length(hunks) == 0L) return(source)

  # EOFNL handling on the final hunk.
  last_hunk_lines <- hunks[[length(hunks)]]$lines
  prev_line <- ""
  remove_eofnl <- FALSE
  add_eofnl <- FALSE
  for (k in seq_along(last_hunk_lines)) {
    line <- last_hunk_lines[k]
    if (substr(line, 1L, 1L) == "\\") {
      if (substr(prev_line, 1L, 1L) == "+") remove_eofnl <- TRUE
      else if (substr(prev_line, 1L, 1L) == "-") add_eofnl <- TRUE
    }
    prev_line <- line
  }
  last_is_empty <- function() length(lines) > 0L && lines[length(lines)] == ""
  if (remove_eofnl) {
    if (add_eofnl) {
      if (fuzz_factor == 0L && last_is_empty()) return(FALSE)
    } else if (last_is_empty()) {
      lines <- lines[-length(lines)]
    } else if (fuzz_factor == 0L) {
      return(FALSE)
    }
  } else if (add_eofnl) {
    if (!last_is_empty()) {
      lines <- c(lines, "")
    } else if (fuzz_factor == 0L) {
      return(FALSE)
    }
  }

  line_at <- function(p) if (p >= 0L && p < length(lines)) lines[p + 1L] else NA_character_

  apply_hunk <- function(hunk_lines, to_pos, max_errors, hunk_lines_i = 0L,
                         last_context_line_matched = TRUE,
                         patched_lines = character(0), patched_lines_length = 0L) {
    n_consec <- 0L
    next_ctx_must <- FALSE
    hi <- hunk_lines_i
    while (hi < length(hunk_lines)) {
      hunk_line <- hunk_lines[hi + 1L]
      operation <- if (nchar(hunk_line) > 0L) substr(hunk_line, 1L, 1L) else " "
      content <- if (nchar(hunk_line) > 0L) substring(hunk_line, 2L) else hunk_line

      if (operation == "-") {
        cur <- line_at(to_pos)
        if (!is.na(cur) && compare_line(to_pos + 1L, cur, operation, content)) {
          to_pos <- to_pos + 1L
          n_consec <- 0L
        } else {
          if (max_errors == 0L || is.na(line_at(to_pos))) return(NULL)
          patched_lines[patched_lines_length + 1L] <- line_at(to_pos)
          return(apply_hunk(hunk_lines, to_pos + 1L, max_errors - 1L, hi, FALSE,
                            patched_lines, patched_lines_length + 1L))
        }
      }
      if (operation == "+") {
        if (!last_context_line_matched) return(NULL)
        patched_lines[patched_lines_length + 1L] <- content
        patched_lines_length <- patched_lines_length + 1L
        n_consec <- 0L
        next_ctx_must <- TRUE
      }
      if (operation == " ") {
        n_consec <- n_consec + 1L
        cur <- line_at(to_pos)
        patched_lines[patched_lines_length + 1L] <- if (is.na(cur)) NA_character_ else cur
        if (!is.na(cur) && compare_line(to_pos + 1L, cur, operation, content)) {
          patched_lines_length <- patched_lines_length + 1L
          last_context_line_matched <- TRUE
          next_ctx_must <- FALSE
          to_pos <- to_pos + 1L
        } else {
          if (next_ctx_must || max_errors == 0L) return(NULL)
          if (!is.na(line_at(to_pos))) {
            r <- apply_hunk(hunk_lines, to_pos + 1L, max_errors - 1L, hi + 1L, FALSE,
                            patched_lines, patched_lines_length + 1L)
            if (!is.null(r)) return(r)
            r <- apply_hunk(hunk_lines, to_pos + 1L, max_errors - 1L, hi, FALSE,
                            patched_lines, patched_lines_length + 1L)
            if (!is.null(r)) return(r)
          }
          return(apply_hunk(hunk_lines, to_pos, max_errors - 1L, hi + 1L, FALSE,
                            patched_lines, patched_lines_length))
        }
      }
      hi <- hi + 1L
    }
    patched_lines_length <- patched_lines_length - n_consec
    to_pos <- to_pos - n_consec
    result_lines <- if (patched_lines_length > 0L) patched_lines[seq_len(patched_lines_length)] else character(0)
    list(patched_lines = result_lines, old_line_last_i = to_pos - 1L)
  }

  result_lines <- character(0)
  min_line <- 0L
  prev_hunk_offset <- 0L

  for (h in seq_along(hunks)) {
    hunk <- hunks[[h]]
    max_line <- length(lines) - hunk$old_lines + fuzz_factor
    hunk_result <- NULL
    success_to_pos <- NULL
    for (max_errors in 0:fuzz_factor) {
      to_pos <- hunk$old_start + prev_hunk_offset - 1L
      iterator <- distance_iterator(to_pos, min_line, max_line)
      repeat {
        if (is.null(to_pos)) break
        hunk_result <- apply_hunk(hunk$lines, to_pos, max_errors)
        if (!is.null(hunk_result)) {
          success_to_pos <- to_pos
          break
        }
        to_pos <- iterator()
      }
      if (!is.null(hunk_result)) break
    }
    if (is.null(hunk_result)) return(FALSE)

    if (success_to_pos > min_line) {
      result_lines <- c(result_lines, lines[(min_line + 1L):success_to_pos])
    }
    if (length(hunk_result$patched_lines)) {
      result_lines <- c(result_lines, hunk_result$patched_lines)
    }
    min_line <- hunk_result$old_line_last_i + 1L
    prev_hunk_offset <- success_to_pos + 1L - hunk$old_start
  }

  if (min_line < length(lines)) {
    result_lines <- c(result_lines, lines[(min_line + 1L):length(lines)])
  }
  paste(result_lines, collapse = "\n")
}

#' Apply one or more patches via callbacks
#'
#' @param uni_diff A unified diff string (possibly multi-file) or a list of
#'   `jsdiff_patch` objects.
#' @param load_file `function(index)` returning the source string for a patch.
#' @param patched `function(index, content)` called with the patched content.
#' @param complete `function(err = NULL)` called once all patches are processed.
#' @param ... Further arguments passed to [apply_patch()].
#' @return Invisibly `NULL`.
#' @export
apply_patches <- function(uni_diff, load_file, patched, complete = function(err = NULL) NULL, ...) {
  sp_diff <- if (is.character(uni_diff)) parse_patch(uni_diff) else uni_diff
  for (idx in seq_along(sp_diff)) {
    index <- sp_diff[[idx]]
    data <- tryCatch(load_file(index), error = function(e) {
      complete(e); return(NULL)
    })
    updated <- apply_patch(data, index, ...)
    patched(index, updated)
  }
  complete(NULL)
  invisible(NULL)
}
