# Unified-diff patch creation, ported from jsdiff's src/patch/create.ts.
#
# A structured patch is represented as a named list (class "jsdiff_patch") with
# fields: old_file_name, new_file_name, old_header, new_header, hunks. Each hunk
# is a named list: old_start, old_lines, new_start, new_lines, lines (character
# vector of "+"/"-"/" " prefixed lines). Git patches may additionally carry
# is_git, is_rename, is_copy, is_create, is_delete, old_mode, new_mode, etc.

new_patch <- function(fields) {
  structure(fields, class = "jsdiff_patch")
}

is_patch <- function(x) inherits(x, "jsdiff_patch")

# JavaScript String.split('\n') semantics: yields (count of '\n') + 1 pieces,
# keeping empty fields (which R's strsplit handles inconsistently at the ends).
js_split_nl <- function(text) {
  if (!grepl("\n", text, fixed = TRUE)) return(text)
  m <- gregexpr("\n", text, fixed = TRUE)[[1]]
  starts <- c(1L, m + 1L)
  ends <- c(m - 1L, nchar(text))
  substring(text, starts, ends)
}

# Split text into lines, each retaining its trailing newline where present
# (jsdiff's splitLines).
split_lines <- function(text) {
  has_trailing_nl <- endsWith(text, "\n")
  result <- paste0(js_split_nl(text), "\n")
  if (has_trailing_nl) {
    result <- result[-length(result)]
  } else {
    last <- result[length(result)]
    result[length(result)] <- substr(last, 1L, nchar(last) - 1L)
  }
  result
}

#' Produce a structured patch describing the difference between two strings
#'
#' @param old_file_name,new_file_name File names recorded in the patch header.
#' @param old_str,new_str The two texts to compare.
#' @param old_header,new_header Optional header strings (e.g. timestamps).
#' @param context Number of unchanged context lines around each change.
#' @param ignore_whitespace If `TRUE`, ignore leading/trailing whitespace when
#'   comparing lines.
#' @param strip_trailing_cr If `TRUE`, normalise `\r\n` to `\n` before diffing.
#' @return A `jsdiff_patch` object.
#' @export
structured_patch <- function(old_file_name, new_file_name, old_str, new_str,
                             old_header = NULL, new_header = NULL, context = 4,
                             ignore_whitespace = FALSE, strip_trailing_cr = FALSE) {
  changes <- diff_lines(old_str, new_str, ignore_whitespace = ignore_whitespace,
                        strip_trailing_cr = strip_trailing_cr)
  m <- nrow(changes)
  diff <- vector("list", m + 1L)
  for (i in seq_len(m)) {
    diff[[i]] <- list(value = changes$value[i], added = changes$added[i],
                      removed = changes$removed[i], lines = NULL)
  }
  diff[[m + 1L]] <- list(value = "", added = FALSE, removed = FALSE,
                         lines = character(0))

  context_lines <- function(lines) paste0(" ", lines)
  get_lines <- function(rec) if (is.null(rec$lines)) split_lines(rec$value) else rec$lines

  hunks <- list()
  old_range_start <- 0L; new_range_start <- 0L
  cur_range <- character(0)
  old_line <- 1L; new_line <- 1L
  N <- length(diff)

  for (i in seq_len(N)) {
    cur <- diff[[i]]
    lines <- get_lines(cur)
    diff[[i]]$lines <- lines

    if (isTRUE(cur$added) || isTRUE(cur$removed)) {
      if (old_range_start == 0L) {
        old_range_start <- old_line
        new_range_start <- new_line
        if (i > 1L) {
          prev_lines <- get_lines(diff[[i - 1L]])
          diff[[i - 1L]]$lines <- prev_lines
          cur_range <- if (context > 0L) {
            context_lines(utils::tail(prev_lines, context))
          } else {
            character(0)
          }
          old_range_start <- old_range_start - length(cur_range)
          new_range_start <- new_range_start - length(cur_range)
        }
      }
      pref <- if (isTRUE(cur$added)) "+" else "-"
      cur_range <- c(cur_range, paste0(pref, lines))
      if (isTRUE(cur$added)) {
        new_line <- new_line + length(lines)
      } else {
        old_line <- old_line + length(lines)
      }
    } else {
      if (old_range_start != 0L) {
        if (length(lines) <= context * 2L && i < N - 1L) {
          cur_range <- c(cur_range, context_lines(lines))
        } else {
          context_size <- min(length(lines), context)
          if (context_size > 0L) {
            cur_range <- c(cur_range, context_lines(lines[seq_len(context_size)]))
          }
          hunks[[length(hunks) + 1L]] <- list(
            old_start = old_range_start,
            old_lines = old_line - old_range_start + context_size,
            new_start = new_range_start,
            new_lines = new_line - new_range_start + context_size,
            lines = cur_range
          )
          old_range_start <- 0L; new_range_start <- 0L
          cur_range <- character(0)
        }
      }
      old_line <- old_line + length(lines)
      new_line <- new_line + length(lines)
    }
  }

  # Step 2: strip trailing newlines and insert "\ No newline at end of file".
  for (h in seq_along(hunks)) {
    lines <- hunks[[h]]$lines
    new_lines <- character(0)
    for (k in seq_along(lines)) {
      ln <- lines[k]
      if (endsWith(ln, "\n")) {
        new_lines <- c(new_lines, substr(ln, 1L, nchar(ln) - 1L))
      } else {
        new_lines <- c(new_lines, ln, "\\ No newline at end of file")
      }
    }
    hunks[[h]]$lines <- new_lines
  }

  new_patch(list(
    old_file_name = old_file_name, new_file_name = new_file_name,
    old_header = old_header, new_header = new_header, hunks = hunks
  ))
}

#' Format a structured patch (or list of patches) as unified-diff text
#'
#' @param patch A `jsdiff_patch` object or a list of them (e.g. from
#'   [parse_patch()]).
#' @param include_index,include_underline,include_file_headers Header components
#'   to emit. All default to `TRUE`.
#' @return A length-1 character string.
#' @export
format_patch <- function(patch, include_index = TRUE, include_underline = TRUE,
                         include_file_headers = TRUE) {
  if (is.list(patch) && !is_patch(patch)) {
    if (length(patch) > 1L && !include_file_headers &&
        !all(vapply(patch, function(p) isTRUE(p$is_git), logical(1)))) {
      stop("Cannot omit file headers on a multi-file patch.")
    }
    return(paste(vapply(patch, function(p) {
      format_patch(p, include_index, include_underline, include_file_headers)
    }, character(1)), collapse = "\n"))
  }

  ret <- character(0)
  is_git <- isTRUE(patch$is_git)
  if (is_git) {
    include_index <- TRUE; include_underline <- TRUE; include_file_headers <- TRUE
    if (is.null(patch$old_file_name)) stop("old_file_name must be specified for Git patches")
    if (is.null(patch$new_file_name)) stop("new_file_name must be specified for Git patches")
    git_old <- patch$old_file_name
    git_new <- patch$new_file_name
    if (isTRUE(patch$is_create) && identical(git_old, "/dev/null")) {
      git_old <- sub("^b/", "a/", git_new)
    } else if (isTRUE(patch$is_delete) && identical(git_new, "/dev/null")) {
      git_new <- sub("^a/", "b/", git_old)
    }
    ret <- c(ret, paste0("diff --git ", quote_file_name_if_needed(git_old), " ",
                         quote_file_name_if_needed(git_new)))
    if (isTRUE(patch$is_delete)) {
      ret <- c(ret, paste0("deleted file mode ", patch$old_mode %||% "100644"))
    }
    if (isTRUE(patch$is_create)) {
      ret <- c(ret, paste0("new file mode ", patch$new_mode %||% "100644"))
    }
    if (!is.null(patch$old_mode) && !is.null(patch$new_mode) &&
        !isTRUE(patch$is_delete) && !isTRUE(patch$is_create)) {
      ret <- c(ret, paste0("old mode ", patch$old_mode),
               paste0("new mode ", patch$new_mode))
    }
    if (isTRUE(patch$is_rename)) {
      ret <- c(ret,
               paste0("rename from ", quote_file_name_if_needed(sub("^a/", "", patch$old_file_name %||% ""))),
               paste0("rename to ", quote_file_name_if_needed(sub("^b/", "", patch$new_file_name %||% ""))))
    }
    if (isTRUE(patch$is_copy)) {
      ret <- c(ret,
               paste0("copy from ", quote_file_name_if_needed(sub("^a/", "", patch$old_file_name %||% ""))),
               paste0("copy to ", quote_file_name_if_needed(sub("^b/", "", patch$new_file_name %||% ""))))
    }
  } else {
    if (include_index && identical(patch$old_file_name, patch$new_file_name) &&
        !is.null(patch$old_file_name)) {
      ret <- c(ret, paste0("Index: ", patch$old_file_name))
    }
    if (include_underline) {
      ret <- c(ret, "===================================================================")
    }
  }

  has_hunks <- length(patch$hunks) > 0L
  if (include_file_headers && !is.null(patch$old_file_name) &&
      !is.null(patch$new_file_name) && (!is_git || has_hunks)) {
    ret <- c(ret,
             paste0("--- ", quote_file_name_if_needed(patch$old_file_name),
                    if (!is.null(patch$old_header) && nzchar(patch$old_header)) paste0("\t", patch$old_header) else ""),
             paste0("+++ ", quote_file_name_if_needed(patch$new_file_name),
                    if (!is.null(patch$new_header) && nzchar(patch$new_header)) paste0("\t", patch$new_header) else ""))
  }

  for (hunk in patch$hunks) {
    old_start <- if (hunk$old_lines == 0L) hunk$old_start - 1L else hunk$old_start
    new_start <- if (hunk$new_lines == 0L) hunk$new_start - 1L else hunk$new_start
    ret <- c(ret, paste0("@@ -", old_start, ",", hunk$old_lines,
                         " +", new_start, ",", hunk$new_lines, " @@"))
    ret <- c(ret, hunk$lines)
  }

  paste0(paste(ret, collapse = "\n"), "\n")
}

#' Create a unified diff patch between two files
#' @inheritParams structured_patch
#' @param include_index,include_underline,include_file_headers Header components
#'   passed to [format_patch()].
#' @return A length-1 character string in unified diff format.
#' @export
create_two_files_patch <- function(old_file_name, new_file_name, old_str, new_str,
                                   old_header = NULL, new_header = NULL, context = 4,
                                   ignore_whitespace = FALSE, strip_trailing_cr = FALSE,
                                   include_index = TRUE, include_underline = TRUE,
                                   include_file_headers = TRUE) {
  patch <- structured_patch(old_file_name, new_file_name, old_str, new_str,
                            old_header, new_header, context = context,
                            ignore_whitespace = ignore_whitespace,
                            strip_trailing_cr = strip_trailing_cr)
  format_patch(patch, include_index, include_underline, include_file_headers)
}

#' Create a unified diff patch for a single file
#' @param file_name File name recorded on both sides of the patch.
#' @inheritParams create_two_files_patch
#' @return A length-1 character string in unified diff format.
#' @export
create_patch <- function(file_name, old_str, new_str, old_header = NULL,
                         new_header = NULL, context = 4,
                         ignore_whitespace = FALSE, strip_trailing_cr = FALSE,
                         include_index = TRUE, include_underline = TRUE,
                         include_file_headers = TRUE) {
  create_two_files_patch(file_name, file_name, old_str, new_str, old_header,
                         new_header, context = context,
                         ignore_whitespace = ignore_whitespace,
                         strip_trailing_cr = strip_trailing_cr,
                         include_index = include_index,
                         include_underline = include_underline,
                         include_file_headers = include_file_headers)
}

# --- C-style filename quoting (create.ts) ----------------------------------
needs_quoting <- function(s) {
  if (is.null(s) || !nzchar(s)) return(FALSE)
  cps <- utf8ToInt(s)
  any(cps < 0x20L | cps > 0x7eL) || grepl('"', s, fixed = TRUE) ||
    grepl("\\", s, fixed = TRUE)
}

quote_file_name_if_needed <- function(s) {
  if (is.null(s)) return(s)
  if (!needs_quoting(s)) return(s)
  bytes <- as.integer(charToRaw(enc2utf8(s)))
  esc <- vapply(bytes, function(b) {
    switch(as.character(b),
           "7" = "\\a", "8" = "\\b", "9" = "\\t", "10" = "\\n",
           "11" = "\\v", "12" = "\\f", "13" = "\\r", "34" = "\\\"",
           "92" = "\\\\",
           if (b >= 0x20L && b <= 0x7eL) intToUtf8(b)
           else paste0("\\", formatC(as.integer(b), width = 3, flag = "0", format = "o")))
  }, character(1))
  paste0("\"", paste0(esc, collapse = ""), "\"")
}
