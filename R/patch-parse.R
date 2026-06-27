# Unified-diff parser, ported from jsdiff's src/patch/parse.ts. Understands
# Git's dialect of unified diff (rename/copy/create/delete, modes, binary).
# Returns a list of `jsdiff_patch` objects (one per file in the patch).

#' Parse a unified diff string into structured patch objects
#'
#' @param uni_diff A length-1 character string in unified diff format.
#' @return A list of `jsdiff_patch` objects.
#' @export
parse_patch <- function(uni_diff) {
  diffstr <- js_split_nl(uni_diff)
  len <- length(diffstr)
  i <- 1L
  out <- list()

  cur_char <- function(s, k) substr(s, k, k)

  is_git_diff_header <- function(line) grepl("^diff --git ", line)
  is_diff_header <- function(line) {
    is_git_diff_header(line) || grepl("^Index:\\s", line, perl = TRUE) ||
      grepl("^diff(?: -r \\w+)+\\s", line, perl = TRUE)
  }
  is_file_header <- function(line) grepl("^(---|\\+\\+\\+)\\s", line, perl = TRUE)
  is_hunk_header <- function(line) grepl("^@@\\s", line, perl = TRUE)

  parse_quoted_file_name <- function(s) {
    if (!startsWith(s, "\"")) return(NULL)
    result <- ""
    nch <- nchar(s)
    j <- 2L
    while (j <= nch) {
      cj <- cur_char(s, j)
      if (cj == "\"") {
        return(list(file_name = result, raw_length = j))
      }
      if (cj == "\\" && j + 1L <= nch) {
        j <- j + 1L
        cj <- cur_char(s, j)
        simple <- c(a = "\a", b = "\b", f = "\f", n = "\n", r = "\r",
                    t = "\t", v = "\v", "\\" = "\\", "\"" = "\"")
        if (cj %in% names(simple)) {
          result <- paste0(result, simple[[cj]])
        } else if (cj %in% as.character(0:7)) {
          if (j + 2L > nch) return(NULL)
          d2 <- cur_char(s, j + 1L); d3 <- cur_char(s, j + 2L)
          if (!(d2 %in% as.character(0:7)) || !(d3 %in% as.character(0:7))) return(NULL)
          bytes <- strtoi(substr(s, j, j + 2L), 8L)
          j <- j + 3L
          while (cur_char(s, j) == "\\" && cur_char(s, j + 1L) %in% as.character(0:7)) {
            if (j + 3L > nch) return(NULL)
            if (!(cur_char(s, j + 2L) %in% as.character(0:7)) ||
                !(cur_char(s, j + 3L) %in% as.character(0:7))) return(NULL)
            bytes <- c(bytes, strtoi(substr(s, j + 1L, j + 3L), 8L))
            j <- j + 4L
          }
          decoded <- rawToChar(as.raw(bytes))
          Encoding(decoded) <- "UTF-8"
          result <- paste0(result, decoded)
          next
        } else {
          return(NULL)
        }
      } else {
        result <- paste0(result, cj)
      }
      j <- j + 1L
    }
    NULL
  }

  unquote_if_quoted <- function(s) {
    if (startsWith(s, "\"")) {
      parsed <- parse_quoted_file_name(s)
      if (!is.null(parsed)) return(parsed$file_name)
    }
    s
  }

  parse_git_diff_header <- function(line) {
    rest <- substring(line, nchar("diff --git ") + 1L)
    if (startsWith(rest, "\"")) {
      old_path <- parse_quoted_file_name(rest)
      if (is.null(old_path)) return(NULL)
      after_old <- substring(rest, old_path$raw_length + 1L + 1L)  # +1 for space
      if (startsWith(after_old, "\"")) {
        new_path <- parse_quoted_file_name(after_old)
        if (is.null(new_path)) return(NULL)
        new_file_name <- new_path$file_name
      } else {
        new_file_name <- after_old
      }
      return(list(old_file_name = old_path$file_name, new_file_name = new_file_name))
    }
    quote_idx <- regexpr("\"", rest, fixed = TRUE)
    if (quote_idx > 1L) {
      old_file_name <- substr(rest, 1L, quote_idx - 2L)
      new_path <- parse_quoted_file_name(substring(rest, quote_idx))
      if (is.null(new_path)) return(NULL)
      return(list(old_file_name = old_file_name, new_file_name = new_path$file_name))
    }
    if (startsWith(rest, "a/")) {
      splits <- integer(0)
      idx <- 0L
      repeat {
        nxt <- regexpr(" b/", substring(rest, idx + 1L), fixed = TRUE)
        if (nxt == -1L) break
        idx <- idx + nxt
        splits <- c(splits, idx)
      }
      if (length(splits) > 0L) {
        mid <- splits[floor(length(splits) / 2) + 1L]
        return(list(old_file_name = substr(rest, 1L, mid - 1L),
                    new_file_name = substring(rest, mid + 1L)))
      }
    }
    NULL
  }

  parse_file_header <- function(index) {
    line <- diffstr[i]
    fh <- regmatches(line, regexec("^(---|\\+\\+\\+)\\s+", line, perl = TRUE))[[1]]
    if (length(fh) == 0L) return(index)
    prefix <- fh[2L]
    data <- strsplit(trimws(substring(line, 4L)), "\t", fixed = TRUE)[[1]]
    header <- if (length(data) >= 2L) trimws(data[2L]) else ""
    file_name <- data[1L]
    if (startsWith(file_name, "\"")) {
      file_name <- unquote_if_quoted(file_name)
    } else {
      file_name <- gsub("\\\\", "\\", file_name, fixed = TRUE)
    }
    if (prefix == "---") {
      index$old_file_name <- file_name
      index$old_header <- header
    } else {
      index$new_file_name <- file_name
      index$new_header <- header
    }
    i <<- i + 1L
    index
  }

  parse_hunk <- function() {
    chunk_header_index <- i
    chunk_header_line <- diffstr[i]
    i <<- i + 1L
    ch <- regmatches(chunk_header_line,
                     regexec("^@@ -(\\d+)(?:,(\\d+))? \\+(\\d+)(?:,(\\d+))? @@",
                             chunk_header_line, perl = TRUE))[[1]]
    old_lines <- if (ch[3L] == "") 1L else as.integer(ch[3L])
    new_lines <- if (ch[5L] == "") 1L else as.integer(ch[5L])
    hunk <- list(
      old_start = as.integer(ch[2L]),
      old_lines = old_lines,
      new_start = as.integer(ch[4L]),
      new_lines = new_lines,
      lines = character(0)
    )
    if (hunk$old_lines == 0L) hunk$old_start <- hunk$old_start + 1L
    if (hunk$new_lines == 0L) hunk$new_start <- hunk$new_start + 1L

    add_count <- 0L; remove_count <- 0L
    while (i <= len &&
           (remove_count < hunk$old_lines || add_count < hunk$new_lines ||
            (i <= len && startsWith(diffstr[i], "\\")))) {
      line <- diffstr[i]
      operation <- if (nchar(line) == 0L && i != len) " " else substr(line, 1L, 1L)
      if (operation %in% c("+", "-", " ", "\\")) {
        hunk$lines <- c(hunk$lines, line)
        if (operation == "+") add_count <- add_count + 1L
        else if (operation == "-") remove_count <- remove_count + 1L
        else if (operation == " ") { add_count <- add_count + 1L; remove_count <- remove_count + 1L }
      } else {
        stop(sprintf("Hunk at line %d contained invalid line %s", chunk_header_index, line))
      }
      i <<- i + 1L
    }
    if (add_count == 0L && hunk$new_lines == 1L) hunk$new_lines <- 0L
    if (remove_count == 0L && hunk$old_lines == 1L) hunk$old_lines <- 0L
    if (add_count != hunk$new_lines) {
      stop(sprintf("Added line count did not match for hunk at line %d", chunk_header_index))
    }
    if (remove_count != hunk$old_lines) {
      stop(sprintf("Removed line count did not match for hunk at line %d", chunk_header_index))
    }
    if (i <= len && nzchar(diffstr[i]) && grepl("^[+ -]", diffstr[i]) &&
        !is_file_header(diffstr[i])) {
      stop(sprintf("Hunk at line %d has more lines than expected", chunk_header_index))
    }
    hunk
  }

  parse_index <- function() {
    index <- list(hunks = list())
    seen_diff_header <- FALSE
    early_return <- FALSE

    while (i <= len) {
      line <- diffstr[i]
      if (is_file_header(line) || is_hunk_header(line)) break
      if (is_git_diff_header(line)) {
        if (seen_diff_header) { early_return <- TRUE; break }
        seen_diff_header <- TRUE
        index$is_git <- TRUE
        paths <- parse_git_diff_header(line)
        if (!is.null(paths)) {
          index$old_file_name <- paths$old_file_name
          index$new_file_name <- paths$new_file_name
        }
        i <<- i + 1L
        while (i <= len) {
          ext_line <- diffstr[i]
          if (is_file_header(ext_line) || is_hunk_header(ext_line) || is_diff_header(ext_line)) break
          m <- regmatches(ext_line, regexec("^rename from (.*)", ext_line))[[1]]
          if (length(m)) { index$old_file_name <- paste0("a/", unquote_if_quoted(m[2L])); index$is_rename <- TRUE }
          m <- regmatches(ext_line, regexec("^rename to (.*)", ext_line))[[1]]
          if (length(m)) { index$new_file_name <- paste0("b/", unquote_if_quoted(m[2L])); index$is_rename <- TRUE }
          m <- regmatches(ext_line, regexec("^copy from (.*)", ext_line))[[1]]
          if (length(m)) { index$old_file_name <- paste0("a/", unquote_if_quoted(m[2L])); index$is_copy <- TRUE }
          m <- regmatches(ext_line, regexec("^copy to (.*)", ext_line))[[1]]
          if (length(m)) { index$new_file_name <- paste0("b/", unquote_if_quoted(m[2L])); index$is_copy <- TRUE }
          m <- regmatches(ext_line, regexec("^new file mode (\\d+)", ext_line, perl = TRUE))[[1]]
          if (length(m)) { index$is_create <- TRUE; index$new_mode <- m[2L] }
          m <- regmatches(ext_line, regexec("^deleted file mode (\\d+)", ext_line, perl = TRUE))[[1]]
          if (length(m)) { index$is_delete <- TRUE; index$old_mode <- m[2L] }
          m <- regmatches(ext_line, regexec("^old mode (\\d+)", ext_line, perl = TRUE))[[1]]
          if (length(m)) index$old_mode <- m[2L]
          m <- regmatches(ext_line, regexec("^new mode (\\d+)", ext_line, perl = TRUE))[[1]]
          if (length(m)) index$new_mode <- m[2L]
          if (grepl("^Binary files ", ext_line)) index$is_binary <- TRUE
          i <<- i + 1L
        }
        next
      } else if (is_diff_header(line)) {
        if (seen_diff_header) { early_return <- TRUE; break }
        seen_diff_header <- TRUE
        hm <- regexpr("^(?:Index:|diff(?: -r \\w+)+)\\s+", line, perl = TRUE)
        if (hm != -1L) {
          index$index <- trimws(substring(line, attr(hm, "match.length") + 1L))
        }
      }
      i <<- i + 1L
    }

    if (!early_return) {
      index <- parse_file_header(index)
      index <- parse_file_header(index)
      if (is.null(index$old_file_name) != is.null(index$new_file_name)) {
        stop("Missing file header")
      }
      while (i <= len) {
        line <- diffstr[i]
        if (is_diff_header(line) || is_file_header(line) ||
            grepl("^===================================================================", line)) break
        if (is_hunk_header(line)) {
          index$hunks[[length(index$hunks) + 1L]] <- parse_hunk()
        } else {
          i <<- i + 1L
        }
      }
    }
    out[[length(out) + 1L]] <<- new_patch(index)
  }

  while (i <= len) {
    parse_index()
  }
  out
}
