# Coloured console printing of rendered diffs.
#
# A diff rendered into a data.frame / data.table column is plain text with
# markup markers ("the [-cat-]{+dog+} sat") rather than ANSI colour, because
# plain text is the only form that survives printing: print.data.table and
# print.data.frame both format their cells and hand them to print.default,
# which *escapes* the ESC byte of a colour sequence. A column of coloured
# strings therefore prints as a literal "\033[31mcat\033[0m", and padding it
# would be wrong anyway - the printer counts the escape bytes when it computes
# column widths.
#
# Colour needs its own printer, and only cat() / writeLines() emit escapes
# untouched. diff_print() does the formatting itself: it converts markup to
# ANSI, measures every cell by its *visible* width, pads to that, and cats the
# result. diff_print_auto() wires it into console auto-printing.

# A single ANSI SGR sequence.
.ansi_rx <- "\033\\[[0-9;]*m"

# Internal: drop colour sequences, so widths can be measured.
.strip_ansi <- function(x) gsub(.ansi_rx, "", x, perl = TRUE)

# Internal: visible width of each element (wide characters count as 2).
.disp_width <- function(x) {
  x <- .strip_ansi(x)
  w <- nchar(x, type = "width", allowNA = TRUE)
  bad <- is.na(w)
  if (any(bad)) w[bad] <- nchar(x[bad], type = "bytes")
  w
}

# Internal: pad to a visible width, on the left (right-justified) by default.
.pad_to <- function(x, width, left = FALSE) {
  pad <- strrep(" ", pmax(0L, width - .disp_width(x)))
  if (left) paste0(x, pad) else paste0(pad, x)
}

# Internal: make control characters visible. Cells in a table have to be one
# line long, or the columns come apart; a line diff's values contain newlines.
.escape_ctrl <- function(x) {
  if (!any(grepl("[\n\r\t]", x), na.rm = TRUE)) return(x)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  gsub("\t", "\\t", x, fixed = TRUE)
}

# Internal: regex for one markup span. (?s) so a span can cover the newlines in
# a line/sentence diff; \Q..\E so markers are matched literally.
.markup_rx <- function(open, close) {
  paste0("(?s)\\Q", open, "\\E(.*?)\\Q", close, "\\E")
}

.check_markers <- function(add, remove) {
  if (length(add) != 2L || length(remove) != 2L) {
    stop("`add` and `remove` must each be length-2 character vectors.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Convert inline diff markup to ANSI colour
#'
#' Turns the markers produced by [changes_to_markup()] (and so by the
#' vectorised diff functions - see [vectorised-diffs]) into ANSI colour
#' sequences: insertions green, deletions red, markers removed. The result is
#' meant for [cat()] or [writeLines()]; [print()] escapes ANSI sequences, which
#' is why [diff_print()] exists.
#'
#' @param x A character vector of markup strings (anything else is coerced with
#'   [as.character()]).
#' @param add,remove Length-2 character vectors giving the opening and closing
#'   markers to look for. Default to the options `jsdiffr.markup_add` and
#'   `jsdiffr.markup_remove`, matching [changes_to_markup()].
#' @param color Logical; force colour on/off. Defaults to auto-detection. When
#'   `FALSE`, `x` is returned unchanged - the markers are left in place, since
#'   they are all that distinguishes a change without colour.
#' @return A character vector the same length as `x`.
#' @seealso [diff_print()] to print a whole column or table in colour,
#'   [changes_to_markup()] for the markup itself.
#' @examples
#' m <- diff_words(c("the cat sat", "12 Smith St"),
#'                 c("the dog sat", "12 Smith Street"))
#' cat(markup_to_ansi(m, color = TRUE), sep = "\n")
#' @export
markup_to_ansi <- function(x,
                           add = getOption("jsdiffr.markup_add", c("{+", "+}")),
                           remove = getOption("jsdiffr.markup_remove", c("[-", "-]")),
                           color = supports_color()) {
  .check_markers(add, remove)
  if (!is.character(x)) x <- as.character(x)
  if (!length(x) || !color) return(x)
  x <- gsub(.markup_rx(add[1L], add[2L]),
            paste0(ansi$green, "\\1", ansi$reset), x, perl = TRUE)
  gsub(.markup_rx(remove[1L], remove[2L]),
       paste0(ansi$red, "\\1", ansi$reset), x, perl = TRUE)
}

# Internal: does this character vector contain diff markup?
.is_markup <- function(x, add, remove) {
  if (!is.character(x) || !length(x)) return(FALSE)
  any(grepl(.markup_rx(add[1L], add[2L]), x, perl = TRUE)) ||
    any(grepl(.markup_rx(remove[1L], remove[2L]), x, perl = TRUE))
}

# Internal: is this a list column of per-row jsdiff_changes (format =
# "changes")? Those can be rendered inline too.
.is_changes_col <- function(x) {
  if (!is.list(x) || is.data.frame(x)) return(FALSE)
  keep <- !vapply(x, is.null, logical(1L))
  any(keep) && all(vapply(x[keep], is_changes, logical(1L)))
}

#' Does an object contain inline diff markup?
#'
#' Used by [diff_print()] to decide which columns to colour. For a
#' `data.frame` / `data.table` only a sample of rows is scanned (the head and
#' tail), so that the check stays cheap on long tables.
#'
#' @param x A character vector, or a `data.frame` / `data.table`.
#' @param add,remove Markers to look for, as in [markup_to_ansi()].
#' @param max_scan Maximum number of rows (or elements) to scan from each end.
#' @return A logical scalar.
#' @examples
#' has_markup(diff_words(c("the cat", "a b"), c("the dog", "a b")))
#' has_markup(c("no", "markup", "here"))
#' @export
has_markup <- function(x,
                       add = getOption("jsdiffr.markup_add", c("{+", "+}")),
                       remove = getOption("jsdiffr.markup_remove", c("[-", "-]")),
                       max_scan = 200L) {
  .check_markers(add, remove)
  sample_of <- function(v) {
    n <- length(v)
    if (n <= 2L * max_scan) v else c(v[seq_len(max_scan)], v[seq.int(n - max_scan + 1L, n)])
  }
  if (is.data.frame(x)) {
    for (j in seq_along(x)) {
      col <- sample_of(x[[j]])
      if (.is_changes_col(col)) return(TRUE)
      if (is.character(col) && .is_markup(col, add, remove)) return(TRUE)
    }
    return(FALSE)
  }
  x <- sample_of(x)
  if (.is_changes_col(x)) return(TRUE)
  .is_markup(x, add, remove)
}

# Internal: one display string per element of a column.
.render_cells <- function(col, markup, color, add, remove) {
  if (.is_changes_col(col)) {
    return(vapply(col, function(ch) {
      if (is.null(ch)) "NA" else format.jsdiff_changes(ch, color = color)
    }, character(1L)))
  }
  if (is.list(col) && !is.data.frame(col)) {
    return(vapply(col, function(e) {
      if (is.null(e)) "NULL" else paste0("<", class(e)[1L], "[", length(e), "]>")
    }, character(1L)))
  }
  # Character and factor cells are used as they stand: format() would pad them
  # to a common width, and for a coloured cell that width counts the escape
  # bytes. All padding happens later, measured on the visible width.
  if (is.character(col)) {
    out <- if (markup) {
      markup_to_ansi(col, add = add, remove = remove, color = color)
    } else {
      col
    }
    out[is.na(col)] <- "NA"
    return(out)
  }
  if (is.factor(col)) {
    out <- as.character(col)
    out[is.na(col)] <- "NA"
    return(out)
  }
  out <- format(col, trim = TRUE)
  out[is.na(col)] <- "NA"
  out
}

# Internal: should this column be left-justified? Text reads better flush left;
# numbers line up on the right, as in print.data.table.
.left_justify <- function(col, markup) {
  markup || .is_changes_col(col) || is.character(col) || is.factor(col) ||
    is.list(col)
}

#' Print a diff in colour
#'
#' @description
#' Prints a rendered diff - a column of inline markup, a whole `data.frame` /
#' `data.table` containing such columns, a list column of
#' [jsdiff_changes][jsdiff_changes] objects, or a single `jsdiff_changes`
#' object - with insertions in green and deletions in red.
#'
#' This exists because colour cannot survive ordinary printing.
#' `print.data.table()` and `print.data.frame()` format their cells and pass
#' them to [print.default()], which escapes the ESC byte of an ANSI sequence:
#' a coloured cell prints as the literal text `\033[31mcat\033[0m`, whatever
#' the terminal. `diff_print()` formats the table itself and writes it with
#' [cat()], measuring each cell by its *visible* width so that the columns
#' still line up around the (zero-width) colour sequences.
#'
#' ```r
#' dt <- data.table::data.table(iris)
#' diff_print(dt[, .(blah = diff_chars(Sepal.Length, Sepal.Width), Species)])
#' ```
#'
#' See [diff_print_auto()] to have the console do this automatically.
#'
#' @param x A `data.frame`, `data.table`, character vector of markup, list of
#'   `jsdiff_changes` objects, or a single `jsdiff_changes` object.
#' @param cols For a table, the columns to treat as diff markup: names or
#'   indices. The default (`NULL`) detects them by looking for the markers,
#'   which also picks up columns produced with `format = "changes"`.
#' @param color Logical; force colour on/off. Defaults to auto-detection (see
#'   the `jsdiffr.color` option). With colour off the markup is printed as it
#'   stands, markers and all.
#' @param add,remove Markers to convert, as in [markup_to_ansi()].
#' @param nrows Print at most this many rows; longer tables show the first and
#'   last `topn` rows. `Inf` prints everything.
#' @param topn Number of rows to show at each end of a truncated table.
#' @param row_names Logical; show row numbers (`data.table` style) or row
#'   names.
#' @param ... Unused, for method-like calls.
#' @return `x`, invisibly.
#' @seealso [markup_to_ansi()], [diff_print_auto()], [vectorised-diffs].
#' @examples
#' old <- c("the cat sat", "12 Smith St", "hello")
#' new <- c("the dog sat", "12 Smith Street", "hello")
#'
#' diff_print(diff_words(old, new), color = TRUE)
#'
#' df <- data.frame(old = old, new = new)
#' df$diff <- diff_words(old, new)
#' diff_print(df, color = TRUE)
#' @export
diff_print <- function(x, cols = NULL, color = supports_color(),
                       add = getOption("jsdiffr.markup_add", c("{+", "+}")),
                       remove = getOption("jsdiffr.markup_remove", c("[-", "-]")),
                       nrows = getOption("jsdiffr.print_nrows", 100L),
                       topn = getOption("jsdiffr.print_topn", 5L),
                       row_names = TRUE, ...) {
  .check_markers(add, remove)
  if (is_changes(x)) {
    print(x, color = color)
    return(invisible(x))
  }
  if (is.data.frame(x)) {
    .diff_print_table(x, cols = cols, color = color, add = add, remove = remove,
                      nrows = nrows, topn = topn, row_names = row_names)
    return(invisible(x))
  }
  if (!is.character(x) && !is.list(x) && !is.atomic(x)) {
    stop("`x` must be a data.frame, a vector of diff markup, or a ",
         "jsdiff_changes object.", call. = FALSE)
  }
  n <- length(x)
  if (n == 0L) {
    writeLines(if (is.list(x)) "list()" else "character(0)")
    return(invisible(x))
  }
  # Subset before rendering: printing the head and tail of a long column must
  # not cost a pass over the whole thing.
  idx <- .print_rows(n, nrows, topn)
  cells <- .render_cells(x[idx$rows], markup = TRUE, color = color, add = add,
                         remove = remove)
  labs <- paste0("[", idx$rows, "]")
  labs <- .pad_to(labs, max(.disp_width(labs)))
  lines <- paste0(labs, " ", cells)
  if (!is.na(idx$split)) {
    lines <- append(lines, .pad_to("---", max(.disp_width(labs))),
                    after = idx$split)
  }
  writeLines(sub("[ ]+$", "", lines))
  invisible(x)
}

# Internal: which rows to print, and where to break for the "---" marker.
.print_rows <- function(n, nrows, topn) {
  if (!is.finite(nrows) || n <= nrows || topn <= 0L || 2L * topn >= n) {
    return(list(rows = seq_len(n), split = NA_integer_))
  }
  topn <- as.integer(topn)
  list(rows = c(seq_len(topn), seq.int(n - topn + 1L, n)), split = topn)
}

.diff_print_table <- function(x, cols, color, add, remove, nrows, topn,
                              row_names) {
  n <- nrow(x)
  nms <- names(x)
  if (!length(nms)) nms <- character(0)
  if (n == 0L || !length(nms)) {
    writeLines(paste0(
      "Empty table: ", n, " rows, ", length(nms), " cols",
      if (length(nms)) paste0(" (", paste(nms, collapse = ", "), ")") else ""
    ))
    return(invisible(NULL))
  }
  idx <- .print_rows(n, nrows, topn)
  rows <- idx$rows

  # Which columns hold markup. Detected on the printed rows only, so a long
  # table costs no more than a short one.
  if (is.null(cols)) {
    is_diff <- vapply(x, function(col) {
      is.character(col) && is.null(dim(col)) && .is_markup(col[rows], add, remove)
    }, logical(1L))
  } else {
    if (is.character(cols)) {
      bad <- setdiff(cols, nms)
      if (length(bad)) {
        stop("Unknown column(s) in `cols`: ", paste(bad, collapse = ", "),
             call. = FALSE)
      }
      sel <- match(cols, nms)
    } else {
      sel <- as.integer(cols)
      if (any(sel < 1L | sel > length(nms))) {
        stop("`cols` is out of range for a table with ", length(nms),
             " columns.", call. = FALSE)
      }
    }
    is_diff <- logical(length(nms))
    is_diff[sel] <- TRUE
  }

  body <- vector("list", length(nms))
  left <- logical(length(nms))
  for (j in seq_along(nms)) {
    col <- x[[j]]
    # A matrix or data.frame column (legal in a data.frame) has no per-element
    # cell, so collapse each row of it into one.
    if (!is.null(dim(col))) {
      col <- apply(as.matrix(col), 1L,
                   function(r) paste(format(r, trim = TRUE), collapse = " "))
    }
    body[[j]] <- .escape_ctrl(.render_cells(col[rows], markup = is_diff[j],
                                            color = color, add = add,
                                            remove = remove))
    left[j] <- .left_justify(col, is_diff[j])
  }

  labs <- if (!isTRUE(row_names)) {
    rep("", length(rows))
  } else if (inherits(x, "data.table")) {
    paste0(rows, ":")
  } else {
    as.character(row.names(x))[rows]
  }
  lab_w <- if (length(labs)) max(.disp_width(c(labs, ""))) else 0L
  labs <- .pad_to(labs, lab_w)

  out <- character(length(rows) + 1L)
  out[1L] <- strrep(" ", lab_w)
  for (j in seq_along(nms)) {
    w <- max(.disp_width(c(body[[j]], nms[j])))
    out[1L] <- paste0(out[1L], "  ", .pad_to(nms[j], w, left = left[j]))
    out[-1L] <- paste0(out[-1L], "  ", .pad_to(body[[j]], w, left = left[j]))
  }
  out[-1L] <- paste0(labs, out[-1L])
  if (!is.na(idx$split)) {
    out <- append(out, .pad_to("---", lab_w), after = idx$split + 1L)
  }
  writeLines(sub("[ ]+$", "", out))
  if (length(rows) < n) {
    writeLines(paste0(" ", n, " rows (", length(rows), " shown), ",
                      length(nms), " cols"))
  }
  invisible(NULL)
}

#' Colour diffs in console auto-printing
#'
#' @description
#' Makes the console print tables containing diff markup in colour, so that
#'
#' ```r
#' dt[, .(blah = diff_chars(Sepal.Length, Sepal.Width), Species)]
#' ```
#'
#' comes out coloured without wrapping the call in [diff_print()].
#'
#' There is no way for a package to do this on its own: auto-printing
#' dispatches to `print.data.table()` / `print.data.frame()`, which escape ANSI
#' sequences (see [diff_print()]), and which method is used is settled by the
#' owning packages. What *does* take precedence is a method defined in the
#' global environment, so `diff_print_auto()` defines one there: a wrapper that
#' sends tables with diff markup to [diff_print()] and everything else to the
#' method that would otherwise have run.
#'
#' It therefore changes the user's workspace, and so is never enabled by
#' default - call it yourself, interactively or from `.Rprofile`.
#' `diff_print_auto(FALSE)` removes the wrappers again; it only ever removes
#' its own.
#'
#' @param enable `TRUE` to install the wrappers, `FALSE` to remove them.
#' @param classes Classes to wrap. Defaults to both table classes.
#' @param envir Environment to define the methods in. Defaults to the global
#'   environment, which is what gives them precedence at the console.
#' @param quiet Logical; suppress the message naming the objects that were
#'   defined or removed. Defaults to quiet outside an interactive session.
#' @return `TRUE` if the wrappers are now installed, `FALSE` otherwise,
#'   invisibly.
#' @seealso [diff_print()] for the explicit form, which needs no wrappers.
#' @examples
#' \dontrun{
#' diff_print_auto()
#' dt <- data.table::data.table(iris)
#' dt[, .(blah = diff_chars(Sepal.Length, Sepal.Width), Species)]
#' diff_print_auto(FALSE)
#' }
#' @export
diff_print_auto <- function(enable = TRUE,
                            classes = c("data.table", "data.frame"),
                            envir = globalenv(),
                            quiet = !interactive()) {
  stopifnot(is.logical(enable), length(enable) == 1L, !is.na(enable))
  touched <- character(0)
  for (cl in classes) {
    nm <- paste0("print.", cl)
    existing <- if (exists(nm, envir = envir, inherits = FALSE)) {
      get(nm, envir = envir, inherits = FALSE)
    } else {
      NULL
    }
    mine <- !is.null(existing) && isTRUE(attr(existing, "jsdiffr_print_auto"))
    if (!enable) {
      if (mine) {
        wrapped <- attr(existing, "jsdiffr_wrapped")
        if (is.null(wrapped)) {
          rm(list = nm, envir = envir)
        } else {
          # it wrapped a method that was already here: put that back
          assign(nm, wrapped, envir = envir)
        }
        touched <- c(touched, nm)
      }
      next
    }
    if (mine) next
    original <- existing
    if (is.null(original)) {
      original <- utils::getS3method("print", cl, optional = TRUE)
    }
    if (is.null(original)) next
    assign(nm, .print_auto_shim(original, wrapped = existing), envir = envir)
    touched <- c(touched, nm)
  }
  installed <- enable &&
    any(vapply(paste0("print.", classes), exists, logical(1L),
               envir = envir, inherits = FALSE))
  if (!quiet && length(touched)) {
    message(if (enable) "Defined " else "Removed ",
            paste(touched, collapse = ", "),
            if (enable) " in the global environment; diff_print_auto(FALSE) undoes it." else "")
  }
  invisible(installed)
}

# Internal: build the wrapper, holding the method it replaces in its closure.
.print_auto_shim <- function(original, wrapped = NULL) {
  force(original)  # or the closure would see the loop's next method
  f <- function(x, ...) {
    if (has_markup(x)) {
      diff_print(x, ...)
    } else {
      original(x, ...)
    }
  }
  attr(f, "jsdiffr_print_auto") <- TRUE
  # Only set when a method was already defined here, so that disabling can put
  # it back rather than removing it.
  attr(f, "jsdiffr_wrapped") <- wrapped
  f
}
