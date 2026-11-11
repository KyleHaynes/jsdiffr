# Vectorised (data.frame / data.table friendly) diffing.
#
# The scalar diff_* functions take two length-1 strings and return a
# `jsdiff_changes` table, which is the wrong shape for a column: inside a
# data.frame or data.table expression what is wanted is one value per row. So
# when either input has length != 1, the diff is computed element-wise
# (recycling a length-1 side) and each row's change objects are rendered down to
# a single string by `format`:
#
#   dt[, .(blah = diff_chars(Sepal.Length, Sepal.Width), Species)]
#
# `format = "changes"` opts out of that rendering and returns a list column of
# the per-row `jsdiff_changes` tables instead. Passing `format` explicitly with
# length-1 inputs renders a single string, so the two paths stay consistent;
# the only reason length matters is to keep the historical default (a
# `jsdiff_changes` table) for scalar calls.

# Supported values of `format`. "changes" is the non-rendering case.
diff_formats <- c("changes", "markup", "ansi", "html", "xml", "added", "removed")

#' Render change objects as an inline markup string
#'
#' Collapses a diff to one string, wrapping insertions and deletions in markers
#' (`{+added+}` and `[-removed-]` by default) so the whole change is readable in
#' a single line of plain text - which is what makes it usable as a column in a
#' `data.frame` / `data.table`. This is the renderer behind
#' `format = "markup"` in the vectorised diff functions (see
#' [vectorised-diffs]).
#'
#' @param changes A `jsdiff_changes` object from a string-based diff, or `NULL`
#'   (which returns `NA_character_`).
#' @param add,remove Length-2 character vectors giving the opening and closing
#'   markers for insertions and deletions. Default to the options
#'   `jsdiffr.markup_add` and `jsdiffr.markup_remove`, so they can be changed
#'   session-wide.
#' @return A length-1 character string, or `NA_character_` if `changes` is
#'   `NULL`.
#' @seealso [markup_to_ansi()] and [diff_print()], which turn this markup back
#'   into colour for the console.
#' @examples
#' changes_to_markup(diff_words("the cat sat", "the dog sat"))
#'
#' # wdiff-style markers
#' changes_to_markup(diff_chars("abc", "abd"),
#'                   add = c("+[", "]"), remove = c("-[", "]"))
#' @export
changes_to_markup <- function(changes,
                              add = getOption("jsdiffr.markup_add", c("{+", "+}")),
                              remove = getOption("jsdiffr.markup_remove", c("[-", "-]"))) {
  if (is.null(changes)) return(NA_character_)
  if (identical(attr(changes, "jsdiffr_value_type"), "list")) {
    stop("changes_to_markup() requires a string-based diff, not an array diff.",
         call. = FALSE)
  }
  if (length(add) != 2L || length(remove) != 2L) {
    stop("`add` and `remove` must each be length-2 character vectors.",
         call. = FALSE)
  }
  val <- changes$value
  if (!length(val)) return("")
  pieces <- ifelse(
    changes$added, paste0(add[1L], val, add[2L]),
    ifelse(changes$removed, paste0(remove[1L], val, remove[2L]), val)
  )
  paste0(pieces, collapse = "")
}

# Internal: function(changes) -> length-1 character, for each string `format`.
diff_renderer <- function(format) {
  switch(
    format,
    markup  = changes_to_markup,
    ansi    = function(ch) format.jsdiff_changes(ch, color = TRUE),
    html    = function(ch) .inline_html(ch),
    xml     = convert_changes_to_xml,
    added   = function(ch) paste0(ch$value[ch$added], collapse = ""),
    removed = function(ch) paste0(ch$value[ch$removed], collapse = ""),
    stop("Unsupported diff format: ", format, call. = FALSE)
  )
}

# Internal: coerce a diff input to character. Numerics and factors are the
# common case in a data.frame column (e.g. iris$Sepal.Length, iris$Species).
as_diff_chr <- function(x, arg) {
  if (is.character(x)) return(x)
  if (is.null(x)) return(character(0))
  if (is.factor(x)) return(as.character(x))
  if (is.list(x)) {
    stop("`", arg, "` must be a character vector or coercible to one, not a list.",
         call. = FALSE)
  }
  as.character(x)
}

# Internal: data.frame-style recycling of two lengths (equal, or one of them 1).
recycle_length <- function(n_old, n_new) {
  if (n_old == n_new) return(n_old)
  if (n_old == 1L) return(n_new)
  if (n_new == 1L) return(n_old)
  stop("`old_str` and `new_str` must be the same length, or one of them ",
       "length 1: got ", n_old, " and ", n_new, ".", call. = FALSE)
}

# Internal: an integer group id per row, equal for identical (old, new) pairs,
# so a diff is computed once per distinct pair rather than once per row. Worth
# it because real data (addresses, names, codes) repeats heavily.
pair_groups <- function(old, new) {
  oi <- data.table::chmatch(old, unique(old))
  ni <- data.table::chmatch(new, unique(new))
  # Doubles, not integers: oi * stride can exceed .Machine$integer.max, and the
  # product stays exact in a double well past any plausible row count.
  key <- as.numeric(oi) * (max(ni, na.rm = TRUE) + 1) + as.numeric(ni)
  match(key, unique(key))
}

# Internal: element-wise diff. `one` is a closure taking two length-1 strings
# and returning a jsdiff_changes object (or NULL, when max_edit_length/timeout
# aborted the diff).
diff_vectorised <- function(one, old, new, format) {
  old <- as_diff_chr(old, "old_str")
  new <- as_diff_chr(new, "new_str")
  n <- recycle_length(length(old), length(new))
  if (n == 0L) {
    return(if (identical(format, "changes")) list() else character(0))
  }
  if (length(old) != n) old <- rep_len(old, n)
  if (length(new) != n) new <- rep_len(new, n)

  # NA in either input gives NA out, rather than diffing the literal "NA".
  ok <- !is.na(old) & !is.na(new)
  g <- pair_groups(old, new)
  firsts <- match(seq_len(max(g)), g)

  if (identical(format, "changes")) {
    res <- vector("list", length(firsts))
    for (k in seq_along(firsts)) {
      i <- firsts[k]
      if (ok[i]) res[[k]] <- one(old[i], new[i])
    }
    return(res[g])
  }

  render <- diff_renderer(format)
  out <- character(length(firsts))
  for (k in seq_along(firsts)) {
    i <- firsts[k]
    if (!ok[i]) {
      out[k] <- NA_character_
    } else {
      ch <- one(old[i], new[i])
      out[k] <- if (is.null(ch)) NA_character_ else render(ch)
    }
  }
  out[g]
}

# Internal: shared entry point for the public string diff functions. Chooses
# between the scalar (jsdiff_changes) and vectorised (one value per element)
# shapes, then renders.
diff_dispatch <- function(one, old, new, format) {
  vectorised <- length(old) != 1L || length(new) != 1L
  if (is.null(format)) format <- if (vectorised) "markup" else "changes"
  format <- match.arg(format, diff_formats)
  if (!vectorised && identical(format, "changes")) {
    old <- as_diff_chr(old, "old_str")
    new <- as_diff_chr(new, "new_str")
    if (is.na(old) || is.na(new)) return(NULL)
    return(one(old, new))
  }
  diff_vectorised(one, old, new, format)
}

#' Diffing a pair of columns
#'
#' @description
#' The string diff functions ([diff_chars()], [diff_words()],
#' [diff_words_with_space()], [diff_lines()], [diff_trimmed_lines()],
#' [diff_sentences()], [diff_css()]) are vectorised: given two vectors of equal
#' length (or one vector and one length-1 value, which is recycled) they diff
#' them element by element and return one value per element, so they can be
#' used directly inside a `data.frame` or `data.table` expression.
#'
#' ```r
#' dt <- data.table::data.table(iris)
#' dt[, .(blah = diff_chars(Sepal.Length, Sepal.Width), Species)]
#' ```
#'
#' Non-character inputs are coerced with [as.character()], so numeric and
#' factor columns work without ceremony. `NA` on either side gives `NA` out.
#'
#' @section Return shape:
#' With two length-1 inputs and no `format`, the diff functions return a
#' [jsdiff_changes] table, as they always have. In every other case they return
#' a vector as long as the (recycled) inputs, with each element rendered
#' according to `format`:
#'
#' \describe{
#'   \item{`"markup"`}{(the vectorised default) inline markup via
#'     [changes_to_markup()], e.g. `the [-cat-]{+dog+} sat`.}
#'   \item{`"ansi"`}{the coloured console form, as [print()] shows it. Best
#'     passed to [cat()]: ANSI escapes do not survive [print()] at all (see
#'     the colour section below), and they confuse column-width calculations
#'     when printed as a `data.table` column.}
#'   \item{`"html"`}{`<span>`-wrapped HTML for one table cell, matching the
#'     classes in [diff_css_default()].}
#'   \item{`"xml"`}{`<ins>`/`<del>` markup, via [convert_changes_to_xml()].}
#'   \item{`"added"`, `"removed"`}{only the inserted (respectively deleted)
#'     text, concatenated - useful for asking *what* changed rather than
#'     displaying it.}
#'   \item{`"changes"`}{no rendering: a list column of per-element
#'     [jsdiff_changes] tables (`NULL` where an input was `NA`).}
#' }
#'
#' Elements whose diff was aborted by `max_edit_length` or `timeout` are
#' `NA_character_` (or `NULL` for `format = "changes"`).
#'
#' @section Colour:
#' The default rendering is plain-text markup rather than colour because
#' colour cannot survive ordinary printing: `print.data.table()` and
#' `print.data.frame()` hand their formatted cells to [print.default()], which
#' escapes the ESC byte of an ANSI sequence, so a coloured cell prints as the
#' literal text `\033[31mcat\033[0m`. To see a table in colour, print it with
#' [diff_print()], which does its own formatting (and measures cells by their
#' visible width, so the columns still line up):
#'
#' ```r
#' diff_print(dt[, .(blah = diff_chars(Sepal.Length, Sepal.Width), Species)])
#' ```
#'
#' [diff_print_auto()] installs that as the console's printing for tables
#' containing diff markup, so the colour appears without wrapping each call,
#' and [markup_to_ansi()] converts a single markup column for use with
#' [cat()].
#'
#' @section Performance:
#' The diff is computed once per *distinct* pair of values, not once per
#' element, so repeated pairs in a long column are nearly free.
#'
#' @seealso [diff_print()] and [diff_print_auto()] for coloured printing,
#'   [changes_to_markup()] for the markers used by `"markup"`, and
#'   [diff_html()] for a side-by-side HTML table of two vectors.
#' @name vectorised-diffs
#' @examples
#' old <- c("the cat sat", "12 Smith St", "hello")
#' new <- c("the dog sat", "12 Smith Street", "hello")
#'
#' diff_words(old, new)
#' diff_print(diff_words(old, new), color = TRUE)
#' diff_words(old, new, format = "added")
#'
#' df <- data.frame(old = old, new = new)
#' df$diff <- diff_words(df$old, df$new)
#' df
#'
#' # one value recycled against a column
#' diff_chars(c("abc", "abd", "xbc"), "abc")
NULL
