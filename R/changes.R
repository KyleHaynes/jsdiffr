# Construction, classing and printing of the "change objects" returned by the
# diff functions. Mirrors jsdiff's array-of-change-objects, but materialised as a
# data.table with columns: value, added, removed, count.

#' Change objects returned by the diff functions
#'
#' All `diff_*()` functions return a `jsdiff_changes` object: a [data.table][data.table::data.table]
#' (subclassed) with one row per change and the columns below. It has a coloured
#' [print][print.jsdiff_changes] method and can be coerced with
#' [as.data.table][as.data.table.jsdiff_changes].
#'
#' \describe{
#'   \item{`value`}{The text (or, for [diff_arrays()], the list of elements) of
#'     the change. For unchanged and added blocks this comes from the new input;
#'     for removed blocks, from the old input.}
#'   \item{`added`}{`TRUE` if the block was inserted in the new input.}
#'   \item{`removed`}{`TRUE` if the block was deleted from the old input.}
#'   \item{`count`}{The number of tokens (characters, words, lines, ...) in the
#'     block.}
#' }
#'
#' @name jsdiff_changes
NULL

new_changes <- function(value, added, removed, count, value_type = "character",
                        mode = "chars") {
  dt <- data.table::data.table(
    value = value,
    added = added,
    removed = removed,
    count = as.integer(count)
  )
  data.table::setattr(dt, "class", c("jsdiff_changes", class(dt)))
  data.table::setattr(dt, "jsdiffr_value_type", value_type)
  data.table::setattr(dt, "jsdiffr_mode", mode)
  dt
}

#' Are these jsdiff change objects?
#' @param x An object.
#' @return A logical scalar.
#' @export
is_changes <- function(x) inherits(x, "jsdiff_changes")

supports_color <- function() {
  opt <- getOption("jsdiffr.color", NULL)
  if (!is.null(opt)) return(isTRUE(opt))
  if (!is.null(getOption("knitr.in.progress"))) return(FALSE)
  if (nzchar(Sys.getenv("NO_COLOR"))) return(FALSE)
  if (identical(Sys.getenv("RSTUDIO"), "1")) return(TRUE)
  isatty(stdout()) && interactive()
}

ansi <- list(
  red       = "\033[31m",
  green     = "\033[32m",
  dim       = "\033[2m",
  reset     = "\033[0m"
)

#' Format jsdiff change objects as a single coloured string
#'
#' Concatenates the change values into one string, wrapping additions and
#' deletions in ANSI colour codes (green and red respectively) when colour is
#' enabled. Works for any string-based diff (characters, words, lines, ...).
#'
#' @param x A `jsdiff_changes` object.
#' @param color Logical; force colour on/off. Defaults to auto-detection.
#' @param ... Unused.
#' @return A length-1 character string.
#' @export
format.jsdiff_changes <- function(x, color = supports_color(), ...) {
  vt <- attr(x, "jsdiffr_value_type")
  if (identical(vt, "list")) {
    # Array diff: no single string representation; defer to data.table.
    return(paste(utils::capture.output(print(data.table::as.data.table(x))),
                 collapse = "\n"))
  }
  val <- x$value
  if (!color) {
    pieces <- ifelse(x$added, val, ifelse(x$removed, val, val))
    return(paste0(pieces, collapse = ""))
  }
  pieces <- ifelse(
    x$added,   paste0(ansi$green, val, ansi$reset),
    ifelse(x$removed, paste0(ansi$red, val, ansi$reset), val)
  )
  paste0(pieces, collapse = "")
}

#' @rdname format.jsdiff_changes
#' @export
print.jsdiff_changes <- function(x, color = supports_color(), ...) {
  vt <- attr(x, "jsdiffr_value_type")
  if (identical(vt, "list")) {
    cat(format(x, color = color), "\n", sep = "")
    return(invisible(x))
  }
  cat(format.jsdiff_changes(x, color = color), "\n", sep = "")
  invisible(x)
}

#' Coerce jsdiff change objects to a plain data.table / data.frame
#' @param x A `jsdiff_changes` object.
#' @param ... Unused.
#' @return A `data.table` with columns `value`, `added`, `removed`, `count`.
#' @exportS3Method data.table::as.data.table
as.data.table.jsdiff_changes <- function(x, ...) {
  dt <- data.table::copy(x)
  data.table::setattr(dt, "class", c("data.table", "data.frame"))
  data.table::setattr(dt, "jsdiffr_value_type", NULL)
  data.table::setattr(dt, "jsdiffr_mode", NULL)
  dt
}
