# Array diff, ported from jsdiff's src/diff/array.ts. Compares two sequences of
# arbitrary R elements with strict equality, or a custom comparator. Unlike the
# string diffs, the `value` column is a list: each entry holds the sub-sequence
# of elements for that change.

#' Diff two sequences (arrays)
#'
#' @param old_arr,new_arr Atomic vectors or lists to compare element-wise.
#' @param comparator Optional function `function(left, right)` returning `TRUE`
#'   when two elements should be considered equal. Must define an equivalence
#'   relation. Defaults to strict equality.
#' @inheritParams diff_chars
#' @return A [jsdiff_changes] object whose `value` column is a list of
#'   sub-sequences.
#' @examples
#' d <- diff_arrays(c("a", "b", "c"), c("a", "c", "d"))
#' d$value
#' @export
diff_arrays <- function(old_arr, new_arr, comparator = NULL,
                        max_edit_length = NULL, timeout = NULL,
                        one_change_per_token = FALSE) {
  old_tokens <- as.list(old_arr)
  new_tokens <- as.list(new_arr)
  n_old <- length(old_tokens)

  if (is.null(comparator) && is.atomic(old_arr) && is.atomic(new_arr)) {
    combined <- c(old_arr, new_arr)
    codes_all <- match(combined, unique(combined))
  } else {
    eq <- comparator %||% function(a, b) identical(a, b)
    codes_all <- identity_codes(c(old_tokens, new_tokens), eq)
  }
  old_codes <- codes_all[seq_len(n_old)]
  new_codes <- codes_all[n_old + seq_along(new_tokens)]

  atomic_in <- is.atomic(old_arr) && is.atomic(new_arr)
  join_arr <- if (atomic_in) {
    function(x) unlist(x, use.names = FALSE)
  } else {
    function(x) x
  }

  run_diff_core(old_codes, new_codes, old_tokens, new_tokens,
                join = join_arr, value_type = "list", mode = "array",
                max_edit_length = max_edit_length %||% 0,
                timeout = timeout %||% 0,
                one_change_per_token = one_change_per_token)
}

# Assign integer class ids to elements using an arbitrary equivalence predicate.
identity_codes <- function(elems, eq) {
  n <- length(elems)
  codes <- integer(n)
  reps <- list()
  for (i in seq_len(n)) {
    found <- 0L
    for (c in seq_along(reps)) {
      if (isTRUE(eq(reps[[c]], elems[[i]]))) {
        found <- c
        break
      }
    }
    if (found == 0L) {
      reps[[length(reps) + 1L]] <- elems[[i]]
      found <- length(reps)
    }
    codes[i] <- found
  }
  codes
}
