# Whitespace de-duplication for word diffs, ported from the postProcess /
# dedupeWhitespaceInChangeObjects logic in jsdiff's src/diff/word.ts. Operates on
# a jsdiff_changes data.table, mutating the `value` column so that trailing
# whitespace in one change object is not duplicated as leading whitespace in the
# next.

post_process_words <- function(changes, options = list()) {
  values <- changes$value
  added <- changes$added
  removed <- changes$removed
  n <- length(values)

  last_keep <- NA_integer_
  insertion <- NA_integer_
  deletion <- NA_integer_

  for (j in seq_len(n)) {
    if (added[j]) {
      insertion <- j
    } else if (removed[j]) {
      deletion <- j
    } else {
      if (!is.na(insertion) || !is.na(deletion)) {
        values <- dedupe_ws(values, last_keep, deletion, insertion, j)
      }
      last_keep <- j
      insertion <- NA_integer_
      deletion <- NA_integer_
    }
  }
  if (!is.na(insertion) || !is.na(deletion)) {
    values <- dedupe_ws(values, last_keep, deletion, insertion, NA_integer_)
  }

  changes$value <- values
  changes
}

dedupe_ws <- function(values, start_keep, deletion, insertion, end_keep) {
  has_del <- !is.na(deletion)
  has_ins <- !is.na(insertion)
  has_sk <- !is.na(start_keep)
  has_ek <- !is.na(end_keep)

  if (has_del && has_ins) {
    lt_old <- leading_and_trailing_ws(values[deletion])
    lt_new <- leading_and_trailing_ws(values[insertion])
    old_ws_prefix <- lt_old[1L]; old_ws_suffix <- lt_old[2L]
    new_ws_prefix <- lt_new[1L]; new_ws_suffix <- lt_new[2L]
    if (has_sk) {
      common_ws_prefix <- longest_common_prefix(old_ws_prefix, new_ws_prefix)
      values[start_keep] <- replace_suffix(values[start_keep], new_ws_prefix, common_ws_prefix)
      values[deletion] <- remove_prefix(values[deletion], common_ws_prefix)
      values[insertion] <- remove_prefix(values[insertion], common_ws_prefix)
    }
    if (has_ek) {
      common_ws_suffix <- longest_common_suffix(old_ws_suffix, new_ws_suffix)
      values[end_keep] <- replace_prefix(values[end_keep], new_ws_suffix, common_ws_suffix)
      values[deletion] <- remove_suffix(values[deletion], common_ws_suffix)
      values[insertion] <- remove_suffix(values[insertion], common_ws_suffix)
    }
  } else if (has_ins) {
    if (has_sk) {
      ws <- leading_ws(values[insertion])
      values[insertion] <- substring(values[insertion], nchar(ws) + 1L)
    }
    if (has_ek) {
      ws <- leading_ws(values[end_keep])
      values[end_keep] <- substring(values[end_keep], nchar(ws) + 1L)
    }
  } else if (has_sk && has_ek) {
    new_ws_full <- leading_ws(values[end_keep])
    lt <- leading_and_trailing_ws(values[deletion])
    del_ws_start <- lt[1L]; del_ws_end <- lt[2L]
    new_ws_start <- longest_common_prefix(new_ws_full, del_ws_start)
    values[deletion] <- remove_prefix(values[deletion], new_ws_start)
    new_ws_end <- longest_common_suffix(remove_prefix(new_ws_full, new_ws_start), del_ws_end)
    values[deletion] <- remove_suffix(values[deletion], new_ws_end)
    values[end_keep] <- replace_prefix(values[end_keep], new_ws_full, new_ws_end)
    keep_prefix <- substr(new_ws_full, 1L, nchar(new_ws_full) - nchar(new_ws_end))
    values[start_keep] <- replace_suffix(values[start_keep], new_ws_full, keep_prefix)
  } else if (has_ek) {
    end_keep_ws_prefix <- leading_ws(values[end_keep])
    deletion_ws_suffix <- trailing_ws(values[deletion])
    overlap <- maximum_overlap(deletion_ws_suffix, end_keep_ws_prefix)
    values[deletion] <- remove_suffix(values[deletion], overlap)
  } else if (has_sk) {
    start_keep_ws_suffix <- trailing_ws(values[start_keep])
    deletion_ws_prefix <- leading_ws(values[deletion])
    overlap <- maximum_overlap(start_keep_ws_suffix, deletion_ws_prefix)
    values[deletion] <- remove_prefix(values[deletion], overlap)
  }
  values
}
