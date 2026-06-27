# The shared diff engine. Each public diff_* function tokenizes its inputs and
# computes a comparison "key" per token (reducing custom equality semantics to
# plain equality), then hands off to run_diff_core(), which integer-encodes the
# keys, calls the C++ Myers core, reconstructs the change values, and assembles
# the result.

default_join <- function(tokens) paste0(tokens, collapse = "")

# old_codes/new_codes : integer token codes (equal codes == equal tokens)
# old_tokens/new_tokens: the original tokens, used to reconstruct values
# join          : function(token_vector) -> value (string, or list element)
# use_longest   : for common runs, pick the longer of the old/new token (JSON)
# value_type    : "character" or "list" (arrays)
run_diff_core <- function(old_codes, new_codes, old_tokens, new_tokens,
                          join = default_join, use_longest = FALSE,
                          value_type = "character", mode = "chars",
                          max_edit_length = 0, timeout = 0,
                          one_change_per_token = FALSE,
                          post_process = NULL, options = list()) {
  res <- myers_diff_cpp(as.integer(old_codes), as.integer(new_codes),
                        as.numeric(max_edit_length), as.numeric(timeout))
  if (is.null(res)) return(NULL)

  op <- res$op
  count <- res$count
  if (one_change_per_token && length(op)) {
    op <- rep(op, count)
    count <- rep.int(1L, length(op))
  }

  vals <- build_values(op, count, old_tokens, new_tokens, join,
                       use_longest, value_type)
  changes <- new_changes(vals$value, op == 1L, op == -1L, count,
                         value_type = value_type, mode = mode)

  if (!is.null(post_process) && !one_change_per_token && nrow(changes)) {
    changes <- post_process(changes, options)
  }
  changes
}

# Reconstruct the value of each component by walking the edit script, mirroring
# jsdiff's Diff.prototype.buildValues.
build_values <- function(op, count, old_tokens, new_tokens, join,
                         use_longest, value_type) {
  n <- length(op)
  if (n == 0L) {
    return(list(value = if (value_type == "list") list() else character(0)))
  }
  is_list <- value_type == "list"
  value <- if (is_list) vector("list", n) else character(n)
  old_pos <- 0L
  new_pos <- 0L
  for (i in seq_len(n)) {
    cnt <- count[i]
    o <- op[i]
    if (o == -1L) {
      toks <- old_tokens[(old_pos + 1L):(old_pos + cnt)]
      old_pos <- old_pos + cnt
    } else if (o == 1L) {
      toks <- new_tokens[(new_pos + 1L):(new_pos + cnt)]
      new_pos <- new_pos + cnt
    } else {
      idx_new <- (new_pos + 1L):(new_pos + cnt)
      toks <- new_tokens[idx_new]
      if (use_longest) {
        otoks <- old_tokens[(old_pos + 1L):(old_pos + cnt)]
        longer <- nchar(otoks) > nchar(toks)
        toks[longer] <- otoks[longer]
      }
      new_pos <- new_pos + cnt
      old_pos <- old_pos + cnt
    }
    if (is_list) {
      value[[i]] <- join(toks)
    } else {
      value[i] <- join(toks)
    }
  }
  list(value = value)
}

# Integer-encode character keys over the union of old and new.
encode_keys <- function(old_keys, new_keys) {
  all_keys <- c(old_keys, new_keys)
  u <- unique(all_keys)
  codes <- data.table::chmatch(all_keys, u)
  list(old = codes[seq_along(old_keys)],
       new = codes[length(old_keys) + seq_along(new_keys)])
}

# Drop empty-string tokens (jsdiff's Diff.prototype.removeEmpty), keeping the
# parallel key vector in sync.
remove_empty <- function(tokens, keys) {
  keep <- nzchar(tokens)
  list(tokens = tokens[keep], keys = keys[keep])
}

# Trim leading/trailing (Unicode) whitespace.
str_trim <- function(x) gsub("^[\\s]+|[\\s]+$", "", x, perl = TRUE)
