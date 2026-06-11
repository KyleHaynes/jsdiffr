# Public text diff functions: diff_chars, diff_words, diff_words_with_space,
# diff_lines, diff_trimmed_lines, diff_sentences, diff_css.
#
# Each returns a `jsdiff_changes` object: a data.table with columns
# `value` (character), `added` (logical), `removed` (logical), `count` (integer).

# Helper: run a string diff given tokens + a key function.
run_string_diff <- function(old_tokens, new_tokens, key_fun,
                            join = default_join, use_longest = FALSE,
                            mode = "chars", max_edit_length = NULL,
                            timeout = NULL, one_change_per_token = FALSE,
                            post_process = NULL, options = list()) {
  oe <- remove_empty(old_tokens, key_fun(old_tokens))
  ne <- remove_empty(new_tokens, key_fun(new_tokens))
  codes <- encode_keys(oe$keys, ne$keys)
  run_diff_core(codes$old, codes$new, oe$tokens, ne$tokens,
                join = join, use_longest = use_longest,
                value_type = "character", mode = mode,
                max_edit_length = max_edit_length %||% 0,
                timeout = timeout %||% 0,
                one_change_per_token = one_change_per_token,
                post_process = post_process, options = options)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Diff two strings character by character
#'
#' @param old_str,new_str Length-1 character strings to compare.
#' @param ignore_case If `TRUE`, characters differing only in case are treated
#'   as equal.
#' @param max_edit_length If set, abort and return `NULL` once the edit distance
#'   exceeds this value.
#' @param timeout If set (milliseconds), abort and return `NULL` if the diff
#'   takes longer than this.
#' @param one_change_per_token If `TRUE`, emit one change object per token
#'   rather than coalescing runs of like changes.
#' @return A [jsdiff_changes] object, or `NULL` if `max_edit_length`/`timeout`
#'   was exceeded.
#' @examples
#' diff_chars("abc", "abd")
#' @export
diff_chars <- function(old_str, new_str, ignore_case = FALSE,
                       max_edit_length = NULL, timeout = NULL,
                       one_change_per_token = FALSE) {
  key_fun <- if (ignore_case) function(t) tolower(t) else function(t) t
  run_string_diff(tokenize_chars(old_str), tokenize_chars(new_str), key_fun,
                  mode = "chars", max_edit_length = max_edit_length,
                  timeout = timeout, one_change_per_token = one_change_per_token)
}

#' Diff two strings word by word
#'
#' Whitespace is preserved in the output but ignored when deciding whether two
#' words match (use [diff_words_with_space()] to treat whitespace as
#' significant).
#'
#' @inheritParams diff_chars
#' @param ignore_whitespace Backwards-compatibility shim from jsdiff: if
#'   explicitly `FALSE`, behaves like [diff_words_with_space()].
#' @return A [jsdiff_changes] object.
#' @examples
#' diff_words("The quick brown fox", "The slow brown fox")
#' @export
diff_words <- function(old_str, new_str, ignore_case = FALSE,
                       ignore_whitespace = NULL, max_edit_length = NULL,
                       timeout = NULL, one_change_per_token = FALSE) {
  if (!is.null(ignore_whitespace) && !isTRUE(ignore_whitespace)) {
    return(diff_words_with_space(old_str, new_str, ignore_case = ignore_case,
                                 max_edit_length = max_edit_length,
                                 timeout = timeout,
                                 one_change_per_token = one_change_per_token))
  }
  key_fun <- if (ignore_case) {
    function(t) str_trim(tolower(t))
  } else {
    function(t) str_trim(t)
  }
  run_string_diff(tokenize_words(old_str), tokenize_words(new_str), key_fun,
                  join = word_join, mode = "words",
                  max_edit_length = max_edit_length, timeout = timeout,
                  one_change_per_token = one_change_per_token,
                  post_process = post_process_words)
}

word_join <- function(tokens) {
  if (!length(tokens)) return("")
  if (length(tokens) > 1L) {
    tokens[-1L] <- sub("^\\s+", "", tokens[-1L], perl = TRUE)
  }
  paste0(tokens, collapse = "")
}

#' Diff two strings word by word, treating whitespace as significant
#' @inheritParams diff_chars
#' @return A [jsdiff_changes] object.
#' @export
diff_words_with_space <- function(old_str, new_str, ignore_case = FALSE,
                                  max_edit_length = NULL, timeout = NULL,
                                  one_change_per_token = FALSE) {
  key_fun <- if (ignore_case) function(t) tolower(t) else function(t) t
  run_string_diff(tokenize_words_with_space(old_str),
                  tokenize_words_with_space(new_str), key_fun,
                  mode = "words", max_edit_length = max_edit_length,
                  timeout = timeout, one_change_per_token = one_change_per_token)
}

#' Diff two strings line by line
#'
#' @inheritParams diff_chars
#' @param ignore_whitespace If `TRUE`, leading/trailing whitespace is ignored
#'   when comparing lines.
#' @param ignore_newline_at_eof If `TRUE`, a missing trailing newline at the end
#'   of the file does not by itself cause the last line to differ.
#' @param newline_is_token If `TRUE`, newlines are emitted as their own tokens
#'   instead of being attached to the preceding line.
#' @param strip_trailing_cr If `TRUE`, `\r\n` is normalised to `\n` before
#'   diffing.
#' @return A [jsdiff_changes] object.
#' @examples
#' diff_lines("line one\nline two\n", "line one\nline 2\n")
#' @export
diff_lines <- function(old_str, new_str, ignore_whitespace = FALSE,
                       ignore_newline_at_eof = FALSE, newline_is_token = FALSE,
                       strip_trailing_cr = FALSE, ignore_case = FALSE,
                       max_edit_length = NULL, timeout = NULL,
                       one_change_per_token = FALSE) {
  tok <- function(s) tokenize_lines(s, newline_is_token = newline_is_token,
                                    strip_trailing_cr = strip_trailing_cr)
  key_fun <- function(t) {
    k <- t
    if (ignore_whitespace) {
      cond <- if (newline_is_token) !grepl("\n", k, fixed = TRUE) else rep(TRUE, length(k))
      k[cond] <- str_trim(k[cond])
    } else if (ignore_newline_at_eof && !newline_is_token) {
      ends <- endsWith(k, "\n")
      k[ends] <- substr(k[ends], 1L, nchar(k[ends]) - 1L)
    }
    if (ignore_case) k <- tolower(k)
    k
  }
  run_string_diff(tok(old_str), tok(new_str), key_fun, mode = "lines",
                  max_edit_length = max_edit_length, timeout = timeout,
                  one_change_per_token = one_change_per_token)
}

#' Diff two strings line by line, ignoring leading/trailing whitespace
#' @inheritParams diff_lines
#' @return A [jsdiff_changes] object.
#' @export
diff_trimmed_lines <- function(old_str, new_str, ignore_case = FALSE,
                               newline_is_token = FALSE,
                               strip_trailing_cr = FALSE,
                               max_edit_length = NULL, timeout = NULL,
                               one_change_per_token = FALSE) {
  diff_lines(old_str, new_str, ignore_whitespace = TRUE,
             newline_is_token = newline_is_token,
             strip_trailing_cr = strip_trailing_cr, ignore_case = ignore_case,
             max_edit_length = max_edit_length, timeout = timeout,
             one_change_per_token = one_change_per_token)
}

#' Diff two strings sentence by sentence
#' @inheritParams diff_chars
#' @return A [jsdiff_changes] object.
#' @export
diff_sentences <- function(old_str, new_str, ignore_case = FALSE,
                           max_edit_length = NULL, timeout = NULL,
                           one_change_per_token = FALSE) {
  key_fun <- if (ignore_case) function(t) tolower(t) else function(t) t
  run_string_diff(tokenize_sentences(old_str), tokenize_sentences(new_str),
                  key_fun, mode = "sentences",
                  max_edit_length = max_edit_length, timeout = timeout,
                  one_change_per_token = one_change_per_token)
}

#' Diff two CSS strings
#' @inheritParams diff_chars
#' @return A [jsdiff_changes] object.
#' @export
diff_css <- function(old_str, new_str, ignore_case = FALSE,
                     max_edit_length = NULL, timeout = NULL,
                     one_change_per_token = FALSE) {
  key_fun <- if (ignore_case) function(t) tolower(t) else function(t) t
  run_string_diff(tokenize_css(old_str), tokenize_css(new_str), key_fun,
                  mode = "css", max_edit_length = max_edit_length,
                  timeout = timeout, one_change_per_token = one_change_per_token)
}
