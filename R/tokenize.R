# Tokenizers ported from jsdiff's src/diff/*.ts. Each returns a character vector
# of tokens (before removeEmpty is applied by the core).

# Replicates JavaScript's String.prototype.split(/(...)/) where the pattern is a
# single capturing group equal to the whole match: returns a vector alternating
# the text between matches (possibly "") and the matched separators.
js_split_capture <- function(s, pattern, perl = TRUE) {
  if (!nzchar(s)) return("")
  m <- gregexpr(pattern, s, perl = perl)[[1]]
  if (m[1L] == -1L) return(s)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  ends <- starts + lens - 1L
  nmatch <- length(starts)
  text_starts <- c(1L, ends + 1L)
  text_ends <- c(starts - 1L, nchar(s))
  texts <- substring(s, text_starts, text_ends)  # length nmatch+1
  seps <- substring(s, starts, ends)             # length nmatch
  out <- character(2L * nmatch + 1L)
  out[seq.int(1L, by = 2L, length.out = nmatch + 1L)] <- texts
  out[seq.int(2L, by = 2L, length.out = nmatch)] <- seps
  out
}

# --- Characters ------------------------------------------------------------
tokenize_chars <- function(value) {
  if (!nzchar(value)) return(character(0))
  strsplit(value, "", useBytes = FALSE)[[1]]
}

# --- Words -----------------------------------------------------------------
# Based on https://en.wikipedia.org/wiki/Latin_script_in_Unicode (see word.ts).
extended_word_chars <- paste0(
  "a-zA-Z0-9_\\x{AD}\\x{C0}-\\x{D6}\\x{D8}-\\x{F6}\\x{F8}-\\x{2C6}",
  "\\x{2C8}-\\x{2D7}\\x{2DE}-\\x{2FF}\\x{1E00}-\\x{1EFF}"
)

tokenize_words <- function(value) {
  value <- enc2utf8(value)
  pattern <- paste0("(*UTF)(*UCP)[", extended_word_chars, "]+|\\s+|[^",
                    extended_word_chars, "]")
  parts <- regmatches(value, gregexpr(pattern, value, perl = TRUE))[[1]]
  if (!length(parts)) return(character(0))
  is_ws <- grepl("\\s", parts, perl = TRUE)
  tokens <- character(0)
  prev_part <- NULL
  prev_ws <- FALSE
  for (k in seq_along(parts)) {
    part <- parts[k]
    if (is_ws[k]) {
      if (is.null(prev_part)) {
        tokens <- c(tokens, part)
      } else {
        tokens[length(tokens)] <- paste0(tokens[length(tokens)], part)
      }
    } else if (!is.null(prev_part) && prev_ws) {
      if (length(tokens) && tokens[length(tokens)] == prev_part) {
        tokens[length(tokens)] <- paste0(tokens[length(tokens)], part)
      } else {
        tokens <- c(tokens, paste0(prev_part, part))
      }
    } else {
      tokens <- c(tokens, part)
    }
    prev_part <- part
    prev_ws <- is_ws[k]
  }
  tokens
}

tokenize_words_with_space <- function(value) {
  value <- enc2utf8(value)
  pattern <- paste0("(*UTF)(*UCP)(\\r?\\n)|[", extended_word_chars,
                    "]+|[^\\S\\n\\r]+|[^", extended_word_chars, "]")
  m <- regmatches(value, gregexpr(pattern, value, perl = TRUE))[[1]]
  if (!length(m)) character(0) else m
}

# --- Lines -----------------------------------------------------------------
tokenize_lines <- function(value, newline_is_token = FALSE,
                           strip_trailing_cr = FALSE) {
  if (strip_trailing_cr) {
    value <- gsub("\r\n", "\n", value, fixed = TRUE)
  }
  lan <- js_split_capture(value, "\r\n|\n")
  if (length(lan) && !nzchar(lan[length(lan)])) {
    lan <- lan[-length(lan)]
  }
  if (!length(lan)) return(character(0))
  if (newline_is_token) {
    return(lan)
  }
  n <- length(lan)
  content_idx <- seq.int(1L, n, by = 2L)
  sep_idx <- content_idx + 1L
  seps <- ifelse(sep_idx <= n, lan[sep_idx], "")
  paste0(lan[content_idx], seps)
}

# --- Sentences -------------------------------------------------------------
is_sentence_end_punct <- function(ch) ch == "." || ch == "!" || ch == "?"

tokenize_sentences <- function(value) {
  chars <- tokenize_chars(value)
  len <- length(chars)
  if (len == 0L) return(character(0))
  result <- character(0)
  token_start <- 1L
  i <- 1L
  while (i <= len) {
    if (i == len) {
      result <- c(result, paste0(chars[token_start:len], collapse = ""))
      break
    }
    if (is_sentence_end_punct(chars[i]) && grepl("\\s", chars[i + 1L], perl = TRUE)) {
      result <- c(result, paste0(chars[token_start:i], collapse = ""))
      i <- i + 1L
      token_start <- i
      while (i + 1L <= len && grepl("\\s", chars[i + 1L], perl = TRUE)) {
        i <- i + 1L
      }
      result <- c(result, paste0(chars[token_start:i], collapse = ""))
      token_start <- i + 1L
    }
    i <- i + 1L
  }
  result
}

# --- CSS -------------------------------------------------------------------
tokenize_css <- function(value) {
  js_split_capture(value, "[{}:;,]|\\s+")
}
