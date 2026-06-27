# String helpers ported from jsdiff's src/util/string.ts. These are used by the
# word-diff post-processing (whitespace de-duplication) and the patch utilities.
# All operate on single character scalars (length-1 character vectors).

# Number of leading characters shared by two strings.
longest_common_prefix <- function(str1, str2) {
  n <- min(nchar(str1), nchar(str2))
  if (n == 0L) return("")
  a <- substring(str1, 1:n, 1:n)
  b <- substring(str2, 1:n, 1:n)
  mism <- which(a != b)
  if (length(mism)) substr(str1, 1L, mism[1L] - 1L) else substr(str1, 1L, n)
}

longest_common_suffix <- function(str1, str2) {
  l1 <- nchar(str1); l2 <- nchar(str2)
  if (l1 == 0L || l2 == 0L) return("")
  n <- min(l1, l2)
  i <- 0L
  while (i < n &&
         substr(str1, l1 - i, l1 - i) == substr(str2, l2 - i, l2 - i)) {
    i <- i + 1L
  }
  if (i == 0L) "" else substr(str1, l1 - i + 1L, l1)
}

replace_prefix <- function(string, old_prefix, new_prefix) {
  if (substr(string, 1L, nchar(old_prefix)) != old_prefix) {
    stop(sprintf("string %s doesn't start with prefix %s; this is a bug",
                 shQuote(string), shQuote(old_prefix)))
  }
  paste0(new_prefix, substring(string, nchar(old_prefix) + 1L))
}

replace_suffix <- function(string, old_suffix, new_suffix) {
  if (!nzchar(old_suffix)) return(paste0(string, new_suffix))
  if (substr(string, nchar(string) - nchar(old_suffix) + 1L, nchar(string)) != old_suffix) {
    stop(sprintf("string %s doesn't end with suffix %s; this is a bug",
                 shQuote(string), shQuote(old_suffix)))
  }
  paste0(substr(string, 1L, nchar(string) - nchar(old_suffix)), new_suffix)
}

remove_prefix <- function(string, old_prefix) replace_prefix(string, old_prefix, "")
remove_suffix <- function(string, old_suffix) replace_suffix(string, old_suffix, "")

# Length of the longest suffix of string1 that is a prefix of string2.
overlap_count <- function(a, b) {
  if (!nzchar(a) || !nzchar(b)) return(0L)
  la <- nchar(a); lb <- nchar(b)
  ac <- substring(a, 1:la, 1:la)
  bc <- substring(b, 1:lb, 1:lb)
  startA <- if (la > lb) la - lb else 0L
  endB <- if (la < lb) la else lb
  if (endB == 0L) return(0L)
  map <- integer(endB)
  k <- 0L
  map[1L] <- 0L
  j <- 1L
  while (j < endB) {
    if (bc[j + 1L] == bc[k + 1L]) {
      map[j + 1L] <- map[k + 1L]
    } else {
      map[j + 1L] <- k
    }
    while (k > 0L && bc[j + 1L] != bc[k + 1L]) k <- map[k + 1L]
    if (bc[j + 1L] == bc[k + 1L]) k <- k + 1L
    j <- j + 1L
  }
  k <- 0L
  i <- startA
  while (i < la) {
    while (k > 0L && ac[i + 1L] != bc[k + 1L]) k <- map[k + 1L]
    if (ac[i + 1L] == bc[k + 1L]) k <- k + 1L
    i <- i + 1L
  }
  k
}

maximum_overlap <- function(string1, string2) {
  substr(string2, 1L, overlap_count(string1, string2))
}

leading_ws <- function(string) {
  m <- regmatches(string, regexpr("^\\s*", string, perl = TRUE))
  if (length(m)) m else ""
}

trailing_ws <- function(string) {
  n <- nchar(string)
  if (n == 0L) return("")
  i <- n
  while (i >= 1L && grepl("\\s", substr(string, i, i), perl = TRUE)) i <- i - 1L
  substring(string, i + 1L)
}

leading_and_trailing_ws <- function(string) {
  c(leading_ws(string), trailing_ws(string))
}

# Line-ending detection (src/util/string.ts).
has_only_win_line_endings <- function(string) {
  grepl("\r\n", string, fixed = TRUE) &&
    !startsWith(string, "\n") &&
    !grepl("[^\r]\n", string, perl = TRUE)
}

has_only_unix_line_endings <- function(string) {
  !grepl("\r\n", string, fixed = TRUE) && grepl("\n", string, fixed = TRUE)
}
