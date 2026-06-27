# Helpers for comparing against bundled jsdiff golden fixtures.

fn_map <- list(
  diffChars = diff_chars, diffWords = diff_words,
  diffWordsWithSpace = diff_words_with_space, diffLines = diff_lines,
  diffTrimmedLines = diff_trimmed_lines, diffSentences = diff_sentences,
  diffCss = diff_css, diffArrays = diff_arrays
)

camel_to_snake <- function(x) tolower(gsub("([A-Z])", "_\\1", x))

run_golden_case <- function(cs) {
  fn <- fn_map[[cs$fn]]
  args <- cs$args
  if (cs$fn == "diffArrays") args <- list(unlist(args[[1]]), unlist(args[[2]]))
  opts <- cs$options
  if (length(opts)) names(opts) <- camel_to_snake(names(opts))
  do.call(fn, c(args, opts))
}

norm_value <- function(v) if (is.list(v)) as.character(unlist(v)) else as.character(v)

# Returns NULL when `got` matches the expected change list, else a message.
diff_mismatch <- function(got, expected) {
  if (is.null(got)) return("got NULL")
  if (nrow(got) != length(expected)) {
    return(sprintf("nrow %d != %d", nrow(got), length(expected)))
  }
  for (i in seq_len(nrow(got))) {
    e <- expected[[i]]
    if (!identical(norm_value(got$value[[i]]), norm_value(e$value))) {
      return(sprintf("row %d value mismatch", i))
    }
    if (!identical(isTRUE(got$added[i]), isTRUE(e$added)) ||
        !identical(isTRUE(got$removed[i]), isTRUE(e$removed))) {
      return(sprintf("row %d flags mismatch", i))
    }
    if (!identical(as.integer(got$count[i]), as.integer(e$count))) {
      return(sprintf("row %d count mismatch", i))
    }
  }
  NULL
}
