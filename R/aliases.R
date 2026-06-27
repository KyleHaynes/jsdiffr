# camelCase aliases matching the original jsdiff API names, for users porting
# JavaScript code. These forward to the snake_case functions, which are the
# primary, documented API.

#' jsdiff-style camelCase aliases
#'
#' These thin wrappers expose the original jsdiff names. See the snake_case
#' functions they forward to for documentation and arguments.
#'
#' @param ... Passed through to the underlying snake_case function.
#' @name jsdiff-aliases
#' @return The same value as the underlying function.
#' @export
diffChars <- function(...) diff_chars(...)

#' @rdname jsdiff-aliases
#' @export
diffWords <- function(...) diff_words(...)

#' @rdname jsdiff-aliases
#' @export
diffWordsWithSpace <- function(...) diff_words_with_space(...)

#' @rdname jsdiff-aliases
#' @export
diffLines <- function(...) diff_lines(...)

#' @rdname jsdiff-aliases
#' @export
diffTrimmedLines <- function(...) diff_trimmed_lines(...)

#' @rdname jsdiff-aliases
#' @export
diffSentences <- function(...) diff_sentences(...)

#' @rdname jsdiff-aliases
#' @export
diffCss <- function(...) diff_css(...)

#' @rdname jsdiff-aliases
#' @export
diffJson <- function(...) diff_json(...)

#' @rdname jsdiff-aliases
#' @export
diffArrays <- function(...) diff_arrays(...)

#' @rdname jsdiff-aliases
#' @export
structuredPatch <- function(...) structured_patch(...)

#' @rdname jsdiff-aliases
#' @export
formatPatch <- function(...) format_patch(...)

#' @rdname jsdiff-aliases
#' @export
createTwoFilesPatch <- function(...) create_two_files_patch(...)

#' @rdname jsdiff-aliases
#' @export
createPatch <- function(...) create_patch(...)

#' @rdname jsdiff-aliases
#' @export
parsePatch <- function(...) parse_patch(...)

#' @rdname jsdiff-aliases
#' @export
applyPatch <- function(...) apply_patch(...)

#' @rdname jsdiff-aliases
#' @export
applyPatches <- function(...) apply_patches(...)

#' @rdname jsdiff-aliases
#' @export
reversePatch <- function(...) reverse_patch(...)

#' @rdname jsdiff-aliases
#' @export
convertChangesToXML <- function(...) convert_changes_to_xml(...)

#' @rdname jsdiff-aliases
#' @export
convertChangesToDMP <- function(...) convert_changes_to_dmp(...)
