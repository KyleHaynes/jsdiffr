# jsdiffr

A faithful R port of the [jsdiff](https://github.com/kpdecker/jsdiff)
JavaScript library. Compute differences between two pieces of text or two
sequences at the level of characters, words, lines, sentences, CSS tokens, JSON
values, or arbitrary arrays, using Myers' O(ND) difference algorithm. The core
diff loop is implemented in C++ (via Rcpp) for speed on large inputs.

The output of every diff function matches jsdiff 9.0.0 exactly: the test suite
checks > 4,000 randomly generated inputs against the real library.

## Installation

Requires a C++ compiler (Rtools on Windows).

```r
# install.packages("remotes")
remotes::install_local("jsdiffr")
```

## Quick start

```r
library(jsdiffr)

diff_chars("the quick brown fox", "the quiet brown ox")
#>             value added removed count
#> 1:      the qui  FALSE   FALSE     7
#> 2:           ck FALSE    TRUE     2
#> 3:           et  TRUE   FALSE     2
#> 4:    k brown    FALSE   FALSE     8
#> ...

diff_words("The quick brown fox", "The slow brown fox")
diff_lines("alpha\nbeta\n", "alpha\nBETA\n")
```

Each function returns a `data.table` of change objects with columns `value`,
`added`, `removed`, and `count`. Printing a diff colours additions green and
deletions red.

## Features

* **Diffing**: `diff_chars`, `diff_words`, `diff_words_with_space`,
  `diff_lines`, `diff_trimmed_lines`, `diff_sentences`, `diff_css`, `diff_json`,
  `diff_arrays`.
* **Patches**: `structured_patch`, `create_patch`, `create_two_files_patch`,
  `format_patch`, `parse_patch`, `apply_patch`, `apply_patches`, `reverse_patch`,
  plus Unix/Windows line-ending helpers.
* **Converters**: `convert_changes_to_xml`, `convert_changes_to_dmp`.
* **Display**: ANSI-coloured console printing and `diff_to_html()` for Shiny /
  R Markdown (with `diff_css_default()`).
* `camelCase` aliases (`diffChars`, `createPatch`, ...) for users porting JS.

## Performance

A line diff of two 50,000-line files runs in about 0.25s; creating and applying
the corresponding patch takes under a second combined.

## Credits

Ported from [jsdiff](https://github.com/kpdecker/jsdiff) by Kevin Decker,
which is distributed under the BSD 3-Clause license.
