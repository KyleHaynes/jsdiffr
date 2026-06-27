# Interactive address-diff dashboard ---------------------------------------
#
# A small Shiny app demonstrating jsdiffr::diff_to_html(). Edit either column
# of addresses (one address per line, aligned by line number) and the diff
# updates live.
#
# Run with:
#   shiny::runApp(system.file("shiny/address_diff", package = "jsdiffr"))
# or, during development:
#   pkgload::load_all(); shiny::runApp("inst/shiny/address_diff")

library(shiny)

# Works whether the package is installed or loaded via pkgload::load_all().
if (!requireNamespace("jsdiffr", quietly = TRUE)) {
  stop("Install/load the jsdiffr package before running this app.")
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# Sample address pairs (original vs. cleaned/standardised) ------------------
sample_left <- paste(
  "12 Baker Street, London, NW1 6XE",
  "1600 Pennsylvania Ave NW, Washington, DC 20500",
  "350 Fifth Avenue, New York, NY 10118",
  "Flat 4, 22 Acacia Avenue, Manchester M14 5TP",
  "742 Evergreen Terrace, Springfield",
  sep = "\n"
)
sample_right <- paste(
  "12 Baker St, London, NW1 6XE",
  "1600 Pennsylvania Avenue NW, Washington, DC 20500",
  "350 5th Avenue, New York, NY 10118",
  "Flat 4B, 22 Acacia Ave, Manchester M14 5TP",
  "742 Evergreen Terrace, Springfield, OR",
  sep = "\n"
)

methods <- c(
  "Characters" = "diff_chars",
  "Words"      = "diff_words",
  "Sentences"  = "diff_sentences"
)

# UI -----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML(jsdiffr::diff_css_default())),
    tags$style(HTML("
      body { font-family: ui-sans-serif, system-ui, sans-serif; margin: 16px 24px; }
      h2 { margin-top: 0; }
      textarea { font-family: ui-monospace, SFMono-Regular, Menlo, monospace !important;
                 font-size: 13px; white-space: pre; }
      .jsdiff-table { width: 100%; border-collapse: collapse; font-size: 13px; }
      .jsdiff-table th, .jsdiff-table td {
        padding: 6px 10px; border: 1px solid #d0d7de; vertical-align: top;
        white-space: pre-wrap; font-family: ui-monospace, monospace; }
      .jsdiff-table th { background: #f6f8fa; font-weight: 600; text-align: left; }
      .jsdiff-table td:nth-child(1) { width: 3ch; color: #8b949e; text-align: right; background:#fafbfc; }
      .jsdiff-table td:nth-child(2) { background: #fff8f8; }
      .jsdiff-table td:nth-child(3) { background: #f8fff8; }
      .jsdiff-table td:nth-child(4) { background: #fcfcfd; }
      .controls { margin-bottom: 12px; }
    "))
  ),
  titlePanel("Live address diff"),
  p("Edit either column (one address per line). Rows are paired by line number; ",
    "the ", strong("Changes"), " column shows a live ",
    code("jsdiffr::diff_to_html()"), " diff."),

  fluidRow(
    column(6, textAreaInput("left", "Original", value = sample_left,
                            rows = 8, width = "100%", resize = "vertical")),
    column(6, textAreaInput("right", "Updated", value = sample_right,
                            rows = 8, width = "100%", resize = "vertical"))
  ),

  div(class = "controls",
    fluidRow(
      column(4, radioButtons("method", "Diff granularity",
                             choices = methods, selected = "diff_words", inline = TRUE)),
      column(4, textInput("ignore", "Ignore words (comma separated)",
                          value = "", placeholder = "e.g. street, st, avenue, ave")),
      column(4, checkboxInput("show_inline", "Show combined inline column", value = TRUE))
    )
  ),

  uiOutput("diff_table")
)

# Server -------------------------------------------------------------------
server <- function(input, output, session) {

  split_lines <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) return(character(0))
    strsplit(txt, "\n", fixed = TRUE)[[1]]
  }

  ignore_words <- reactive({
    raw <- trimws(strsplit(input$ignore %||% "", ",", fixed = TRUE)[[1]])
    raw[nzchar(raw)]
  })

  # Is a change made up entirely of ignored words?
  all_ignored <- function(text, ignore_lc) {
    if (!length(ignore_lc)) return(FALSE)
    words <- unlist(regmatches(text, gregexpr("\\S+", text, perl = TRUE)))
    length(words) > 0L && all(tolower(words) %in% ignore_lc)
  }

  # Render one side, suppressing the opposite side's edits and any
  # ignored-word changes (mirrors the logic used by jsdiffr::diff_html()).
  side_html <- function(changes, side, ignore_lc) {
    keep_added   <- side == "right"
    pieces <- vapply(seq_len(nrow(changes)), function(i) {
      v  <- htmltools::htmlEscape(changes$value[i])
      ig <- all_ignored(changes$value[i], ignore_lc)
      if (changes$added[i]) {
        if (!keep_added) return("")
        if (ig) sprintf('<span class="jsdiff-context">%s</span>', v)
        else    sprintf('<span class="jsdiff-added">%s</span>', v)
      } else if (changes$removed[i]) {
        if (keep_added) return("")
        if (ig) sprintf('<span class="jsdiff-context">%s</span>', v)
        else    sprintf('<span class="jsdiff-removed">%s</span>', v)
      } else {
        sprintf('<span class="jsdiff-context">%s</span>', v)
      }
    }, character(1))
    paste0(pieces, collapse = "")
  }

  output$diff_table <- renderUI({
    left  <- split_lines(input$left)
    right <- split_lines(input$right)
    n <- max(length(left), length(right))
    if (n == 0L) return(em("Enter some addresses above."))
    length(left)  <- n
    length(right) <- n
    left[is.na(left)]   <- ""
    right[is.na(right)] <- ""

    method_fn <- get(input$method, envir = asNamespace("jsdiffr"))
    ig_lc <- tolower(ignore_words())

    rows <- vapply(seq_len(n), function(i) {
      ch <- method_fn(left[i], right[i])
      # diff_to_html() gives the combined inline view (the public API entry point).
      inline <- jsdiffr::diff_to_html(ch, wrap = TRUE, pre = FALSE)
      lcell <- side_html(ch, "left",  ig_lc)
      rcell <- side_html(ch, "right", ig_lc)
      inline_cell <- if (isTRUE(input$show_inline))
        sprintf("<td>%s</td>", inline) else ""
      sprintf("<tr><td>%d</td><td>%s</td><td>%s</td>%s</tr>",
              i, lcell, rcell, inline_cell)
    }, character(1))

    head_inline <- if (isTRUE(input$show_inline)) "<th>Changes</th>" else ""
    html <- sprintf(
      '<table class="jsdiff-table"><thead><tr><th>#</th><th>Original</th><th>Updated</th>%s</tr></thead><tbody>%s</tbody></table>',
      head_inline, paste0(rows, collapse = "")
    )
    HTML(html)
  })
}

shinyApp(ui, server)
