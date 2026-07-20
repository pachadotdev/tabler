#' @title Create a Reusable Controls Card and Histogram Output (UI)
#' @noRd
histogram_section <- function(title, subtitle, col_input_id, col_choices, col_selected,
                               bins_input_id, plot_output_id, download_output_id) {
  list(
    header(title = title, subtitle = subtitle),
    div(
      class = "page-body",
      div(
        class = "container-xl",
        row(
          col4(
            card(
              title = "Controls",
              selectInput(col_input_id, "Column", choices = col_choices, selected = col_selected),
              sliderInput(bins_input_id, "Number of bins:", min = 1, max = 10, value = 5),
              downloadButton(download_output_id, label = "Download CSV")
            )
          ),
          col8(
            card(
              title  = "Output",
              footer = "Histogram",
              plotOutput(plot_output_id)
            )
          )
        )
      )
    )
  )
}
