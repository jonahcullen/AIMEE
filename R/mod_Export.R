#' Export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Export_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Sample data",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      # h1("Welcome to AIMEE!"),
      # p("Meta data for all included samples"),
      div(style = "display: flex; align-items: center;",
          span("Meta data for all included samples", style = "margin-right: 10px;"),
          downloadButton(ns("download_sample_data"), "Sample data")
      )
    ),
    shinydashboard::box(
      title = "Canonical expression data",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      # p("Expression data set for identified canonical miRNAs"),
      # downloadButton(ns("download_canonical_data"), "CANONICAL RPMS")
      div(style = "display: flex; align-items: center;",
          span("Expression data set for identified canonical miRNAs", style = "margin-right: 10px;"),
          downloadButton(ns("download_canonical_data"), "RPMs")
      )
    ),
    shinydashboard::box(
      title = "IsomiR expression data",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      div(style = "display: flex; align-items: center;",
          span("Expression data set for identified isomiRs", style = "margin-right: 10px;"),
          downloadButton(ns("download_isomir_data"), "RPMs")
      )
    )
  )
}

#' Export Server Functions
#'
#' @noRd
mod_Export_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # placeholder
    version <- "v0.9"

    output$download_sample_data <- downloadHandler(
      filename = function() {
        paste("aimee_meta.", version, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(tissues %>% dplyr::select(-post_counts), file, row.names = FALSE)
      }
    )

    output$download_canonical_data <- downloadHandler(
      filename = function() {
        paste("aimee_canon_mirnas.", version, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(canons, file, row.names = FALSE)
      }
    )

    output$download_isomir_data <- downloadHandler(
      filename = function() {
        paste("aimee_isomirs.", version, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(uid_rpms, file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_Export_ui("Export_1")

## To be copied in the server
# mod_Export_server("Export_1")
