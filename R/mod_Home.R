#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Home_ui <- function(id){
  ns <- NS(id)
  tagList(
    # shinydashboard::box(
    #   title = "miRNA tissue atlas",
    #   status = "primary",
    #   solidHeader = TRUE,
    #   collapsible = FALSE,
    #   width = 12,
    #   h1("Welcome to AIMEE!"),
    #   p("AIMEE (Animal IsomiR and MiRNA Expression Explorer) currently hosts miRNA and isomiR expression data across 461 equine small RNA-seq samples, spanning 61 tissue types."),
    #   p("The included samples were sourced from three sets: 221 from 12 American Quarter Horses, 82 from FAANG Thoroughbreds, and 158 from public data respositories."),
    #   p("All samples were processed using FARmiR (REF).")
    # )
    wellPanel(
        h2("Welcome to AIMEE"),
        p("AIMEE (Animal IsomiR and MiRNA Expression Explorer) currently hosts miRNA and isomiR expression data across 461 equine small RNA-seq samples, spanning 61 tissue types."),
        p("Included samples were sourced from"),
        shiny::tags$ul(
          shiny::tags$li("12 American Quarter Horses (n=221)"),
          shiny::tags$li("4 FAANG Thoroughbreds (n=82)"),
          shiny::tags$li("Public data respositories (n=158)")
        ),
        p("All samples were processed using FARmiR (REF).")
    )
  )
}

#' Home Server Functions
#'
#' @noRd
mod_Home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")
