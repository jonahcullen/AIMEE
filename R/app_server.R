#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # source utility functions
 #source(system.file("utils.R", package = "AIMEE"), local = TRUE)
  # load all data files
  load_aimee_data("AIMEE")

  mirna_space <- reactive({
    input$exclusive_choice
  })

  # application server logic
  mod_Home_server("Home_1")
  mod_Export_server("Export_1")
  mod_Tissues_server("Tissues_1")
  mod_ReadLoss_server("ReadLoss_1")
  mod_ProcExplore_server("ProcExplore_1")
  mod_TopMirs_server("TopMirs_1", mirna_space)
  mod_Overlap_server("Overlap_1", mirna_space)
  mod_RankAgg_server("RankAgg_1", mirna_space)
  mod_ByMiRNA_server("ByMiRNA_1")
  mod_Search_server("Search_1")
}
