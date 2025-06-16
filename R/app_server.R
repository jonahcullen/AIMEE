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

  # update miRNA-space picker input subtext dynamically
  observe({
    counts <- list(
      "All" = nrow(mirna_space_ids),
      "Exclusive" = mirna_space_ids %>%
        dplyr::filter(filter_type == "Exclusive") %>%
        dplyr::pull(id) %>% length(),
      "Exclusive repeat" = mirna_space_ids %>%
        dplyr::filter(filter_type == "Exclusive & repeat") %>%
        dplyr::pull(id) %>% length(),
      "Ambiguous" = mirna_space_ids %>%
        dplyr::filter(filter_type == "Ambiguous") %>%
        dplyr::pull(id) %>% length(),
      "Ambiguous repeat" = mirna_space_ids %>%
        dplyr::filter(filter_type == "Ambiguous & repeat") %>%
        dplyr::pull(id) %>% length(),
      "MirGeneDB-only" = mirgenedb_map$id %>% unique() %>% length()
    )

    shinyWidgets::updatePickerInput(
      session,
      inputId = "exclusive_choice",
      choices = names(counts),
      selected = isolate(input$exclusive_choice),
      choicesOpt = list(subtext = paste0("(n=", counts, ")"))
    )
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
  mod_ByMiRNA_server("ByMiRNA_1", mirna_space)
  mod_Search_server("Search_1")
}


