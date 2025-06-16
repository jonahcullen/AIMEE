#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Define this page as a dashboard page to signal we're using the dashboard page format
    shinydashboardPlus::dashboardPage(

      header = shinydashboardPlus::dashboardHeader(
        title = "AIMEE"
      ),
      # Create our navigation menu that links to each of the tabs we defined
      sidebar = shinydashboardPlus::dashboardSidebar(
        shinydashboard::sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",
          shinydashboard::menuItem(text = "Dashboard", tabName = "dashboard"),
          div(
            style = "padding: 5px; margin-bottom: -10px;",
            # shinyWidgets::pickerInput(
            #   inputId = "exclusive_choice",
            #   label = tags$span(style = "font-size: 12px;", "miRNA-space:"),
            #   choices = c("All", "Exclusive", "Exclusive repeat", "Ambiguous", "Ambiguous repeat", "MirGeneDB-only"),
            #   choicesOpt = list(
            #     subtext = c("(n=6951)", "(n=5498)", "(n=116)", "(n=853)", "(n=484)", "(n=259)")
            #   ),
            #   options = shinyWidgets::pickerOptions(
            #     container = "body",
            #     style = "btn-sm"
            #   ),
            #   width = "100%",
            #   selected = "ALL"
            # )
            shinyWidgets::pickerInput(
              inputId = "exclusive_choice",
              label = tags$span(style = "font-size: 12px;", "miRNA-space:"),
              choices = NULL,
              choicesOpt = list(),
              options = shinyWidgets::pickerOptions(container = "body", style = "btn-sm"),
              width = "100%",
              selected = "All"
            )
          ),
          shinydashboard::menuItem(text = "By Tissue", tabName = "by_tissue", icon = icon("lungs"),
            shinydashboard::menuSubItem("Tissue counts", tabName = "tiss_overview", icon = icon("chart-simple")),
            shinydashboard::menuSubItem("Stepwise read attrition", tabName = "read_loss", icon = icon("arrow-down-wide-short")),
            shinydashboard::menuSubItem("Process explorer", tabName = "proc_explore", icon = icon("square-arrow-up-right")),
            shinydashboard::menuSubItem("Top miRs", tabName = "mir_tops", icon = icon("ranking-star")),
            shinydashboard::menuSubItem("miR overlap", tabName = "mir_overlap", icon = icon("chart-pie")),
            shinydashboard::menuSubItem("Rank aggregation", tabName = "rank_agg", icon = icon("list-ol"))
          ),
          shinydashboard::menuItem(text = "By miRs", tabName = "by_mirs", icon = icon("down-left-and-up-right-to-center"),
            shinydashboard::menuSubItem("miRNAs", tabName = "mirnas", icon = icon("square-poll-vertical")),
            shinydashboard::menuSubItem("Search", tabName = "search", icon = icon("magnifying-glass"))
          ),
          shinydashboard::menuItem(text = "Downloads", tabName = "export", icon = icon("download"))
        )
      ),
      # Show the appropriate tab's content in the main body of our dashboard when we select it
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "dashboard", mod_Home_ui("Home_1")),
          shinydashboard::tabItem(tabName = "export", mod_Export_ui("Export_1")),
          shinydashboard::tabItem(tabName = "tiss_overview", mod_Tissues_ui("Tissues_1")),
          shinydashboard::tabItem(tabName = "read_loss", mod_ReadLoss_ui("ReadLoss_1")),
          shinydashboard::tabItem(tabName = "proc_explore", mod_ProcExplore_ui("ProcExplore_1")),
          shinydashboard::tabItem(tabName = "mir_tops", mod_TopMirs_ui("TopMirs_1")),
          shinydashboard::tabItem(tabName = "mir_overlap", mod_Overlap_ui("Overlap_1")),
          shinydashboard::tabItem(tabName = "rank_agg", mod_RankAgg_ui("RankAgg_1")),
          shinydashboard::tabItem(tabName = "mirnas", mod_ByMiRNA_ui("ByMiRNA_1")),
          shinydashboard::tabItem(tabName = "search", mod_Search_ui("Search_1"))
        )
      ),
      # rightsidebar = NULL,
      title = "AIMEE Dashboard"
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AIMEE"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
