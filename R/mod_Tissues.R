#' Tissues UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Tissues_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
      #   column(
      #     width = 5,
      #     h2("Welcome!")
      #   ),
        column(
          width = 3,
          # selectInput(
          #   inputId = ns("tiss_select"),
          #   label = "Tissues",
          #   choices = c("All", unique(tissues$tissue)),
          #   # selected = "liver",
          #   multiple = TRUE
          # )
          shinyWidgets::pickerInput(
            ns("source_select"),
            label = "Source",
            choices = unique(tissues$source),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              # `multiple-separator` = " | ",
              `selected-text-format`= "count",
              `count-selected-text` = "{0} sources selected"
            ),
            multiple = TRUE,
            selected = unique(tissues$source)
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("system_select"),
            label = "System",
            choices = unique(tissues$system),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              # `multiple-separator` = " | ",
              `selected-text-format`= "count",
              `count-selected-text` = "{0} systems selected"
            ),
            multiple = TRUE,
            selected = NULL
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tiss_select"),
            label = "Tissue",
            choices = unique(tissues$tissue),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} tissues selected"
            ),
            multiple = TRUE,
            selected = NULL
          )
        )
      ),
      fluidRow(
        shinydashboard::box(
          # title = "Plot",
          width = 12,
          plotly::plotlyOutput(ns("tiss_plot"))
          # plotOutput(ns("tiss_plot"), hover = hoverOpts(id = "plot_hover"))
        )
      ),
      fluidRow(
        shinydashboard::box(
          # title = "MTCARS",
          width = 12,
          DT::dataTableOutput(ns("tiss_table"))
        )
      ),
      fluidRow(
        column(width = 3, textOutput(ns('team_text')))
      )
    )
  )
}

#' Tissues Server Functions
#'
#' @noRd
mod_Tissues_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    tissue_cols <- colorRampPalette(
      rev(RColorBrewer::brewer.pal(11, "Spectral"))
    )(length(unique(tissues$tissue)))
    names(tissue_cols) <- unique(levels(tissues$tissue))

    # selected <- reactive({
    #   if (is.null(input$tiss_select)) {df <- tissues
    #   } else df <- tissues[tissues$tissue %in% input$tiss_select, ]
    #   df
    # })

    # selected <- reactive({
    #   if (is.null(input$tiss_select)) {df <- tissues
    #   } else df <- tissues %>% dplyr::filter(tissue %in% input$tiss_select)
    #   df
    # })

    # data <- read.table("se_coldata_final.tsv", header = TRUE, sep = "\t")
    # selected <- reactive({
    #   data <- tissues
    #   data <- data %>%
    #     dplyr::select(system, tissue, sample) %>%
    #     dplyr::filter(tissue %in% input$tiss_select)
    #   data
    # })

    # HIERARCHICAL
    source_selected <- reactive({
      dplyr::filter(tissues, source %in% input$source_select)
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$system)
      shinyWidgets::updatePickerInput(session, inputId = "system_select", choices = choices)
    })

    system_selected <- reactive({
      req(input$system_select)
      dplyr::filter(source_selected(), system %in% input$system_select)
    })

    observeEvent(system_selected(), {
      choices <- unique(system_selected()$tissue)
      shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    })

    selected_all <- reactive({
      system_selected() %>%
        dplyr::filter(tissue %in% input$tiss_select) %>%
        dplyr::select(system, tissue, new_lab, sample, source, breed, sex) # remove post_counts
    })

    output$tiss_plot <- plotly::renderPlotly({
      # shinipsum::random_ggplot(type = "line")
      ggplot2::ggplot(
        selected_all(),
        ggplot2::aes(x = new_lab, fill = tissue)) +
        ggplot2::geom_bar(color = "black") +
        ggplot2::scale_fill_manual(values = tissue_cols) +
        # ggplot2::scale_color_manual(values = "black")
        ggplot2::labs(y = "Tissue count") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
          axis.title.x = ggplot2::element_blank(),
          # axis.ticks.x = element_blank(),
          # axis.line.x.bottom = element_blank(),
          # axis.title.x = element_blank(),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(size = 12),
          # panel.spacing.x = unit(0.3, "mm"),
          # panel.grid.major.x = element_blank(),
          # legend.text = element_text(size = 8),
          legend.position = "none",
          # legend.key.size = unit(0.5, "cm"),
          # panel.grid.major.y = element_line(color = "grey90"),
          # plot.margin = margin(0, 0, 0, 0, "pt")
        )
    })


    # WORKING SIMPLE VERSION
    # tiss_selected <- reactive({
    #   # tissues %>%
    #   #   dplyr::select(system, tissue, sample) %>%
    #     # tissues %>% dplyr::filter(tissue %in% input$tiss_select)
    #   dplyr::filter(tissues, tissue %in% input$tiss_select)
    #
    # })

    output$tiss_table <- DT::renderDT({
      # req(input$tiss_select)
      # system_selected() %>% filter(tissue %in% input$tiss_select)
      # source_selected() %>% dplyr::filter(system %in% input$system_select)
      selected_all() %>%
        dplyr::select(-tissue) %>% # drop the raw tissue column
        dplyr::rename(
          tissue = new_lab
        ) %>%
        dplyr::select(source, system, tissue, sample, breed, sex)

        # dplyr::filter(tissue %in% input$tiss_select) %>%
        # dplyr::select(system, tissue, sample, source, breed, sex, post_counts)
    })

    # output$team_text = renderText(length(unique(tissues$tissue)))
    output$team_text = renderText(input$tiss_select)

  })
}

## To be copied in the UI
# mod_Tissues_ui("Tissues_1")

## To be copied in the server
# mod_Tissues_server("Tissues_1")
