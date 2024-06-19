#' ByMiRNA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ByMiRNA_ui <- function(id){
  ns <- NS(id)
  tagList(
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/topmirs.css"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("source_select"),
            label = "Source",
            choices = unique(canons$source),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              # `multiple-separator` = " | ",
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} sources selected"
            ),
            multiple = TRUE,
            selected = unique(canons$source)
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("mirna_select"),
            label = "Canonical miRNA",
            choices = sort(unique(canons$mir_names)),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE
              # `selected-text-format`= "count",
              # `count-selected-text` = "{0} tissues selected"
            ),
            multiple = FALSE,
            # selected = NULL
          )
        ),
        # column(
        #   width = 3,
        #   shinyWidgets::checkboxGroupButtons(
        #     ns("type_select"),
        #     label = "Type",
        #     choices = unique(uid_rpms$type),
        #     direction = "horizontal",
        #     # inline = TRUE,
        #     justified = TRUE,
        #     selected = "Canon",
        #     checkIcon = list(
        #       yes = icon("circle-check", class = "fa-regular"),
        #       no = icon("circle", class = "fa-regular")
        #     )
        #   )
        # ),
        # column(
        #   width = 3,
        #   shinyWidgets::pickerInput(
        #     ns("top_n"),
        #     label = "Top N",
        #     choices = c(5, 10, 20, 50, 100),
        #     multiple = FALSE,
        #     selected = 5
        #     # status = "danger"
        #   )
        # )
      ),
      fluidRow(
        shinydashboard::box(
          # title = "Plot",
          width = 12,
          plotly::plotlyOutput(ns("mirna_plot"))
          # plotOutput(ns("tiss_plot"), hover = hoverOpts(id = "plot_hover"))
        )
      ),
      # fluidRow(
      #   column(
      #     width = 5,
      #     shinyWidgets::awesomeRadio(
      #       ns("sum_type"),
      #       label = "Summary table",
      #       choices = c("Samples" = "samples", "Tissues" = "tissues"),
      #       inline = TRUE,
      #       width = "800px",
      #       selected = "tissues",
      #     )
      #     # tags$head(tags$style(HTML("
      #     #   .checkbox-inline, .radio-inline {
      #     #     margin-right: 20px;
      #     #   }
      #     # ")))
      #   )
      # ),
      fluidRow(
        shinydashboard::box(
          # title = "MTCARS",
          width = 12,
          DT::dataTableOutput(ns("mirna_table"))
        )
      ),
      fluidRow(
        column(width = 3, textOutput(ns('team_text'))),
        # br(),
        # column(width = 3, textOutput(ns('team_text2')))
      )
    )
  )
}

#' ByMiRNA Server Functions
#'
#' @noRd
mod_ByMiRNA_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mirna_cols <- colorRampPalette(
      RColorBrewer::brewer.pal(6, "Dark2")
    )(length(unique(canons$mir_names)))
    names(mirna_cols) <- unique(levels(canons$mir_names))

    # HIERARCHICAL
    source_selected <- reactive({
      dplyr::filter(canons, source %in% input$source_select)
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$mir_names)
      shinyWidgets::updatePickerInput(session, inputId = "mirna_select", choices = choices)
    })

    mirna_selected <- reactive({
      # req(input$tiss_select)
      dplyr::filter(source_selected(), mir_names %in% input$mirna_select)
    })

    # observeEvent(tissue_selected(), {
    #   choices <- unique(tissue_selected()$type)
    #   shinyWidgets::updatePickerInput(session, inputId = "type_select", choices = choices)
    # })

    mirna_summary <- reactive({
      req(input$mirna_select)

      mirna_selected() %>%
        # dplyr::filter(source %in% input$source_select) %>%
        dplyr::group_by(tissue, new_lab, mir_names, Read) %>%
        dplyr::filter(any(rpm != 0)) %>%
        dplyr::summarise(
          mean = round(mean(rpm), 2),
          SEM = round(sd(rpm)/sqrt(dplyr::n()), 2),
          median = round(median(rpm), 2),
          Q1 = round(quantile(rpm, 0.25), 2),
          Q3 = round(quantile(rpm, 0.75), 2)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(mean)) #%>%
        # dplyr::distinct(id, .keep_all = TRUE) %>%
        # dplyr::slice_head(n = as.numeric(input$top_n))
    })

    # top_selected <- reactive({
    #   req(input$type_select)
    #
    #   tissue_summary() %>%
    #     dplyr::pull(mir_names) # was id
    # })

    # NEED TO SPLIT THIS SO THERE CAN BE A TABLE CHOICE BETWEEN SUMMARY AT THE
    # TISSUE LEVEL OR INDIVIDUAL LEVEL

    # selected_all <- reactive({
    #   # req(top_selected())
    #   req(input$mirna_select)
    #
    #   source_selected() %>%
    #     dplyr::filter( %in% input$type_select) %>%
    #     dplyr::select(id, mir_names, parents, variants, type, rpm, system, tissue, source, sample, breed, sex) %>%
    #     dplyr::filter(mir_names %in% top_selected()) # was id
    # })

    # get top mean mirs

    output$mirna_plot <- plotly::renderPlotly({

      ggplot2::ggplot(
        mirna_selected(),
        ggplot2::aes(
          x = new_lab,
          y = rpm,
          fill = mir_names,
          label = tissue_sample
        )) +
        ggplot2::geom_violin() +
        ggbeeswarm::geom_quasirandom(method = "smiley", groupOnX = FALSE, width = 0.1) +
        ggplot2::scale_fill_manual(values = mirna_cols) +
        ggplot2::scale_y_continuous(labels = scales::label_comma()) +
        ggplot2::labs(y = "Reads per million (RPM)") +
        ggplot2::theme_light() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
          axis.title.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 10),
          axis.title.y = ggplot2::element_text(size = 10),
          legend.position = "none",
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

    output$mirna_table <- DT::renderDT({

      mirna_summary() %>%
        dplyr::select(-tissue) %>% # drop the raw tissue column
        dplyr::rename(
          tissue = new_lab
        )

    })

    # output$team_text = renderText(length(unique(tissues$tissue)))
    # output$team_text = renderText(input$tiss_select)
    # output$team_text = renderText(dim(uid_rpms))
    # output$team_text = renderText(colnames(selected_sorted))
    output$team_text = renderText(colnames(mirna_selected()))
    # output$team_text2 = renderText(colnames(selected_all()))
    # output$team_text = renderText(input$top_n)

  })
}

## To be copied in the UI
# mod_ByMiRNA_ui("ByMiRNA_1")

## To be copied in the server
# mod_ByMiRNA_server("ByMiRNA_1")
