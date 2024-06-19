#' TopMirs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TopMirs_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/topmirs.css"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("source_select"),
            label = "Source",
            choices = unique(uid_rpms$source),
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
            selected = unique(uid_rpms$source)
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tiss_select"),
            label = "Tissue",
            choices = unique(uid_rpms$tissue),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} tissues selected"
            ),
            multiple = FALSE,
            selected = NULL
          )
        ),
        column(
          width = 3,
          shinyWidgets::checkboxGroupButtons(
            ns("type_select"),
            label = "Type",
            choices = sort(unique(uid_rpms$type)),
            direction = "horizontal",
            # inline = TRUE,
            justified = TRUE,
            selected = "canon",
            # checkIcon = list(
            #   yes = icon("circle-check", class = "fa-regular"),
            #   no = icon("circle", class = "fa-regular")
            # )
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("top_n"),
            label = "Top N",
            choices = c(5, 10, 20, 50, 100),
            multiple = FALSE,
            selected = 5
            # status = "danger"
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
        column(
          width = 5,
          shinyWidgets::awesomeRadio(
            ns("sum_type"),
            label = "Summary table",
            choices = c("Samples" = "samples", "Tissues" = "tissues"),
            inline = TRUE,
            width = "800px",
            selected = "tissues",
          )
          # tags$head(tags$style(HTML("
          #   .checkbox-inline, .radio-inline {
          #     margin-right: 20px;
          #   }
          # ")))
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
        column(width = 3, textOutput(ns('team_text'))),
        br(),
        column(width = 3, textOutput(ns('team_text2')))
      )
    )
  )
}

#' TopMirs Server Functions
#'
#' @noRd
mod_TopMirs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # tissue_cols <- colorRampPalette(
    #   RColorBrewer::brewer.pal(11, "Spectral")
    # )(length(unique(tissues$tissue)))
    # names(tissue_cols) <- unique(levels(tissues$tissue))

    # HIERARCHICAL
    source_selected <- reactive({
      dplyr::filter(uid_rpms, source %in% input$source_select)
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$tissue)
      shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    })

    tissue_selected <- reactive({
      # req(input$tiss_select)
      dplyr::filter(source_selected(), tissue %in% input$tiss_select)
    })

    # observeEvent(tissue_selected(), {
    #   choices <- unique(tissue_selected()$type)
    #   shinyWidgets::updatePickerInput(session, inputId = "type_select", choices = choices)
    # })

    tissue_summary <- reactive({
      req(input$type_select)
      req(input$tiss_select)

      tissue_selected() %>%
        dplyr::filter(type %in% input$type_select) %>%
        dplyr::group_by(tissue, new_lab, id, mir_names) %>% # DOUBLE CHECK THIS GROUP_BY WITH id AND mir_names IS GOOD !!!
        dplyr::filter(any(rpm != 0)) %>%
        dplyr::summarise(
          mean = round(mean(rpm), 2),
          SEM = round(sd(rpm)/sqrt(dplyr::n()), 2),
          median = round(median(rpm), 2),
          Q1 = round(quantile(rpm, 0.25), 2),
          Q3 = round(quantile(rpm, 0.75), 2)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(mean)) %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::slice_head(n = as.numeric(input$top_n))
    })

    top_selected <- reactive({
      req(input$type_select)

      tissue_summary() %>%
        dplyr::pull(mir_names) # was id
    })

    # NEED TO SPLIT THIS SO THERE CAN BE A TABLE CHOICE BETWEEN SUMMARY AT THE
    # TISSUE LEVEL OR INDIVIDUAL LEVEL

    selected_all <- reactive({
      req(top_selected())
      tissue_selected() %>%
        dplyr::filter(type %in% input$type_select) %>%
        dplyr::select(
          id, mir_names, parents, variants, type, rpm,
          system, tissue, new_lab, source, sample, breed, sex
        ) %>%
        dplyr::filter(mir_names %in% top_selected()) # was id
    })

    # get top mean mirs

    output$tiss_plot <- plotly::renderPlotly({
      # shinipsum::random_ggplot(type = "line")
      # selected_all()$mir_names <- factor(selected_all()$mir_names, levels = unique(top_selected()))

      ggplot2::ggplot(
        selected_all(),
        ggplot2::aes(x = factor(mir_names, level = unique(top_selected())), y = rpm, fill = mir_names, label = sample)) +
        ggplot2::geom_violin(alpha = 0.8) +
        # ggbeeswarm::geom_beeswarm() +
        ggbeeswarm::geom_quasirandom(method = "smiley", width = 0.1) +
        ggplot2::scale_fill_manual(
          values = colorRampPalette(
            RColorBrewer::brewer.pal(6, "Accent")
          )(input$top_n)
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_comma()) +
        # ggplot2::scale_fill_brewer(palette="Dark2") +
        # ggplot2::aes(x = tissue, fill = tissue)) +
        # ggplot2::geom_bar(color = "black") +
        # ggplot2::scale_fill_manual(values = tissue_cols) +
        # # ggplot2::scale_color_manual(values = "black")
        ggplot2::labs(y = "Reads per million (RPM)") +
        ggplot2::theme_light() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
          axis.title.x = ggplot2::element_blank(),
          # axis.ticks.x = element_blank(),
          # axis.line.x.bottom = element_blank(),
          # axis.title.x = element_blank(),
          axis.text.y = ggplot2::element_text(size = 10),
          axis.title.y = ggplot2::element_text(size = 10),
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
      # source_selected()
      # system_selected()
      if (input$sum_type == "tissues"){
        tissue_summary() %>%
          dplyr::select(-tissue) %>% # drop the raw tissue column
          dplyr::rename(
            tissue = new_lab
          ) %>%
          dplyr::select(-id)
          # dplyr::select(source, tissue, mir_names)
      } else if (input$sum_type == "samples") {
        selected_all() %>%
          dplyr::select(-tissue) %>%
          dplyr::rename(
            tissue = new_lab
          ) %>%
          dplyr::select(
            source, tissue, sample, mir_names, parents, variants, type, rpm
          )
      }
      # dplyr::filter(tissue %in% input$tiss_select) %>%
      # dplyr::select(system, tissue, sample, source, breed, sex, post_counts)
    })

    # output$team_text = renderText(length(unique(tissues$tissue)))
    # output$team_text = renderText(input$tiss_select)
    # output$team_text = renderText(dim(uid_rpms))
    # output$team_text = renderText(colnames(selected_sorted))
    # output$team_text = renderText(colnames(top_selected()))
    output$team_text2 = renderText(colnames(selected_all()))
    # output$team_text = renderText(input$top_n)

  })
}

## To be copied in the UI
# mod_TopMirs_ui("TopMirs_1")

## To be copied in the server
# mod_TopMirs_server("TopMirs_1")
