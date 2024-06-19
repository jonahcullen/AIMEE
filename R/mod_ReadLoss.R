#' ReadLoss UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ReadLoss_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/topmirs.css"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tiss_select"),
            label = "Tissue",
            choices = unique(pre_rank$tissue),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              # `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} tissues selected"
            ),
            multiple = TRUE,
            selected = NULL
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("source_select"),
            label = "Source",
            choices = unique(pre_rank$source),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} sources selected"
            ),
            multiple = TRUE,
            selected = unique(pre_rank$source)
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("sample_select"),
            label = "Sample",
            choices = sort(unique(na.omit(as.character(pre_rank$sample)))),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} samples selected"
            ),
            multiple = TRUE,
            selected = unique(pre_rank$sample)
          )
        ),
        column(
          width = 3,
          shinyWidgets::awesomeRadio(
            ns("sum_type"),
            label = "Level",
            choices = c("Samples" = "samples", "Tissues" = "tissues"),
            inline = TRUE,
            width = "800px",
            selected = "tissues",
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            width = 12,
            # plotOutput(ns("line_chart"))
            plotly::plotlyOutput(ns("line_chart"))
          )
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

#' ReadLoss Server Functions
#'
#' @noRd
mod_ReadLoss_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tissue_selected <- reactive({
      # req(input$tiss_select)
      dplyr::filter(proc_cts, tissue %in% input$tiss_select)
    })

    observeEvent(tissue_selected(), {
      choices <- unique(tissue_selected()$source)
      shinyWidgets::updatePickerInput(session, inputId = "source_select", choices = choices)
    })

    source_selected <- reactive({
      req(input$source_select)
      dplyr::filter(tissue_selected(), source %in% input$source_select)
    })

    # observeEvent(source_selected(), {
    #   choices <- unique(source_selected()$tissue)
    #   shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    # })

    # tissue_selected <- reactive({
    #   req(input$tiss_select)
    #   dplyr::filter(source_selected(), tissue %in% input$tiss_select)
    # })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$sample)
      shinyWidgets::updatePickerInput(session, inputId = "sample_select", choices = choices)
    })

    sample_selected <- reactive({
      # dplyr::filter(tissue_selected(), sample %in% input$sample_select)
      req(input$sample_select)
      dplyr::filter(source_selected(), sample %in% input$sample_select)
    })

    plt_df <- reactive({

      if(input$sum_type == "tissues") {
        sample_selected() %>%
          dplyr::group_by(new_lab, source, step) %>%
          dplyr::summarise(
            tissue_mean = mean(count),
            lower = tissue_mean - qnorm(0.975)*sd(count),
            upper = tissue_mean + qnorm(0.975)*sd(count)
          ) %>%
          dplyr::mutate(
            tissue_mean = round(tissue_mean, 2),
            lower = round(lower, 2),
            upper = round(upper, 2),
            source = forcats::fct_relevel(source, "FAANG", "Primary")
          )
      } else if(input$sum_type == "samples") {
        sample_selected() %>%
          dplyr::select(
            source, source_mod, new_lab, tissue_sample,
            sample, breed, sex, step, count
          ) %>%
          dplyr::mutate(
            source_mod = forcats::fct_relevel(source_mod, "FAANG", "Primary")
          )
      }

    })

    # labels for last step
    label_data <- reactive({
      plt_df() %>%
        dplyr::group_by(source) %>%
        dplyr::filter(step == tail(levels(step), 1))
    })

    # common tissue colors from tissue data
    tissue_cols <- colorRampPalette(
      rev(RColorBrewer::brewer.pal(11, "Spectral"))
    )(length(unique(tissues$new_lab)))
    names(tissue_cols) <- unique(levels(tissues$new_lab))

    output$line_chart <- plotly::renderPlotly({
    # output$line_chart <- renderPlot({
      # req(plt_df())
      # req(label_data())
      req(input$sum_type)

      if(input$sum_type == "tissues") {
        ggplot2::ggplot(plt_df(),
                        ggplot2::aes(
                          x = step,
                          y = tissue_mean,
                          color = new_lab,
                          group = new_lab
                        )) +
          ggplot2::facet_wrap(~ source, ncol = 3) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = new_lab), alpha = 0.3) +
          ggplot2::geom_line(linewidth = 1, color = "black") +
          ggplot2::geom_point(ggplot2::aes(fill = new_lab), shape = 21, color = "black") +
          # ggrepel::geom_label_repel(
          #   data = label_data(),
          #   ggplot2::aes(label = new_lab),
          #   color = "black",
          #   nudge_x = 1.5,
          #   box.padding = 0.5) +
          ggplot2::scale_color_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_fill_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::theme_dark() +
          ggplot2::labs(x = "Processing step", y = "Sequence count") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
            axis.title.x = ggplot2::element_text(size = 10),
            axis.text.y = ggplot2::element_text(size = 8),
            axis.title.y = ggplot2::element_text(size = 10),
            legend.position = "none"
          )
      } else if(input$sum_type == "samples") {
        ggplot2::ggplot(plt_df(),
                        ggplot2::aes(
                          x = step,
                          y = count,
                          color = new_lab,
                          group = tissue_sample
                        )) +
          ggplot2::facet_wrap(~ source_mod, ncol = 3) +
          ggplot2::geom_line(linewidth = 1, color = "black") +
          ggplot2::geom_point(ggplot2::aes(fill = new_lab), shape = 21, color = "black") +
          # ggrepel::geom_label_repel(data = label_data(),
          #                           ggplot2::aes(label = sample),
          #                           color = "black",
          #                           nudge_x = 1.5,
          #                           box.padding = 0.5) +
          ggplot2::scale_color_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_fill_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::theme_dark() +
          ggplot2::labs(x = "Processing step", y = "Sequence count") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
            axis.title.x = ggplot2::element_text(size = 10),
            axis.text.y = ggplot2::element_text(size = 8),
            axis.title.y = ggplot2::element_text(size = 10),
            legend.position = "none"
          )
      }


    })

    output$tiss_table <- DT::renderDT({

      out <- plt_df() %>%
        dplyr::rename(tissue = new_lab)

      if(input$sum_type == "samples") {
        out <- plt_df() %>%
          dplyr::select(-c(tissue_sample)) %>%
          tidyr::pivot_wider(names_from = step, values_from = count)
      }

      return(out)

    })

    output$team_text = renderText(colnames(plt_df()))
    # output$team_text2 = renderText(names(mir_cols()))

  })
}

## To be copied in the UI
# mod_ReadLoss_ui("ReadLoss_1")

## To be copied in the server
# mod_ReadLoss_server("ReadLoss_1")
