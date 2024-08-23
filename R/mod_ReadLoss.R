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
            choices = c("Sample" = "sample", "Tissue" = "tissue"),
            inline = TRUE,
            width = "800px",
            selected = "tissue",
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            width = 12,
            plotly::plotlyOutput(ns("line_chart"))
          )
        )
      ),
      fluidRow(
        div(
          style = "display: flex; justify-content: center; margin-bottom: 20px;",
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_table"), "Attrition table")),
          shiny::downloadButton(ns("download_plot"), "Line plot")
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          DT::dataTableOutput(ns("tiss_table"))
        )
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

    # unclear if keeping version information in exports
    version <- "v0.9"

    source_selected <- reactive({
      proc_cts %>%
        dplyr::filter(source %in% input$source_select) %>%
        dplyr::group_by(tissue) %>%
        dplyr::filter(dplyr::n_distinct(source) == length(input$source_select)) %>%
        dplyr::ungroup()
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$tissue)
      if (length(choices) == 0) {
        choices <- "0 shared tissues"
        shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices, selected = choices)
      } else {
        shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
      }
    })

    tissue_selected <- reactive({
      dplyr::filter(source_selected(), tissue %in% input$tiss_select)
    })

    observeEvent(tissue_selected(), {
      choices <- unique(tissue_selected()$sample)
      shinyWidgets::updatePickerInput(session, inputId = "sample_select", choices = choices)
    })

    sample_selected <- reactive({
      dplyr::filter(tissue_selected(), sample %in% input$sample_select)
    })

    plt_df <- reactive({
      req(input$sum_type)

      if(input$sum_type == "tissue") {
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
      } else if(input$sum_type == "sample") {
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

    selected_table <- reactive({

      out <- plt_df() %>%
        dplyr::rename(tissue = new_lab)

      if(input$sum_type == "sample") {
        out <- plt_df() %>%
          dplyr::select(-c(tissue_sample, source_mod)) %>%
          tidyr::pivot_wider(names_from = step, values_from = count)
      }
      return(out)
    })

    # common tissue colors from tissue data
    tissue_cols <- colorRampPalette(
      rev(RColorBrewer::brewer.pal(11, "Spectral"))
    )(length(unique(tissues$new_lab)))
    names(tissue_cols) <- unique(levels(tissues$new_lab))

    output$line_chart <- plotly::renderPlotly({
      req(input$sum_type)
      validate(
        need(nrow(plt_df()) > 0, "no data available for the selected filters")
      )

      if(input$sum_type == "tissue") {
        p <- ggplot2::ggplot(plt_df(),
                        ggplot2::aes(
                          x = step,
                          y = tissue_mean,
                          color = new_lab,
                          text = paste(
                            "step:", step,
                            "<br>tissue:", new_lab,
                            "<br>mean:", tissue_mean
                          ),
                          group = new_lab
                        )) +
          ggplot2::facet_wrap(~ source, ncol = 3) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = new_lab), alpha = 0.3) +
          ggplot2::geom_line(linewidth = 1, color = "black") +
          ggplot2::geom_point(ggplot2::aes(fill = new_lab), shape = 21, color = "black") +
          ggplot2::scale_color_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_fill_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
          ggplot2::theme_dark() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
            axis.text.y = ggplot2::element_text(size = 8),
            axis.title = ggplot2::element_blank(),
            # legend.position = "none"
          )
      } else if(input$sum_type == "sample") {
        p <- ggplot2::ggplot(plt_df(),
                        ggplot2::aes(
                          x = step,
                          y = count,
                          color = new_lab,
                          text = paste(
                            "step:", step,
                            "<br>tissue:", new_lab,
                            "<br>count:", count
                          ),
                          group = tissue_sample
                        )) +
          ggplot2::facet_wrap(~ source_mod, ncol = 3) +
          ggplot2::geom_line(linewidth = 1, color = "black") +
          ggplot2::geom_point(ggplot2::aes(fill = new_lab), shape = 21, color = "black") +
          ggplot2::scale_color_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_fill_manual(
            values = tissue_cols
          ) +
          ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
          ggplot2::theme_dark() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
            axis.text.y = ggplot2::element_text(size = 8),
            axis.title = ggplot2::element_blank(),
          )
      }
      # p + ggplot2::theme(legend.position = "none")
      plotly::ggplotly(p, tooltip = c("text")) %>%
        plotly::layout(showlegend = FALSE)
    })

    output$download_table <- downloadHandler(
      filename = function() {
        paste("aimee_attrition.", version, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(selected_table(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("aimee_attrition_plot.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 8, height = 5, units = "in", res = 300)
        p <- ggplot2::last_plot() +
          ggplot2::guides(
            color = ggplot2::guide_legend(title = "Tissue", override.aes = list(size = 3)),
            fill = ggplot2::guide_legend(title = "Tissue")
          ) +
          ggplot2::theme(
            legend.position = "right",
            legend.title = ggplot2::element_text(size = 10),
            legend.key = ggplot2::element_rect(fill = NA),
            legend.key.size = ggplot2::unit(1, "lines")
          )

        print(p)
        dev.off()
      }
    )

    output$tiss_table <- DT::renderDT({
      selected_table()
    })
  })
}

## To be copied in the UI
# mod_ReadLoss_ui("ReadLoss_1")

## To be copied in the server
# mod_ReadLoss_server("ReadLoss_1")
