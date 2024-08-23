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
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} sources selected"
            ),
            multiple = TRUE,
            selected = NULL
            # selected = unique(canons$source)
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tiss_select"),
            label = "Tissue",
            choices = unique(canons$new_lab),
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
            ),
            multiple = TRUE,
            selected = NULL
          )
        ),
        column(
          width = 2,
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
        shinydashboard::box(
          # title = "Plot",
          width = 12,
          plotly::plotlyOutput(ns("mirna_plot"))
        )
      ),
      fluidRow(
        div(
          style = "display: flex; justify-content: center; margin-bottom: 20px;",
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_table"), "Expression (RPMs)")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_plot"), "Box plot")),
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$p("Note: When exploring at the sample level, you can interact with the plot legend to focus on specific miRNAs. Double-click on a legend entry (i.e. the selected miRNAs) to isolate a particular miRNA, making it easier to examine its expression across samples. Double-click on the legend entry to restore the full plot view. Additionally, due to a technical issue, data points defined as outliers we'll appear twice.")
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          DT::dataTableOutput(ns("mirna_table"))
        )
      )
    )
  )
}

#' ByMiRNA Server Functions
#'
#' @noRd
mod_ByMiRNA_server <- function(id, mirna_space){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # unclear if keeping version information in exports
    version <- "v0.9"

    mirna_cols <- colorRampPalette(
      RColorBrewer::brewer.pal(6, "Dark2")
    )(length(unique(canons$mir_names)))
    names(mirna_cols) <- unique(levels(canons$mir_names))

    tissue_cols <- colorRampPalette(
      rev(RColorBrewer::brewer.pal(11, "Spectral"))
    )(length(unique(tissues$new_lab)))
    names(tissue_cols) <- unique(levels(tissues$new_lab))

    spaced_data <- reactive({
      req(mirna_space())

      data <- canons
      mirna_space_val <- mirna_space()

      if (mirna_space_val == "Exclusive") {
        filter_ids <- mirna_space_ids %>%
          dplyr::filter(filter_type == "Exclusive") %>%
          dplyr::pull(id)
        data <- data %>%
          dplyr::filter(id %in% filter_ids)
      } else if (mirna_space_val == "Exclusive repeat") {
        filter_ids <- mirna_space_ids %>%
          dplyr::filter(filter_type == "Exclusive repeat") %>%
          dplyr::pull(id)
        data <- data %>%
          dplyr::filter(id %in% filter_ids)
      } else if (mirna_space_val == "Ambiguous") {
        filter_ids <- mirna_space_ids %>%
          dplyr::filter(filter_type == "Ambiguous") %>%
          dplyr::pull(id)
        data <- data %>%
          dplyr::filter(id %in% filter_ids)
      } else if (mirna_space_val == "Ambiguous repeat") {
        filter_ids <- mirna_space_ids %>%
          dplyr::filter(filter_type == "Ambiguous repeat") %>%
          dplyr::pull(id)
        data <- data %>%
          dplyr::filter(id %in% filter_ids)
      }

      return(data)
    })

    source_selected <- reactive({
      validate(
        need(input$source_select, "please select at least one source"),
      )
      req(spaced_data(), input$source_select)

      # dplyr::filter(spaced_data(), source %in% input$source_select)
      spaced_data() %>%
        dplyr::filter(source %in% input$source_select) %>%
        dplyr::group_by(tissue) %>%
        dplyr::filter(dplyr::n_distinct(source) == length(input$source_select)) %>%
        dplyr::ungroup()
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$new_lab)
      shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    })

    tissue_selected <- reactive({
      req(source_selected())
      validate(
        need(input$tiss_select, "please select at least one tissue")
      )

      dplyr::filter(source_selected(), new_lab %in% input$tiss_select)
    })

    observeEvent(tissue_selected(), {
      choices <- unique(source_selected()$mir_names)
      shinyWidgets::updatePickerInput(session, inputId = "mirna_select", choices = choices)
    })

    mirna_selected <- reactive({
      # req(tissue_selected())
      validate(
        need(nrow(tissue_selected()) > 0, "please select at least one tissue")
      )

      dplyr::filter(tissue_selected(), mir_names %in% input$mirna_select)
    })

    mirna_summary <- reactive({
      req(mirna_selected())

      if (input$sum_type == "tissue"){
        mirna_selected() %>%
          dplyr::group_by(tissue, new_lab, mir_names, Read) %>%
          # dplyr::filter(any(rpm != 0)) %>%
          dplyr::summarise(
            mean = round(mean(rpm), 2),
            SEM = round(sd(rpm)/sqrt(dplyr::n()), 2),
            median = round(median(rpm), 2),
            Q1 = round(quantile(rpm, 0.25), 2),
            Q3 = round(quantile(rpm, 0.75), 2)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(desc(mean))
      } else if (input$sum_type == "sample"){
        mirna_selected() %>%
          dplyr::mutate(
            sample = stringr::str_replace(
              tissue_sample, paste0(tissue, "_"), "")
            ) %>%
          dplyr::select(
            source, system, tissue, new_lab, sample,
            breed, sex, Read, id, mir_names, rpm
            )
      }
    })

    selected_table <- reactive({
      req(mirna_summary())

      mirna_summary() %>%
        dplyr::select(-tissue) %>%
        dplyr::rename(
          tissue = new_lab
        )
    })

    p <- reactive({
      validate(
        need(nrow(mirna_selected()) > 0, "please select at least one miRNA")
      )

      plt_df <- mirna_selected() %>%
        dplyr::mutate(
          sample = stringr::str_replace(tissue_sample, paste0(tissue, "_"), ""),
          wrapped_label = stringr::str_replace(interaction(new_lab, mir_names), "\\.", "\n"))


      if (input$sum_type == "tissue"){
        p <- ggplot2::ggplot(
          plt_df,
          ggplot2::aes(
            x = new_lab,
            y = rpm,
            fill = new_lab,
            text = paste(
              "sample:", sample,
              "<br>tissue:", new_lab,
              "<br>mir_names:", mir_names,
              "<br>rpm:", rpm),
          )) +
          ggplot2::geom_boxplot() +
          ggbeeswarm::geom_quasirandom(method = "smiley", groupOnX = FALSE, width = 0.1) +
          ggplot2::scale_fill_manual(values = tissue_cols) +
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
      } else if (input$sum_type == "sample"){
        p <- ggplot2::ggplot(
          plt_df,
          ggplot2::aes(
            x = wrapped_label,
            # x = interaction(new_lab, mir_names),
            y = rpm,
            fill = mir_names,
            text = paste(
              "sample:", sample,
              "<br>tissue:", new_lab,
              "<br>mir_names:", mir_names,
              "<br>rpm:", rpm),
          )) +
          ggplot2::geom_boxplot(
            position = ggplot2::position_dodge(width = 0.75),
            outlier.color = NA,
            outlier.size = 0,
            outlier.shape = NA) +
          ggplot2::geom_jitter(position = ggplot2::position_dodge(width = 0.75)) +
          ggplot2::scale_fill_manual(values = mirna_cols) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(y = "Reads per million (RPM)") +
          ggplot2::theme_light() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
            axis.title.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(size = 10),
            axis.title.y = ggplot2::element_text(size = 10),
            legend.title = ggplot2::element_blank()
          )
      }
    })

    output$mirna_plot <- plotly::renderPlotly({
      # validate(
      #   need(nrow(mirna_selected()) > 0, "please select at least one miRNA")
      # )
      plotly::ggplotly(p(), tooltip = c("text"))
    })

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("aimee_by_mirna.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 8, height = 6, units = "in", res = 300)
        print(p())
        dev.off()
      }
    )

    output$download_table <- downloadHandler(
      filename = function() {
        paste("aimee_by_mirna.", version, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(selected_table(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$mirna_table <- DT::renderDT({
      # req(mirna_summary())

        selected_table()
    })
  })
}

## To be copied in the UI
# mod_ByMiRNA_ui("ByMiRNA_1")

## To be copied in the server
# mod_ByMiRNA_server("ByMiRNA_1")
