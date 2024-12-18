#' TopMirs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
utils::globalVariables(c("uid_rpms", "mirna_space_ids", "filter_type", "new_lab",
                         "variants", "type", "id", "rpm", "tissue", "system",
                         "sample", "breed", "sex", "source", "mir_names", "mir_names_ordered"))

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
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} sources selected"
            ),
            multiple = TRUE,
            # selected = NULL
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
            multiple = TRUE,
            selected = NULL
          )
        ),
        column(
          width = 2,
          shinyWidgets::checkboxGroupButtons(
            ns("type_select"),
            label = "Type",
            choices = sort(unique(uid_rpms$type)),
            direction = "horizontal",
            justified = TRUE,
            selected = "canon",
          )
        ),
        column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("top_n"),
            label = "Top N",
            choices = c(5, 10, 20, 50, 100),
            multiple = FALSE,
            selected = 5
            # status = "danger"
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
          width = 12,
          plotly::plotlyOutput(ns("tiss_plot"))
        )
      ),
      fluidRow(
        div(
          style = "display: flex; justify-content: center; margin-bottom: 20px;",
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_table"), "Expression (RPMs)")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_plot"), "Violin plot")),
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          DT::dataTableOutput(ns("tiss_table"))
        )
      ),
    )
  )
}

#' TopMirs Server Functions
#'
#' @noRd
mod_TopMirs_server <- function(id, mirna_space){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # unclear if keeping version information in exports
    version <- "v0.9"

    # pre-filter based on mirna space
    spaced_data <- reactive({
      req(mirna_space())

      data <- uid_rpms
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
      req(spaced_data())

      spaced_data() %>%
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

    tissue_summary <- reactive({
      req(input$type_select, input$tiss_select)

      tissue_selected() %>%
        dplyr::filter(type %in% input$type_select) %>%
        tidyr::separate_rows(variants, sep = "\\|") %>%
        dplyr::distinct() %>%
        dplyr::group_by(dplyr::across(-variants)) %>%
        dplyr::mutate(variants = stringr::str_c(unique(variants), collapse = "|")) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::group_by(tissue, new_lab, id, mir_names, variants) %>%
        dplyr::filter(any(rpm != 0)) %>%
        dplyr::summarise(
          mean = round(mean(rpm), 2),
          SEM = round(stats::sd(rpm)/sqrt(dplyr::n()), 2),
          median = round(stats::median(rpm), 2),
          Q1 = round(stats::quantile(rpm, 0.25), 2),
          Q3 = round(stats::quantile(rpm, 0.75), 2)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(mean)) %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::slice_head(n = as.numeric(input$top_n))
    })

    top_selected <- reactive({
      req(input$type_select)

      tissue_summary() %>%
        dplyr::pull(mir_names) # was id
    })

    selected_all <- reactive({
      req(top_selected())

      data <- tissue_selected() %>%
        dplyr::filter(type %in% input$type_select) %>%
        dplyr::mutate(
          mir_names_ordered = factor(mir_names, levels = unique(top_selected()))
        ) %>%
        dplyr::select(
          id, mir_names, mir_names_ordered, parents, variants, type, rpm,
          system, tissue, new_lab, source, sample, breed, sex
        ) %>%
        dplyr::filter(mir_names %in% top_selected()) # was id

      return(data)
    })

    selected_table <- reactive({
      req(tissue_summary(), selected_all())

      if (input$sum_type == "tissue"){
        tissue_summary() %>%
          dplyr::select(-tissue) %>% # drop the raw tissue column
          dplyr::rename(
            tissue = new_lab
          ) %>%
          dplyr::select(-id)
        # dplyr::select(source, tissue, mir_names)
      } else if (input$sum_type == "sample") {
        selected_all() %>%
          dplyr::select(-tissue) %>%
          dplyr::rename(
            tissue = new_lab
          ) %>%
          dplyr::select(
            source, tissue, sample, mir_names, parents, variants, type, rpm
          )
      }
    })

    output$tiss_plot <- plotly::renderPlotly({
      validate(
        need(nrow(selected_all()) > 0, "no data available for the selected filters"),
      )

      p <- ggplot2::ggplot(
        selected_all(),
        ggplot2::aes(
          x = mir_names_ordered,
          y = rpm,
          fill = mir_names,
          text = paste(
            # "mir_names:", mir_names,
            "<br>sample:", sample,
            "<br>rpm:", rpm),
          group = mir_names_ordered
        )) +
        ggplot2::geom_violin(alpha = 0.8) +
        ggbeeswarm::geom_quasirandom(method = "smiley", width = 0.1) +
        ggplot2::scale_fill_manual(
          values = grDevices::colorRampPalette(
            RColorBrewer::brewer.pal(6, "Accent")
          )(input$top_n)
        ) +
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

      plotly::ggplotly(p, tooltip = c("text"))
    })

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("aimee_top_mirs.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 8, height = 6, units = "in", res = 300)
        print(ggplot2::last_plot())
        grDevices::dev.off()
      }
    )

    output$download_table <- downloadHandler(
      filename = function() {
        paste("aimee_top_mirs.", version, ".csv", sep = "")
      },
      content = function(file) {
          utils::write.csv(selected_table(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$tiss_table <- DT::renderDT({
      validate(
        need(input$tiss_select, "please select at least one tissue"),
        need(input$sum_type, "please select a summary level (sample or tissue)"),
        need(input$type_select, "please select a miRNA type (canon or isomiR)")
      )

      selected_table()
    })
  })
}

## To be copied in the UI
# mod_TopMirs_ui("TopMirs_1")

## To be copied in the server
# mod_TopMirs_server("TopMirs_1")
