#' Tissues UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
utils::globalVariables(c("tissues", "system", "new_lab", "tissue", "sample",
                         "source", "breed", "sex"))

mod_Tissues_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("source_select"),
            label = "Source",
            choices = unique(tissues$source),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
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
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("breed_select"),
            label = "Breed",
            choices = sort(unique(tissues$breed)),
            selected = sort(unique(tissues$breed)),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} breeds selected"
            ),
            multiple = TRUE
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("sex_select"),
            label = "Sex",
            choices = sort(unique(tissues$sex)),
            selected = sort(unique(tissues$sex)),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format`= "count",
              `count-selected-text` = "{0} sexes selected"
            ),
            multiple = TRUE
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
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_table"), "Sample data")),
          shiny::downloadButton(ns("download_plot"), "Bar plot")
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

#' Tissues Server Functions
#'
#' @noRd
mod_Tissues_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # unclear if keeping version information in exports
    version <- "v0.9"

    tissue_cols <- grDevices::colorRampPalette(
      rev(RColorBrewer::brewer.pal(11, "Spectral"))
    )(length(unique(tissues$tissue)))
    names(tissue_cols) <- unique(levels(tissues$tissue))

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

      if (length(choices) == 0) {
        choices <- "0 shared tissues"
        shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices, selected = choices)
      } else {
        shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
      }
    })

    observeEvent(system_selected(), {
      # update breed and sex options
      filtered_breeds <- unique(system_selected()$breed)
      shinyWidgets::updatePickerInput(
        session,
        inputId = "breed_select",
        choices = sort(filtered_breeds),
        selected = sort(filtered_breeds)
      )

      filtered_sexes <- unique(system_selected()$sex)
      shinyWidgets::updatePickerInput(
        session,
        inputId = "sex_select",
        choices = sort(filtered_sexes),
        selected = sort(filtered_sexes)
      )
    })

    # selected_all <- reactive({
    #   system_selected() %>%
    #     dplyr::filter(tissue %in% input$tiss_select) %>%
    #     dplyr::select(system, tissue, new_lab, sample, source, breed, sex) # remove post_counts
    # })

    selected_all <- reactive({
      system_selected() %>%
        dplyr::filter(
          tissue %in% input$tiss_select,
          breed %in% input$breed_select,
          sex %in% input$sex_select
        ) %>%
        dplyr::select(system, tissue, new_lab, sample, source, breed, sex)
    })

    selected_table <- reactive({
      selected_all() %>%
        dplyr::select(-tissue) %>% # drop the raw tissue column
        dplyr::rename(
          tissue = new_lab
        ) %>%
        dplyr::select(source, system, tissue, sample, breed, sex)
    })

    output$tiss_plot <- plotly::renderPlotly({
      validate(
        need(nrow(selected_all()) > 0, "no data available for the selected filters"),
      )

      counts <- selected_all() %>%
        dplyr::group_by(new_lab) %>%
        dplyr::summarise(count = dplyr::n())

      # add counts back
      selected_counts <- selected_all() %>%
        dplyr::left_join(counts, by = "new_lab")

      p <- ggplot2::ggplot(
        selected_counts,
        ggplot2::aes(
          x = new_lab,
          fill = tissue,
          text = paste("tissue:", new_lab, "<br>count:", count)
        )) +
        ggplot2::geom_bar(color = "black") +
        ggplot2::scale_fill_manual(values = tissue_cols) +
        ggplot2::labs(y = "Sample count") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
          axis.title.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(size = 12),
          legend.position = "none",
        )

      plotly::ggplotly(p, tooltip = c("text"))
    })

    output$download_table <- downloadHandler(
      filename = function() {
        paste("aimee_samples.", version, ".csv", sep = "")
      },
      content = function(file) {
          utils::write.csv(selected_table(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("aimee_sample_plot.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 10, height = 8, units = "in", res = 300)
        print(ggplot2::last_plot())
        grDevices::dev.off()
      }
    )

    output$tiss_table <- DT::renderDT({
      selected_table()
    })

  })
}

## To be copied in the UI
# mod_Tissues_ui("Tissues_1")

## To be copied in the server
# mod_Tissues_server("Tissues_1")
