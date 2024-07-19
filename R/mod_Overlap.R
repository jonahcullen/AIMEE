#' Overlap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Overlap_ui <- function(id){
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
            selected = unique(uid_rpms$source),
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tiss_select"),
            label = "Tissue",
            choices = unique(uid_rpms$new_lab),
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
            # inline = TRUE,
            justified = TRUE,
            selected = "canon"
          )
        ),
        column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("top_n"),
            label = "# largest intersections",
            choices = c(5, seq(10, 100, by = 10), "all"),
            multiple = FALSE,
            selected = 20,
            options = list(size = 5)
            # status = "danger"
          )
        ),
        column(
          width = 2,
          numericInput(
            ns("rpm_cutoff"),
            label = HTML("&ge; RPM cutoff"),
            value = 20,
            min = 0
          )
        )
      ),
      fluidRow(
        shinydashboard::box(
          # title = "Plot",
          width = 12,
          shiny::plotOutput(ns("tiss_plot"))
          # plotly::plotlyOutput(ns("tiss_plot"))
        )
      ),
      fluidRow(
        div(
          style = "display: flex; justify-content: center; margin-bottom: 20px;",
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_presence"), "Intersection table")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_rpm"), "Expression (RPMs)")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_count"), "Expression (counts)")),
          shiny::downloadButton(ns("download_plot"), "Upset plot")
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$p("The # largest intersections setting does not impact the output expression files, which will contain all miRNAs in the selected tissue comparison. RPM cutoff changes will be reflected in the exported data. The count expression data will be filtered using retained RPM-based miRNA IDs.")
        )
      )
    )
  )
}

#' Overlap Server Functions
#'
#' @noRd
mod_Overlap_server <- function(id, mirna_space){
  moduleServer( id, function(input, output, session){
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

      dplyr::filter(spaced_data(), source %in% input$source_select)
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$new_lab)
      shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    })

    tissue_selected <- reactive({
      dplyr::filter(source_selected(), new_lab %in% input$tiss_select)
    })

    upset_ready <- reactive({
      req(input$tiss_select, input$rpm_cutoff)

      tissue_selected() %>%
        dplyr::select(-c(parents, mir_names, variants)) %>%
        dplyr::mutate(presence = ifelse(rpm > input$rpm_cutoff, 1, 0)) %>%
        dplyr::select(-rpm) %>%
        tidyr::pivot_wider(
          names_from = new_lab,
          values_from = presence,
          values_fill = list(presence = 0)) %>%
        dplyr::group_by(id, type) %>%
        dplyr::summarise(across(where(is.numeric), max), .groups = 'drop') %>%
        dplyr::mutate(across(where(is.numeric), ~ . > 0)) %>%
        dplyr::rename(name = id) %>% # column id results in duplicated columns error from complexupset
        dplyr::mutate(type = factor(type)) #%>%
        # dplyr::mutate(type = forcats::fct_relevel(type, "isomiR", "canon"))
    })

    type_selected <- reactive({
      req(upset_ready(), input$type_select)

      upset_ready() %>%
        dplyr::filter(type %in% input$type_select) %>%
        dplyr::mutate(type = forcats::fct_relevel(type, "isomiR", "canon"))

      # data <- upt %>%
      #   dplyr::mutate(type = forcats::fct_relevel(type, "isomiR", "canon"))
      #
      # return(data)
    })

    n_intersections <- reactive({
      if (input$top_n == "all") {
        indicator_cols <- setdiff(names(type_selected()), c("name", "type"))
        max_intersections <- 2^length(indicator_cols)
        return(max_intersections)
      } else {
        return(as.numeric(input$top_n))
      }
    })

    pull_ids <- reactive({
      req(type_selected())

      # type_selected() %>% dplyr::pull(name)
      type_selected() %>%
        dplyr::select(-type) %>%
        dplyr::filter(if_any(-name, ~ . == TRUE)) %>%
        dplyr::pull(name)
    })

    output$tiss_plot <- renderPlot({
      indicator_cols <- setdiff(names(type_selected()), c("name", "type"))

      validate(
        need(nrow(type_selected()) > 0, "no data available for the selected filters"),
        need(length(indicator_cols) >= 2, "needs at least two selected tissues")
      )

      ComplexUpset::upset(
        type_selected(),
        indicator_cols,
        base_annotations = list(
          'Intersection size' = (
            ComplexUpset::intersection_size(
              counts = TRUE,
              mapping = ggplot2::aes(
                fill = type,
                color = "black"
              ),
              text_colors = c(on_background = 'black', on_bar = 'black'),
              text = list(
                # angle = 45
                # position = ggplot2::position_nudge(y = 00)
              )
            ) + ggplot2::scale_fill_manual(values = c(
              'canon' = '#6388b4',
              'isomiR' = '#ffae34'
            )) + ggplot2::theme(
              plot.background = ggplot2::element_rect(fill = "#D3D3D3"),
              legend.title = ggplot2::element_blank()
            ) + ggplot2::ylab('Number of miRNAs in intersection')
          )
        ),
        set_sizes = (
          ComplexUpset::upset_set_size(filter_intersections=TRUE) +
            ggplot2::geom_text(
              ggplot2::aes(label = ggplot2::after_stat(count)),
              hjust = 1.1,
              stat = 'count',
              size = 3) +
            ggplot2::expand_limits(y = length(unique(type_selected()$name))) +
            ggplot2::theme(
            axis.text.x=ggplot2::element_text(angle=90),
            axis.title.x = ggplot2::element_blank()
          )),
        width_ratio = 0.1,
        height_ratio = 1,
        n_intersections = n_intersections(),
      ) + ggplot2::theme(axis.title.x = ggplot2::element_blank())
      # plotly::ggplotly(p)
    })

    output$download_presence <- downloadHandler(
      filename = function() {
        paste("aimee_overlap_table.", version, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(type_selected(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$download_rpm <- downloadHandler(
      filename = function() {
        paste("aimee_overlap_rpm.", version, ".csv", sep = "")
      },
      content = function(file) {

        filter_df <- uid_rpms %>%
          dplyr::filter(id %in% pull_ids()) %>%
          dplyr::filter(source %in% input$source_select) %>%
          dplyr::filter(new_lab %in% input$tiss_select) %>%
          dplyr::filter(type %in% input$type_select) %>%
          # dplyr::filter(rpm > input$rpm_cutoff) %>%
          dplyr::mutate(variants = gsub(",", "&", variants))

        write.csv(filter_df, file, quote = FALSE, row.names = FALSE)
      }
    )

    output$download_count <- downloadHandler(
      filename = function() {
        paste("aimee_overlap_cts.", version, ".csv", sep = "")
      },
      content = function(file) {

        filter_df <- uid_cts %>%
          dplyr::filter(id %in% pull_ids()) %>%
          dplyr::filter(source %in% input$source_select) %>%
          dplyr::filter(new_lab %in% input$tiss_select) %>%
          dplyr::filter(type %in% input$type_select) %>%
          dplyr::mutate(variants = gsub(",", "&", variants))

        write.csv(filter_df, file, quote = FALSE, row.names = FALSE)
      }
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("aimee_upset_plot.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 10, height = 8, units = "in", res = 300)
        print(ggplot2::last_plot())
        dev.off()
      }
    )
  })
}

## To be copied in the UI
# mod_Overlap_ui("Overlap_1")

## To be copied in the server
# mod_Overlap_server("Overlap_1")
