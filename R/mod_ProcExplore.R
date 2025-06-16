#' ProcExplore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
utils::globalVariables(c("proc_cts", "source", "tissue", "new_lab", "step",
                         "count", "tissue_sample", "tissue_pop", "tissue_alpha",
                         "se.X", "se.Y", ".data", "tissue_name", ":="))

mod_ProcExplore_ui <- function(id){
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
            ns("breed_select"),
            label = "Breed",
            choices = unique(tissues$breed),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format` = "count",
              `count-selected-text` = "{0} breeds selected"
            ),
            multiple = TRUE,
            selected = NULL
          )
        ),
        column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("sex_select"),
            label = "Sex",
            choices = unique(tissues$sex),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format` = "count",
              `count-selected-text` = "{0} sexes selected"
            ),
            multiple = TRUE,
            selected = NULL
          )
        )
      ),
      fluidRow(
        column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("x_axis"),
            label = "X axis",
            choices = c(unique(stats::na.omit(as.character(proc_cts$step))), "Ref_miRs", "IsomiRs"),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `live-search` = TRUE
            ),
            selected = NULL
          ),
          shinyWidgets::pickerInput(
            ns("y_axis"),
            label = "Y axis",
            choices = c(unique(stats::na.omit(as.character(proc_cts$step))), "Ref_miRs", "IsomiRs"),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              `live-search` = TRUE
            ),
            selected = NULL
          )
        ),
        column(
          width = 10,
          shinydashboard::box(
            width = 12,
            plotOutput(ns("scatter"))
          )
        )
      ),
      fluidRow(
        div(
          style = "display: flex; justify-content: center; margin-bottom: 20px;",
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_tiss_table"), "Tissue table")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_cent_table"), "Centroid table")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_plot"), "Centroid plot")),
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$p("The plot above shows individual sample data points and their centroids by tissue. Centroids (and their standard error bars) represent the mean data values of the selected processing step for each tissue. All tissue centroids are displayed to facilitate comparisons between the tissue(s) of interest and all other included tissues. The centroid table below provides the selected mean values and standard errors for each tissue across data sources.")
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          title = "Tissues",
          DT::dataTableOutput(ns("tiss_table"))
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          title = "Centroids",
          DT::dataTableOutput(ns("cent_table"))
        )
      )
    )
  )
}

#' ProcExplore Server Functions
#'
#' @noRd
mod_ProcExplore_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # unclear if keeping version information in exports
    version <- "v0.9"

    source_selected <- reactive({
      dplyr::filter(proc_cts, source %in% input$source_select)
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$tissue)
      shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    })

    observeEvent(input$tiss_select, {
      df <- source_selected() %>%
        dplyr::filter(tissue %in% input$tiss_select)

      shinyWidgets::updatePickerInput(session, "breed_select", choices = unique(df$breed))
      shinyWidgets::updatePickerInput(session, "sex_select", choices = unique(df$sex))
    })

    tissue_selected <- reactive({
      dplyr::filter(proc_cts, tissue %in% input$tiss_select)
    })

    quote_col_name <- function(col_name) {
      if (grepl("[[:space:]|-]", col_name)) {
        return(paste0("`", col_name, "`"))
      }
      return(col_name)
    }

    common_validate <- function() {
      validate(
        need(input$x_axis, "please select an X axis"),
        need(input$y_axis, "please select a Y axis"),
        need(input$x_axis != input$y_axis, "X and Y axes must be different")
      )
    }

    f <- function(z)stats::sd(z)/sqrt(length(z)) # function to calculate std.err

    tissue_cols <- grDevices::colorRampPalette(
      rev(RColorBrewer::brewer.pal(11, "Spectral"))
    )(length(unique(tissues$tissue)))
    names(tissue_cols) <- unique(levels(tissues$tissue))

    tissue_cols["NA"] <- "grey90"

    plt_df <- reactive({
      req(input$tiss_select)

      tmp <- uid_rpms %>%
        dplyr::select(-c(system, source, breed, sex)) %>%
        dplyr::group_by(sample, tissue, new_lab) %>%
        dplyr::filter(rpm > 0) %>%
        dplyr::summarise(
          Ref_miRs = sum(type == "canon"),
          IsomiRs = sum(type == "isomiR")
        ) %>%
        dplyr::mutate(
          tissue_sample = paste0(tissue, "_", sample)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(tissue, sample, new_lab))
      # print(paste("tmp:", paste(names(tmp), collapse = ", ")))

      proc_mirs <- proc_cts %>%
        dplyr::filter(
          source %in% input$source_select,
          tissue %in% input$tiss_select,
          if (!is.null(input$breed_select)) breed %in% input$breed_select else TRUE,
          if (!is.null(input$sex_select)) sex %in% input$sex_select else TRUE
        ) %>%
        tidyr::pivot_wider(names_from = step, values_from = count) %>%
        dplyr::left_join(tmp, by = "tissue_sample") %>%
        dplyr::mutate(
          tissue_pop = as.factor(ifelse(tissue %in% input$tiss_select, as.character(tissue), "")),
          tissue_alpha = ifelse(tissue %in% input$tiss_select, 1, 0.2)
        )

      return(proc_mirs)
    })

    centroids <- reactive({
      common_validate()
      df <- plt_df()

      x_col <- quote_col_name(input$x_axis)
      y_col <- quote_col_name(input$y_axis)

      if (!(input$x_axis %in% names(df)) || !(input$y_axis %in% names(df))) {
        return(tibble::tibble(
          tissue = character(),
          !!input$x_axis := numeric(),
          !!input$y_axis := numeric(),
          se.X = numeric(),
          se.Y = numeric(),
          new_lab = character(),
          tissue_pop = factor(),
          tissue_alpha = numeric(),
          tissue_name = character()
        ))
      }

      form <- stats::as.formula(paste("cbind(", x_col, ", ", y_col, ") ~ tissue"))
      means <- stats::aggregate(form, data = plt_df(), FUN = mean)

      form_se <- stats::as.formula(paste("cbind(se.X = ", x_col, ", se.Y = ", y_col, ") ~ tissue"))
      se <- stats::aggregate(form_se, data = plt_df(), FUN = f)

      dplyr::left_join(means, se, by = "tissue") %>%
        dplyr::left_join(
          df %>% dplyr::select(tissue, new_lab) %>% dplyr::distinct(),
          by = "tissue"
        ) %>%
        dplyr::mutate(
          se.X = round(se.X, 2),
          se.Y = round(se.Y, 2),
          !!input$x_axis := round(!!rlang::sym(input$x_axis), 2),
          !!input$y_axis := round(!!rlang::sym(input$y_axis), 2),
          tissue_pop = as.factor(ifelse(tissue %in% input$tiss_select, as.character(tissue), "NA")),
          tissue_alpha = ifelse(tissue %in% input$tiss_select, 1, 0.2),
          tissue_name = ifelse(tissue %in% input$tiss_select, as.character(new_lab), "")
        )
    })

    filtered_tiss_table <- reactive({
      common_validate()
      df <- plt_df()
      req(input$x_axis %in% names(df), input$y_axis %in% names(df))

      df %>%
        dplyr::filter(
          source %in% input$source_select,
          tissue %in% input$tiss_select,
          if (!is.null(input$breed_select)) breed %in% input$breed_select else TRUE,
          if (!is.null(input$sex_select)) sex %in% input$sex_select else TRUE
        ) %>%
        dplyr::select(
          source, tissue, new_lab, sample, breed, sex,
          !!rlang::sym(input$x_axis),
          !!rlang::sym(input$y_axis)
        )
    })

    filtered_cent_table <- reactive({
      common_validate()
      df <- centroids()
      req(input$x_axis %in% names(df), input$y_axis %in% names(df))

      # centroids() %>%
      df %>%
        dplyr::filter(
          # source %in% input$source_select &
          tissue %in% input$tiss_select
        ) %>%
        dplyr::select(tissue, !!rlang::sym(input$x_axis), se.X, !!rlang::sym(input$y_axis), se.Y)
    })

    output$scatter <- renderPlot({
      common_validate()
      validate(
        need(nrow(plt_df()) > 0, "no data available for the selected filters")
      )

      ggplot2::ggplot(plt_df(),
                      ggplot2::aes(
                        x = .data[[input$x_axis]],
                        y = .data[[input$y_axis]],
                      )) +
        ggplot2::geom_errorbarh(
          data = centroids(),
          height = 0.1,
          ggplot2::aes(
            xmin = .data[[input$x_axis]] - se.X,
            xmax = .data[[input$x_axis]] + se.X,
            alpha = tissue_alpha
          )
        ) +
        ggplot2::geom_errorbar(
          data = centroids(),
          width = 0.1,
          ggplot2::aes(
            ymin = .data[[input$y_axis]] - se.Y,
            ymax = .data[[input$y_axis]] + se.Y,
            alpha = tissue_alpha
          )
        ) +
        ggplot2::geom_point(ggplot2::aes(color = tissue_pop)) +
        ggplot2::geom_point(
          data = centroids(),
          ggplot2::aes(fill = tissue_pop, alpha = tissue_alpha),
          size = 7,
          shape = 21,
        ) +
        ggrepel::geom_label_repel(
          data = centroids(),
          ggplot2::aes(label = tissue_name),
          max.overlaps = 100,
          box.padding = 0.9,
          segment.linetype = 3,
        ) +
        ggplot2::scale_color_manual(values = tissue_cols) +
        ggplot2::scale_fill_manual(values = tissue_cols) +
        ggplot2::scale_x_continuous(labels = scales::label_comma()) +
        ggplot2::scale_y_continuous(labels = scales::label_comma()) +
        ggplot2::theme_dark() +
        ggplot2::theme(
          legend.position = "none"
        )

    })

    output$download_tiss_table <- downloadHandler(
      filename = function() {
        paste("aimee_proc_explore.tissues.", version, ".csv", sep = "")
      },
      content = function(file) {
          utils::write.csv(filtered_tiss_table(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$download_cent_table <- downloadHandler(
      filename = function() {
        paste("aimee_proc_explore.centroids.", version, ".csv", sep = "")
      },
      content = function(file) {
          utils::write.csv(filtered_cent_table(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste("aimee_proc_explore.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 8, height = 5, units = "in", res = 300)
        print(ggplot2::last_plot())
        grDevices::dev.off()
      }
    )

    output$tiss_table <- DT::renderDT({
      filtered_tiss_table()
    })

    output$cent_table <- DT::renderDT({
      filtered_cent_table()
    })
  })
}

## To be copied in the UI
# mod_ProcExplore_ui("ProcExplore_1")

## To be copied in the server
# mod_ProcExplore_server("ProcExplore_1")
