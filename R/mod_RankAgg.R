#' RankAgg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_RankAgg_ui <- function(id){
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
        # column(
        #   width = 3,
        #   shinyWidgets::checkboxGroupButtons(
        #     ns("type_select"),
        #     label = "Type",
        #     choices = sort(unique(uid_rpms$type)),
        #     direction = "horizontal",
        #     justified = TRUE,
        #     selected = "canon",
        #   )
        # ),
        column(
          width = 3,
          numericInput(
            ns("rpm_cutoff"),
            label = HTML("&ge; RPM cutoff"),
            value = 50,
            min = 0,
            max = 500,
            step = 10
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            width = 12,
            plotOutput(ns("bump_chart"))
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            # title = "Plot",
            width = 12,
            plotly::plotlyOutput(ns("box_plot"))
          )
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

#' RankAgg Server Functions
#'
#' @noRd
mod_RankAgg_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # tissue_cols <- colorRampPalette(
    #   RColorBrewer::brewer.pal(11, "Spectral")
    # )(length(unique(tissues$tissue)))
    # names(tissue_cols) <- unique(levels(tissues$tissue))

    # source_selected <- reactive({
    #   dplyr::filter(pre_rank, source %in% input$source_select)
    # })
    #
    # observeEvent(source_selected(), {
    #   choices <- unique(source_selected()$tissue)
    #   shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    # })
    #
    # tissue_selected <- reactive({
    #   req(input$tiss_select)
    #   dplyr::filter(source_selected(), tissue %in% input$tiss_select)
    # })
    #
    # observeEvent(system_selected(), {
    #   choices <- unique(tissue_selected()$tissue)
    #   shinyWidgets::updatePickerInput(session, inputId = "sample_select", choices = choices)
    # })
    source_selected <- reactive({
      dplyr::filter(pre_rank, source %in% input$source_select)
    })

    observeEvent(source_selected(), {
      choices <- unique(source_selected()$tissue)
      shinyWidgets::updatePickerInput(session, inputId = "tiss_select", choices = choices)
    })

    tissue_selected <- reactive({
      # req(input$tiss_select)
      dplyr::filter(source_selected(), tissue %in% input$tiss_select)
    })

    observeEvent(tissue_selected(), {
      choices <- unique(tissue_selected()$sample)
      shinyWidgets::updatePickerInput(session, inputId = "sample_select", choices = choices)
    })

    sample_selected <- reactive({
      # dplyr::filter(tissue_selected(), sample %in% input$sample_select)
      req(input$sample_select)

      tissue_selected() %>%
        dplyr::filter(sample %in% input$sample_select) %>%
        dplyr::mutate(mir_id = paste(mir_names, id, sep = "&")) %>%
        dplyr::select(mir_id, mir_names, id, tissue_sample, rpm) %>%
        tidyr::pivot_wider(names_from = tissue_sample, values_from = rpm)

    })

    # generate rank list
    ranked_list <- reactive({
      # req(input$rpm_cutoff)

      list_ge_rpm <- lapply(sample_selected()[, -c(1:3)], function(col) {
        ids_ge_val <- sample_selected()$mir_id[col >= input$rpm_cutoff]
        ranked <- ids_ge_val[order(col[col >= input$rpm_cutoff], decreasing = TRUE)]
      })

      list_ge_rpm

    })

    # apply rra
    rra_scores <- reactive({
      RobustRankAggreg::aggregateRanks(ranked_list(), full = FALSE)
    })

    # top n names, colors, and ranks
    top_names <- reactive({
      rra_scores() %>%
        dplyr::arrange(Score) %>%
        dplyr::slice_head(n = 10) %>%
        dplyr::pull(Name)
    })

    mir_cols <- reactive({
      req(top_names())
      cols <- hcl.colors(n = length(top_names()), palette = "zissou")
      names(cols) <- top_names()

      return(cols)
    })
    # mir_cols <- hcl.colors(n = length(top_names()), palette = "zissou")
    # names(mir_cols) <- top_names()

    top_ranks <- reactive({
      req(top_names())
      # tops <- rra_scores() %>%
      #   dplyr::arrange(Score) %>%
      #   dplyr::slice_head(n = 10) %>%
      #   dplyr::pull(Name)

      ids_to_ranks <- function(ids) {
        sapply(top_names(), function(id) {
          if (id %in% ids) {
            return(match(id, ids))
          } else {
            return(NA)
          }
        })
      }

      df <- as.data.frame(
        do.call(
          rbind,
          lapply(ranked_list(), ids_to_ranks)
        )
      ) %>%
        tibble::rownames_to_column(var = "tissue_sample")

      df
    })

    # modify top_ranks to long format and include rank
    top_ranks_long <- reactive({
      req(top_ranks())

      top_ranks() %>%
        tidyr::pivot_longer(cols = -tissue_sample, names_to = "id", values_to = "rank") %>%
        dplyr::filter(!is.na(rank)) %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        dplyr::mutate(aggregated_rank = paste0("Rank ", match(id, top_names())))

    })

    tissue_summary <- reactive({

      top_ranks()

    })

    output$bump_chart <- renderPlot({
      # req(mir_cols())
      # shinipsum::random_ggplot(type = "line")

      # df_long <- top_ranks() %>%
      #   tidyr::pivot_longer(cols = -tissue_sample, names_to = "id", values_to = "rank") %>%
      #   dplyr::filter(!is.na(rank)) %>%
      #   dplyr::mutate_if(is.character, as.factor) %>%
      #   dplyr::mutate(ranked_label = paste0("Rank ", match(id, top_names())))

      # df_long$id <- factor(df_long$id, levels = rev(top_names()))

      reverse_log10 <- scales::trans_new(
        name = "reverse_log10",
        transform = function(x) -log10(x),
        inverse = function(x) 10^(-x),
      )

      custom_breaks <- c(1:10, 10^seq(2, 5))

      # mir_cols <- hcl.colors(n = length(top_names()), palette = "zissou")
      # names(mir_cols) <- top_names()

      last_level <- tail(levels(top_ranks_long()$tissue_sample), 1)

      ggplot2::ggplot(top_ranks_long(),
                  ggplot2::aes(
                    x = tissue_sample,
                    y = rank,
                    # color = id,
                    fill = id,
                    group = id
                  )) +
        ggplot2::geom_line(ggplot2::aes(color = id, alpha = 1), linewidth = 2) +
        ggplot2::geom_point(ggplot2::aes(color = id), shape = 21, size = 4, color = "black") +
        ggrepel::geom_label_repel(data = subset(top_ranks_long(), tissue_sample == last_level),
                                  ggplot2::aes(label = aggregated_rank),
                                  # direction = "y",
                                  box.padding = 0.8,
                                  point.padding = 0.8,
                                  segment.size = 0.2,
                                  # hjust = 1.5,
                                  # nudge_x = 0.5
                                  ) +
        ggplot2::scale_y_continuous(trans = reverse_log10, breaks = custom_breaks) +
        ggplot2::scale_color_manual(
          values = mir_cols()
        ) +
        ggplot2::scale_fill_manual(
          values = mir_cols()
        ) +
        ggplot2::theme_dark() +
        ggplot2::labs(x = "Tissue and sample", y = "RPM-based rank") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
          axis.title.x = ggplot2::element_text(size = 10),
          axis.text.y = ggplot2::element_text(size = 8),
          axis.title.y = ggplot2::element_text(size = 10),
          legend.position = "none"
        )

      # p + ggplot2::expand_limits(x = c(NA, 24))

    })

    output$box_plot <- plotly::renderPlotly({
      # req(mir_cols())
      # shinipsum::random_ggplot(type = "line")

      # df_long <- top_ranks() %>%
      #   tidyr::pivot_longer(cols = -tissue_sample, names_to = "id", values_to = "rank") %>%
      #   dplyr::filter(!is.na(rank)) %>%
      #   dplyr::mutate_if(is.character, as.factor)
      #
      # df_long$id <- factor(df_long$id, levels = rev(top_names()))

      # custom_breaks <- c(1:10, 10^seq(2, 5))

      # breaks <- c(1:10, seq(20, max(df_long$rank)))
      custom_breaks <- function(limits) {
        union(1:10, scales::log_breaks()(limits))
      }

      ggplot2::ggplot(
        top_ranks_long(),
        ggplot2::aes(
          x = factor(id, levels = rev(top_names())),
          y = rank,
          fill = id,
          # label = tissue_sample
        )) +
        ggplot2::geom_boxplot(alpha = 0.8) +
        ggplot2::scale_x_discrete(labels = function(x) sub("&.*", "", x)) +
        ggplot2::scale_y_log10(breaks = c(1:10, 10^seq(2, 5))) +
        ggplot2::scale_fill_manual(
          values = mir_cols()
        ) +
        ggplot2::labs(x = "Aggregated rank", y = "Rank") +
        ggplot2::theme_light() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 8),
          axis.title.x = ggplot2::element_text(size = 10),
          axis.text.y = ggplot2::element_text(size = 8),
          axis.title.y = ggplot2::element_text(size = 10),
          legend.position = "none",
        ) +
        ggplot2::coord_flip()

      # p <- plotly::ggplotly(p) %>%
      #   plotly::style(outliercolor = 'rgba(0,0,0,0)')
      #
      # return(p)

    })

    output$tiss_table <- DT::renderDT({
      # tissue_summary()
      top_ranks_long()
    })

    output$team_text = renderText(top_names())
    output$team_text2 = renderText(names(mir_cols()))

  })
}

## To be copied in the UI
# mod_RankAgg_ui("RankAgg_1")

## To be copied in the server
# mod_RankAgg_server("RankAgg_1")
