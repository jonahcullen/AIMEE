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
          numericInput(
            ns("rpm_cutoff"),
            label = HTML("&ge; RPM cutoff"),
            value = 20,
            min = 0
          )
        ),
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
        div(
          style = "display: flex; justify-content: center; margin-bottom: 20px;",
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_bump"), "Bump chart")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_box"), "Box plot")),
          div(style = "margin-right: 10px;", shiny::downloadButton(ns("download_table"), "Rank table")),
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$p(
            "Rank aggregation is a method used to combine multiple ranked lists into a single consensus ranking. This approach helps identify the most consistently top-ranked items across different samples or conditions. For AIMEE, rank aggregation allows for the identification of miRNAs that are consistently highly expressed across multiple datasets or sources. Rank aggregation is implemented with the R package ",
            tags$a(href = "https://cran.r-project.org/web/packages/RobustRankAggreg/RobustRankAggreg.pdf", "RobustRankAggreg", target = "_blank"),
            "."
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
        shinydashboard::box(
          width = 12,
          DT::dataTableOutput(ns("tiss_table"))
        )
      )
    )
  )
}

#' RankAgg Server Functions
#'
#' @noRd
mod_RankAgg_server <- function(id, mirna_space){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # unclear if keeping version information in exports
    version <- "v0.9"

    # pre-filter based on mirna space
    spaced_data <- reactive({
      req(mirna_space())

      data <- pre_rank
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

      spaced_data() %>%
        dplyr::filter(source %in% input$source_select) %>%
        dplyr::group_by(tissue) %>%
        dplyr::filter(dplyr::n_distinct(source) == length(input$source_select)) %>%
        dplyr::ungroup() #%>%
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
      req(source_selected())
      validate(
        need(input$tiss_select, "please select at least one tissue"),
      )

      dplyr::filter(source_selected(), tissue %in% input$tiss_select)
    })

    observeEvent(tissue_selected(), {
      choices <- unique(tissue_selected()$sample)
      shinyWidgets::updatePickerInput(session, inputId = "sample_select", choices = choices)
    })

    sample_selected <- reactive({
      req(tissue_selected())
      validate(
        need(input$sample_select, "please select at least one sample"),
      )

      tissue_selected() %>%
        dplyr::filter(sample %in% input$sample_select) %>%
        dplyr::mutate(mir_id = paste(mir_names, id, sep = "&")) %>%
        dplyr::select(mir_id, mir_names, id, tissue_sample, rpm) %>%
        tidyr::pivot_wider(names_from = tissue_sample, values_from = rpm)

    })

    # generate rank list
    ranked_list <- reactive({
      req(sample_selected(), input$rpm_cutoff)

      list_ge_rpm <- lapply(sample_selected()[, -c(1:3)], function(col) {
        ids_ge_val <- sample_selected()$mir_id[col >= input$rpm_cutoff]
        ranked <- ids_ge_val[order(col[col >= input$rpm_cutoff], decreasing = TRUE)]
      })
      list_ge_rpm
    })

    # apply rra
    rra_scores <- reactive({
      validate(
        need(length(ranked_list()) > 0, "no ranked lists available")
      )

      RobustRankAggreg::aggregateRanks(ranked_list(), full = FALSE)
    })

    # top n names, colors, and ranks
    top_names <- reactive({
      # req(rra_scores())
      validate(
        need(nrow(rra_scores()) > 0, "no ranks available")
      )

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

    top_ranks <- reactive({
      req(top_names())

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

    p_bump <- reactive({
      req(top_ranks_long(), mir_cols())
      validate(
        need(input$sample_select, "XXXplease select at least one sample")
      )

      reverse_log10 <- scales::trans_new(
        name = "reverse_log10",
        transform = function(x) -log10(x),
        inverse = function(x) 10^(-x),
      )

      custom_breaks <- c(1:10, 10^seq(2, 5))
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

    })

    output$bump_chart <- renderPlot({
      p_bump()
    })

    output$download_bump <- downloadHandler(
      filename = function() {
        paste("aimee_rank_agg.bump.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 8, height = 6, units = "in", res = 300)
        print(p_bump())
        dev.off()
      }
    )

    p_box <- reactive({
      req(top_ranks_long(), top_names())
      validate(
        need(input$tiss_select, "XXXplease select at least one tissue")
      )

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

    })

    output$box_plot <- plotly::renderPlotly({
      # common_validate()
      p_box()
    })

    output$download_box <- downloadHandler(
      filename = function() {
        paste("aimee_rank_agg.box.", version, ".png", sep = "")
      },
      content = function(file) {
        ragg::agg_png(file, width = 8, height = 6, units = "in", res = 300)
        print(p_box())
        dev.off()
      }
    )

    output$download_table <- downloadHandler(
      filename = function() {
        paste("aimee_rank_agg.", version, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(top_ranks_long(), file, quote = FALSE, row.names = FALSE)
      }
    )

    output$tiss_table <- DT::renderDT({
      top_ranks_long()
    })
  })
}

## To be copied in the UI
# mod_RankAgg_ui("RankAgg_1")

## To be copied in the server
# mod_RankAgg_server("RankAgg_1")
