#' ProcExplore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ProcExplore_ui <- function(id){
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
          width = 2,
          shinyWidgets::pickerInput(
            ns("x_axis"),
            label = "X axis",
            choices = c(unique(na.omit(as.character(proc_cts$step))), "Ref_miRs", "IsomiRs"),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              # `actions-box` = TRUE,
              `live-search` = TRUE
              # `selected-text-format`= "count",
              # `count-selected-text` = "{0} samples selected"
            )
          ),
          shinyWidgets::pickerInput(
            ns("y_axis"),
            label = "Y axis",
            choices = c(unique(na.omit(as.character(proc_cts$step))), "Ref_miRs", "IsomiRs"),
            options = list(
              `virtualScroll` = 10,
              size = 10,
              # `actions-box` = TRUE,
              `live-search` = TRUE
              # `selected-text-format`= "count",
              # `count-selected-text` = "{0} samples selected"
            )
          )
        ),
        column(
          width = 10,
          shinydashboard::box(
            width = 12,
            plotOutput(ns("scatter"))
            # plotly::plotlyOutput(ns("scatter"))
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
        shinydashboard::box(
          # title = "MTCARS",
          width = 12,
          DT::dataTableOutput(ns("cent_table"))
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

#' ProcExplore Server Functions
#'
#' @noRd
mod_ProcExplore_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tissue_selected <- reactive({
      req(input$tiss_select)
      dplyr::filter(proc_cts, tissue %in% input$tiss_select)
    })

    # observeEvent(tissue_selected(), {
    #   choices <- unique(tissue_selected()$source)
    #   shinyWidgets::updatePickerInput(session, inputId = "source_select", choices = choices)
    # })
    #
    # source_selected <- reactive({
    #   req(input$source_select)
    #   dplyr::filter(tissue_selected(), source %in% input$source_select)
    # })

    quote_col_name <- function(col_name) {
      if (grepl("[[:space:]|-]", col_name)) {
        return(paste0("`", col_name, "`"))
      }
      return(col_name)
    }

    f <- function(z)sd(z)/sqrt(length(z)) # function to calculate std.err

    tissue_cols <- colorRampPalette(
      rev(RColorBrewer::brewer.pal(11, "Spectral"))
    )(length(unique(tissues$tissue)))
    names(tissue_cols) <- unique(levels(tissues$tissue))

    tissue_cols["NA"] <- "grey90"


    plt_df <- reactive({

      req(input$tiss_select)

      # REQUIRE USER RPM VALUE
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

      proc_mirs <- proc_cts %>%
        tidyr::pivot_wider(names_from = step, values_from = count) %>%
        dplyr::left_join(tmp, by = "tissue_sample") %>%
        dplyr::mutate(
          tissue_pop = as.factor(ifelse(tissue %in% input$tiss_select, as.character(tissue), "")),
          tissue_alpha = ifelse(tissue %in% input$tiss_select, 1, 0.2)
        )

      return(proc_mirs)
    })

    X <- rlang::sym("Library size")
    # X <- reactive({
    #   req(input$x_axis)
    #   # rlang::sym(input$x_axis)
    #   return(input$x_axis)
    # })

    Y <- rlang::sym("Pre-quantification with\nmaximum length (25)")
    # Y <- rlang::sym("Adapters removed")
    # Y <- reactive({
    #   req(input$y_axis)
    #   # REQUIRE Y
    #   # rlang::sym("canons")
    #   return(input$y_axis)
    # })

    # form <- as.formula(paste("cbind(", quote_col_name(X),
    #                          ", ", quote_col_name(Y),
    #                          ") ~ tissue", sep = ""))
    #
    # form_se <- as.formula(paste("cbind(se.raw_in = ", quote_col_name(X),
    #                             ", se.total = ", quote_col_name(Y),
    #                             ") ~ tissue", sep = ""))

    centroids <- reactive({

      req(input$tiss_select, input$x_axis, input$y_axis)
      # req(input$source_select)

      # x_col <- X()
      # y_col <- Y()
      x_col <- quote_col_name(input$x_axis)
      y_col <- quote_col_name(input$y_axis)

      # OLD VERSION
      form <- as.formula(paste("cbind(", quote_col_name("Library size"),
                               ", ", quote_col_name(Y),
                               ") ~ tissue", sep = ""))

      form_se <- as.formula(paste("cbind(se.X = ", quote_col_name("Library size"),
                                  ", se.Y = ", quote_col_name(Y),
                                  ") ~ tissue", sep = ""))

      # NEW VERSION
      # form <- as.formula(paste("cbind(", quote_col_name(x_col),
      #                          ", ", quote_col_name(y_col),
      #                          ") ~ tissue"))
      #
      # form_se <- as.formula(paste("cbind(se.X = ", quote_col_name(x_col),
      #                             ", se.Y = ", quote_col_name(y_col),
      #                             ") ~ tissue"))

      # OLD VERSION
      means <- aggregate(form, data = plt_df(), FUN = mean)
      se <- aggregate(form_se, data = plt_df(), FUN = f)

      # NEW VERSION
      # means <- aggregate(form, data = plt_df(), FUN = mean)
      # se <- aggregate(form_se, data = plt_df(), FUN = f)

      # OLD VERSION
      df <- merge(means, se) %>%
        dplyr::left_join(plt_df() %>% dplyr::select(tissue, new_lab) %>% dplyr::distinct(), by = "tissue") %>%
        dplyr::mutate(
          se.X = round(se.X, 2),
          se.Y = round(se.Y, 2),
          !!X := round(!!X, 2),
          !!Y := round(!!Y, 2),
          tissue_pop = as.factor(ifelse(tissue %in% input$tiss_select, as.character(tissue), "NA")),
          tissue_alpha = ifelse(tissue %in% input$tiss_select, 1, 0.2),
          tissue_name = ifelse(tissue %in% input$tiss_select, as.character(new_lab), "")
        )

      # NEW VERSION
      # df <- merge(means, se) %>%
      #   dplyr::left_join(plt_df() %>% dplyr::select(tissue, new_lab) %>% dplyr::distinct(), by = "tissue") %>%
      #   dplyr::mutate(
      #     se.X = round(se.X, 2),
      #     se.Y = round(se.Y, 2),
      #     !!rlang::sym(x_col) := round(!!rlang::sym(x_col), 2),
      #     !!rlang::sym(y_col) := round(!!rlang::sym(y_col), 2),
      #     tissue_pop = as.factor(ifelse(tissue %in% input$tiss_select, as.character(tissue), "NA")),
      #     tissue_alpha = ifelse(tissue %in% input$tiss_select, 1, 0.2),
      #     tissue_name = ifelse(tissue %in% input$tiss_select, as.character(new_lab), "")
      #   )

      # form <- as.formula(paste("cbind(", x_col, ", ", y_col, ") ~ tissue"))
      # means <- aggregate(form, data = plt_df(), FUN = mean)

      return(df)
    })
    # centroids <- aggregate(cbind(`Pre-quantification with\nmaximum length (25)`, canons) ~ tissue, proc_mirs, mean)
    # centroids <- aggregate(form, data = proc_mirs, FUN = mean)

    # se <- aggregate(cbind(se.raw_in = `Pre-quantification with\nmaximum length (25)`, se.total = canons) ~ tissue, proc_mirs, f)
    # se <- aggregate(form_se, data = proc_mirs, FUN = f)

    # TISSUE SELECT TO HIGHLIGHT
    # tissue_select <- c("serum", "heart_left_ventricle", "leukocyte", "adrenal_cortex", "whole_blood", "testis")
    # centroids <- merge(centroids, se) %>%
    #   plyr::mutate(
    #     tissue_pop = as.factor(ifelse(tissue %in% tissue_select, as.character(tissue), "NA")),
    #     tissue_alpha = ifelse(tissue %in% tissue_select, 1, 0.2)
    #   )



    # labels for last step
    # label_data <- reactive({
    #   plt_df() %>%
    #     dplyr::group_by(source) %>%
    #     dplyr::filter(step == tail(levels(step), 1))
    # })

    # output$scatter <- plotly::renderPlotly({
    output$scatter <- renderPlot({
      # output$line_chart <- renderPlot({
      # req(plt_df())
      # req(centroids())
      # req(label_data())
      # req(input$sum_type)

      ggplot2::ggplot(plt_df(),
                      ggplot2::aes(
                        x = !!X,
                        y = !!Y,
                        # color = new_lab
                      )
      ) +
        ggplot2::geom_errorbarh(
          data = centroids(),
          height = 0.1,
          ggplot2::aes(
            xmin = !!X - se.X,
            xmax = !!X + se.X,
            alpha = tissue_alpha
          )
        ) +
        ggplot2::geom_errorbar(
          data = centroids(),
          width = 0.1,
          ggplot2::aes(
            ymin = !!Y - se.Y,
            ymax = !!Y + se.Y,
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
        # ggplot2::scale_x_continuous(labels = scales::label_comma()) +
        ggrepel::geom_label_repel(
          data = centroids(),
          ggplot2::aes(label = tissue_name),
          max.overlaps = 100,
          box.padding = 0.9,
          segment.linetype = 3,
        ) +
        # ggplot2::scale_x_log10(labels = scales::comma) +
        # ggplot2::scale_y_log10(labels = scales::comma) +
        ggplot2::scale_color_manual(
          values = tissue_cols
        ) +
        ggplot2::scale_fill_manual(
          values = tissue_cols
        ) +
        # ggplot2::scale_alpha_manual(
        #   values = tissue_alpha
        # ) +
        ggplot2::theme_dark() +
        ggplot2::theme(
          legend.position = "none"
        )

    })

    output$tiss_table <- DT::renderDT({

      plt_df() %>%
        dplyr::filter(tissue %in% input$tiss_select) %>%
        dplyr::select(
          source, tissue, new_lab, sample, breed, sex,
          !!X,
          !!Y,
          # dplyr::everything(),
          -c(tissue_sample, system, abvs, source_mod, tissue_pop, tissue_alpha)
        )
      # out <- plt_df() %>%
      #   dplyr::rename(tissue = new_lab)

      # if(input$sum_type == "samples") {
      #   out <- plt_df() %>%
      #     dplyr::select(-c(tissue_sample)) %>%
      #     tidyr::pivot_wider(names_from = step, values_from = count)
      # }

      # return(out)

    })

    output$cent_table <- DT::renderDT({
      # req(input$tiss_select)
      centroids() %>%
        # dplyr::select(tissue, new_lab, !!X, se.X, !!Y, se.Y) %>%
        dplyr::select(tissue, !!X, se.X, !!Y, se.Y) %>%
        dplyr::filter(tissue %in% input$tiss_select)
    })

    # output$team_text = renderText(colnames(plt_df()))
    # output$team_text = renderText(X())
    # output$team_text = renderText(names(plt_df))
    # output$team_text2 = renderText(names(mir_cols()))

  })
}

## To be copied in the UI
# mod_ProcExplore_ui("ProcExplore_1")

## To be copied in the server
# mod_ProcExplore_server("ProcExplore_1")
