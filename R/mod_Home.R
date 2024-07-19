#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Home_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .home-container {
          text-align: center;
          margin-top: 20px;
        }
        .home-text {
          margin-bottom: 20px;
          text-align: left;
          display: inline-block;
          width: 100%;
          max-width: 800px;
          margin-left: auto;
          margin-right: auto;
        }
        .home-text h2 {
          text-align: center;
        }
        .image-container {
          display: flex;
          justify-content: center;
          flex-wrap: wrap;
        }
        .image-box {
          flex: 1;
          margin: 10px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
          border-radius: 8px;
          overflow: hidden;
          max-width: 250px;
          max-height: 250px; /* Added to set max height */
        }
        .image-box img {
          width: 100%;
          height: 100%;
          object-fit: cover; /* Ensures image covers the container */
          display: block;
        }
      "))
    ),
    div(class = "home-container",
        div(class = "home-text",
            h2("Welcome to AIMEE"),
            p("AIMEE (Animal IsomiR and MiRNA Expression Explorer) currently hosts miRNA and isomiR expression data across 461 equine small RNA-seq samples, spanning 61 tissue types."),
            p("Included tissues were sourced from:"),
            shiny::tags$ul(
              shiny::tags$li("12 American Quarter Horses (n=221)"),
              shiny::tags$li("4 FAANG Thoroughbreds (n=82)"),
              shiny::tags$li("Public data repositories (n=158)")
            ),
            p("All samples were processed using FARmiR (REF)."),
            p(tags$b("NOTE:"), " IsoMiRmap defines 'miRNA-space' as the union of all miRNA precursors. An miRNA (or isomiR) is tagged as 'exclusive' if its sequence is found only within miRNA-space. Alternatively, if the miRNA sequence is found anywhere in the EquCab3 genome outside of the miRNA-space, it is tagged as 'ambiguous.' Independent of ambiguous/exclusive tagging, information regarding whether a sequence overlaps with known repeat elements is accessible from the Downloads tab. For more information, see the ",
              a("IsoMiRmap", href = "https://cm.jefferson.edu/isomirmap/", target = "_blank"), "documentation.")
        ),
        div(class = "image-container",
            div(class = "image-box",
                img(src = "www/horse_01.jpg")
            ),
            div(class = "image-box",
                img(src = "www/horse_02.jpg")
            ),
            div(class = "image-box",
                img(src = "www/horse_03.jpg")
            )
        )
    )
  )
}

#' Home Server Functions
#'
#' @noRd
mod_Home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")
