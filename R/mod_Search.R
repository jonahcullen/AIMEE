#' Search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Search_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Search Server Functions
#'
#' @noRd 
mod_Search_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Search_ui("Search_1")
    
## To be copied in the server
# mod_Search_server("Search_1")
