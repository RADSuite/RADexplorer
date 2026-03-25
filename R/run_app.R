#' Run the RADexplorer application
#'
#' Launches the Shiny application for RADexplorer.
#'
#' @return A Shiny app object.
#' @export
run_app <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
