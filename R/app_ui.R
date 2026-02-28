# UI
app_ui <- function() {
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "app/taxaSelect.css"),
      shiny::tags$script(src = "app/taxaSelect.js")
    ),
    shiny::uiOutput("page")
  )
}