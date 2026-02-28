# UI
app_ui <- function() {
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "app/www/menu.css"),
      shiny::tags$script(src = "app/www/menu.js")
    ),
    shiny::uiOutput("page")
  )
}