library(shiny)

sidebarUI <- function() {
  fluidPage(
    tags$div(
      id = "sidebar",
      class = "sidebar",
      hidden = TRUE,  # Hide the sidebar initially
      tags$img(src = "path_to_logo.png", width = "200px"), # Replace 'path_to_logo.png' with the actual path to your logo
      tags$hr(),
      tags$div(
        class = "menu-wrapper",
        tags$ul(
          class = "menu",
          tags$li("Data"),
          tags$ul(
            tags$li("Visualization"),
            tags$li("Analysis")
          ),
          tags$li("Interpretation")
        )
      ),
      actionButton("exitBtn", "Exit", class = "btn btn-danger mt-4")
    )
  )
}

sidebarServer <- function(input, output, session) {
  observeEvent(input$exitBtn, {
    session$close()
  })
  
  # Server logic goes here
}
