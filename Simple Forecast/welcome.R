library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .center {
          display: flex;
          justify-content: center;
          align-items: center;
          height: 100vh;
        }
      ")
    )
  ),
  
  div(class = "center",
      div(
        img(src = "path_to_your_logo.png", width = 200, height = 200),
        h2("Your Tagline"),
        actionButton("continueButton", "Continue to Main Page", class = "btn btn-primary")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$continueButton, {
    # Perform any necessary actions before navigating to the main page
    # For example, you can use updateQueryString() to append parameters to the URL
    
    # Navigate to the main page
    if (file.exists("main.r")) {
      source("main.r", local = TRUE)
    } else {
      # Handle the case when 'main.r' does not exist
      # You can redirect to an error page or display an error message
      showNotification("Main page not found.", type = "error")
    }
  })
}


shinyApp(ui, server)
