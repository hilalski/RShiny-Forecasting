ibrary(shiny)
library(plotly)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(plyr)
library(rintrojs)
library(DT)
library(forecast)
library(haven)

options(shiny.autoreload = TRUE)

df <- read.csv("Temp in F, USA 1925 - 2022.csv", header = TRUE, sep = ",")
fcast <- 1:10

# Define UI
ui <- fluidPage(
  tabPanel("Forecasting",
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 tabsetPanel(id = "tabs",
                             tabPanel("Forecasting", value = "Forecasting",
                                      selectInput("State", "State:",
                                                  choices = colnames(df[2:49])),
                                      hr(),
                                      helpText("Silakan pilih Negara Bagian, forecasting ini akan memberikan peramalan rata-rata suhu selama 8 Tahun Kedepan")),
                             tabPanel("Interpretation", value = "Interpretation",
                                      numericInput("Year", "Tahun:", min = 2023, max = 2031, value = 2023),
                                      actionButton("InterpretButton", "Interpretasi"))
                 )
               ),
               mainPanel(
                 plotlyOutput("Plot1"),
                 div(style = "margin-top: 20px;",
                     verbatimTextOutput("interpretation"))
               )
             )
           )
  )
)

server <- function(input, output) {
  forecast_values <- reactive({
    state_col <- colnames(df) == input$State
    state_data <- df[, state_col]
    arima_model <- auto.arima(state_data)
    forecast(arima_model, h = 9)
  })
  
  output$Plot1 <- renderPlotly({
    state_col <- colnames(df) == input$State
    state_data <- df[, state_col]
    forecast_values_data <- forecast_values()
    
    # Plot the historical data and forecast
    plot_ly() %>%
      add_trace(x = df$Year, y = state_data, name = 'Data sesungguhnya',
                type = 'scatter', mode = 'lines', line = list(color = 'cyan', width = 4)) %>%
      add_trace(x = c(2023:2031), y = forecast_values_data$mean, name = 'Data forecast',
                line = list(color = 'red', width = 4, dash = 'line')) %>%
      layout(title = 'FORECASTING',
             xaxis = list(title = 'Tahun', range = c(1925, 2031)),
             yaxis = list(title = 'Suhu'),
             margin = list(l = 80, r = 50, b = 50, t = 80))
  })
  
  interpretation <- reactiveVal("")
  
  observeEvent(input$InterpretButton, {
    forecast_values_data <- forecast_values()
    interpretation_val <- paste("Interpretasi forecast untuk negara bagian", input$State, "pada tahun", input$Year, ":")
    interpretation_val <- c(interpretation_val, "")
    interpretation_val <- c(interpretation_val, paste("Tahun", input$Year, ": Rata-rata suhu diprediksi sekitar", round(forecast_values_data$mean[input$Year - 2022], 2), "F"))
    interpretation(interpretation_val)
  })
  
  output$interpretation <- renderText({
    interpretation()
  })
}

# Run the application
shinyApp(ui = ui, server = server)