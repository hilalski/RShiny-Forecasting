library(shiny)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(plyr)
library(rintrojs)
library(DT)
library(forecast)
library(haven)
library(leaflet)
library(RColorBrewer)
library(sf)
library(dplyr)
library(readxl)

file_choices <- c("Temp in F, USA 1925 - 2022.csv")
df <- read.csv("Temp in F, USA 1925 - 2022.csv", header = TRUE, sep = ",")
fcast <- 1:10

ui <- dashboardPage(
  dashboardHeader(title = "Simple Forecast"),
  dashboardSidebar(
    sidebarMenu(style = "gap : 15px;",
      menuItem("Select Data", tabName = "pilih_data", icon = icon("database")),
      menuItem("Visualization", tabName = "visualisasikan_data", icon = icon("eye")),
      menuItem("Forecast", tabName = "forecast", icon = icon("line-chart")),
      menuItem("About", tabName = "tentang", icon = icon("info-circle")),
      actionButton("exitButton", "Exit")
    )
  ),
  dashboardBody(
     includeCSS("style.css"),
    tabItems(
      tabItem(tabName = "pilih_data",
              h2("Select Data"),
              hr(),
              sidebarPanel(
                h4("Pilih File Excel:"),
                selectInput("file", "File:", choices = file_choices),
                actionButton("submit", "Tampilkan Data")
              ),
              mainPanel(
                h4("Data yang Dipilih:"),
                tableOutput("selected_data")
              )
      ),
      tabItem(tabName = "visualisasikan_data",
              h2("Data Visualization"),
              hr(),
              fluidRow(
                column(width = 12,
                       selectInput("tahun", "Pilih Tahun", c(1925:2022))
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  leafletOutput("map")
                )
              )
      ),
      tabItem(tabName = "forecast",
              h2("Data Forecast"),
              hr(),
              fluidRow(
                column(
                  width = 4,
                  tabsetPanel(id = "tabs",
                              tabPanel("Forecasting", value = "Forecasting",
                                       selectInput("State", "State:",
                                                   choices = colnames(df[2:49])),
                                       helpText("Silakan pilih Negara Bagian, forecasting ini akan memberikan peramalan rata-rata suhu selama 9 Tahun Kedepan")
                              ),
                              tabPanel("Interpretation", value = "Interpretation",
                                       numericInput("Year", "Tahun:", min = 2023, max = 2031, value = 2023),
                                       actionButton("InterpretButton", "Interpretasi")
                              )
                  )
                ),
                column(
                  width = 8,
                  plotlyOutput("Plot1"),
                  div(
                    id = "interpretation",
                    style = "margin-top: 200px;",
                    verbatimTextOutput("interpretation")
                  )
                )
              )
      ),
      tabItem(tabName = "tentang",
              h2("About"),
              hr(),
              mainPanel(
                h3("Kelompok 4 2KS4"),
                br(),
                p("1. Irgi Fahrozi"),
                p("2. Oktavianto Asset P"),
                p("3. Sonya Ananta P"),
                p("4. Umar Hadi P"),
                p("5. Yanuar Nurul H")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  showWelcome <- reactiveVal(TRUE)
  
  output$welcomeUI <- renderUI({
    if (showWelcome()) {
      div(class = "center",
          div(
            img(src = "path_to_your_logo.png", width = 200, height = 200),
            h2("Your Tagline"),
            actionButton("continueButton", "Continue", class = "btn btn-primary")
          )
      )
    }
  })
  
  selected_data <- reactive({
    req(input$file)
    data <- read.csv(input$file)
    data
  })
  
  output$selected_data <- renderTable({
    if (input$submit > 0) {
      selected_data()
    }
  })
  
  observeEvent(input$continueButton, {
    showWelcome(FALSE)
  })
  
  observe({
    if (!showWelcome()) {
      updateTabItems(session, "sidebar", selected = "pilih_data")
    }
  })
  
  output$map <- renderLeaflet({
    data <- read_xlsx("Temp in F, USA 1925 - 2022.xlsx")
    usa <- read_sf("States_shapefile.shp")
    usa_map <- usa %>% dplyr::select(State_Name) %>% subset(State_Name != "DISTRICT OF COLUMBIA" & State_Name != "HAWAII")
    jml <- data %>% subset(Year == input$tahun) %>% dplyr::select(-Year)
    usa_map <- usa_map %>% tibble::add_column(t(jml))
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = usa_map$`t(jml)`
    )
    leaflet(usa_map) %>% 
      addTiles() %>%
      addPolygons(
        color = ~pal(`t(jml)`),
        weight = 2,
        opacity = 1,
        dashArray = "solid",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 1,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = paste0(usa_map$State_Name, " ", usa_map$`t(jml)`, " \u00B0F"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>% 
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = ~`t(jml)`,
        title = "Temperatur"
      )
  })
  
  forecast_values <- reactive({
    state_col <- colnames(df) == input$State
    state_data <- df[, state_col]
    arima_model <- auto.arima(state_data)
    forecast(arima_model, h = 9)
  })
  
  
  
  output$Plot1 <- renderPlotly({
    color_palette <- c("#4C78A8", "#F58518", "#E45756", "#72B7B2", "#54A24B", "#EECA3B", "#B279A2", "#FF9DA6")
    state_col <- colnames(df) == input$State
    state_data <- df[, state_col]
    forecast_values_data <- forecast_values()
    
    plot_ly() %>%
      add_trace(x = df$Year, y = state_data, name = 'Data sesungguhnya',
                type = 'scatter', mode = 'lines', line = list(color = "#4C78A8", width = 4)) %>%
      add_trace(x = c(2023:2031), y = forecast_values_data$mean, name = 'Data forecast',
                line = list(color = "#EECA3B", width = 4, dash = 'line')) %>%
      layout(title = 'FORECASTING',
             xaxis = list(title = 'Tahun', range = c(1925, 2031)),
             yaxis = list(title = 'Suhu'),
             margin = list(l = 80, r = 50, b = 50, t = 80),
             colorway = color_palette,
             legend = list(x = 0, y = -0.3, orientation = 'h'),
             height = 600)
  })
  
  
  interpretation <- reactiveVal("")
  
  observeEvent(input$InterpretButton, {
    forecast_values_data <- forecast_values()
    interpretation_val <- c(paste("Tahun", input$Year, ": Rata-rata suhu diprediksi sekitar", round(forecast_values_data$mean[input$Year - 2022], 2), "F"))
    interpretation(interpretation_val)
  })
  
  output$interpretation <- renderText({
    interpretation()
  })
  
  observeEvent(input$exitButton, {
    showModal(modalDialog(
      title = "Confirmation",
      "Are you sure you want to exit?",
      footer = tagList(
        actionButton("cancelExit", "Cancel"),
        modalButton("exit", "Exit")
      )
    ))
  })
  
  observeEvent(input$exit, {
    removeModal()
    stopApp()
  })
  
  observeEvent(input$cancelExit, {
    removeModal()
  })
}

# Handle sidebar menu item clicks
observeEvent(input$sidebarMenu, {
  selected_tab <- input$sidebarMenu
  updateTabItems(session, "sidebarMenu", selected = selected_tab)
})

shinyApp(ui, server)
