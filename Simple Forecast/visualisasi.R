library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(dplyr)
library(readxl)

ui <- fluidPage(
  fluidRow(
    column(width = 12,
           # Konten baris pertama
           selectInput("tahun", "Pilih Tahun", c(1925:2022))
    )
  ),
  fluidRow(
    column(
      width = 12,
      # Konten baris pertama
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    # Menampilkan opsi yang dipilih dari dropdown
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
          bringToFront = TRUE),
        label = paste0(usa_map$State_Name," ",usa_map$`t(jml)`," \u00B0F"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = ~`t(jml)`,
        title = "Temperatur"
      )
  })
}

shinyApp(ui, server)
