library(shiny)

# List nama file Excel
file_choices <- c("data1.xlsx", "data2.xlsx", "data3.xlsx")  # Ganti dengan nama file Excel yang sesuai

# Definisikan UI
ui <- fluidPage(
  titlePanel("Pemilihan Data"),
  sidebarLayout(
    sidebarPanel(
      h4("Pilih File Excel:"),
      selectInput("file", "File:", choices = file_choices),
      actionButton("submit", "Tampilkan Data")
    ),
    mainPanel(
      h4("Data yang Dipilih:"),
      tableOutput("selected_data")
    )
  )
)

# Definisikan server
server <- function(input, output) {
  selected_data <- reactive({
    data <- read.csv(input$file)  # Mengganti read.csv dengan fungsi yang sesuai untuk membaca file Excel (misalnya readxl::read_excel)
    data
  })
  
  output$selected_data <- renderTable({
    if (input$submit > 0) {
      selected_data()
    }
  })
}

# Menjalankan aplikasi Shiny
shinyApp(ui = ui, server = server)
