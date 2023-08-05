library(shiny)

# Definisi UI
ui <- fluidPage(
  titlePanel("Kelompok Forcasttt"),
  sidebarLayout(
    mainPanel(
      h3("Tentang Kelompok Forcasttt"),
      p("Kami adalah kelompok yang berkomitmen untuk melakukan prediksi dan analisis data dengan menggunakan berbagai metode dan algoritma."),
      h4("Anggota Kelompok:"),
      br(),
      p("1. Irgi Fahrozi"),
      p("2. Oktavianto Asset P"),
      p("3. Sonya Ananta P"),
      p("4. Umar Hadi P"),
      p("5. Yanuar Nurul H")
    )
  )
)

# Definisi server
server <- function(input, output) {
  
}

# Menjalankan aplikasi Shiny
shinyApp(ui = ui, server = server)
