
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load("thematic","bslib","bs4Dash","DT","pacman","dplyr","lubridate","plotly","shiny","fontawesome")

niq_logo = "https://scontent.fmex5-1.fna.fbcdn.net/v/t39.30808-6/313409870_5902282423149472_489567208425163943_n.jpg?_nc_cat=1&ccb=1-7&_nc_sid=09cbfe&_nc_eui2=AeH9G_emx7IdBa7ZrDMP8SUshs2Lgk4hNkeGzYuCTiE2Ry3x0aWaj-Vr2z57TamCGQp6D1U93qoZGTOM0QUCC5XF&_nc_ohc=Fzpey1RlHo4AX9d53QT&_nc_ht=scontent.fmex5-1.fna&oh=00_AfCKxAdojvHvlUNY4Mj91aFq9VQ93RKP2lbHNPzQYTNWlw&oe=63C5034B"

ui_2 <- dashboardPage(
  dashboardHeader(title = "BR - SO", img(src = niq_logo, height = (90), width = 90)),
  dashboardSidebar(
    sidebarMenu(
      menuItem("RESUMEN", tabName = "RESUMEN",icon = icon("layer-group")),
      menuItem("ANÁLISIS", tabName = "ANALISIS",icon = icon("bar-chart")),
      menuItem("MIXER", tabName = "MIXER",icon = icon("atom")),
      menuItem("REPORTE", tabName = "REPORTE",icon = icon("clipboard-check"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "RESUMEN",
              bs4Card(
                title = "NIVEL MERCADO",width  =12,collapsible = FALSE,
                div(DTOutput("tabla_1"), style = "font-size: 100%; width: 80%")
              ),
              
              bs4Card(
                title = "VENTAS HISTORICAS",width = 12,collapsible = FALSE,
                
                sliderInput(inputId = "bins",
                            label = "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30),
                
                plotlyOutput(outputId = "distPlot")
                
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}



shinyApp(ui = ui_2, server = server)


