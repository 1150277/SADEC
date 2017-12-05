## testes ##
## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Smart House Control",
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  )
                  
  ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Controls", tabName = "controls", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Histogram", background = "maroon", solidHeader = TRUE,plotOutput("plot1", height = 250))
                
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "controls", fluidRow(
        box(
          title = "Controls Ambient Temperature",background = "maroon", solidHeader = TRUE,
          sliderInput("slider", " Ambient Temperature Celsius Degrees:", 10, 30, 21)
        )
      ),
      fluidRow(
        box(
          title = "Controls Water Temperature",background = "blue", solidHeader = TRUE,
          sliderInput("slider", " Water Temperature Celsius Degrees:", 10, 40, 25)
        )
      )
      
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)