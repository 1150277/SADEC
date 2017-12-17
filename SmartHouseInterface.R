## testes ##
## app.R ##
## teste ##
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinymaterial)

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
                                        "Shopping"
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
      menuItem("Controls", tabName = "controls", icon = icon("th")),
      menuItem("Events", tabName = "events", icon = icon("calendar")),
      menuItem("Charts", tabName = "charts", icon = icon("area-chart")),
      menuItem("Config", tabName = "config", icon = icon("home")),
      menuItem("Help", tabName = "help", icon = icon("question"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Histogram", background = "maroon", solidHeader = TRUE,plotOutput("plot1", height = 250))
              ),
              # infoBoxes with fill=FALSE
              fluidRow(
                # A static infoBox
                infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "controls", 
      fluidRow(
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
      ),
      fluidRow(
        box(
          title = "Controls Sound Volume ",background = "green", solidHeader = TRUE,
          sliderInput("slider", " Sound Volume Control:", 0, 100, 35)
        )
      ),
      fluidRow(
        box(
          title = "Controls Light Intensity",background = "yellow", solidHeader = TRUE,
          sliderInput("slider", " Light Intensity:", 0, 100, 75)
        )
      )
      
      ),
      # Third tab content
      tabItem(tabName = "events",
              fluidRow(
                box(title = "Histogram", background = "blue", solidHeader = TRUE,plotOutput("plot2", height = 250))
                
                
              )
      ),
      # Fourth tab content
      tabItem(tabName = "charts",
              fluidRow(
                box(title = "Histogram", background = "green", solidHeader = TRUE,plotOutput("plot3", height = 250))
                
                
              )
      ),
      # Fifth tab content
      tabItem(tabName = "config",
              fluidRow(
                box(title = "Rooms Config", background = "red", solidHeader = TRUE,numericInput("num1", h3("How many rooms do you have:"), value = 1)),
                box(title = "People Config", background = "green", solidHeader = TRUE,numericInput("num2", h3("How many people live in the house:"), value = 1)),
                box(title = "Swimming Pool Config", background = "blue", solidHeader = TRUE,checkboxInput("checkbox1", "Yes i have a Swimming Pool", value = TRUE) ),
                box(title = "Garden Config", background = "yellow", solidHeader = TRUE,checkboxInput("checkbox2", "Yes i have a Garden", value = TRUE) )
                  
              )
      ),
      # Six tab content
      tabItem(tabName = "help",
              fluidRow(
                box(title = "Help Page", "If you need more help please visit our website...", 
                                    actionButton(inputId='ab1', label="Visit Website", 
                                                        icon = icon("th"), 
                                                        onclick ="window.open('https://www.control4.com/solutions/smart-home-overview', '_blank')")
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
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  
  
}

shinyApp(ui, server)