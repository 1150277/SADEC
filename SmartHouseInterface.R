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
      menuItem("Help", tabName = "help", icon = icon("question")),
      menuItem("Teste", tabName = "teste", icon = icon("question"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              tags$iframe( seamless = "seamless",  src="https://forecast.io/embed/#lat=41.1495&lon=-8.6108&name=Porto&color=#00aaff&font=Georgia&units=ca",
                height = 300, width = 300
              ),
              
             
              tags$iframe( src="http://www.youtube.com/embed/Ko2WZrUV1QI?autoplay=1", width=600 ,height=300, frameborder=0),
              
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
                box(title = "Event Date", background = "blue", solidHeader = TRUE,
                    (selectInput("select", h3("Choose Event"),choices = list("Barbecue" = 1, "Lunch" = 2, "Dinner" = 3), selected = 1)) ,   
                (dateInput("date",h3("Choose Date"),value = "2018-01-01") ),
                (sliderInput("slider1", h3("Choose Hour"),min = 6, max = 23, value = 21)),
                 submitButton("Submit")
                   
                )
              ),
              mainPanel(
                box(title = "Recommendations", background = "green", solidHeader = TRUE,textOutput("selected_var"),textOutput("min_max"))
              )
              
      ),
      # Fourth tab content
      tabItem(tabName = "charts",
              fluidRow(
                box(title = "Ambient Temperature Histogram", background = "green", solidHeader = TRUE,plotOutput("plot2", height = 250)),
                box(title = " Water Temperature Histogram", background = "yellow", solidHeader = TRUE,plotOutput("plot3", height = 250))
                
                
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
                ))),
              # Seventh tab content
              tabItem(tabName = "teste",
                      fluidRow(
                        box(title = "Organize event", "Choose your options and then click OK",width = 10, 
                        shinyApp(
                          ui = fluidPage(
                            selectInput("tipoevento", "Choose event type:",list("churrascada","outro")),
                              textOutput("tipoevento_result"),
                            selectInput("clima", "Choose type of weather:",list("Quente","Ameno")),
                              textOutput("clima_result"),
                            actionButton("goButton", "Run"),
                            textOutput("message")
                            ,tableOutput("table"),
                            #isto oculta uma mensagem de erro que não estou a conseguir perceber
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }")
                          ),
                          server = function(input, output) {
                            #output$tipoevento_result <- renderText({paste("You chose", input$tipoevento)})
                            #output$clima_result <- renderText({paste("You chose", input$clima)})
 
                            runandmessage <- eventReactive(input$goButton, ({
                              
                              # RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
                              #install.packages("lubridate")
                              #install.packages("plyr")
                              #install.packages("arules")
                              #install.packages("data.table")
                              #install.packages("DT")
                              library("lubridate")
                              library("plyr")
                              library("arules")
                              library("DT")
                              library("data.table")
                              rm(list=ls())
                              setwd("C:/Users/jferreira/Dropbox/SADEC/Projecto 3")
                              #kdb = knowledge database
                              kdb = read.table(file = "datasetTeste1.csv", header = T, sep = ";")
                              #criar coluna recomendação = tipo.config + valor
                              kdb$recomendacao=sapply(paste0(kdb$tipo.config),as.factor)
                              #eliminar coluna id, tipo.config e valor
                              kdb$id <- NULL
                              kdb$tipo.config <- NULL
                              kdb$source <- NULL
                              #filtrar condições -------------------------------------------------------------------
                              kdb <- kdb[!kdb$Clima != paste(input$clima), ]
                              kdb <- kdb[!kdb$Hora != "Dia", ]
                              kdb <- kdb[!kdb$tipo.casa != "tipo1", ]
                              kdb <- kdb[!kdb$tipo.evento != paste(input$tipoevento), ]
                              #apriori
                              rules <- apriori(kdb,control = list(verbose=F),
                                               parameter = list(minlen=5, supp=0.001, conf=0.001))
                              rules<- sort(rules, by="confidence")
                              
                              rules <- subset(rules, subset = rhs %pin% "recomendacao=")
                              rules <- subset(rules, subset = lhs %pin% "valor=")
                              rules <- subset(rules, subset = lhs %ain% c( paste0("tipo.evento=",input$tipoevento)
                                                                          ,paste0("Clima=",input$clima)
                                                                          ,"Hora=Dia","tipo.casa=tipo1"))
                              
                              #write(rules, file="rules", sep=",", quote=TRUE, row.names=FALSE)                 
                              myruleslist <- data.table( lhs = labels( lhs(rules) ),rhs = labels( rhs(rules) ), 
                                                      quality(rules) )[ order(-lift), ]
                              df_myruleslist<-as(myruleslist,"data.frame")
                              df_myruleslist$recomendation<-substr(df_myruleslist$rhs,regexpr('=', df_myruleslist$rhs)+1,nchar(df_myruleslist$rhs)-1)
                              df_myruleslist$value<-substr(df_myruleslist$lhs,
                                                          regexpr('valor=', df_myruleslist$lhs)+6,nchar(df_myruleslist$lhs))
                              df_myruleslist$value<-substr(df_myruleslist$value,1,regexpr(',', df_myruleslist$value)-1)

                              recomendations<-df_myruleslist[c("recomendation", "value","confidence")]
                              # RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
                              output$table <- renderTable ({recomendations[1:3]})

                            })
                            ) 
                            
  
                            output$message <- renderText ({runandmessage()})
                          } #close function
                      )  #close shinyapp
                     )#close box
                    )#close fluidrow
                        
              )#close tabitem
 
    ) #close tabitens
  ) #close dashoboardbody
) #close dashboardPage

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  output$plot3 <- renderPlot({
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
  
  output$selected_var <- renderText({ 
    "You have selected this"
  })
  
  output$min_max <- renderText({ 
    paste("As suas escolhas foram:","Data:",
          input$date[1])
  })
  
  
  
}

shinyApp(ui, server)