## testes ##
## app.R ##
## teste ##
library(shiny) 
library(shinydashboard)
library(shinyjs)
library(shinymaterial)
library(readr)
library(ggplot2)
library(scales) 
library(XLConnect)
library(lubridate)
library(plyr)
library(arules)
library(DT)
library(data.table)

# Set the working directory
 #setwd("C:/Fred_Data/ISEP/SADEC/SADEC/Projecto 3/Projecto R Fred/SADEC")
 setwd("C:/Users/jferreira/Documents/SADEC")

#working.directory<-"c:/Fred_Data/ISEP/SADEC/SADEC/Projecto 3/Projecto R Fred/SADEC"
#working.directory<-"C:/Users/jferreira/Dropbox/SADEC/Projecto 3"
working.directory<-"C:/Users/jferreira/Documents/SADEC"

ui <- dashboardPage(
  dashboardHeader(title = "Smart House Control",
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "3 users logged today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("shopping-cart"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "UPS Batteries charged at 72%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Shopping Approve Order"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Check House Lights"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Swimming Pool Clean"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Garden Water Drainage "
                               )
                  )
                  
  ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Controls", tabName = "controls", icon = icon("th")),
      menuItem("Events", tabName = "events", icon = icon("calendar")),
      menuItem("Schedulle Event", tabName = "sevent", icon = icon("calendar-plus-o")),
      menuItem("Recomendations", tabName = "recomendations", icon = icon("cubes")),
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
              
              tags$iframe( seamless = "seamless",  src="https://forecast.io/embed/#lat=41.1495&lon=-8.6108&name=Porto&color=#00aaff&font=Georgia&units=ca",
                           height = 300, width = 300
              ),
              
              
              tags$iframe( src="http://www.youtube.com/embed/Ko2WZrUV1QI?autoplay=1", width=600 ,height=300, frameborder=0),
              
              # infoBoxes with fill=FALSE
              fluidRow(
                # Dynamic infoBoxes
                infoBoxOutput("temperatureBox"),
                infoBoxOutput("lightsBox"),
                infoBoxOutput("alarmBox"),
                # A static infoBox
                infoBox("Shopping List", 10 * 2, icon = icon("shopping-cart"))
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "controls", 
              fluidRow(
                box(
                  title = "Controls Ambient Temperature",background = "maroon", solidHeader = TRUE,
                  sliderInput("slider1", " Ambient Temperature Celsius Degrees:", 10, 30, 21)
                )
              ),
              fluidRow(
                box(
                  title = "Controls Water Temperature",background = "blue", solidHeader = TRUE,
                  sliderInput("slider2", " Water Temperature Celsius Degrees:", 10, 40, 25)
                )
              ),
              fluidRow(
                box(
                  title = "Controls Sound Volume ",background = "green", solidHeader = TRUE,
                  sliderInput("slider3", " Sound Volume Control:", 0, 100, 35)
                )
              ),
              fluidRow(
                box(
                  title = "Controls Light Intensity",background = "yellow", solidHeader = TRUE,
                  sliderInput("slider4", " Light Intensity:", 0, 100, 75)
                )
              )
              
      ),
      # Third tab content
      tabItem(tabName = "events",
              fluidRow(
                box(title = "Event Date", background = "blue", solidHeader = TRUE,
                    selectInput("select_source", "Choose Source",choices = list("Specialist" = 'E', "Community" = 'C', "House" = 'H'),selected='E'),
                    selectInput("select_clima", "Choose Type of Weather:",list("Hot","Mild","Cold")),  
                    selectInput("select_hora", "Choose Time Range:",list("Day time","Night time")), 
                    sliderInput("select_date", "Choose Date", min = Sys.Date() - 10,max =Sys.Date() + 10,value=Sys.Date(),timeFormat="%d-%m-%Y"),
                    submitButton("Submit")
                    
                )
              ),
              mainPanel(
                box(title = "Events", background = "green", solidHeader = FALSE,textOutput("selected_var"),
                    textOutput("selected_source"),textOutput("selected_clima"),textOutput("selected_hora"),
                    textOutput("selected_date")),
                
                box(title = "Events", background = "red", solidHeader = FALSE, tableOutput("table_events"))
                
              )
              
      ),
      # Fourth tab content
      tabItem(tabName = "charts",
              fluidRow(
                box(title = "Ambient Temperature Histogram", background = "red", solidHeader = TRUE,
                    sliderInput("slider6", "Time", min = Sys.Date() - 10,max =Sys.Date() + 10,value=Sys.Date(),timeFormat="%d-%m-%Y"),
                    submitButton("Submit"),
                    plotOutput(outputId = "plot2", height = 250)
                ),
                
                box(title = " Water Temperature Histogram", background = "yellow", solidHeader = TRUE,
                    sliderInput("slider7", "Time", min = Sys.Date() - 10,max =Sys.Date() + 10,value=Sys.Date(),timeFormat="%d-%m-%Y"),
                    submitButton("Submit"),
                    plotOutput("plot3", height = 250)
                )
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
      tabItem(tabName = "recomendations",
              fluidRow(
              
                  
                  box(title = "Event Date", background = "blue", solidHeader = TRUE,
                     selectInput("tipoevento","Choose event type:",list("Barbecue","Lunch","Dinner","Other"),width = '300px'), 
                      selectInput("clima", "Choose type of weather:",list("Hot","Mild","Cold")),  
                      selectInput("hora", "Choose time range:",list("Day time","Night time")), 
                     actionButton("goButton", "Ask for recomendations"),submitButton("Submit")
                     #textOutput("message"),
                     #submitButton("Ask for recomendation")
                      
                  )
                
                
              ),#close fluidrow
              
              mainPanel(
                
                box(title = "Recommendations", background = "green", solidHeader = TRUE,tableOutput("table")
                    #isto oculta uma mensagem de erro que não estou a conseguir perceber
                    ,tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }")
                )
              )
              
              
      ),#close tabitem
      
      # Eighth tab content
      tabItem(tabName = "sevent",
              fluidRow(
                
                  
                box(title = "Event Date", background = "blue", solidHeader = TRUE,
                    selectInput("tipoevento","Choose event type:",list("Barbecue","Lunch","Dinner","Other"),width = '300px'), 
                    selectInput("clima", "Choose type of weather:",list("Hot","Mild","Cold")),  
                    selectInput("hora", "Choose time range:",list("Day time","Night time")),  
                    selectInput("configevento", "Choose config:",list("beer","cleanig service","water")),
                    actionButton("goButtonSave", "Save Event") ,submitButton("Submit") 
                    #textOutput("message"),
                    #submitButton("Ask for recomendation")
                    
                )
                    
               
                
              ),#close fluidrow
              
              mainPanel(
                box(title = "Recommendations", background = "green", solidHeader = TRUE,textOutput("selected_var1"),
                    textOutput("selected_event1"),textOutput("selected_config1"),textOutput("selected_date1"),textOutput("selected_hour1"))
              )
              
              
      )#close tabitem      
      
    )
      
    ) #close tabitens
  ) #close dashoboardbody



server <- function(input, output) {
  set.seed(122)
  
  
  # Set the working directory
  # setwd("C:/Fred_Data/ISEP/SADEC/SADEC/Projecto 3/Projecto R Fred/SADEC")
  setwd(working.directory)
  
  
  # Read CSV into R
  
  DataAmbTemp <- read_delim("data/Ambient_Temp.csv", 
                            ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                            trim_ws = TRUE)
  
  
  DataWaterTemp <- read_delim("data/Water_Temp.csv", 
                              ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                              trim_ws = TRUE)
  
  
  output$plot2 <- renderPlot({
    x1<- input$slider6 - 5
    x2<-input$slider6 + 5
    
    ggplot(data=DataAmbTemp, aes(x=date, y=insideTemperature)) +
      geom_bar(stat="identity", fill="steelblue", position = 'dodge' )+
      geom_text(aes(label=insideTemperature), color="white",position = position_dodge(width = 1),vjust = 1,size=3 )+
      theme_minimal()+ggtitle("Ambient Temperature") +
      scale_x_date(date_breaks = "1 day", 
                   labels=date_format("%d-%m-%Y"),
                   limits = as.Date(c(x1,x2)))
  })
  
  
  
  output$plot3 <- renderPlot({
    
    x11<- input$slider7 - 5
    x12<-input$slider7 + 5
    
    ggplot(data=DataWaterTemp, aes(x=date, y=waterTemperature)) +
      geom_bar(stat="identity", fill="steelblue", position = 'dodge' )+
      geom_text(aes(label=waterTemperature), color="white",position = position_dodge(width = 1),vjust = 1,size=3 )+
      theme_minimal()+ggtitle("Water Temperature") +
      scale_x_date(date_breaks = "1 day", 
                   labels=date_format("%d-%m-%Y"),
                   limits = as.Date(c(x11,x12)))
  })
  
  output$temperatureBox <- renderInfoBox({
    infoBox(
      "House Temperature",paste0(0 + input$slider1, "ºC"), icon = icon("thermometer-full"),
      color = "purple"
    )
    
  })
  output$lightsBox <- renderInfoBox({
    infoBox(
      "House Lights", "On", icon = icon("lightbulb-o"),
      color = "yellow"
    )
  })
  output$alarmBox <- renderInfoBox({
    infoBox(
      "House Alarm", "Off", icon = icon("key"),
      color = "blue"
    )
  })
  
  output$selected_var <- renderText({ 
    "You have selected this"
  })
  
  output$selected_source <- renderText({ 
    paste("Source:", input$select_source)
  })
  
  output$selected_clima <- renderText({ 
    paste("Weather:", input$select_clima)
  })
  
  output$selected_hora <- renderText({ 
    paste("Time Range:", input$select_hora)
  })
  
  output$selected_date <- renderText({ 
    paste("Data:", input$select_date)
  })
  
  output$table_events <- renderTable({ 
    
    fileXls1 <- paste(working.directory,"datasetKDB.xlsx",sep='/')
    exc1 <- loadWorkbook(fileXls1, create = TRUE)
    
    #1º copia conteudo do datasetkdb.xls para esta df
    datasetevents <- readWorksheet(exc1,"datasetkdb", header = TRUE)                      
    #datasetevents2 <-  data.frame(datasetevents,"source"=paste(input$select_source),"tipo.evento","clima","hora","id")
    datasetevents<-subset(datasetevents, source==paste(input$select_source))
    datasetevents<-subset(datasetevents, clima==paste(input$select_clima))
    datasetevents<-subset(datasetevents, hora==paste(input$select_hora))
  })
  
  
  

  ##tab de adicao eventos  
  
  output$selected_var1 <- renderText({ 
    "You have selected this"
  })
  
  output$selected_event1 <- renderText({ 
    paste("Event:", input$tipoevento)
  })
  
  output$selected_config1 <- renderText({ 
    paste("Config:", input$configevento)
  })
  
  output$selected_date1 <- renderText({ 
    paste("Clima:", input$clima)
  })
  
  output$selected_hour1 <- renderText({ 
    paste("Hour:", input$hora)
  })
  
  
##componente Eventos 
  #runandmessage <- eventReactive(input$goButtonSave, ({
  observeEvent(input$goButtonSave, {
    
    fileXls <- paste(working.directory,"datasetKDB.xlsx",sep='/')
    exc <- loadWorkbook(fileXls, create = TRUE)
    
    #1º copia conteudo do datasetkdb.xls para esta df
    datasetkdb <- readWorksheet(exc,"datasetkdb", header = TRUE)                      
    #2º abre o datasetkdb.xls e escreve lá tudo o que estava mais o novo registo
    fileXls <- paste(working.directory,"datasetKDB.xlsx",sep='/')
    exc <- loadWorkbook(fileXls, create = TRUE)
    datasetkdb <- rbind( datasetkdb, data.frame("source"="H", 
                                                "tipo.casa"="type1", 
                                                "tipo.evento"=paste(input$tipoevento),
                                                "tipo.config"=paste(input$configevento), 
                                                "valor"="na", 
                                                "clima"=paste(input$clima), 
                                                "hora"=paste(input$hora), 
                                                "id"="new3"))
    writeWorksheet(exc, datasetkdb, sheet = "datasetkdb", startRow = 1, startCol = 1)
    saveWorkbook(exc)
    
  }) #close observe
  

##componente Recomendacoes  
  #runandmessage <- eventReactive(input$goButton, ({
  observeEvent(input$goButton, {
    # RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
    #install.packages("lubridate")
    #install.packages("plyr")
    #install.packages("arules")
    #install.packages("data.table")
    #install.packages("DT")
    #install.packages("XLConnect")
    library("XLConnect")
    library("lubridate")
    library("plyr")
    library("arules")
    library("DT")
    library("data.table")
    fileXls <- paste(working.directory,"datasetKDB.xlsx",sep='/')
    exc <- loadWorkbook(fileXls, create = TRUE)
    
    kdb <- readWorksheet(exc,"datasetkdb", header = TRUE)
    kdb<-as(kdb,"data.frame")
    #saveWorkbook(exc)
    #criar coluna recomendação = tipo.config + valor
    kdb$recomendacao=sapply(paste0(kdb$tipo.config),as.factor)
    #eliminar coluna id, tipo.config e valor
    kdb$id <- NULL
    kdb$tipo.config <- NULL
    kdb$source <- NULL
    #filtrar condições -------------------------------------------------------------------
    kdb <- kdb[!kdb$hora != paste(input$hora), ]
    kdb <- kdb[!kdb$tipo.casa != "type1", ]
    kdb <- kdb[!kdb$clima != paste(input$clima), ]
    kdb <- kdb[!kdb$tipo.evento != paste(input$tipoevento), ]
    #apriori
    kdb$tipo.casa=sapply(kdb$tipo.casa,as.factor)
    kdb$tipo.evento=sapply(kdb$tipo.evento,as.factor)
    kdb$valor=sapply(kdb$valor,as.factor)
    kdb$clima=sapply(kdb$clima,as.factor)
    kdb$hora=sapply(kdb$hora,as.factor)
    rules <- apriori(kdb,control = list(verbose=F),
                     parameter = list(minlen=5, supp=0.001, conf=0.001))
    rules<- sort(rules, by="support")
    
    rules <- subset(rules, subset = rhs %pin% "recomendacao=")
    rules <- subset(rules, subset = lhs %pin% "valor=")
    rules <- subset(rules, subset = lhs %ain% c( paste0("tipo.evento=",input$tipoevento)
                                                 ,paste0("clima=",input$clima)
                                                 ,paste0("hora=",input$hora)
                                                 ,"tipo.casa=type1"))
    
    #write(rules, file="rules", sep=",", quote=TRUE, row.names=FALSE)                 
    myruleslist <- data.table( lhs = labels( lhs(rules) ),rhs = labels( rhs(rules) ), 
                               quality(rules) )[ order(-lift), ]
    df_myruleslist<-as(myruleslist,"data.frame")
    df_myruleslist$recomendation<-substr(df_myruleslist$rhs,regexpr('=', df_myruleslist$rhs)+1,nchar(df_myruleslist$rhs)-1)
    df_myruleslist$value<-substr(df_myruleslist$lhs,
                                 regexpr('valor=', df_myruleslist$lhs)+6,nchar(df_myruleslist$lhs))
    df_myruleslist$value<-substr(df_myruleslist$value,1,regexpr(',', df_myruleslist$value)-1)
    
    
    df_myruleslist<-df_myruleslist[order(-df_myruleslist$support),]
    recomendations<-df_myruleslist[c("recomendation", "value","support")]
    # RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
    output$table <- renderTable ({recomendations[1:3]},spacing = "xs")
    
    
  }) #close observe
  
  
  #}) #close event reactive parameter
  #) #close event reactive
  
  
  #output$message <- renderText ({runandmessage()})
  
  
}

shinyApp(ui, server)