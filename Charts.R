##Load Data and Make Plots

library(readr)
library(ggplot2)
library(scales)

# Set the working directory
setwd("C:/Fred_Data/ISEP/SADEC/SADEC/Projecto 3/Projecto R Fred/SADEC")

# Read CSV into R


DataAmbTemp <- read_delim("data/Ambient_Temp.csv", 
                          ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                          trim_ws = TRUE);


DataWaterTemp <- read_delim("data/Water_Temp.csv", 
                          ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                          trim_ws = TRUE)


str(DataAmbTemp)


ggplot(data=DataAmbTemp, aes(x=date, y=insideTemperature)) +
  geom_bar(stat="identity", fill="steelblue" )+
  geom_text(aes(label=insideTemperature), color="white")+
  theme_minimal()+ggtitle("Ambient Temperature") +
  scale_x_date(date_breaks = "1 day", 
               labels=date_format("%d-%m-%Y"),
               limits = as.Date(c('2011-01-01','2011-01-15')))








