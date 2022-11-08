#dicrectory
setwd("/Users/mariagranell/Repositories/data_summaries/")

#Creating a shiny app from sratch
library(shiny)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(wesanderson) # nice colours

#data
d <- read.csv("swabs-20-09-22.CSV")
d<-within(d,{
  ManualDate<-as.Date(ManualDate)
  Date<-as.Date(format(as.POSIXct(Date, format="%d/%m/%Y"), "%Y-%m-%d"))
})

#d <- na.exclude(d)
#data only after swabs talk
#swabs_talk <- sampling_effort$Date < "2022-09-5"
#sampling_effort <- sampling_effort[!swabs_talk,]


#Define the UI. The layout of the app
mtcars <-mtcars
choices <- names(mtcars)

ui <- fluidPage(
  selectInput("p","p",choices = names(mtcars)),
  plotOutput("myplot"))

server <- function(input, output, session) {
  
  output$myplot <- renderPlot({
    boxplot(mtcars[,input$p])
  })
}

shinyApp(ui, server)