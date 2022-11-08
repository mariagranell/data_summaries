#directory
setwd("/Users/mariagranell/Repositories/data_summaries/")

#Creating a shiny app from sratch
library(shiny)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(wesanderson) # nice colours

#data
d <- read.csv2("swabs-20-09-22.CSV")
d<-within(d,{
  ManualDate<-as.Date(ManualDate)
  Date<-as.Date(format(as.POSIXct(Date, format="%d/%m/%Y"), "%Y-%m-%d"))
  Group<-as.factor(Group)
})

levels(d$Group) <- c("AK", "BD", "KU", "LT", "NH")
#d <- na.exclude(d)
#data only after swabs talk
#swabs_talk <- sampling_effort$Date < "2022-09-5"
#sampling_effort <- sampling_effort[!swabs_talk,]

#colors for the groups
custom.col <- c("#FFDB6D", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


#Define the UI. The layout of the app
ui <- fluidPage(
  titlePanel("Saliva sampling"),
  
  sidebarLayout(
    sidebarPanel(
      Position = "Left",
      
      #text before the choices
      "Choose which parameters you want to plot",
      
      #create a checkbox
      checkboxGroupInput("troup", 
                         h4("Select a Group"),
                         choices = c("AK", "BD", "KU", "LT", "NH"), selected = "NH"),

      #date range
      dateRangeInput("dates", h4("Date range"))),
    mainPanel("main panel",
       
      #output to put in the main panel
      plotOutput(outputId = "barplot")
              
              )
  ),
)
    
#Define the server logic
server <- function(input, output) {
  output$barplot <- renderPlot({
    
      table_summary<-d%>%
        group_by(Group)%>%
        tally()
      table_summary<- fortify(table_summary)
      table_summary[which(table_summary$Group %in% input$troup),]%>%
        ggplot(aes(x=Group, y=n, fill=Group))+
        geom_col()+
        scale_fill_manual(values = custom.col)+
        theme_set(
          theme_minimal())
  })
}
 



#Run the app
shinyApp(ui = ui,server = server)
