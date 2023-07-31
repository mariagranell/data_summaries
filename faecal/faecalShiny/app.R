#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load the required libraries
library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemr)
library(stringr)
library(readxl)
library(waffle)
library(shinydashboard)

# library ---------------------
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(lubridate)
library(ggthemr)
library(readxl)
library(stringr)
library(waffle)

# path ------------------------
setwd("/Users/mariagranell/Repositories/hormones/hormone_fecal")

dfield <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_fecal/crybertrackert_2023-07-16.CSV")

dfield <- dfield %>%
  mutate(Date = ymd(Date),
         WBS = as.numeric(str_replace_all(WBS, ";",".")),
         Time.1 = as.numeric(str_replace_all(Time.1, ";",":")),
         SampleNb = as.numeric(SampleNb),
         Collector = if_else(Collector == '', 'NA', Collector),
         ManualTime = str_replace_all(Time.1, ";",":")) %>%
  filter(!is.na(Group), Group != "Migrating/lone")

# plot colours --------
ggthemr('pale', type = 'outer', layout = "minimal", spacing =2)
swatch()
gray_solarized <- "#073642"
blue_solarized <- "#3262ab"
red_solarized <- "#eb9050"

## HORMONES DATA TO MERGE----------------

#TODO ERRORS -------------
# sample 243 appears twice in dlab

#data collected in the lab
dlab <- read_excel("/Users/mariagranell/Repositories/hormones/hormone_fecal/fecal_hormones_11.06.23.xlsx")
dlab <- dlab %>% select(-c("corrected_process_date", "...9")) %>% # ignore warnings
  mutate(wbs = as.numeric(wbs),
         was = as.numeric(was),
         total = as.numeric(total),
         process_date = ymd(process_date),
         SampleType = "Hormones",
         dry_weight = as.numeric(dry_weight)) %>%
  filter(replica == 1) %>%
  # removing of errors
  filter(n_sample != 243)

#combination of both data sets
dfield_h <- left_join(dfield, dlab, by = c( "SampleNb" = "n_sample", "SampleType" = "SampleType"))

#I will confider appropiate if it desviates 20% of 0.4, i.e. 0.08, but I will up it to 0.1.
#Thus a proper value is between 0.5 and 0.3
dfield_h$appropiate_amount <- ifelse(between(dfield_h$total, 0.3, 0.5), "Appropiate", "Inappropiate")

# GENETICS DATASET
d_genetics <-  read_excel("/Users/mariagranell/Desktop/IVP/Genetic_samples_overview.xlsx")

d_genetics <- d_genetics %>% mutate(Complete = na_if(Complete, "Complete?"))
d_genetics$Complete <- ifelse(is.na(d_genetics$Complete), "No", d_genetics$Complete)
genetics_plot <- d_genetics %>%
  group_by(Group, Complete) %>%
  summarise(number= as.numeric(n()))


# Define the UI for the Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Poop Data Analysis"),
  dashboardSidebar(
    # Input widget: Date range selector
    dateRangeInput(inputId = "date_range",
                   label = "Select date range:",
                   start = min(dfield$Date, na.rm = TRUE),
                   end = max(dfield$Date, na.rm = TRUE)
    ),
    
    checkboxGroupInput(inputId = "group_filter",
                       label = "Select Group(s) to include:",
                       choices = unique(dfield_h$Group),
                       selected = unique(dfield_h$Group))
  ),
  dashboardBody(
    # Output: Plot 1
    box(
      title = "Hormones",
      status = "info", 
      solidHeader = T,  # This ensures the header has a background color,
      div("Are we collecting the right amount of poop?", style = "font-size: 14px;"), #subtitle
      plotOutput(outputId = "hormones_histogram", height = 300)
    ),
    
    # Output: Plot 2
    box(
      title = "Hormones",
      status = "info", 
      solidHeader = T,  # This ensures the header has a background color,
      div("Number of hormones samples by sex", style = "font-size: 14px;"), #subtitle
      plotOutput(outputId = "hormones_sex", height = 300)
    ),
    
    # Output: Plot 3
    box(
      title = "eDNA",
      status = "success", 
      solidHeader = T,  # This ensures the header has a background color,
      div("Number of eDNA samples by sex", style = "font-size: 14px;"), #subtitle
      plotOutput(outputId = "edna_sex", height = 300)
    ),
    
    # Output: Plot 4
    box(
      title = "Genetics",
      status = "danger", 
      solidHeader = T,  # This ensures the header has a background color,
      div("Number of individuals that need Genetics", style = "font-size: 14px;"), #subtitle
      plotOutput(outputId = "genetics_waffle", height = 300)
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {

  # Reactive function for filtering the data based on the date range input
  filtered_dfield_h <- reactive({
    dfield_h %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2],
             Group %in% input$group_filter)

  })

  filtered_genetics_plot <- reactive({
    genetics_plot %>%
      filter(Group %in% input$group_filter)
  })

  # Reactive function to generate the plot based on the filtered data
  output$hormones_histogram <- renderPlot({
    filtered_dfield_h() %>%
      filter(!is.na(appropiate_amount)) %>%
      ggplot(.,aes( total ,fill = appropiate_amount)) +
      geom_histogram(na.rm =T, bins = 100) +
      facet_grid(Group ~ ., switch = "y") +
      geom_vline(xintercept = 0.4, color = "#073642", linetype = "dashed", size = 1) +
  #geom_text(data = d, aes(x = 2, y = 12, label = paste0(round(percentage_appropriate, 0), "%")),
  #          vjust = 2, hjust = 0, size = 4, color = "#073642") +
  # Add your customizations below
      labs(x = "Weight of a sample",
           y = "Number of hormone samples",
           #fill = "Are we collecting the right amount of poop?",
           caption = "The dotted line represents a Weight of >= 0.4, i.e. the appropiate amount
           and the % is the amount of correct hormones poop collected"
      ) +
      #scale_y_discrete(position = "left") +
      theme(
        # Adjust the appearance of axis text
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        plot.caption = element_text(size = 10, , hjust = 0.5),
        plot.title = element_text(size = 10, hjust = 0.5, vjust = -1, face = "plain"),
        legend.title = element_blank(),
      )
  })

  # Reactive function to generate Plot 2 based on the filtered data
  output$hormones_sex <- renderPlot({
    filtered_dfield_h() %>%
      filter(!is.na(appropiate_amount)) %>%
      group_by(Group, Sex)%>%
      tally() %>%
      ggplot(aes(x=Group, y=n, fill=Sex))+
      geom_col(position = "dodge")+
      geom_text(aes(label = n), position = position_dodge(width = 0.9),  hjust = 1.2, vjust = 0.5, colour = "white") +
      ylab("Number of fecal samples") +
      coord_flip() +
      labs(y = "Number of hormone samples"
           #title = "Fecal hormone samples by Group"
      ) +
      theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(size = 15, face = "bold")
      ) +
      scale_fill_manual(values = c("Female" = red_solarized, "Male" = blue_solarized))

  })

  ### EDNA -----------

  # maria estas aqui
  output$edna_sex <- renderPlot({
    filtered_dfield_h() %>%
    filter(SampleType == "eDNA") %>%
    group_by(Group, Sex)%>%
    tally() %>%
    filter(Sex == "Female" | Sex == "Male") %>%
    ggplot(aes(x=Group, y=n, fill=Sex))+
    geom_col(position = "dodge")+
    geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = 1.2, vjust = 0.5, colour = "white") +
      ylab("Number of eDNA samples") +
    coord_flip() +
    labs(y = "Number of eDNA samples"
       #title = "Fecal eDNA samples by Group"
    ) +
    theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 15, face = "bold")
    ) +
    scale_fill_manual(values = c("Female" = red_solarized, "Male" = blue_solarized))

})

### GENETICS WAFFLE

  output$genetics_waffle <- renderPlot({
    filtered_genetics_plot() %>%
    ggplot(., aes(fill=Complete, values=number)) +
  geom_waffle(color = "white", n_rows = 6, na.rm = TRUE) +
  facet_wrap(~Group, ncol=4) +
  #geom_text(aes(x = 9, y = 5, label = paste0(prop_yes, "/", all)), size = 4, color = gray_solarized) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    x = "",
    y = "",
    #title = "Number of individuals with Genetics completed",
    #subtitle = "The number represent how many individuals are NOT completed / total"
  ) +
  theme (
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "top",
  ) +
  scale_fill_manual(values = c("No" = red_solarized, "Yes" = blue_solarized))

})
}

# Run the Shiny app
shinyApp(ui = ui, server = server)