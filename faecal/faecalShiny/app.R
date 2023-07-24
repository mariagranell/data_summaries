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

# Load the data and perform necessary preprocessing
dfield <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_fecal/crybertrackert_2023-07-16.CSV")

# ... (rest of the data preprocessing code, as it is in your script)

# Define global variables for colors
ggthemr('solarized', type = 'outer', layout = "minimal", spacing =2)
gray_solarized <- "#073642"
blue_solarized <- "#268bd2"
red_solarized <- "#dc322f"

## HORMONES DATA ----------------
dfield_h <- dfield %>%
  filter(SampleType == "Hormones") %>%
  mutate(
    WBS = as.numeric(str_replace_all(WBS, ";",".")),
    ManualTime = str_replace_all(Time.1, ";",":")
  ) %>%
  select(-c("SampleType", "GPSS","GPSE","Observers","DataInfo","Remarks","DeviceId","Data", "Time.1"))

#data collected in the lab
dlab <- read_excel("/Users/mariagranell/Repositories/hormones/hormone_fecal/fecal_hormones_11.06.23.xlsx")
dlab <- dlab %>% select(-c("corrected_process_date", "...9")) %>% # ignore warnings
  mutate(wbs = as.numeric(wbs),
         was = as.numeric(was),
         total = as.numeric(total),
         process_date = ymd(process_date),
         dry_weight = as.numeric(dry_weight))

#combination of both data sets
dh <- merge(dlab,dfield_h, by.x="n_sample", by.y = "SampleNb")

#I will confider appropiate if it desviates 20% of 0.4, i.e. 0.08, but I will up it to 0.1.
#Thus a proper value is between 0.5 and 0.3
dh$appropiate_amount <- ifelse( between(dh$total, 0.3, 0.5), "Appropiate", "Inappropiate")

dh<-dh %>%
  mutate(Collector = if_else(Collector == '', 'NA', Collector)) %>%
  filter(!is.na(Group))
dh$total <-dh$was - dh$wbs

# percentage of appropiates --------------
d_percentage <- dh %>%
  group_by(Group) %>%
  filter(!is.na(appropiate_amount)) %>%
  summarise(percentage_appropriate = (sum(appropiate_amount == "Appropiate")/n())*100)

dh <- left_join(dh, d_percentage, by = "Group")

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Poop Data Analysis"), # App title

  # Sidebar layout with input widgets
  sidebarLayout(
    sidebarPanel(
      # Input widget: Date range selector
      dateRangeInput(inputId = "date_range",
                     label = "Select date range:",
                     start = min(dfield$Date, na.rm = TRUE),
                     end = max(dfield$Date, na.rm = TRUE)
      )
    ),

    # Main panel to display the plot
    mainPanel(
      # Output: Plot 1
      plotOutput(outputId = "poop_plot"),

      # Output: Plot 2
      plotOutput(outputId = "group_plot")
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {

  # Reactive function for filtering the data based on the date range input
  filtered_data <- reactive({
    data_filtered <- dh %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2])

    d_percentage <- data_filtered %>%
      group_by(Group) %>%
      filter(!is.na(appropiate_amount)) %>%
      summarise(percentage_appropriate = (sum(appropiate_amount == "Appropiate") / n()) * 100)

    left_join(data_filtered, d_percentage, by = "Group")
  })

  percentage_labels <- reactive({
    filtered_data() %>%
      group_by(Group) %>%
      filter(!is.na(appropiate_amount)) %>%
      summarise(percentage_appropriate = (sum(appropiate_amount == "Appropiate") / n()) * 100)

  })

  eDNAfilter <- reactive({
    eDNAfilter <- dfield_e %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2])
  })

  # Reactive function to generate the plot based on the filtered data
  output$poop_plot <- renderPlot({
    filtered_data() %>%
      ggplot(., aes(total, fill = appropiate_amount)) +
      geom_histogram(na.rm = TRUE, bins = 100) +
      facet_grid(Group ~ ., switch = "y") +
      geom_vline(xintercept = 0.4, color = gray_solarized, linetype = "dashed", size = 1) +
      #geom_text(data = percentage_labels(), aes(x = total, y = 12, label = paste0(round(percentage_appropriate, 0), "%")),
      #          vjust = 2, hjust = 0.5, size = 4, color = gray_solarized) +
      labs(x = "Weight of a sample",
           y = "Number of hormone samples",
           fill = "Are we collecting the right amount of poop?",
           title = "The dotted line represents a Weight of >= 0.4, i.e. the appropriate amount\nand the % is the amount of correct hormones poop collected",
           caption = NULL
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        plot.title = element_text(size = 10, hjust = 0.5, vjust = -1, face = "plain"),
        legend.title = element_text(size = 15, face = "bold")
      )
  })

  # Reactive function to generate Plot 2 based on the filtered data
  output$group_plot <- renderPlot({
    filtered_data() %>%
      group_by(Group, Sex) %>%
      tally() %>%
      ggplot(aes(x = Group, y = n, fill = Sex)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.1, vjust = 0.5) +
      ylab("Number of fecal samples") +
      coord_flip() +
      labs(y = "Number of hormone samples",
           title = "Fecal hormone samples by Group"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15, face = "bold")
      ) +
      scale_fill_manual(values = c("Female" = red_solarized, "Male" = blue_solarized))
  })

  ### EDNA -----------

  # maria estas aqui
  output$group_plot <- renderPlot({
    filtered_data() %>%
  plot_edna <- dfield_e%>%
  group_by(Group, Sex)%>%
  tally() %>%
  filter(Sex == "Female" | Sex == "Male" & Group != "Migrating/lone") %>%
  ggplot(aes(x=Group, y=n, fill=Sex))+
  geom_col(position = "dodge")+
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.1, vjust = 0.5) +
  ylab("Number of eDNA samples") +
  coord_flip() +
  labs(y = "Number of eDNA samples",
       title = "Fecal eDNA samples by Group"
  ) +
    theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 15, face = "bold")
  ) +
  scale_fill_manual(values = c("Female" = red_solarized, "Male" = blue_solarized))

})

# Run the Shiny app
shinyApp(ui = ui, server = server)