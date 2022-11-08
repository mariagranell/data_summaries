#set the working directory
setwd("/Users/mariagranell/Repositories/data_summaries/")

#packages
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(wesanderson) # nice colours
names(wes_palettes)

d <- read.csv2("swabs-20-09-22.CSV")
str(d)



d<-within(d,{
  ManualDate<-as.Date(ManualDate)
  Date<-as.Date(format(as.POSIXct(Date, format="%d/%m/%Y"), "%Y-%m-%d"))
  Group <- as.factor(Group)
  })

# Rename the levels in the factor to avoid spaces in e.g. "Lemon tree"
levels(d$Group) <- c("AK", "BD", "KU", "LT", "NH")




table_summary<-d%>%
  group_by(Group,SwabsDescription)%>%
  tally()
table_summary<- fortify(table_summary)
table_summary%>%
  ggplot(aes(x=Group, y=n, fill=SwabsDescription))+
  geom_col()

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

#THIS IS WHAT I AM TRYING TO PLOT IN THE SHINY APP  
table_summary<-d%>%
  group_by(Group)%>%
  tally()
table_summary<- fortify(table_summary)
table_summary%>%
  ggplot(aes(x=Group, y=n, fill=Group))+
  geom_col()+
  scale_fill_manual(values = custom.col)



table_summary<-d%>%
  group_by(IDIndividual1)%>%
  tally()
ID_summary


sampling_effort <- d%>%
  group_by(Date, Group) %>%
  tally()
sampling_effort <- na.exclude(sampling_effort)
#data only after swabs talk
swabs_talk <- sampling_effort$Date < "2022-09-5"
sampling_effort <- sampling_effort[!swabs_talk,]

sampling_effort <- fortify(sampling_effort)
sampling_effort%>%
  ggplot(aes(x=Date, y=n, fill=Group))+
  geom_col(position = "dodge")+
  geom_hline(yintercept = 5) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d")+
  scale_y_continuous(breaks = c(1:5,10,20))+
  scale_fill_manual(values = wes_palette("Rushmore"))+
  theme_light()+
  labs(y= "Number of swabs presented", x= "September 2022", title = "Saliva sampling effort during September")


df <- d%>%  group_by(Group) %>% tally()
g <- ggplot(df, aes( y = n, x = Group))
g + geom_col()
