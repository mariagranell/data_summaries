setwd("/Users/mariagranell/Repositories/data_summaries/injury_report/")



#packages ----
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(wesanderson) # nice colours


d <- read.csv("injury.CSV")
d <- within(d,{
  Date<-as.Date(format(as.POSIXct(Date, format="%d/%m/%Y"), "%Y-%m-%d"))
  Group<-as.factor(Group)
  IDIndividual1<-as.factor(IDIndividual1)
  InjuryType<-as.factor(InjuryType)
  BodyPart<-as.factor(BodyPart)
  Size<-as.factor(Size)
  Consequence<-as.factor(Consequence)
  NewInjury<-as.factor(NewInjury)
  KnownFight<-as.factor(KnownFight)
})
str(d)


table_summary <- d%>%
  group_by(InjuryType, Consequence)%>%
  tally()
table_summary


