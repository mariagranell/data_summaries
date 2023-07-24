#I would like to create a function to link the LH data to the forms we are using. 
#First simply create a new collumn in each data sex with sex and age

#funtion(d,lh)

#beeing x the data frame you want to add the collumns to and y the last LH update

#SEX
#extract the sex from lh and make a dictionary 
d$sex <- lh

#Convert the injury form

#I am going to calculate the age of the different individuals in the life history.
#For that I need to create a new column with DOB and last seen
#packages
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(wesanderson) # nice colours

lh <- read.csv2("/Users/mariagranell/Repositories/data_summaries/life_history/IVP_Lifehistory_020822.csv")

# CLEANING LH DATA INCONSISTENCIES -----------

# CLEANING SEX COLUMN
# clean the Sex column on "" and "f".
lh$Sex[lh$Sex==""] <- NA
lh$Sex[lh$Sex=="f"] <- "F"

# add some missing sexes that were named

for (i in seq_len(nrow(lh))){
  if(is.na(lh$Sex[i])){
    lh$Sex[i] <- ifelse(nchar(lh$Code[i]) == 3, "M",
                  ifelse(nchar(lh$Code[i]) == 4, "F", NA))

  }
}

# check final numbers
lh%>%
  group_by(Sex)%>%
  tally()


#make chr, factors
lh <- within(lh,{
             DOB<-as.Date(format(as.POSIXct(DOB, format="%d/%m/%Y"), "%Y-%m-%d"))
             FirstRecorded<-as.Date(format(as.POSIXct(FirstRecorded, format="%d/%m/%Y"), "%Y-%m-%d"))
             LastSeen1<-as.Date(format(as.POSIXct(LastSeen1, format="%d/%m/%Y"), "%Y-%m-%d"))
             LastSeen2<-as.Date(format(as.POSIXct(LastSeen2, format="%d/%m/%Y"), "%Y-%m-%d"))
             Sex<-as.factor(Sex)
            })

#CALCULATE IF THEY ARE MOTHERS
lh$IsMother <- lh$Individual %in% lh$Mother

#FEMALES AGE

#Subset the females lh data
lh_females<- lh[lh$Sex== "F",]
lh_females<- lh_females[!is.na(lh_females$Sex),]
lh_females<- subset(lh_females, select = -c(Nicknames, EarMark)) #remove the columns that are meaningless

#The goal is the have a first date and an ending date and substract between them

#FIRST RECORDED
#compare DOB and First Record. There are no "Problems", i.e. where the DOB and the First recorded don´t align. 
#Thus we will continue forward using First recorded.
lh_females$compDOBFirstRecord <- ifelse(lh_females$DOB == lh_females$FirstRecorded, "Equal", "Problem")
lh_females%>%
  group_by(compDOBFirstRecord)%>%
  tally()
#I will remove the individuals that don´t have a First recorded sighting
lh_females <- lh_females[!is.na(lh_females$FirstRecorded),]


#CURRENT LAST DATE
#check how many have a last date
lh_females$mortality <- ifelse(is.na(lh_females$LastSeen1), "alive", "dead")
lh_females%>%
  group_by(mortality)%>%
  tally()

#create the current last date collumn
lh_females$CurrentLastSeen <- lh_females$LastSeen1 #just put the last seen date if that is the case
lh_females$CurrentLastSeen <- as.character(lh_females$CurrentLastSeen) #rename the collum as a charachter to be able to add the Sys.date()

currentDate <- as.character(Sys.Date()) #get the Sys.Date() as a chr to be able to include it
lh_females$CurrentLastSeen[lh_females$mortality == "alive"] <- currentDate
lh_females$CurrentLastSeen <- as.Date(lh_females$CurrentLastSeen) #make everything as data once they are all the in the same format.

#CALCULATE THE AGE

lh_females$AgeInDays <- as.numeric(difftime(lh_females$CurrentLastSeen, lh_females$FirstRecorded), units="days")
lh_females$AgeInYears <- lh_females$AgeInDays *0.00273973

hist(lh_females$AgeInYears)
lh_females%>%
  summarise(min= min(AgeInYears), max= max(AgeInYears))

#CALCULATE CATEGORIES
lh_females$AgeCategory <- ifelse(lh_females$AgeInYears<1, "baby", ifelse(between(lh_females$AgeInYears, 1,4), "juvenile", "adult"))


lh_females%>%
  group_by(IsMother, AgeCategory)%>%
  tally()

error <- subset(lh_females, lh_females$AgeCategory== "baby" & lh_females$IsMother==T)


# EXPORT CLEAN LH

write.csv(lh, "/Users/mariagranell/Repositories/data_summaries/life_history/lh_clean.csv", row.names = FALSE)

# GOAL 2----------------------

library(rstatix)

d <- read.csv("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/factchecked_LH.csv")
head(d)

sum <- d %>%
      group_by(Sex) %>% #grouping variable (sex)
      get_summary_stats(Age_yr) #summary of continues variables 'feb23age'

sum

summary_stats_all <- d %>%
  filter(Age_class == "adult") %>%
      group_by(Sex) %>% #grouping variable (sex)
      get_summary_stats(Age_yr) #summary of continues variables 'feb23age'

summary_stats_all

#age at death across sex
d %>%
  filter(Fate_probable == "dead") %>%
   filter(Age_class == "juvenile") %>%
  group_by(Sex) %>% #grouping variable (sex)
  get_summary_stats(Age_yr)


