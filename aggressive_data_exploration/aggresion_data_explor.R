# ---------------
# Title: aggresuive data exploration
# Date: 20-04-2023
# Author: mgranellruiz
# Goal: Just explore a bit the aggresive data from the IVP
# ---------------

# library ---------------------
library("ggplot2")
library("tidyverse")
library("dplyr")
library("readxl")
library("lubridate")
library("splitstackshape")
source("/Users/mariagranell/Repositories/male_services_index/functions.R")

# path ------------------------
setwd("/Users/mariagranell/Repositories/data_summaries/aggressive_data_exploration")

# data ------
datajo <- read_excel("/Users/mariagranell/Repositories/male_services_index/data/June-October/Agonistic.xls")
datanj <- read_excel("/Users/mariagranell/Repositories/male_services_index/data/November_January/Week_1/Agonistic.xls")

ag<- rbind(datajo, datanj) # merge df from the bottom
str(ag)

lh <- read.csv("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/factchecked_LH.csv")
lh <- change_group_names(lh, "Group_mb")
str(lh)

# determine winner-losser ---------------
# I will only consider winning when one of the individuals performs a leaving behaviour
leaving_bhv <- c("le","fl","rt","cr","ja")

# create function to count leaving behaviours in a string
count_bhv <- function(string_data, list_bhv) {
  sum(grepl(paste(list_bhv, collapse = "|"), string_data))
}

# count leaving behaviours in each column of the data frame
ag <- ag %>%
  rowwise() %>%
  mutate( bhv1_le = count_bhv(BehaviourIndiv1, leaving_bhv), bhv2_le =  count_bhv(BehaviourIndiv2, leaving_bhv)) %>%
  mutate(losser = ifelse(bhv1_le != 0 & bhv2_le == 0, "IDIndividual1",
                         ifelse(bhv1_le == 0 & bhv2_le != 0, "IDIndividual2",
                                ifelse(bhv1_le != 0 & bhv2_le != 0, "Unk", "Unk"))))

# create summary table
# in where 1 is that there was a leaving behaviour.
ag %>%
  group_by(bhv1_le,bhv2_le) %>%
  summarise(n = n())

# asumptions ---------------
# for this data exploration I will assume that indv 1 is the aggressor which might not be the case.
# is important to check at the specific behaviours to determine the aggressor.
# right now I am not analyzing suport behaviours

sag <- ag%>%
  select(Date, Time, Group, IDIndividual1, BehaviourIndiv1, IDIndividual2, BehaviourIndiv2, bhv1_le, bhv2_le, losser) %>%
  pivot_longer(cols = c(IDIndividual1, IDIndividual2), values_to = "ID", names_to = "Iniciator") %>%
  pivot_longer(cols = c(BehaviourIndiv1, BehaviourIndiv2), values_to = "Behaviour", names_to = NULL) %>%
  distinct(Date, Time, Group, ID, .keep_all = TRUE) %>%
  change_group_names(., "Group")

d <- sag %>%
  left_join(., lh, by = c("ID" = "AnimalCode"), multiple = "all") %>%
  subset(., Group == Group_mb)

# TODO
# the age is not calculated properly. You need to calculate it from the date of the encounter
# but for now I´ll use age calculated for today.

# define aggressive behaviours----------
agg_behv <- c("st", "at", "gb", "tp", "bi", "hi", "ch")
victim_behv <- c("av", "ja", "cr", "rt", "fl")

# create a column bully to see if the performed or attemtep to do aggressive behaviours
d <- d %>%
  mutate(Bully = ifelse(grepl(paste(agg_behv, collapse = "|"), Behaviour), "yes", "no"))


#  HYPOTHESIS TO TEST
#  as low rank individuals experience more frequent defeats in social antagonistic interactions
# from this quick graph that is not that cristal clear in my system

d_defeats <- d %>%
  filter(losser == Iniciator)

d_defeats %>%
  filter(Group_mb == "KB") %>%
  group_by(AnimalID, Age_class, Sex) %>%
  summarise(n_loser = n()) %>%
  ggplot(aes(x = AnimalID, y = n_loser, fill = Sex)) +
    geom_col() +
  coord_flip() +
  labs( title = "How many fights they have lost")






# dataset to plot! NOT STATS! ------------
n_indv <- d %>%
  distinct(ID, .keep_all = TRUE) %>%
  group_by(Age_class, Group, Sex) %>%
  summarise(n_ind = n())

n_agg <- d %>%
  group_by(Age_class, Group, Sex) %>%
  summarise(agg_part = n())

to_plot <- n_indv %>%
  left_join(., n_agg)

# are diffrences in life stages aggressiveness ---------------
to_plot %>%
  group_by(Age_class) %>%
  summarise(aggresiveness = sum(agg_part)/sum(n_ind)) %>%
  ggplot(aes(x = Age_class, y = aggresiveness)) +
  geom_col() +
  labs(title = "Life stages differences",
       subtitle = "corrected by the number of individuals")

to_plot %>%
  group_by(Age_class) %>%
  summarise(mean_aggresiveness = mean(agg_part)) %>%
  ggplot(aes(x = Age_class, y = mean_aggresiveness)) +
  geom_col() +
  labs(title = "life stages differences")

# are different groups more aggrressive?--------------
to_plot %>%
  group_by(Group) %>%
  summarise(aggresiveness = sum(agg_part)/sum(n_ind)) %>%
  ggplot(aes(x = Group, y = aggresiveness)) +
  geom_col() +
  labs(title = "Group differences",
       subtitle = "corrected by the number of individuals")

to_plot %>%
  group_by(Group) %>%
  summarise(mean_aggresiveness = mean(agg_part)) %>%
  ggplot(aes(x = Group, y = mean_aggresiveness)) +
  geom_col() +
  labs(title = "life stages differences")

# females or males more aggresive? -------------------
to_plot %>%
  group_by(Sex) %>%
  summarise(aggresiveness = sum(agg_part)/sum(n_ind)) %>%
  ggplot(aes(x = Sex, y = aggresiveness)) +
  geom_col() +
  labs(title = "Sex differences",
       subtitle = "corrected by the number of individuals")

to_plot %>%
  group_by(Sex) %>%
  summarise(mean_aggresiveness = sum(agg_part)/sum(n_ind)) %>%
  ggplot(aes(x = Sex, y = mean_aggresiveness)) +
  geom_col() +
  labs(title = "Sex differences")