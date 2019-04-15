library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(MASS)
library(tidyverse)


lf_behavior<-read_excel('Data/LF Adult Behavior Checklist CORE.xlsx')
lf_behavior_entry<-filter(lf_behavior,ExitCklist=='Entry' )
lf_behavior_exit<-filter(lf_behavior,ExitCklist=='Exit' )
lf_behavior_exit<-subset(lf_behavior_exit,select=-c(ExitCklist,Region_Name))

lf_combined<-left_join(lf_behavior_entry,lf_behavior_exit, by=c("Adult_ID"),suffix = c(".entry", ".exit"))

lf_sum<-read_excel("Data/LF Adult Summary.xlsx")
lf_sum<-subset(lf_sum, select = -c(Region_Name))
lf_combined<-left_join(lf_combined,lf_sum, by=c("Adult_ID"))

lf_combined<-na.omit(lf_combined)
lf_combined<- lf_combined %>% mutate(Q01.diff = Q01.exit -Q01.entry)
lf_combined<- lf_combined %>% mutate(Q02.diff = Q02.exit -Q02.entry)
lf_combined<- lf_combined %>% mutate(Q03.diff = Q03.exit -Q03.entry)
lf_combined<- lf_combined %>% mutate(Q04.diff = Q04.exit -Q04.entry)
lf_combined<- lf_combined %>% mutate(Q05.diff = Q05.exit -Q05.entry)
lf_combined<- lf_combined %>% mutate(Q06.diff = Q06.exit -Q06.entry)
lf_combined<- lf_combined %>% mutate(Q07.diff = Q07.exit -Q07.entry)
lf_combined<- lf_combined %>% mutate(Q08.diff = Q08.exit -Q08.entry)
lf_combined<- lf_combined %>% mutate(Q09.diff = Q09.exit -Q09.entry)

lf_combined<- lf_combined %>% mutate(Q10.diff = Q10.exit -Q10.entry)


lf_combined[,'cat']<-'Local food exposed'


non_lf_behavior<-read_excel('Data/NonLF Adult Behavior Checklist CORE.xlsx')
non_lf_behavior_entry<-filter(non_lf_behavior,ExitCklist=='Entry' )
non_lf_behavior_exit<-filter(non_lf_behavior,ExitCklist=='Exit' )
non_lf_behavior_exit<-subset(non_lf_behavior_exit,select=-c(ExitCklist,Region_Name))

non_lf_combined<-left_join(non_lf_behavior_entry,non_lf_behavior_exit, by=c("Adult_ID"),suffix = c(".entry", ".exit"))

non_lf_sum<-read_excel("Data/NonLF Adult Summary.xlsx")
non_lf_sum<-subset(non_lf_sum, select = -c(Region_Name))
non_lf_combined<-left_join(non_lf_combined,non_lf_sum, by=c("Adult_ID"))

non_lf_combined<-na.omit(non_lf_combined)
non_lf_combined<- non_lf_combined %>% mutate(Q01.diff = Q01.exit -Q01.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q02.diff = Q02.exit -Q02.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q03.diff = Q03.exit -Q03.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q04.diff = Q04.exit -Q04.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q05.diff = Q05.exit -Q05.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q06.diff = Q06.exit -Q06.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q07.diff = Q07.exit -Q07.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q08.diff = Q08.exit -Q08.entry)
non_lf_combined<- non_lf_combined %>% mutate(Q09.diff = Q09.exit -Q09.entry)

non_lf_combined<- non_lf_combined %>% mutate(Q10.diff = Q10.exit -Q10.entry)


non_lf_combined[,'cat']<-'Non Local food exposed'

all_combined<-rbind(lf_combined,non_lf_combined)
all_combined_avg<-all_combined %>% group_by(cat) %>% summarise(Q01.diff=mean(Q01.diff),Q02.diff=mean(Q02.diff),Q03.diff=mean(Q03.diff),Q04.diff=mean(Q04.diff),Q05.diff=mean(Q05.diff),Q06.diff=mean(Q06.diff),Q07.diff=mean(Q07.diff),Q08.diff=mean(Q08.diff),Q09.diff=mean(Q09.diff),Q10.diff=mean(Q10.diff))

EFNEP_data<-all_combined