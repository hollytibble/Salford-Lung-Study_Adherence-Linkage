###################################################################
#  Identifying drugs that were dispensed but not prescribed
###################################################################

setwd("/Volumes/SLS Mock Data")
rm(list=ls()) # remove all variables from workspace
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(MASS)
library(survival)
library(survminer)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

load("/Volumes/SLS Mock Data/results_version_final_internal_x.RData")

###################################################################
# Methods
###################################################################

# how many rows of data there are in the prescribing records
length_PRESCR1F
# how many unique people
length_PRESCR1F_people
# Date range
PRESC_date_min
PRESC_date_max

# The same for the dispensing data
length_DISPENSF
length_DISPENSF_people
DISP_date_min
DISP_date_max

# Unique number of drug descriptions
length_unique_CMDRGSYN

###################################################################
# Data Cleaning
###################################################################

# Unique number of drug descriptions
length_unique_CMDRGSYN

# Check OXIS
check_oxis

# length of unique asthma related drug descriptions
length_unique_CMDRGSYN_asthma
length_unique_CMDRGSYN_asthma*100/length_unique_CMDRGSYN

# How many were dropped
length_unique_CMDRGSYN_asthma_drop
length_unique_CMDRGSYN_asthma_drop*100/length_unique_CMDRGSYN_asthma

# Drop records based on keywords
keywords<-c("NASAL","NOSE","NOSTRIL","NASULE","HAYFEVER",
            "EYE","EAR","DROP","TONGUE",
            "FOAM","ENEMA","RECTAL",
            "GASTRO","MODIFIED",
            "CREAM", "APPLY","SKIN","ULCER","OINTMENT","PATCH",
            "CAPSULE", "SACHET", "SPRAY",
            "AZELASTINE","NASONEX","FLIXONASE","ANORA ELLIPTA",
            "SUMATRIPTAN","AVAMYS","RHINOCORT","NASOBEC","NASOFAN")
for (keyword in keywords) {
  print(keyword)
  print(get(paste(keyword,"freq",sep="_")))
}
# check nothing excluded I should have kept
exclusions

# retained records
length_PRESCR1F_asthma
length_DISPENSF_asthma

# Duplicate removal
length_PRESCR1F_asthma_drop
length_PRESCR1F_asthma_final
length_PRESCR1F_asthma_final*100/length_PRESCR1F_asthma
length_DISPENSF_asthma_drop
length_DISPENSF_asthma_final
length_DISPENSF_asthma_final*100/length_DISPENSF_asthma

###################################################################
# Matching
###################################################################

candidate_links

# dropped by dates
dates_dropped
dates_dropped*100/candidate_links
dates_remaining

# Distribution of our weights for all candidate links, before weight-based exclusion
table(candidate_weight_a, useNA = "ifany")*100/dates_remaining
table(candidate_weight_b, useNA = "ifany")*100/dates_remaining
table(candidate_weight_c, useNA = "ifany")*100/dates_remaining
table(candidate_weight_d, useNA = "ifany")*100/dates_remaining
table(candidate_weight_disp, useNA = "ifany")*100/dates_remaining
hist(candidate_weight_disp)
# how many were dropped by weight
weight_dropped
weight_dropped*100/dates_remaining

candidate_links_retained
candidate_links_retained*100/candidate_links

# no_candidates_disp
# no_candidates_disp*100/length_DISPENSF_asthma_final
# 
# no_candidates_presc
# no_candidates_presc*100/length_PRESCR1F_asthma_final

p1ID_unique_merge_check
DID_unique_merge_check

matches
matches*100/length_PRESCR1F_asthma_final
matches*100/length_DISPENSF_asthma_final

# weight of matched records
table(match_weight_a_m, useNA = "ifany")*100/matches
table(match_weight_b_m, useNA = "ifany")*100/matches
table(match_weight_c_m, useNA = "ifany")*100/matches
table(match_weight_d_m, useNA = "ifany")*100/matches
table(match_weight_disp, useNA = "ifany")*100/matches
hist(match_weight_disp)

###################################################################
# Sensitivity analysis
###################################################################

# how many more were dropped by weight
candidate_links_retained_sens
candidate_links_retained_sens*100/candidate_links_retained
matches_sens
matches_sens*100/matches

t.test(match_weight_disp,matches_weights_sens)

###################################################################
# Quality Assurance - unmatched records
###################################################################

sum(results$match=="Presc not Disp")
sum(results$match=="Presc not Disp")*100/length_PRESCR1F_asthma_final
sum(results$match=="Disp not Presc") 
sum(results$match=="Disp not Presc")*100/length_DISPENSF_asthma_final

# Quality assurance - sensitivity analysis
sum(results_sens$match=="Presc not Disp")*100/length_PRESCR1F_asthma_final
sum(results_sens$match=="Disp not Presc")*100/length_DISPENSF_asthma_final

sum(is.na(results$dose_strength))
round(prop.table(table(results$match,is.na(results$dose_strength)),margin=1),2)*100
round(prop.table(table(results$match,is.na(results$QUANTITY_primary)),margin=1),2)*100
round(prop.table(table(results$match,is.na(results$QUANTITY_primary) & is.na(results$dose_strength)),margin=1),2)*100

counts<-results %>%
  group_by(random_ID,match) %>%
  count() %>%
  spread(match, n)
counts$DNP<-ifelse(is.na(counts$`Disp not Presc`),0,counts$`Disp not Presc`)
counts$PND<-ifelse(is.na(counts$`Presc not Disp`),0,counts$`Presc not Disp`)
summary(lm(DNP ~ PND, data=counts))

###################################################################
# Unclaimed medications
###################################################################

# what percentage went unclaimed?
perc_unclaimed
unclaimed_CMDRGSYN 

# per-person claimed
summary(1-person_unclaimed$unclaimed_total/person_unclaimed$presc_total)
sum(person_unclaimed$unclaimed_total/person_unclaimed$presc_total>0.5)*100/nrow(person_unclaimed)

# ICS and LABA together or sep?
claiming_ICS_LABA
sum(claiming_ICS_LABA$`0`+claiming_ICS_LABA$`1`)
prop.test(claiming_ICS_LABA$`1`,claiming_ICS_LABA$`0`+claiming_ICS_LABA$`1`)

# Summary Stats
sum(!is.na(initiation))
summary(initiation)
summary(c(initiation[which(!is.na(initiation))],rep(178,23391))) # 178 days is 6-months
sum(initiation>7,na.rm=T)*100/length(initiation)
sum(initiation>14,na.rm=T)*100/length(initiation)
sum(initiation>21,na.rm=T)*100/length(initiation)
sum(initiation>30,na.rm=T)*100/length(initiation)

# survival for time to claiming
ggsurvplot(survfit(Surv(initiation_days_x,event==1) ~ 1, data=plot),
           data = plot, ggtheme = theme_bw(), 
           legend  = "none", axes.offset=F, break.time.by=1, ylim=c(0,1),
           xlim=c(0,21),xlab="Days since prescription issued",
           ylab="Probability prescription \nremains unclaimed",
           font.x=16, font.y=16, font.tickslab=14, size=2)
round(prop.table(table(plot$initiation_days_x==21,plot$event),margin=2)*100,2)
cox1
round(cox1$coefficients,3)

###################################################################
# Appendix D Figures
###################################################################

# table(candidate_weight_a, useNA = "ifany")*100/dates_remaining
# table(candidate_weight_b, useNA = "ifany")*100/dates_remaining
# table(candidate_weight_c, useNA = "ifany")*100/dates_remaining
# table(candidate_weight_d, useNA = "ifany")*100/dates_remaining
# 
# table(match_weight_a_m, useNA = "ifany")*100/matches
# table(match_weight_b_m, useNA = "ifany")*100/matches
# table(match_weight_c_m, useNA = "ifany")*100/matches
# table(match_weight_d_m, useNA = "ifany")*100/matches

df_1<-data.frame(points=rep(c("0","10","20"),2),
                 values=c(6.3,0,93.7,2.8,0,97.2),
                 matches=c(rep("Candidates",3),rep("Matches",3)))
df_2<-data.frame(points=rep(c("0","10","35"),2),
                 values=c(4.8,18.1,77.2,0,9,91),
                 matches=c(rep("Candidates",3),rep("Matches",3)))
df_3<-data.frame(points=rep(c("0","10","15","35"),2),
                 values=c(4.2,9.8,4.9,81.1,0,0,1.5,98.5),
                 matches=c(rep("Candidates",4),rep("Matches",4)))
df_4<-data.frame(points=rep(c("0","10"),2),
                 values=c(67.2,32.8,1.3,98.7),
                 matches=c(rep("Candidates",2),rep("Matches",2)))


plot_1<-ggplot(data=df_1, aes(x=points, y=values, fill=matches)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_fill_manual(values=c('#999999','#E69F00')) + 
  labs(x="Points", y="Percentage", title="Brand Name") + 
  theme(legend.position = "none", text = element_text(size=20))
plot_2<-ggplot(data=df_2, aes(x=points, y=values, fill=matches)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_fill_manual(values=c('#999999','#E69F00')) + 
  labs(x="Points", y=" ", title="Dose Strength") + 
  theme(legend.position = "none", text = element_text(size=20)) +ylim(0,100)
plot_3<-ggplot(data=df_3, aes(x=points, y=values, fill=matches)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_fill_manual(values=c('#999999','#E69F00')) + 
  labs(x="Points", y="Percentage", title="Quantity") + 
  theme(legend.position = "none", text = element_text(size=20))
plot_4<-ggplot(data=df_4, aes(x=points, y=values, fill=matches)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_fill_manual(values=c('#999999','#E69F00')) + 
  labs(fill=" ",x="Points", y=" ", title="Dates") + 
  theme(legend.position = "bottom", text = element_text(size=20))

legend<-get_legend(plot_4)
plot_4<-plot_4+theme(legend.position = "none")
blankPlot <- ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()

grid.arrange(legend,blankPlot,plot_1,plot_2,plot_3,plot_4,
             nrow=3, ncol=2 , 
             widths = c(2.7, 2.7), heights = c(0.3,2.5,2.5))

###################################################################
#  Benchmarking Analysis
###################################################################

rm(list=ls()) # remove all variables from workspace
load("/Volumes/SLS Mock Data/results_version_final_benchmarking_internal_27Aug2019.RData")

prescriptions
dispensings

first_benchmark
first_benchmark*100/prescriptions
first_benchmark*100/dispensings

second_benchmark
second_benchmark*100/prescriptions
second_benchmark*100/dispensings

prescriptions_cc
dispensings_cc
third_benchmark
third_benchmark*100/prescriptions_cc
third_benchmark*100/dispensings_cc
