###################################################################
#  Set a random seed
###################################################################

set.seed(948756)

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
library(dplyr)

## Drug type keywords
med_salbutamol<-toupper(c("Salbutamol","Albuterol","Ventolin","Airomir",
                          "Salamol","AirSalb","Salapin","Ventmax","Asmasal",
                          "Esi-Breathe","Salbulin","Salipraneb","Ipramol","Combivent"))
med_bambuterol<-toupper(c("Bambec","Bambuterol"))
med_formoterol<-toupper(c("Atimos","Formoterol","Foradil","Fostair","Symbicort",
                          "Flutiform","SPIROMAX","oxis"))
med_salmeterol<-toupper(c("Salmeterol","Neovent","Serevent","Seretide",
                          "SIRDUPLA","AIRFLUSAL"))
med_terbutaline<-toupper(c("Bricanyl","Terbutaline"))
med_tiotropium<-toupper(c("SPIRIVA","TIOTROPIUM"))
med_vilanterol<-toupper(c("vilanterol","relvar", "vilenterol"))
med_glycopyrronium_bromide<-toupper(c("SEEBRI"))
med_ipratropium<-toupper(c("Ipratropium","Atrovent","Respontin","IPRAVENT","Salipraneb",
                           "Ipramol","Combivent"))
med_theophylline<-toupper(c("Theophylline","Neulin","Slo-Phyllin","Uniphyllin"))
med_aminophylline<-toupper(c("Aminophylline","Phyllocontin"))
med_beclometasone<-toupper(c("Beclometasone","Asmabec","Becodisks","Clenil","Qvar",
                             "Fostair"))
med_ciclesonide<-toupper(c("ciclesonide","alvesco"))
med_budesonide<-toupper(c("Budesonide","Budelin","Pulmicort","Symbicort","SPIROMAX"))
med_fluticasone<-toupper(c("fluticasone","Flixotide","Flutiform","Seretide",
                           "SIRDUPLA","AIRFLUSAL","relvar"))
med_mometasone<-toupper(c("Twisthaler","Asmanex","MOMETASONE"))
med_montelukast<-toupper(c("Singulair","Montelukast"))
med_zafirlukast<-toupper(c("Zafirlukast","Accolate"))
med_zileuton<-toupper(c("Zileuton","Zyflo"))
med_nedocromil<-toupper(c("Nedocromil","Tilade"))
med_cromolyn<-toupper(c("Cromoglicate","Cromoglycate","Intal"))
med_omalizumab<-toupper(c("Omalizumab","Xolair"))
med_prednisolone<-toupper(c("Prednisolone"))
med_methotrexate<-toupper(c("Methotrexate","Maxtrex","Metoject","Methofill", 
                            "Nordimet", "Zlatal"))
med_ciclosporin<-toupper(c("Ciclosporin", "Capimune", "Capsorin", "Deximune","Neoral",
                           "Sandimmun"))
med_azathioprine<-toupper(c("Azathioprine", "Imuran"))

# List of all medicine keywords
med_list<-substr(ls()[which(substr(ls(),1,3)=="med")],5,30)

## Drug Class Keywords
key_SABA<-c("salbutamol","bambuterol")
key_LABA<-c("formoterol","salmeterol","terbutaline","tiotropium","vilanterol")
key_LAMA<-c("glycopyrronium_bromide","ipratropium")
key_theophyllines<-c("theophylline", "aminophylline")
key_ICS<-c("beclometasone","ciclesonide","budesonide","fluticasone","mometasone")
key_LTRA<-c("montelukast","zafirlukast","zileuton")
key_cromoglicate<-c("nedocromil","cromolyn")
key_steroid<-c("omalizumab","prednisolone")
key_immuno<-c("methotrexate","ciclosporin","azathioprine")

# List of all drug class keywords
key_list<-substr(ls()[which(substr(ls(),1,3)=="key")],5,30)

# List of all brand names
brands<-toupper(c("Ventolin","Airomir","Salamol","AirSalb","Salapin","Ventmax","Asmasal",
                  "Esi-Breathe","Salbulin","Salipraneb","Ipramol","Combivent","Bambec",
                  "Atimos","Foradil","Fostair","Symbicort",
                  "Flutiform","SPIROMAX","oxis","Neovent","Serevent","Seretide",
                  "SIRDUPLA","AIRFLUSAL","Bricanyl","SPIRIVA","relvar", "SEEBRI",
                  "Atrovent","Respontin","IPRAVENT","Neulin","Slo-Phyllin","Uniphyllin","Asmabec",
                  "Becodisks","Clenil","Qvar","alvesco","Budelin","Pulmicort","Flixotide",
                  "Twisthaler","Asmanex","Singulair","Accolate","Zyflo","Tilade","Intal","Xolair",
                  "Maxtrex","Metoject","Methofill", "Nordimet", "Zlatal", "Capimune", "Capsorin", 
                  "Deximune","Neoral","Sandimmun", "Imuran"))

###################################################################
# Importing Data
###################################################################

PRESCR1F_asthma <- read_csv("PRESCR1F.csv")
temp<-read_csv("PRESCR1F_12APR2019.csv")
PRESCR1F_asthma<-rbind(PRESCR1F_asthma,temp)
temp<-read_csv("PRESCR1F_15JUL2019.csv")
PRESCR1F_asthma<-rbind(PRESCR1F_asthma,temp)

DISPENSF_asthma <- read_csv("DISPENSF.csv")
temp<-read_csv("DISPENSF_12APR2019.csv")
DISPENSF_asthma<-rbind(DISPENSF_asthma,temp)
temp<-read_csv("DISPENSF_15JUL2019.csv")
DISPENSF_asthma<-rbind(DISPENSF_asthma,temp)
rm(temp)

###################################################################
# Benchmark matching
###################################################################

PRESCR1F_asthma$DRUGDESC<-ifelse(is.na(PRESCR1F_asthma$DRUGDESC) | PRESCR1F_asthma$DRUGDESC=="",
                                PRESCR1F_asthma$CMDRGSYN,
                                toupper(PRESCR1F_asthma$DRUGDESC))
DISPENSF_asthma$DRUGDESC<-ifelse(is.na(DISPENSF_asthma$DRUGDESC) | DISPENSF_asthma$DRUGDESC=="",
                                 DISPENSF_asthma$CMDRGSYN,
                                 toupper(DISPENSF_asthma$DRUGDESC))

recode_CMDRGSYN<-unique(as.data.frame(c(PRESCR1F_asthma$DRUGDESC,DISPENSF_asthma$DRUGDESC))) 
names(recode_CMDRGSYN)<-"DRUGDESC"
recode_CMDRGSYN<-recode_CMDRGSYN %>%
  filter(!is.na(recode_CMDRGSYN$DRUGDESC))

# Assigning the medication type
for (med in med_list) {
  filter<- vapply(get(paste0("med_",med)),
                  function(x) str_detect(recode_CMDRGSYN$DRUGDESC, x), 
                  logical(nrow(recode_CMDRGSYN)))
  recode_CMDRGSYN[,med] <- as.logical(apply(filter, 1, sum))
  rm(filter)
}

#  Assign the drug class key flags, and the overall keyword
recode_CMDRGSYN$CMDRGSYN<-""
for (key in key_list) {
  recode_CMDRGSYN[,key] <- 0
  for (med in med_list) {
    if(med %in% get(paste0("key_",key))) {
      recode_CMDRGSYN[,key] <- recode_CMDRGSYN[,key]+recode_CMDRGSYN[,med]
      temp <- recode_CMDRGSYN[,med]==T
      recode_CMDRGSYN[temp, "CMDRGSYN"] <- paste(recode_CMDRGSYN[temp, "CMDRGSYN"],med,sep="_")
      rm(temp)
    }
  }
}
recode_CMDRGSYN$CMDRGSYN<-substr(recode_CMDRGSYN$CMDRGSYN,2,30)

### How many flags does each prescription have?
recode_CMDRGSYN$flag_sum<-0
for (key in key_list) {
  temp <- recode_CMDRGSYN[,key]==T
  recode_CMDRGSYN[temp, "flag_sum"] <- recode_CMDRGSYN[temp, "flag_sum"]+1
  rm(temp)
}

# Drop the ones with no flags
recode_CMDRGSYN<-recode_CMDRGSYN[which(recode_CMDRGSYN$flag_sum!=0),]

### code the drug class in cases with only one flag
recode_CMDRGSYN$drug_class<-NA
for (key in key_list) {
  temp <- recode_CMDRGSYN[,key]==T & recode_CMDRGSYN$flag_sum==1
  recode_CMDRGSYN[temp, "drug_class"] <- key
  rm(temp)
}

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
  recode_CMDRGSYN<-recode_CMDRGSYN[which(str_detect(recode_CMDRGSYN$DRUGDESC,keyword)==F |
                                           (recode_CMDRGSYN$drug_class %in% c("steroid","theophyllines") &
                                              keyword %in% c("GASTRO","MODIFIED","CAPSULE")) |
                                           (recode_CMDRGSYN$CMDRGSYN %in% c("tiotropium","glycopyrronium_bromide") &
                                              keyword=="CAPSULE")),]
}
recode_CMDRGSYN<-recode_CMDRGSYN[,c("DRUGDESC","CMDRGSYN")]

PRESCR1F_asthma<-inner_join(dplyr::select(PRESCR1F_asthma,-c("CMDRGSYN")),recode_CMDRGSYN)
DISPENSF_asthma<-inner_join(dplyr::select(DISPENSF_asthma,-c("CMDRGSYN")),recode_CMDRGSYN)

###################################################################
# Benchmark matching
###################################################################

DISPENSF_asthma<-DISPENSF_asthma %>%
  dplyr::select(SUBJID,CMDRGSYN,DISPDT,DOSETX,QUANTITY,DOSEDIRC) %>%
  mutate(DISPDT=as.Date(DISPDT,"%d%b%Y")) %>%
  arrange(SUBJID,CMDRGSYN,DISPDT) %>%
  group_by(SUBJID,CMDRGSYN) %>%
  mutate(DID = row_number())

PRESCR1F_asthma<-PRESCR1F_asthma %>%
  dplyr::select(SUBJID,CMDRGSYN,PRESCDT,DOSETX,QUANTITY,DOSEDIRC) %>%
  mutate(PRESCDT=as.Date(PRESCDT,"%d%b%Y")) %>%
  arrange(SUBJID,CMDRGSYN,PRESCDT) %>%
  group_by(SUBJID,CMDRGSYN) %>%
  mutate(PID = row_number())

rm(list=setdiff(ls(),c("DISPENSF_asthma","PRESCR1F_asthma",ls()[which(substr(ls(),1,5)=="check")])))

merge_benchmark<-merge(DISPENSF_asthma, PRESCR1F_asthma, all = TRUE)
merge_benchmark<-merge_benchmark[which(!is.na(merge_benchmark$DISPDT) & !is.na(merge_benchmark$PRESCDT)),]
merge_benchmark<-merge_benchmark[which(merge_benchmark$DISPDT>=merge_benchmark$PRESCDT),]
merge_benchmark<-merge_benchmark[which((merge_benchmark$PRESCDT %m+% months(6))>=merge_benchmark$DISPDT),]

# matching
for (i in unique(merge_benchmark$DID)) {
  merge_benchmark<-merge_benchmark %>%
    group_by(SUBJID,CMDRGSYN,DID) %>%
    mutate(match = ifelse(DID==i,PID,0)) %>%
    group_by(SUBJID,CMDRGSYN) %>%
    mutate(flag = max(match))
  # Get rid of the unmatched records for the prescription we just matched, 
  # and the unmatched records for the dispensing we just matched
  merge_benchmark<-merge_benchmark[which((merge_benchmark$DID==i & merge_benchmark$PID==merge_benchmark$flag) |
                       (merge_benchmark$DID!=i & merge_benchmark$PID!=merge_benchmark$flag)),]
  merge_benchmark<-dplyr::select(merge_benchmark,-c("match","flag"))
}

#  Second version, without dose directions
DISPENSF_asthma<-DISPENSF_asthma %>%
  rename(DOSEDIRC_d=DOSEDIRC)
merge_benchmark2<-merge(DISPENSF_asthma, PRESCR1F_asthma, all = TRUE)
merge_benchmark2<-merge_benchmark2[which(!is.na(merge_benchmark2$DISPDT) & !is.na(merge_benchmark2$PRESCDT)),]
merge_benchmark2<-merge_benchmark2[which(merge_benchmark2$DISPDT>=merge_benchmark2$PRESCDT),]
merge_benchmark2<-merge_benchmark2[which((merge_benchmark2$PRESCDT %m+% months(6))>=merge_benchmark2$DISPDT),]

# matching
for (i in unique(merge_benchmark2$DID)) {
  merge_benchmark2<-merge_benchmark2 %>%
    group_by(SUBJID,CMDRGSYN,DID) %>%
    mutate(match = ifelse(DID==i,PID,0)) %>%
    group_by(SUBJID,CMDRGSYN) %>%
    mutate(flag = max(match))
  # Get rid of the unmatched records for the prescription we just matched, 
  # and the unmatched records for the dispensing we just matched
  merge_benchmark2<-merge_benchmark2[which((merge_benchmark2$DID==i & merge_benchmark2$PID==merge_benchmark2$flag) |
                                           (merge_benchmark2$DID!=i & merge_benchmark2$PID!=merge_benchmark2$flag)),]
  merge_benchmark2<-dplyr::select(merge_benchmark2,-c("match","flag"))
}

prescriptions<-nrow(PRESCR1F_asthma)
dispensings<-nrow(DISPENSF_asthma)

#  Third version, complete cases only
DISPENSF_asthma<-DISPENSF_asthma[complete.cases(DISPENSF_asthma),] %>%
  group_by(SUBJID,CMDRGSYN) %>%
  mutate(DID = row_number())
PRESCR1F_asthma<-PRESCR1F_asthma[complete.cases(PRESCR1F_asthma),] %>%
  group_by(SUBJID,CMDRGSYN) %>%
  mutate(PID = row_number())
merge_benchmark3<-merge(DISPENSF_asthma, PRESCR1F_asthma, all = TRUE)
merge_benchmark3<-merge_benchmark3[which(!is.na(merge_benchmark3$DISPDT) & !is.na(merge_benchmark3$PRESCDT)),]
merge_benchmark3<-merge_benchmark3[which(merge_benchmark3$DISPDT>=merge_benchmark3$PRESCDT),]
merge_benchmark3<-merge_benchmark3[which((merge_benchmark3$PRESCDT %m+% months(6))>=merge_benchmark3$DISPDT),]

# matching
for (i in unique(merge_benchmark3$DID)) {
  merge_benchmark3<-merge_benchmark3 %>%
    group_by(SUBJID,CMDRGSYN,DID) %>%
    mutate(match = ifelse(DID==i,PID,0)) %>%
    group_by(SUBJID,CMDRGSYN) %>%
    mutate(flag = max(match))
  # Get rid of the unmatched records for the prescription we just matched, 
  # and the unmatched records for the dispensing we just matched
  merge_benchmark3<-merge_benchmark3[which((merge_benchmark3$DID==i & merge_benchmark3$PID==merge_benchmark3$flag) |
                                             (merge_benchmark3$DID!=i & merge_benchmark3$PID!=merge_benchmark3$flag)),]
  merge_benchmark3<-dplyr::select(merge_benchmark3,-c("match","flag"))
}


prescriptions_cc<-nrow(PRESCR1F_asthma)
dispensings_cc<-nrow(DISPENSF_asthma)
first_benchmark<-nrow(merge_benchmark)
second_benchmark<-nrow(merge_benchmark2)
third_benchmark<-nrow(merge_benchmark3)
rm(list=setdiff(ls(),c("prescriptions","dispensings","prescriptions_cc","dispensings_cc",
                       "first_benchmark","second_benchmark","third_benchmark",
                       ls()[which(substr(ls(),1,5)=="check")])))

# making sure I don't overwrite the actual data from James!
save.image("results_version_final_benchmarking_internal.RData")


