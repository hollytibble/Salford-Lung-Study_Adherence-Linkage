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
# Generating a new random ID for my matching
###################################################################

length_PRESCR1F<-nrow(PRESCR1F_asthma)
length_PRESCR1F_people<-length(unique(PRESCR1F_asthma$SUBJID))
PRESC_date_min<-min(PRESCR1F_asthma$PRESCDT)
PRESC_date_max<-max(PRESCR1F_asthma$PRESCDT)

length_DISPENSF<-nrow(DISPENSF_asthma)
length_DISPENSF_people<-length(unique(DISPENSF_asthma$SUBJID))
DISP_date_min<-min(DISPENSF_asthma$DISPDT)
DISP_date_max<-max(DISPENSF_asthma$DISPDT)

SUBJID<-unique(c(PRESCR1F_asthma$SUBJID,DISPENSF_asthma$SUBJID))
random_ID<-runif(length(SUBJID))
unique_people<-length(SUBJID)
ids<-as.data.frame(cbind(SUBJID, random_ID))
ids<-ids %>%
  arrange(random_ID) %>%
  mutate(random_ID = row_number()) 

###################################################################
# Fix Coding of CMDRGSYN & Coding Drug Class
###################################################################

PRESCR1F_asthma$DRUGDESC<-ifelse(is.na(PRESCR1F_asthma$DRUGDESC) | PRESCR1F_asthma$DRUGDESC=="",
                                 PRESCR1F_asthma$CMDRGSYN,
                                 toupper(PRESCR1F_asthma$DRUGDESC))
DISPENSF_asthma$DRUGDESC<-ifelse(is.na(DISPENSF_asthma$DRUGDESC) | DISPENSF_asthma$DRUGDESC=="",
                                 DISPENSF_asthma$CMDRGSYN,
                                 toupper(DISPENSF_asthma$DRUGDESC))

recode_CMDRGSYN<-as.data.frame(table(PRESCR1F_asthma$DRUGDESC))
names(recode_CMDRGSYN)[2]<-"PRESC_FREQ"
recode_CMDRGSYN<-full_join(recode_CMDRGSYN,as.data.frame(table(DISPENSF_asthma$DRUGDESC)))
names(recode_CMDRGSYN)[c(1,3)]<-c("Unique_description","DISP_FREQ")
length_unique_CMDRGSYN<-nrow(recode_CMDRGSYN)
names(recode_CMDRGSYN)[1]<-"DRUGDESC"
recode_CMDRGSYN$DRUGDESC<-as.character(recode_CMDRGSYN$DRUGDESC)

# Assigning the medication type
for (med in med_list) {
  filter<- vapply(get(paste0("med_",med)),
                  function(x) str_detect(recode_CMDRGSYN$DRUGDESC, x), 
                  logical(nrow(recode_CMDRGSYN)))
  recode_CMDRGSYN[,med] <- as.logical(apply(filter, 1, sum))
  rm(filter)
}

# Check OXIS
check_oxis<-recode_CMDRGSYN[which(str_detect(recode_CMDRGSYN$DRUGDESC,"OXIS")),c("DRUGDESC")]

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

# get rid of the medication flag variables
recode_CMDRGSYN<-dplyr::select(recode_CMDRGSYN,-med_list)

### How many flags does each prescription have?
recode_CMDRGSYN$flag_sum<-0
for (key in key_list) {
  temp <- recode_CMDRGSYN[,key]==T
  recode_CMDRGSYN[temp, "flag_sum"] <- recode_CMDRGSYN[temp, "flag_sum"]+1
  rm(temp)
}

# Drop the ones with no flags
recode_CMDRGSYN<-recode_CMDRGSYN[which(recode_CMDRGSYN$flag_sum!=0),]
length_unique_CMDRGSYN_asthma<-nrow(recode_CMDRGSYN)

# Assigning the brand
recode_CMDRGSYN$brandname<-NA
for (brand in brands) {
  recode_CMDRGSYN$brandname<-ifelse(is.na(recode_CMDRGSYN$brandname) & str_detect(recode_CMDRGSYN$DRUGDESC,brand),
                                    brand,
                                    recode_CMDRGSYN$brandname)
}
recode_CMDRGSYN$brandname<-ifelse(is.na(recode_CMDRGSYN$brandname),"GENERIC",recode_CMDRGSYN$brandname)

### code the drug class in cases with only one flag
recode_CMDRGSYN$drug_class<-NA
for (key in key_list) {
  temp <- recode_CMDRGSYN[,key]==T & recode_CMDRGSYN$flag_sum==1
  recode_CMDRGSYN[temp, "drug_class"] <- key
  rm(temp)
}

# Code the two-flag cases
recode_CMDRGSYN$drug_class<-ifelse(recode_CMDRGSYN$ICS==1 & recode_CMDRGSYN$LABA==1,"ICS+LABA",recode_CMDRGSYN$drug_class)
recode_CMDRGSYN$drug_class<-ifelse(recode_CMDRGSYN$LAMA==1 & recode_CMDRGSYN$SABA==1,"SABA",recode_CMDRGSYN$drug_class)
recode_CMDRGSYN<-recode_CMDRGSYN[,c("DRUGDESC","CMDRGSYN","drug_class","brandname")]

# Drop records based on keywords
keywords<-c("NASAL","NOSE","NOSTRIL","NASULE","HAYFEVER",
            "EYE","EAR","DROP","TONGUE",
            "FOAM","ENEMA","RECTAL",
            "GASTRO","MODIFIED",
            "CREAM", "APPLY","SKIN","ULCER","OINTMENT","PATCH",
            "CAPSULE", "SACHET", "SPRAY",
            "AZELASTINE","NASONEX","FLIXONASE","ANORA ELLIPTA",
            "SUMATRIPTAN","AVAMYS","RHINOCORT","NASOBEC","NASOFAN")
exclusions<-c()
for (keyword in keywords) {
  assign(paste(keyword,"freq",sep="_"), sum(str_detect(recode_CMDRGSYN$DRUGDESC,keyword)==T &
                                              ((recode_CMDRGSYN$drug_class!="steroid" & 
                                                  recode_CMDRGSYN$drug_class!="theophyllines") | 
                                                 !keyword %in% c("GASTRO","MODIFIED","CAPSULE")) &
                                              ((recode_CMDRGSYN$CMDRGSYN!="tiotropium" & recode_CMDRGSYN$CMDRGSYN!="glycopyrronium_bromide") |
                                              keyword!="CAPSULE")))
  exclusions<-c(exclusions,recode_CMDRGSYN[which(str_detect(recode_CMDRGSYN$DRUGDESC,keyword)==T &
                                                   ((recode_CMDRGSYN$drug_class!="steroid" & 
                                                       recode_CMDRGSYN$drug_class!="theophyllines") | 
                                                      !keyword %in% c("GASTRO","MODIFIED","CAPSULE"))&
                                                   ((recode_CMDRGSYN$CMDRGSYN!="tiotropium" & recode_CMDRGSYN$CMDRGSYN!="glycopyrronium_bromide") |
                                                      keyword!="CAPSULE")),
                                           c("DRUGDESC")])
}
exclusions<-unique(exclusions)
recode_CMDRGSYN<-recode_CMDRGSYN[which(!recode_CMDRGSYN$DRUGDESC %in% exclusions),]
length_unique_CMDRGSYN_asthma_drop<-length_unique_CMDRGSYN_asthma-nrow(recode_CMDRGSYN)

count_keyword_unique<-as.data.frame(table(recode_CMDRGSYN$CMDRGSYN, recode_CMDRGSYN$drug_class))

PRESCR1F_asthma<-inner_join(dplyr::select(PRESCR1F_asthma,-c("CMDRGSYN")),recode_CMDRGSYN)
DISPENSF_asthma<-inner_join(dplyr::select(DISPENSF_asthma,-c("CMDRGSYN")),recode_CMDRGSYN)

length_PRESCR1F_asthma<-nrow(PRESCR1F_asthma)
length_DISPENSF_asthma<-nrow(DISPENSF_asthma)

###################################################################
# Prescription Recoding 
###################################################################

PRESCR1F_asthma<-dplyr::select(PRESCR1F_asthma,-c("STUDYID","CMTERM","CMMODIFY","DRUGFORM"))

# Reformat to Date
PRESCR1F_asthma$PRESCDT<-as.Date(PRESCR1F_asthma$PRESCDT,"%d%b%Y")

# Editing the Dose
PRESCR1F_asthma$dose_strength<-toupper(paste(PRESCR1F_asthma$DOSETX,PRESCR1F_asthma$DRUGDESC,sep="_"))
PRESCR1F_asthma$dose_strength<-gsub("MICROGRAMS", "MCG", PRESCR1F_asthma$dose_strength)
PRESCR1F_asthma$dose_strength<-gsub("MICROGRAM", "MCG", PRESCR1F_asthma$dose_strength)
PRESCR1F_asthma$dose_strength<-gsub("MICROG", "MCG", PRESCR1F_asthma$dose_strength)
PRESCR1F_asthma$dose_strength<-gsub("UNITS", "U", PRESCR1F_asthma$dose_strength)
PRESCR1F_asthma$dose_strength<-str_replace_all(PRESCR1F_asthma$dose_strength,pattern=" ", repl="")
# Extract the numbers from the dose (only first if dose/dose)
PRESCR1F_asthma$DOSETX_x2<-NA
for (dose in c("0.5","500","400","320","300","250","200","184","160","125","100","92","80",
               "50","40","25","20","10","6","5","4","2","1")) {
  PRESCR1F_asthma$DOSETX_x2<-ifelse(is.na(PRESCR1F_asthma$DOSETX_x2) & 
                                      (PRESCR1F_asthma$dose_strength==dose |
                                         str_detect(PRESCR1F_asthma$dose_strength,paste0(dose,"MG"))==T |
                                         str_detect(PRESCR1F_asthma$dose_strength,paste0(dose,"MCG"))==T |
                                         str_detect(PRESCR1F_asthma$dose_strength,paste0(dose,"TURBOHALER"))==T |
                                         str_detect(PRESCR1F_asthma$dose_strength,paste0(dose,"EVO"))==T |
                                         str_detect(PRESCR1F_asthma$dose_strength,paste0(dose,"/"))==T),
                                    as.numeric(dose),
                                    PRESCR1F_asthma$DOSETX_x2)
}
PRESCR1F_asthma$dose_strength<-PRESCR1F_asthma$DOSETX_x2
PRESCR1F_asthma$DOSETX_x2<-NULL

###################################################################
# Dispensing Recoding 
###################################################################

DISPENSF_asthma<-dplyr::select(DISPENSF_asthma,-c("STUDYID","CMTERM","CMMODIFY"))

# Reformat to date
DISPENSF_asthma$DISPDT<-as.Date(DISPENSF_asthma$DISPDT,"%d%b%Y")

# Editing the Dose
DISPENSF_asthma$dose_strength<-toupper(paste(DISPENSF_asthma$DOSETX,DISPENSF_asthma$DRUGDESC,sep="_"))
DISPENSF_asthma$dose_strength<-gsub("MICROGRAMS", "MCG", DISPENSF_asthma$dose_strength)
DISPENSF_asthma$dose_strength<-gsub("MICROGRAM", "MCG", DISPENSF_asthma$dose_strength)
DISPENSF_asthma$dose_strength<-gsub("MICROG", "MCG", DISPENSF_asthma$dose_strength)
DISPENSF_asthma$dose_strength<-gsub("UNITS", "U", DISPENSF_asthma$dose_strength)
DISPENSF_asthma$dose_strength<-str_replace_all(DISPENSF_asthma$dose_strength,pattern=" ", repl="")
# Extract the numbers from the dose (only first if dose/dose)
DISPENSF_asthma$DOSETX_x2<-NA
for (dose in c("0.5","500","400","320","300","250","200","184","160","125","100","92","80",
               "50","40","25","20","10","6","5","4","2","1")) {
  DISPENSF_asthma$DOSETX_x2<-ifelse(is.na(DISPENSF_asthma$DOSETX_x2) & 
                                      (DISPENSF_asthma$dose_strength==dose |
                                         str_detect(DISPENSF_asthma$dose_strength,paste0(dose,"MG"))==T |
                                         str_detect(DISPENSF_asthma$dose_strength,paste0(dose,"MCG"))==T |
                                         str_detect(DISPENSF_asthma$dose_strength,paste0(dose,"TURBOHALER"))==T |
                                         str_detect(DISPENSF_asthma$dose_strength,paste0(dose,"EVO"))==T |
                                         str_detect(DISPENSF_asthma$dose_strength,paste0(dose,"/"))==T),
                                    as.numeric(dose),
                                    DISPENSF_asthma$DOSETX_x2)
}
DISPENSF_asthma$dose_strength<-DISPENSF_asthma$DOSETX_x2
DISPENSF_asthma$DOSETX_x2<-NULL

###################################################################
# Quantity Recoding 
###################################################################

quantity<-PRESCR1F_asthma[,c("CMDRGSYN","brandname","QUANTITY","dose_strength")]
quantity<-rbind(quantity,DISPENSF_asthma[,c("CMDRGSYN","brandname","QUANTITY","dose_strength")])

quantity<-quantity %>%
  filter(!is.na(QUANTITY) & QUANTITY>=28) %>%
  add_count(CMDRGSYN,brandname,dose_strength,QUANTITY, name="n") %>%
  add_count(CMDRGSYN,brandname,dose_strength, name="nn") %>%
  distinct() %>%
  arrange(CMDRGSYN,brandname,-n) %>%
  group_by(CMDRGSYN, brandname,dose_strength) %>%
  mutate(rank = row_number()) %>%
  filter(rank<=2) %>%
  mutate(rank_p1 = max(n)*100/nn) %>%
  mutate(rank_p2 = ifelse(min(n)==max(n),NA,min(n)*100/nn)) %>%
  dplyr::select(-n, -nn) %>%
  tidyr::spread(rank,QUANTITY)
names(quantity)<-c("CMDRGSYN","brandname","dose_strength","DOSES1_perc","DOSES2_perc","DOSES1","DOSES2")

PRESCR1F_asthma<-left_join(PRESCR1F_asthma,quantity)
DISPENSF_asthma<-left_join(DISPENSF_asthma,quantity)
rm(quantity)

PRESCR1F_asthma$QUANTITY_primary<-ifelse(is.na(PRESCR1F_asthma$QUANTITY),
                                         PRESCR1F_asthma$DOSES1,
                                         ifelse(PRESCR1F_asthma$QUANTITY>=28,
                                                PRESCR1F_asthma$QUANTITY,
                                                PRESCR1F_asthma$DOSES1*PRESCR1F_asthma$QUANTITY))

DISPENSF_asthma$QUANTITY_primary_d<-ifelse(is.na(DISPENSF_asthma$QUANTITY),
                                         DISPENSF_asthma$DOSES1,
                                         ifelse(DISPENSF_asthma$QUANTITY>=28,
                                                DISPENSF_asthma$QUANTITY,
                                                DISPENSF_asthma$DOSES1*DISPENSF_asthma$QUANTITY))

PRESCR1F_asthma$QUANTITY_alias<-ifelse(is.na(PRESCR1F_asthma$QUANTITY),
                                       PRESCR1F_asthma$DOSES2,
                                       ifelse(PRESCR1F_asthma$QUANTITY<28,
                                              PRESCR1F_asthma$DOSES2*PRESCR1F_asthma$QUANTITY,
                                              ifelse(PRESCR1F_asthma$QUANTITY==PRESCR1F_asthma$DOSES1,
                                                     PRESCR1F_asthma$DOSES2,PRESCR1F_asthma$DOSES1)))

DISPENSF_asthma$QUANTITY_alias_d<-ifelse(is.na(DISPENSF_asthma$QUANTITY),
                                       DISPENSF_asthma$DOSES2,
                                       ifelse(DISPENSF_asthma$QUANTITY<28,
                                              DISPENSF_asthma$DOSES2*DISPENSF_asthma$QUANTITY,
                                              ifelse(DISPENSF_asthma$QUANTITY==DISPENSF_asthma$DOSES1,
                                                     DISPENSF_asthma$DOSES2,DISPENSF_asthma$DOSES1)))

PRESCR1F_asthma<-dplyr::select(PRESCR1F_asthma,-c("DOSES1","DOSES2", "DOSES1_perc","DOSES2_perc"))
DISPENSF_asthma<-dplyr::select(DISPENSF_asthma,-c("DOSES1","DOSES2", "DOSES1_perc","DOSES2_perc"))

###################################################################
# Prescribing Data - Primary Care
###################################################################

PRESCR1F_asthma<-PRESCR1F_asthma %>%
  # remove duplicates 
  group_by(SUBJID, CMDRGSYN,brandname, PRESCDT, QUANTITY_primary, dose_strength) %>%
  slice(1) %>%
  group_by( ) %>%
  # duplicates on all but one variable, and there is missingness
  add_count(SUBJID, CMDRGSYN,brandname, PRESCDT, QUANTITY_primary) %>%
  filter(n==1 | !is.na(dose_strength)) %>%
  add_count(SUBJID, CMDRGSYN,brandname, PRESCDT, dose_strength, name="nn") %>%
  filter(nn==1 | !is.na(QUANTITY_primary))  

length_PRESCR1F_asthma_drop<-length_PRESCR1F_asthma-nrow(PRESCR1F_asthma)
length_PRESCR1F_asthma_final<-nrow(PRESCR1F_asthma)

PRESCR1F_asthma<-PRESCR1F_asthma %>%
  # add overall record ID
  mutate(P1IDx = row_number()) %>%
  # add a subject-specific record ID for the later merging
  arrange(SUBJID,CMDRGSYN,PRESCDT) %>%
  group_by(SUBJID,CMDRGSYN) %>%
  mutate(P1ID = row_number()) %>%
  group_by()

count_keyword_presc<-as.data.frame(table(PRESCR1F_asthma$CMDRGSYN, PRESCR1F_asthma$drug_class))

PRESCR1F_out<- dplyr::select(PRESCR1F_asthma,c("SUBJID","DRUGDESC","CMDRGSYN","PRESCDT","DOSETX","QUANTITY_primary",
                                               "QUANTITY_alias", "RWFLAGCD","drug_class","dose_strength","P1IDx","brandname"))

PRESCR1F_asthma<- dplyr::select(PRESCR1F_asthma,c(SUBJID,CMDRGSYN,PRESCDT,dose_strength,QUANTITY_primary,
                                                  QUANTITY_alias,P1IDx,P1ID, drug_class,brandname))


###################################################################
# Dispensing Data
###################################################################

DISPENSF_asthma<-DISPENSF_asthma %>%
  # remove duplicates 
  group_by(SUBJID, CMDRGSYN, brandname, DISPDT, QUANTITY_primary_d, dose_strength) %>%
  slice(1) %>%
  group_by( ) %>%
  # duplicates on all but one variable, and there is missingness
  add_count(SUBJID, CMDRGSYN, brandname, DISPDT, QUANTITY_primary_d) %>%
  filter(n==1 | !is.na(dose_strength)) %>%
  add_count(SUBJID, CMDRGSYN, brandname, DISPDT, dose_strength, name="nn") %>%
  filter(nn==1 | !is.na(QUANTITY_primary_d)) 

length_DISPENSF_asthma_drop<-length_DISPENSF_asthma-nrow(DISPENSF_asthma)
length_DISPENSF_asthma_final<-nrow(DISPENSF_asthma)

# add a record ID for the later merging
DISPENSF_asthma<-DISPENSF_asthma %>%
  # add overall record ID
  mutate(DIDx = row_number()) %>%
  # add a subject-specific record ID for the later merging
  arrange(SUBJID,CMDRGSYN,DISPDT) %>%
  group_by(SUBJID,CMDRGSYN) %>%
  mutate(DID = row_number()) %>%
  group_by()

count_keyword_disp<-as.data.frame(table(DISPENSF_asthma$CMDRGSYN, DISPENSF_asthma$drug_class))

DISPENSF_out<- dplyr::select(DISPENSF_asthma,c("SUBJID","DRUGDESC","CMDRGSYN","DISPDT","DOSETX","QUANTITY_primary_d",
                                               "QUANTITY_alias_d","RWFLAGCD","drug_class","dose_strength","DIDx","brandname"))

DISPENSF_asthma<- dplyr::select(DISPENSF_asthma,c(SUBJID,CMDRGSYN,DISPDT,dose_strength,QUANTITY_primary_d,
                                                  QUANTITY_alias_d,DIDx,DID, drug_class,brandname))

###################################################################
# Drug / keyword quantities
###################################################################

names(count_keyword_unique)<-c("key","class","unique_freq")
count_keyword_unique<-count_keyword_unique[which(count_keyword_unique$unique_freq!=0),]
names(count_keyword_presc)<-c("key","class","presc_freq")
names(count_keyword_disp)<-c("key","class","disp_freq")

key_quantity_table<-left_join(count_keyword_unique,count_keyword_disp)
key_quantity_table<-left_join(key_quantity_table,count_keyword_presc)
key_quantity_table$disp_freq<-ifelse(is.na(key_quantity_table$disp_freq),0,key_quantity_table$disp_freq)
key_quantity_table$presc_freq<-ifelse(is.na(key_quantity_table$presc_freq),0,key_quantity_table$presc_freq)
rm(count_keyword_unique,count_keyword_presc,count_keyword_disp)

###################################################################
# PRESCRF1 to DISPENSF matching setup
###################################################################

# deterministic matching variables: "SUBJID"   "CMDRGSYN"
# non-matching variables: "PRESCDT" "DISPDT" 
# imperfect matching variables (rename in DISP):  "QUANTITY_primary" "QUANTITY_alias "dose_strength" "brandname"
DISPENSF_asthma<-rename(DISPENSF_asthma, dose_strength_d = dose_strength)
DISPENSF_asthma<-rename(DISPENSF_asthma, brandname_d = brandname)

# use full_join, which is m:m merging
merge<-merge(DISPENSF_asthma, PRESCR1F_asthma, all = TRUE)
DIDx_max<-max(merge$DIDx, na.rm=T)
P1IDx_max<-max(merge$P1IDx, na.rm=T)

# Keep only those with possible matches established by drug and ID
merge<-merge[which(!is.na(merge$P1ID) & !is.na(merge$DID)),]
candidate_links<-nrow(merge)

# get rid of possible links where  dispensing happened before prescription
merge<-merge[which(merge$DISPDT>=merge$PRESCDT),]
# get rid of possible linked dispensings for prescriptions that have expired - more than 6 months ago
merge<-merge[which((merge$PRESCDT %m+% months(6))>=merge$DISPDT),]
dates_dropped<-candidate_links-nrow(merge)
dates_remaining<-nrow(merge)

#  Weight of match
merge$weight_a<-0.1*(is.na(merge$brandname_d) | is.na(merge$brandname))
merge$weight_a<-merge$weight_a+0.2*(!is.na(merge$brandname_d) & !is.na(merge$brandname) & merge$brandname_d==merge$brandname)
merge$weight_b<-0.1*(is.na(merge$dose_strength) | is.na(merge$dose_strength_d))
merge$weight_b<-merge$weight_b+0.35*(!is.na(merge$dose_strength) & !is.na(merge$dose_strength_d) & merge$dose_strength==merge$dose_strength_d)
merge$weight_c<-0.1*(is.na(merge$QUANTITY_primary_d) | is.na(merge$QUANTITY_primary))
merge$weight_c<-merge$weight_c+0.15*(!is.na(merge$QUANTITY_primary_d) &
                                      !is.na(merge$QUANTITY_primary) &
                                      ((!is.na(merge$QUANTITY_alias_d) & merge$QUANTITY_primary==merge$QUANTITY_alias_d) |
                                         (!is.na(merge$QUANTITY_alias) & merge$QUANTITY_alias==merge$QUANTITY_primary_d)))
merge$weight_c<-merge$weight_c+0.35*(!is.na(merge$QUANTITY_primary_d) &
                                   !is.na(merge$QUANTITY_primary) &
                                   merge$QUANTITY_primary==merge$QUANTITY_primary_d)
merge$weight_d<-0.1*((merge$PRESCDT %m+% months(1))>=merge$DISPDT)

candidate_weight_a<-merge$weight_a
candidate_weight_b<-merge$weight_b
candidate_weight_c<-merge$weight_c
candidate_weight_d<-merge$weight_d
merge$weight<- merge$weight_a+merge$weight_b+merge$weight_c+merge$weight_d
candidate_weight_disp<-merge$weight

# time between dispensing and prescribing
merge <- merge %>%
  group_by(SUBJID, CMDRGSYN) %>%
  mutate(initiation_days = as.numeric(difftime(DISPDT,PRESCDT,units="days"))) %>%
  group_by() %>%
  arrange(SUBJID,CMDRGSYN,DID,initiation_days, weight) %>%
  mutate(match=NA) %>%
  dplyr::select(SUBJID, CMDRGSYN, DID, DIDx, initiation_days, P1ID, P1IDx, PRESCDT, drug_class, weight,
                dose_strength,QUANTITY_primary, brandname, weight_a, weight_b, weight_c, weight_d) 

weight_dropped<-nrow(merge[which(merge$weight<0.69),])
merge<-merge[which(merge$weight>=0.69),]
weight_dropped_sens<-nrow(merge[which(merge$weight<0.89),])
merge_sens<-merge[which(merge$weight>=0.89),]

candidate_links_retained<-nrow(merge)
candidate_links_retained_sens<-nrow(merge_sens)

no_candidates_disp<-DIDx_max-length(unique(merge$DIDx))
no_candidates_presc<-P1IDx_max-length(unique(merge$P1IDx))

###################################################################
# Matching algorithm
###################################################################

# matching
for (i in unique(merge$DID)) {
  merge<-merge %>%
    group_by(SUBJID,CMDRGSYN,DID) %>%
    mutate(match = ifelse(DID==i & initiation_days==min(initiation_days) ,P1ID,0)) %>%
    group_by(SUBJID,CMDRGSYN,DID,initiation_days) %>%
    mutate(match = ifelse(match!=0 & weight==max(weight),P1ID,0)) %>%
    group_by(SUBJID,CMDRGSYN) %>%
    mutate(flag = max(match))
  # Get rid of the unmatched records for the prescription we just matched, 
  # and the unmatched records for the dispensing we just matched
  merge<-merge[which((merge$DID==i & merge$P1ID==merge$flag) |
                       (merge$DID!=i & merge$P1ID!=merge$flag)),]
  merge<-dplyr::select(merge,-c("match","flag"))
}
matches<-nrow(merge)
# check all unique matches
p1ID_unique_merge_check<-nrow(unique(merge[,c("SUBJID", "CMDRGSYN", "P1ID")]))==matches
DID_unique_merge_check<-nrow(unique(merge[,c("SUBJID", "CMDRGSYN", "DID")]))==matches

# final weights
match_weight_a_m<-merge$weight_a
match_weight_b_m<-merge$weight_b
match_weight_c_m<-merge$weight_c
match_weight_d_m<-merge$weight_d
match_weight_disp<-merge$weight

###################################################################
# Quality Assurance - unmatched records
###################################################################

# Locate the data in the DISPENSF and PRESCR1F files for the unmatched records
removed_DIDx<-setdiff(seq(1:DIDx_max),unique(merge$DIDx))
removed_P1IDx<-setdiff(seq(1:P1IDx_max),unique(merge$P1IDx))
DISPENSF_out<-DISPENSF_out[which(DISPENSF_out$DIDx %in% removed_DIDx),
                                      c("SUBJID","brandname","CMDRGSYN","DISPDT","DIDx","drug_class",
                                        "dose_strength", "QUANTITY_primary_d")]
DISPENSF_out<-rename(DISPENSF_out,QUANTITY_primary=QUANTITY_primary_d)
PRESCR1F_out<-PRESCR1F_out[which(PRESCR1F_out$P1IDx %in% removed_P1IDx),
                            c("SUBJID","brandname","CMDRGSYN","PRESCDT","P1IDx","drug_class",
                              "dose_strength", "QUANTITY_primary")]

# merge them together
results<-bind_rows(DISPENSF_out,PRESCR1F_out)
results<-results %>%
  mutate(match=ifelse(is.na(P1IDx),"Disp not Presc","Presc not Disp")) %>%
  mutate(date = as.Date(ifelse(!is.na(PRESCDT),PRESCDT,DISPDT), origin = "1970-01-01")) %>%
  arrange(SUBJID,date) %>%
  dplyr::select(CMDRGSYN,brandname,drug_class,match, date,SUBJID,dose_strength, QUANTITY_primary) 

prescriptions<-bind_rows(merge, results[which(results$match=="Presc not Disp"),]) %>%
  mutate(unclaimed=ifelse(is.na(DID),1,0)) %>%
  mutate(PRESCDT=as.Date(ifelse(is.na(PRESCDT), 
                                      date, PRESCDT), 
                               origin = "1970-01-01")) %>%
  mutate(season=ifelse(month(PRESCDT) %in% c(3,4,5), "Spring",
                        ifelse(month(PRESCDT) %in% c(6,7,8), "Summer",
                               ifelse(month(PRESCDT) %in% c(9,10,11), "Autumn",
                                      "Winter")))) %>%
  group_by(SUBJID) %>%
  arrange(SUBJID,PRESCDT) %>%
  mutate(previously_unclaimed = (cumsum(unclaimed)-unclaimed)/(cumsum(!is.na(unclaimed))-!is.na(unclaimed))) %>%
  mutate(previously_unclaimed=ifelse(is.na(previously_unclaimed),0,previously_unclaimed)) 

temp<-prescriptions[which(prescriptions$previously_unclaimed>0),]
t1 <- quantile(temp$previously_unclaimed,probs=0.3333)
t2 <- quantile(temp$previously_unclaimed,probs=0.6666)

prescriptions<- prescriptions %>%
  mutate(previously_unclaimed_bin = ifelse(previously_unclaimed>0,
                                          "YES",
                                           "NO")) %>%
  mutate(previously_unclaimed_cat = ifelse(previously_unclaimed>0,
                                           ifelse(previously_unclaimed>t1,
                                                  ifelse(previously_unclaimed>t2,
                                                         "HIGH",
                                                         "MID"),
                                                  "LOW"),
                                           "LOW")) %>%
  dplyr::select(SUBJID,CMDRGSYN,brandname,initiation_days,PRESCDT, drug_class, unclaimed, season,
                previously_unclaimed,dose_strength,QUANTITY_primary,previously_unclaimed_bin,previously_unclaimed_cat) %>%
  mutate(year=year(PRESCDT))

prescriptions$season<-relevel(as.factor(prescriptions$season),ref="Spring")
prescriptions$drug_class<-relevel(as.factor(prescriptions$drug_class),ref="ICS")
prescriptions$previously_unclaimed_cat<-relevel(as.factor(prescriptions$previously_unclaimed_cat),ref="LOW")

# merge in my random ID
results<-left_join(results,ids)
results$SUBJID<-NULL

###################################################################
# Sensitivity analysis
###################################################################

# matching
for (i in rev(unique(merge_sens$DID))) {
  merge_sens<-merge_sens %>%
    group_by(SUBJID,CMDRGSYN,DID) %>%
    mutate(match = ifelse(DID==i & initiation_days==min(initiation_days),P1ID,0)) %>%
    group_by(SUBJID,CMDRGSYN,DID,initiation_days) %>%
    mutate(match = ifelse(match!=0 & weight==max(weight),P1ID,0)) %>%
    group_by(SUBJID,CMDRGSYN) %>%
    mutate(flag = max(match))
  # Get rid of the unmatched records for the prescription we just matched, 
  # and the unmatched records for the dispensing we just matched
  merge_sens<-merge_sens[which((merge_sens$DID==i & merge_sens$P1ID==merge_sens$flag) |
                       (merge_sens$DID!=i & merge_sens$P1ID!=merge_sens$flag)),]
  merge_sens<-dplyr::select(merge_sens,-c("match","flag"))
}
matches_weights_sens<-merge_sens$weight
matches_sens<-nrow(merge_sens)

# Locate the data in the DISPENSF and PRESCR1F files for the unmatched records
removed_DIDx_sens<-setdiff(seq(1:DIDx_max),unique(merge_sens$DIDx))
removed_P1IDx_sens<-setdiff(seq(1:P1IDx_max),unique(merge_sens$P1IDx))
DISPENSF_out_sens<-DISPENSF_out[which(DISPENSF_out$DIDx %in% removed_DIDx_sens),
                           c("SUBJID","brandname","CMDRGSYN","DISPDT","DIDx","drug_class")]
PRESCR1F_out_sens<-PRESCR1F_out[which(PRESCR1F_out$P1IDx %in% removed_P1IDx_sens),
                           c("SUBJID","brandname","CMDRGSYN","PRESCDT","P1IDx","drug_class")]

# merge them together
results_sens<-bind_rows(DISPENSF_out_sens,PRESCR1F_out_sens)
results_sens<-results_sens %>%
  mutate(match=ifelse(is.na(P1IDx),"Disp not Presc","Presc not Disp")) %>%
  mutate(date = as.Date(ifelse(!is.na(PRESCDT),PRESCDT,DISPDT), origin = "1970-01-01")) %>%
  arrange(SUBJID,date) %>%
  dplyr::select(brandname,CMDRGSYN,drug_class,match, date,SUBJID) 

# merge in my random ID
results_sens<-left_join(results_sens,ids)
results_sens$SUBJID<-NULL

###################################################################
# Results
###################################################################

temp<-prescriptions %>%
  filter(drug_class %in% c("ICS+LABA","ICS","LABA")) %>%
  group_by(SUBJID,year,drug_class) %>%
  add_count() %>%
  slice(1) %>%
  dplyr::select(SUBJID,year,drug_class,n) %>%
  tidyr::spread(drug_class,n) %>%
  mutate(group = ifelse(!is.na(ICS) & !is.na(LABA),"SEP",
                        ifelse(!is.na(`ICS+LABA`),"TOG",
                               NA))) %>%
  filter(!is.na(group)) %>%
  dplyr::select(SUBJID,year,group) 

claiming_ICS_LABA<-inner_join(prescriptions,temp) %>%
  filter((drug_class=="ICS+LABA" & group=="TOG") |
           (drug_class=="ICS" & group=="SEP")) %>%
  group_by(group,unclaimed) %>%
  add_count() %>%
  slice(1) %>%
  dplyr::select(group,unclaimed,n)  %>% 
  tidyr::spread(unclaimed, n) %>% 
  mutate(perc = `0`*100/(`1`+`0`))
rm(temp)

# what percentage went unclaimed?
perc_unclaimed<-sum(prescriptions$unclaimed)*100/nrow(prescriptions)
unclaimed_CMDRGSYN<-as.data.frame(table(prescriptions$unclaimed,prescriptions$drug_class)) %>% 
  tidyr::spread(Var1, Freq) %>% mutate(perc = `0`/(`1`+`0`))

#  Person-level
person_unclaimed<-prescriptions %>%
  add_count(SUBJID,name="presc_total") %>%
  group_by(SUBJID) %>%
  mutate(unclaimed_total = sum(unclaimed))  %>%
  slice(1) %>%
  ungroup %>%
  dplyr::select(presc_total,unclaimed_total)

# Summary Stats
initiation<-prescriptions$initiation_days

# survival for time to claiming
prescriptions$event<-ifelse(prescriptions$unclaimed==1 | prescriptions$initiation_days>21, 0, 1)
prescriptions$initiation_days_x<-ifelse(prescriptions$event==0, 21, prescriptions$initiation_days)
plot<-prescriptions[,c("initiation_days_x", "event","season","previously_unclaimed_bin")] 
cox1<-summary(coxph(Surv(initiation_days_x, event==1) ~ season + drug_class + previously_unclaimed_cat + QUANTITY_primary, 
      data = prescriptions))


rm(list=c(ls()[which(substr(ls(),1,3)=="key" | substr(ls(),1,3)=="med")]))
rm(list=c(ls()[which(substr(ls(),1,5)=="merge")]))
rm(list=c(ls()[which(substr(ls(),1,7)=="removed")]))
rm(list=c(ls()[which(substr(ls(),1,8) %in% c("PRESCR1F","DISPENSF"))]))
rm(ids, dose, recode_CMDRGSYN, random_ID, SUBJID,
   prescriptions, brands,brand,DIDx_max,P1IDx_max,i)
save.image("results_version_final_internal.RData")


