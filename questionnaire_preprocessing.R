## Affective Control Training Study
## Questionniare data import and scoring
## Script written by Jovita T. Leung
## Last update: 24/02/2021

rm(list = ls()) 

## Load libraries 
library (doBy)
library(dplyr)
library(readxl) #read excel file
library(eeptools) # calculate age from dob

## Import data ---
t1=read.csv("/Users/jovitaleung/Dropbox/PhD/Affective Control Training Study/ac_training/data/t1_questionnaire_raw_clean.csv")
colnames(t1)[colnames(t1) == 'subjid'] <- 'subjno'
summary(t1)

d=read.csv("/Users/jovitaleung/Dropbox/PhD/Affective Control Training Study/ac_training/data/baseline_mastersheet.csv")
d1=subset(d,select = c("id","gender","age","ses","pre.post.days","iq.sum","pds.sum"))
names(d1)=c("subjno","gender","decimal_age","ses","test_days","iq_sum","pds")
d1[c("subjno","gender")]=lapply(d1[c("subjno","gender")],factor)
summary(d1)

## Tidy up ---
t1[c("subjno","gender")]= lapply(t1[c("subjno","gender")],factor)
t1$gender=factor(t1$gender, levels=c('1','2','4'),labels = c("female","male","other"))
t1=subset(t1,select = -c(gender,dob))
summary(t1)

# # Calculate decimal age
# age$dob=as.Date(age$dob)
# age$test_date=as.Date(age$test_date)
# age$decimal_age=round(age_calc(age$dob, enddate = age$test_date, units = "years", precise = TRUE),digits = 1)
# summary(age$decimal_age)
# demos=subset(age,select = c("subjno","gender","age","decimal_age"))

## Scoring questionnaire data --- 
# T1 
#SDQ
sdq=subset(t1[,c(1,22:46)])
sdq[,2:26][sdq[,2:26]=="1"] <- 11 #recode values to right value for scoring
sdq[,2:26][sdq[,2:26]=="2"] <- 22
sdq[,2:26][sdq[,2:26]=="3"] <- 33
sdq[,2:26][sdq[,2:26]=="11"] <- 0
sdq[,2:26][sdq[,2:26]=="22"] <- 1
sdq[,2:26][sdq[,2:26]=="33"] <- 2

sdq[,c(8,12,15,22,26)]= 2-sdq[,c(8,12,15,22,26)]#reverse scoring 

sdq$sdq_sum_t1<- rowSums(sdq[c(-1,-2,-5,-10,-18,-21)], na.rm=TRUE) #total difficulties score (sum of all items except prosocial scale)
sdq$sdq_emotional_t1 <- rowSums(sdq[c(4,9,14,17,25)], na.rm=TRUE) #emotional problem subscale
sdq$sdq_conduct_t1 <- rowSums(sdq[c(6,8,13,19,23)], na.rm=TRUE) #conduct problem subscale
sdq$sdq_hyperactivity_t1<- rowSums(sdq[c(3,11,16,22,26)], na.rm=TRUE) #hyperactivity subscale
sdq$sdq_peer_t1<- rowSums(sdq[c(7,12,15,20,24)], na.rm=TRUE) #peer problem subscale
sdq$sdq_prosocial_t1 <- rowSums(sdq[c(2,5,10,18,21)], na.rm=TRUE) #prosocial subscale

sdq1=subset(sdq,select = c('subjno','sdq_sum_t1','sdq_emotional_t1','sdq_conduct_t1','sdq_hyperactivity_t1','sdq_peer_t1','sdq_prosocial_t1'))
sdq1[sdq1==0]=NA #remove zeros created with missing data
summary(sdq1)


#DERS
ders=subset(t1[,c(1,47:82)])
summary(ders)
ders[,c(2,3,7,8,9,11,18,21,23,25,35)]= 5-ders[,c(2,3,7,8,9,11,18,21,23,25,35)] #reverse scoring

ders$ders_sum_t1 <- rowSums(ders[2:37], na.rm=TRUE) #overall score
ders$ders_nonaccept_t1<- rowSums(ders[c(26,22,13,12,30,24)], na.rm=TRUE) #nonacceptance of emotional responses subscale
ders$ders_goals_t1 <- rowSums(ders[c(27,19,14,34,21)], na.rm=TRUE) #difficulties engaging in goal-directed subsclae
ders$ders_impulse_t1 <- rowSums(ders[c(33,28,15,20,4,25)], na.rm=TRUE) #impulse control difficulties 
ders$ders_aware_t1 <- rowSums(ders[c(7,3,11,18,9,35)], na.rm=TRUE) #lack of emotional awareness
ders$ders_strategies_t1 <- rowSums(ders[c(17,16,32,36,29,23,37,31)], na.rm=TRUE) #limited access to emotion regulation strategies
ders$ders_clarity_t1 <- rowSums(ders[c(6,5,10,8,2)], na.rm=TRUE) #lack of emotional clarity

ders1=subset(ders, select = c('subjno','ders_sum_t1','ders_nonaccept_t1','ders_goals_t1','ders_impulse_t1','ders_aware_t1','ders_strategies_t1','ders_clarity_t1'))
ders1[ders1==0]=NA
summary(ders1)


#PANAS
panas=subset(t1[,c(1:21)])
panas$panas_pos_t1<- rowSums(panas[c(2,4,6,10,11,13,15,17,18,20)], na.rm = TRUE)
panas$panas_neg_t1<- rowSums(panas[c(3,5,7,8,9,12,14,16,19,21)], na.rm = TRUE)
panas1=subset(panas, select = c('subjno','panas_pos_t1','panas_neg_t1'))

#Self-control Scale
scs=subset(t1[,c(1,83:95)])
scs[,c(2,4,6,8,9,10,11,12,13)]= 5-scs[,c(2,4,6,8,9,10,11,12,13)] #reverse scoring
scs$scs_t1 <- rowSums(scs[2:14], na.rm = TRUE)
scs1=subset(scs,select = c('subjno','scs_t1'))


## Mergeing T1 questionnaire data --- 
t1_scored=Reduce(function(x,y) merge(x=x,y=y, by="subjno"), 
                 list(demos, sdq1, ders1, panas1, scs1))
length(unique(t1_scored$subjno)) #231
t1_scored=t1_scored[!duplicated(t1_scored$subjno),]
summary(t1_scored)
write.csv(t1_scored,file="/Users/jovitaleung/Dropbox/PhD/Affective Control Training Study/ac_training/processed_data/t1_questionnaire_scored.csv")
