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

## Import data 
t1=read.csv("/Users/jovitaleung/Dropbox/PhD/Affective Control Training Study/ac_training/raw_data/t1_questionnaire_raw_clean.csv")
colnames(t1)[colnames(t1) == 'subjid'] <- 'subjno'
summary(t1)

# dem=read.csv("/Users/jovitaleung/Dropbox/PhD/Affective Control Training Study/ac_training/raw_data/demo_mastersheet.csv")
# demo=subset(dem,select = c("id","gender","age","ses","pre.post.days","iq"))
# names(demo)=c("subjno","gender","decimal_age","ses","days","iq")
# summary(demo)

age=read_excel("/Users/jovitaleung/Dropbox/PhD/Affective Control Training Study/Data/Age and gender numbers.xlsx")
names(age)=c("subjno","gender","dob","test_date","age")
age$gender=as.factor(recode(age$gender, 'Female'=1, 'Male'=2, 'Prefer not to say'=4, '-99'=999))
age$gender[age$gender==999]=NA
age$gender=factor(age$gender, levels=c('1','2','4'),labels = c("female","male","other"))
summary(age)

## Tidy up
t1[c("subjno","gender")]= lapply(t1[c("subjno","gender")],factor)
t1$gender=factor(t1$gender, levels=c('1','2','4'),labels = c("female","male","other"))

# Calculate decimal age
age$dob=as.Date(age$dob)
age$test_date=as.Date(age$test_date)
age$decimal_age=round(age_calc(age$dob, enddate = age$test_date, units = "years", precise = TRUE),digits = 1)
summary(age$decimal_age)

demos=subset(age,select = c("subjno","gender","age","decimal_age"))


