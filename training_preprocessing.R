## Affective Control Training Study
## Training data import and preprocessing
## Script written by Jovita T. Leung
## Last update: 03/03/2021

rm(list = ls()) 

## Load libraries 
library (doBy)
library(dplyr)
library(readxl) #read excel file

## Import data ---
d=read.csv("/Users/jovitaleung/Dropbox/PhD/Affective Control Training Study/ac_training/data/281019wodupl2.csv")

## Extract Digit Span task data
digit=subset(d[c(1,13:27)])
digit[,c("timeStamp_T2_1","timeStamp_T2_2","timeStamp_T2_3")] <- list(NULL)
summary(digit)
# Minimum digit is 2
# Output = score for emo; score for neutral; total score

## Extract Stroop task data 
stroop=subset(d[c(1,40:78)])
stroop[,c("timeStamp_T3_1","timeStamp_T3_2","timeStamp_T3_3")] <- list(NULL)
summary(stroop)
# Output = happy/sad congruent/incongruent/neutral RT; happy/sad congruent/incongruent/neutral accuracy

## Extract Set-shifting task data
setshift=subset(d[c(1,118:135)])
setshift[,c("timeStamp_T5_1","timeStamp_T5_2","timeStamp_T5_3")] <- list(NULL)
summary(setshift)
# Output = random error for emo; random error for neutral; RT for emo; RT for neutral

## Extract N-back (benchmark visuospatial) task data 
nback=subset(d[c(1,79:117)])
nback[,c("timeStamp_T4_1","timeStamp_T4_2","timeStamp_T4_3")] <- list(NULL)
summary(nback)