install.packages('corrplot')
install.packages('purr')
library(lavaan)
library(semPlot)
library (psych)
library (ppcor)
library (corpcor)
library (GPArotation)
library(nFactors)
library(corrplot)
library(dplyr)
library(rlang)
library(pastecs)

setwd("Z:/Data/WT_TrainingStudy")
Data1<-read.csv("281019wodupl.csv")
Data2<-read.csv("T1_data.csv")
Data2<-subset(Data2, select = c("age","gender"))
Data3<-read.csv("baseline_mastersheet.csv")
Data4<-read.csv("T2_data.csv")
Data5<-read.csv("DataTT.csv")
Data1<-merge(Data1,Data2, by = "ID")
Data1<-merge(Data1,Data3, by = "ID")
DataT2<-merge(Data1,Data4, by = "ID")
Data1 <- mutate_all(Data1, function(x) as.numeric(as.character(x)))

Data1_rescale<-Data1

Data1_rescale$Scores.neutColNumRandErrRT_T5_1<-Data1$Scores.neutColNumRandErrRT_T5_1/1000
Data1_rescale$Scores.emoColNumRandErrRT_T5_1 <-Data1$Scores.emoColNumRandErrRT_T5_1/1000         
Data1_rescale$Scores.RTSadIncong_T3_1        <-Data1$Scores.RTSadIncong_T3_1/1000
Data1_rescale$Scores.RTSadCong_T3_1          <-Data1$Scores.RTSadCong_T3_1/1000           
Data1_rescale$Scores.RTHappyCong_T3_1        <-Data1$Scores.RTHappyCong_T3_1/1000
Data1_rescale$Scores.RTHappyIncong_T3_1        <-Data1$Scores.RTHappyIncong_T3_1/1000
Data1_rescale$Scores.RTSadNeut_T3_1          <-Data1$Scores.RTSadNeut_T3_1/1000
Data1_rescale$Scores.RTHappyNeut_T3_1        <-Data1$Scores.RTHappyNeut_T3_1/1000

#######Participants

#Age
stat.desc(Data1_rescale$age)

#Gender
Gender<-table(Data1_rescale$gender)
Gender

#Parental edu
Fatheredu<-table(Data1_rescale$father.edu)
Motheredu<-table(Data1_rescale$mother.edu)
Fatheredu
Motheredu

#SES - check formula used by Jovita
SES<-table(Data1_rescale$ses)
SES

#IQ
stat.desc(Data1_rescale$iq.sum)

Data1_rescale$iq.sum
#Generate the true moel
sim_sus<-'
digneu=~.7*dprim_dn+.7*rt_dn
shiftneu=~.7*re_sn+.7*rt_sn

cogcon=~.7*digneu+.7*shiftneu


digaff=~.7*dprim_da+.7*rt_da
shiftaff=~.7*re_sa+.7*rt_sa

incong=~.8*Acc_inc+.8*rt_inc
cong=~.8*Acc_con+.8*rt_con

aff_con=~.7*digaff+.7*shiftaff+.7*incong+.7*cong

aff_con~~.8*cogcon
'
simdat_susan<-simulateData(sim_sus,sample.nobs = 244)



####Estimate the true model
real<-'
incong=~   Scores.accSadIncongRaw_T3_1+        Scores.RTSadIncong_T3_1
cong=~     Scores.accSadCongRaw_T3_1+          Scores.RTSadCong_T3_1  
digaff=~   Scores.spanEmo_T2_1 
shiftaff=~ Scores.emoColNumRandErr_T5_1 +      Scores.emoColNumRandErrRT_T5_1
aff_con=~digaff+shiftaff+incong+cong

digneu=~   Scores.spanNeu_T2_1
shiftneu=~ Scores.neutColNumRandErr_T5_1 +    Scores.neutColNumRandErrRT_T5_1
cogcon=~digneu+shiftneu

cogcon~age
aff_con~age

cogcon~iq.sum
aff_con~iq.sum

aff_con~~cogcon
'
fit_real<-cfa(real,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real,what='std')


real<-'
incong=~   Scores.accSadIncongRaw_T3_1+        Scores.RTSadIncong_T3_1 + Scores.accHappyIncongRaw_T3_1+        Scores.RTHappyIncong_T3_1
cong=~     Scores.accSadCongRaw_T3_1+          Scores.RTSadCong_T3_1  + Scores.accHappyCongRaw_T3_1+          Scores.RTHappyCong_T3_1  
digaff=~   Scores.spanEmo_T2_1 
shiftaff=~ Scores.emoColNumRandErr_T5_1 +      Scores.emoColNumRandErrRT_T5_1
aff_con=~digaff+shiftaff+incong+cong

digneu=~   Scores.spanNeu_T2_1
shiftneu=~ Scores.neutColNumRandErr_T5_1 +    Scores.neutColNumRandErrRT_T5_1
inhibitneu=~Scores.RTSadNeut_T3_1 + Scores.RTHappyNeut_T3_1 
cogcon=~digneu+shiftneu+inhibitneu

cogcon~age
aff_con~age

cogcon~iq.sum
aff_con~iq.sum

aff_con~~cogcon
'
fit_real<-cfa(real,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real,what='std')

hist(Data1_rescale$Scores.neutColNumRandErr_T5_1)

####Same model with fixed variance of latent factors to 1

real<-'
incong=~   Scores.accSadIncongRaw_T3_1+        Scores.RTSadIncong_T3_1
cong=~     Scores.accSadCongRaw_T3_1+          Scores.RTSadCong_T3_1  
digaff=~   Scores.spanEmo_T2_1 
shiftaff=~ Scores.emoColNumRandErr_T5_1 +      Scores.emoColNumRandErrRT_T5_1
aff_con=~digaff+shiftaff+incong+cong

digneu=~   Scores.spanNeu_T2_1
shiftneu=~ Scores.neutColNumRandErr_T5_1 +    Scores.neutColNumRandErrRT_T5_1
cogcon=~digneu+shiftneu

cogcon~~1*cogcon 
aff_con~~1*aff_con 

aff_con~~cogcon
'
fit_real<-cfa(real,data=Data1_rescale,estimator='mlr', missing='fiml',auto.fix.first=F )
summary(fit_real,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real,what='std')


#Estimate the true model rt only - though note Stroop used for neutral not included in prediction
real_rt<-'

    

cogcon=~Scores.RTSadNeut_T3_1 + Scores.RTHappyNeut_T3_1 +Scores.emoColNumRandErrRT_T5_1

 
aff_con=~Scores.RTSadIncong_T3_1+Scores.RTSadCong_T3_1+Scores.RTHappyIncong_T3_1+Scores.RTHappyCong_T3_1+Scores.emoColNumRandErrRT_T5_1

aff_con~~cogcon



'
fit_real_rt<-cfa(real_rt,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real_rt,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real_rt,what='std')

hist(Data1_rescale$Scores.RTHappyIncong_T3_1)

#Estimate the true model acc only 

real_acc<-'



cogcon=~Scores.spanNeu_T2_1+Scores.neutColNumRandErr_T5_1 

 
aff_con=~Scores.spanEmo_T2_1+Scores.accSadIncongRaw_T3_1+Scores.accSadCongRaw_T3_1+Scores.accHappyIncongRaw_T3_1+Scores.accHappyCongRaw_T3_1+Scores.emoColNumRandErr_T5_1

aff_con~~cogcon

'
fit_real_acc<-cfa(real_acc,data=Data1_rescale,estimator='mlr',missing='fiml')
summary(fit_real_acc,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real_acc,what='std')

hist(Data1_rescale$Scores.accHappyIncongRaw_T3_1)
#Estimate the full true model rt only - though note Stroop used for neutral not included in prediction
real_rt<-'



cogcon=~Scores.RTSadNeut_T3_1 + Scores.RTHappyNeut_T3_1 +Scores.emoColNumRandErrRT_T5_1


aff_con=~Scores.RTSadIncong_T3_1+Scores.RTSadCong_T3_1+Scores.RTHappyIncong_T3_1+Scores.RTHappyCong_T3_1+Scores.emoColNumRandErrRT_T5_1

cogcon~age
aff_con~age

cogcon~iq.sum
aff_con~iq.sum

aff_con~~cogcon



'
fit_real_rt<-cfa(real_rt,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real_rt,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real_rt,what='std')

#Estimate the true full model acc only 

real_acc<-'



cogcon=~Scores.spanNeu_T2_1+Scores.accSadNeutRaw_T3_1+Scores.neutColNumRandErr_T5_1 


aff_con=~Scores.spanEmo_T2_1+Scores.accSadIncongRaw_T3_1+Scores.accSadCongRaw_T3_1+Scores.emoColNumRandErr_T5_1

cogcon~age
aff_con~age

cogcon~iq.sum
aff_con~iq.sum

aff_con~~cogcon

'
fit_real_acc<-cfa(real_acc,data=Data1_rescale,estimator='mlr',missing='fiml')
summary(fit_real_acc,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real_acc,what='std')

#Estimate the true model with loading equality constraints (more stable)
sus_true_cons<-'

digneu=~   fl1*Scores.spanNeu_T2_1
shiftneu=~ fl2*Scores.neutColNumRandErr_T5_1 +    fl2*Scores.neutColNumRandErrRT_T5_1

cogcon=~fl3*digneu+fl3*shiftneu


digaff=~   fl4*Scores.spanEmo_T2_1 
shiftaff=~ fl5*Scores.emoColNumRandErr_T5_1 +      fl5*Scores.emoColNumRandErrRT_T5_1

incong=~   fl6*Scores.accSadIncongRaw_T3_1+        fl6*Scores.RTSadIncong_T3_1
cong=~     fl7*Scores.accSadCongRaw_T3_1+          fl7*Scores.RTSadCong_T3_1  
aff_con=~fl8*digaff+fl8*shiftaff+fl8*incong+fl8*cong

aff_con~~cogcon

'
fit_sus_true_cons<-cfa(sus_true_cons,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_sus_true_cons,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_sus_true_cons,what='std')

#Estimate the true model rt only loading equality constraints (more stable)
real_con<-'

    

cogcon=~fl1*Scores.neutColNumRandErrRT_T5_1 + fl1*Scores.RTSadNeut_T3_1 + fl1*Scores.RTHappyNeut_T3_1

 
aff_con=~fl2*Scores.emoColNumRandErrRT_T5_1+fl2*Scores.RTSadIncong_T3_1+fl2*Scores.RTSadCong_T3_1+fl2*Scores.RTHappyIncong_T3_1+fl2*Scores.RTHappyCong_T3_1

aff_con~~cogcon
'
fit_real_con<-cfa(real_con,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real_con,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real_con,what='std')


#Estimate the true model acc only loading equality constraints (more stable) does not converge with fl1*Scores.accSadNeutRaw_T3_1 + fl1*Scores.accHappyNeutRaw_T3_1 or happy only poor fit with sad only
real_acc_con<-'

    

cogcon=~fl1*Scores.neutColNumRandErr_T5_1 + fl1*Scores.accSadNeutRaw_T3_1 + fl1*Scores.accHappyNeutRaw_T3_1

 
aff_con=~Scores.emoColNumRandErr_T5_1+Scores.accSadIncongRaw_T3_1+Scores.accSadCongRaw_T3_1+Scores.accHappyIncongRaw_T3_1+Scores.accHappyCongRaw_T3_1

aff_con~~cogcon



'
fit_real_acc_con<-cfa(real_acc_con,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real_acc_con,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real_acc_con,what='std')

#Estimate a competing model (unidimensional)
sus_alt_free_unidim<-'
digneu=~   Scores.spanNeu_T2_1
shiftneu=~ Scores.neutColNumRandErr_T5_1 +    Scores.neutColNumRandErrRT_T5_1

cogcon=~digneu+shiftneu


digaff=~   Scores.spanEmo_T2_1 
shiftaff=~ Scores.emoColNumRandErr_T5_1 +      Scores.emoColNumRandErrRT_T5_1

incong=~   Scores.accSadIncongRaw_T3_1+        Scores.RTSadIncong_T3_1
cong=~     Scores.accSadCongRaw_T3_1+          Scores.RTSadCong_T3_1 

aff_con=~digaff+shiftaff+incong+cong

aff_con~~1*cogcon
'
fit_sus_alt_free_unidim<-cfa(sus_alt_free_unidim,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_sus_alt_free_unidim,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_sus_alt_free_unidim,what='std')

#Estimate the  rt only competing unidimensional
real<-'

    

cogcon=~Scores.neutColNumRandErrRT_T5_1 + Scores.RTSadNeut_T3_1+Scores.RTHappyNeut_T3_1

 
aff_con=~Scores.emoColNumRandErrRT_T5_1+Scores.RTSadIncong_T3_1+Scores.RTSadCong_T3_1+Scores.RTHappyIncong_T3_1+Scores.RTHappyCong_T3_1

aff_con~~1*cogcon
'
fit_real<-cfa(real,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real,what='std')

real<-'



cogcon=~Scores.neutColNumRandErrRT_T5_1 + Scores.RTSadNeut_T3_1+Scores.RTHappyNeut_T3_1 + Scores.emoColNumRandErrRT_T5_1+Scores.RTSadIncong_T3_1+Scores.RTSadCong_T3_1+Scores.RTHappyIncong_T3_1+Scores.RTHappyCong_T3_1


'
fit_real<-cfa(real,data=Data1_rescale,estimator='mlr')
summary(fit_real,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real,what='std')

#Compare models
anova(fit_real_rt,fit_real_acc)

#Estimate a competing model (unidimensional) with constraints
sus_alt_cons_unidim<-'
digneu=~fl1*dprim_dn+fl1*rt_dn
shiftneu=~fl2*re_sn+fl2*rt_sn

cogcon=~fl3*digneu+fl3*shiftneu


digaff=~fl4*dprim_da+fl4*rt_da
shiftaff=~fl5*re_sa+fl5*rt_sa
incong=~fl6*Acc_inc+fl6*rt_inc
cong=~fl7*Acc_con+fl7*rt_con


aff_con=~fl8*digaff+f8*shiftaff+fl8*incong+fl8*cong
aff_con~~1*cogcon

'
fit_sus_alt_cons_unidim<-cfa(sus_alt_cons_unidim,data=simdat_susan,estimator='mlr', missing='fiml')
summary(fit_sus_alt_cons_unidim,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_sus_alt_cons_unidim,what='std')

anova(fit_sus_true_cons,fit_sus_alt_cons_unidim)




#Estimate a competing model that uses tasks as factors
sus_alt_task_free<-'

span=~Scores.spanNeu_T2_1+Scores.spanEmo_T2_1
rt=~Scores.neutColNumRandErrRT_T5_1 + Scores.RTSadNeut_T3_1+Scores.RTHappyNeut_T3_1+Scores.emoColNumRandErrRT_T5_1+ Scores.RTSadIncong_T3_1+Scores.RTSadCong_T3_1+Scores.RTHappyIncong_T3_1+Scores.RTHappyCong_T3_1
re=~Scores.neutColNumRandErr_T5_1+Scores.emoColNumRandErr_T5_1
acc=~Scores.accSadIncongRaw_T3_1+Scores.accSadCongRaw_T3_1+Scores.accHappyIncongRaw_T3_1+Scores.accHappyCongRaw_T3_1+Scores.accSadNeutRaw_T3_1 + Scores.accHappyNeutRaw_T3_1

'
fit_sus_alt_task_free<-cfa(sus_alt_task_free,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_sus_alt_task_free,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_sus_alt_task_free,what='std')

#Estimate a competing model that uses tasks as factors, with constraints


sus_alt_task_cons<-'

dprim=~fl1*dprim_dn+fl1*dprim_da
rt=~fl2*rt_dn+fl2*rt_sn+fl2*rt_da+fl2*rt_inc+fl2*rt_con+fl2*rt_sa
re=~fl3*re_sn+fl3*re_sa
acc=~fl4*Acc_inc+fl4*Acc_con

'
fit_sus_alt_task_cons<-cfa(sus_alt_task_cons,data=simdat_susan,estimator='mlr')
summary(fit_sus_alt_task_cons,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_sus_alt_task_cons,what='std')



anova(fit_sus_true_free,fit_sus_alt_task_free)

FAdata<-subset(Data1_rescale,select=c(Scores.spanNeu_T2_1     , Scores.spanEmo_T2_1          , Scores.accHappyNeutRaw_T3_1  ,
                              Scores.neutColNumRandErrRT_T5_1 , Scores.RTSadNeut_T3_1        , Scores.RTHappyNeut_T3_1      , 
                              Scores.RTSadIncong_T3_1         , Scores.RTSadCong_T3_1        , Scores.RTHappyIncong_T3_1    ,
                              Scores.neutColNumRandErr_T5_1   , Scores.emoColNumRandErr_T5_1 , Scores.RTHappyCong_T3_1      ,
                              Scores.emoColNumRandErrRT_T5_1  , Scores.accHappyCongRaw_T3_1  , Scores.accSadNeutRaw_T3_1    ,
                              Scores.accSadIncongRaw_T3_1     , Scores.accSadCongRaw_T3_1    , Scores.accHappyIncongRaw_T3_1, iq.sum))

FAdataNA<-na.omit(FAdata)

FAdataNA<-mutate_all(FAdataNA, function(x) as.numeric(as.character(x)))

evalues<-eigen(cor(FAdataNA))
ap <- parallel(subject=nrow(FAdataNA),var=ncol(FAdataNA), 
               rep=100,cent=.05)
nS <- nScree(x=evalues$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

FA<-fa(FAdataNA, nfactors = 3, rotate = "oblimin", fm = "wls")
print.psych(FA, cut = 0.30, sort = TRUE)

FA<-fa(FAdataNA, nfactors = 2, rotate = "oblimin", fm = "wls")
print.psych(FA, cut = 0.30, sort = TRUE)

FAdata_ACC<-subset(Data1_rescale,select=c(Scores.spanNeu_T2_1       , Scores.spanEmo_T2_1          , Scores.accHappyNeutRaw_T3_1  ,
                              Scores.neutColNumRandErr_T5_1 , Scores.emoColNumRandErr_T5_1 ,
                              Scores.accHappyCongRaw_T3_1   , Scores.accSadNeutRaw_T3_1    ,
                              Scores.accSadIncongRaw_T3_1   , Scores.accSadCongRaw_T3_1    , Scores.accHappyIncongRaw_T3_1))

FAdataNA_ACC<-na.omit(FAdata_ACC)

FAdataNA_ACC<-mutate_all(FAdataNA_ACC, function(x) as.numeric(as.character(x)))

evalues<-eigen(cor(FAdataNA_ACC))
ap <- parallel(subject=nrow(FAdataNA_ACC),var=ncol(FAdataNA_ACC), 
               rep=100,cent=.05)
nS <- nScree(x=evalues$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

FA<-fa(FAdataNA_ACC, nfactors = 2, rotate = "oblimin", fm = "wls")
print.psych(FA, cut = 0.30, sort = TRUE)


FA_Acc<-'
F1=~   Scores.accSadIncongRaw_T3_1+      Scores.accHappyIncongRaw_T3_1+       Scores.accSadNeutRaw_T3_1 +
           Scores.accSadCongRaw_T3_1+        Scores.accHappyCongRaw_T3_1+         Scores.accHappyNeutRaw_T3_1 +
           Scores.neutColNumRandErr_T5_1
F2=~       Scores.spanNeu_T2_1 + Scores.spanEmo_T2_1 + Scores.emoColNumRandErr_T5_1


F1~age
F2~age

F1~iq.sum
F2~iq.sum

F1~~F2
'
fit_real<-cfa(FA_Acc,data=Data1_rescale,estimator='mlr', missing='fiml')
summary(fit_real,rsquare=T,standardized = TRUE,fit.measures=T)
semPaths(fit_real,what='std')

FAdata_ACC_HS<-subset(Data1_rescale,select=c(Scores.spanNeu_T2_1       ,     Scores.spanEmo_T2_1          ,
                                     Scores.neutColNumRandErr_T5_1 , Scores.emoColNumRandErr_T5_1 ,
                                     Scores.accHappyCongRaw_T3_1   , Scores.accHappyIncongRaw_T3_1))

FAdata_ACC_HS<-na.omit(FAdata_ACC_HS)

FAdata_ACC_HS<-mutate_all(FAdata_ACC_HS, function(x) as.numeric(as.character(x)))

evalues<-eigen(cor(FAdata_ACC_HS))
ap <- parallel(subject=nrow(FAdata_ACC_HS),var=ncol(FAdata_ACC_HS), 
               rep=100,cent=.05)
nS <- nScree(x=evalues$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

FA<-fa(FAdata_ACC_HS, nfactors = 3, rotate = "oblimin", fm = "wls")
print.psych(FA, cut = 0.30, sort = TRUE)


THREEF <-
  '
UPDATE        =~ Scores.spanNeu_T2_1 + Scores.spanEmo_T2_1
INHIBIT       =~ Scores.RTHappyIncong_T3_1 + Scores.RTHappyCong_T3_1
SHIFT         =~ Scores.emoColNumRandErr_T5_1
'


fit <- cfa(THREEF, data=Data1_rescale, 
           estimator='wlsmv') # robust estimators, proper missing data method etc.

summary (fit, fit.measures = T) # use standardized variables and request R2


FAdata_RT<-subset(Data1_rescale,select=c(Scores.neutColNumRandErrRT_T5_1 , Scores.RTSadNeut_T3_1, Scores.RTHappyNeut_T3_1  , 
                              Scores.RTSadIncong_T3_1            , Scores.RTSadCong_T3_1, Scores.RTHappyIncong_T3_1,
                              Scores.RTHappyCong_T3_1      ,
                              Scores.emoColNumRandErrRT_T5_1))

FAdata_RT<-na.omit(FAdata_RT)

FAdata_RT<-mutate_all(FAdata_RT, function(x) as.numeric(as.character(x)))

evalues<-eigen(cor(FAdata_RT))
ap <- parallel(subject=nrow(FAdata_RT),var=ncol(FAdata_RT), 
               rep=100,cent=.05)
nS <- nScree(x=evalues$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

FA<-fa(FAdata_RT, nfactors = 2, rotate = "oblimin", fm = "wls")
print.psych(FA, cut = 0.30, sort = TRUE)

FAdata_RT_HS<-subset(Data1_rescale,select=c(Scores.neutColNumRandErrRT_T5_1 , Scores.RTHappyNeut_T3_1, 
                                    Scores.RTHappyIncong_T3_1       , Scores.RTHappyCong_T3_1,
                                    Scores.emoColNumRandErrRT_T5_1))

FAdata_RT_HS<-na.omit(FAdata_RT_HS)

FAdata_RT_HS<-mutate_all(FAdata_RT_HS, function(x) as.numeric(as.character(x)))

evalues<-eigen(cor(FAdata_RT_HS))
ap <- parallel(subject=nrow(FAdata_RT_HS),var=ncol(FAdata_RT_HS), 
               rep=100,cent=.05)
nS <- nScree(x=evalues$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

FA<-fa(FAdata_RT_HS, nfactors = 2, rotate = "oblimin", fm = "wls")
print.psych(FA, cut = 0.30, sort = TRUE)

TWOF <-
  ' 
INHIBIT       =~ Scores.RTHappyNeut_T3_1 + Scores.RTHappyIncong_T3_1 + Scores.RTHappyCong_T3_1
SHIFT         =~ Scores.neutColNumRandErrRT_T5_1
'


fit <- cfa(TWOF, data=FAdataNA, 
           estimator='wlsmv') # robust estimators, proper missing data method etc.

summary (fit, fit.measures = T) # use standardized variables and request R2
