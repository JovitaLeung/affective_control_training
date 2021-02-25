

library(nlme);library(reshape);library(pander);library(metafor);
library(reshape2);library(weightr);library(car);library(cowplot)
library(ggplot2);library(tidyr);library(Hmisc);library(psych);
library(foreign);library(haven);library(aod);library(Rcpp);
library(lmSupport);library(rcompanion);library(base);
library(mgcv);library(ez);library(fastR);library(lavaan);
library(RColorBrewer);library(ggpubr);library(emmeans);
library(semPlot);library(MASS);library(data.table);library(lattice);
library(afex);library(multilevel);library(lme4); library(pastecs);library(WRS2); library(see)

library(lavaan)     ; library(car)     ; library(semPlot) ;
library(psych)      ; library(ppcor)   ; library(corpcor) ;
library(GPArotation); library(nFactors); library(corrplot);
library(dplyr)      ; library(rlang)   ; library(pastecs)


setwd("Z:/Data/WT_TrainingStudy")
Data1<-read.csv("281019wodupl2.csv")
Data2<-read.csv("T1_data.csv")
Data3<-read.csv("T2_data.csv")
Data4<- read.csv("T3_data.csv")


Data1$Group[which(is.na(Data1$Group))]<-999
DataT2$no_of_Tsessions_over_10_mins[which(DataT2$no_of_Tsessions_over_10_mins > 50)]<-50


Data2[] <- lapply(Data2, gsub, pattern = "A little like me", replacement = "1", fixed = TRUE)

Data2[] <- lapply(Data2, gsub, pattern = "Male", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Female", replacement = "1", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Prefer not to say", replacement = "2", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Asian-other", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Bangladeshi", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Black-African", replacement = "1", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Black-British", replacement = "1", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Black-other", replacement = "1", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Chinese", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Chinese", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Indian", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Pakistani", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "White-British", replacement = "2", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "White-other", replacement = "2", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Mixed/ Other", replacement = "3", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Very slightly or not at all", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "A little", replacement = "1", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Modertaly", replacement = "2", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Quite a bit", replacement = "3", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Extremely", replacement = "4", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Not true", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Somewhat true", replacement = "1", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Certainly true", replacement = "2", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Almost never (0-10%)", replacement = "0", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Sometimes (11-35%)", replacement = "1", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "About half of the time (36-65%)", replacement = "2", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Most of the time (66-90%)", replacement = "3", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Almost always (91-100%)", replacement = "4", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Not at all like me", replacement = "0", fixed = TRUE)

Data2[] <- lapply(Data2, gsub, pattern = "Somewhat like me", replacement = "2", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Mostly like me", replacement = "3", fixed = TRUE)
Data2[] <- lapply(Data2, gsub, pattern = "Very much like me", replacement = "4", fixed = TRUE)


Data2$ders.1_20 <- car::recode(Data2$ders.1_20 , "0=3; 1=2; 2=1; 3=0")
Data2$ders.2_4  <- car::recode(Data2$ders.2_4  , "0=3; 1=2; 2=1; 3=0")
Data2$ders.1_2  <- car::recode(Data2$ders.1_2  , "0=3; 1=2; 2=1; 3=0")
Data2$ders.1_6  <- car::recode(Data2$ders.1_6  , "0=3; 1=2; 2=1; 3=0")
Data2$ders.1_8  <- car::recode(Data2$ders.1_8  , "0=3; 1=2; 2=1; 3=0")
Data2$ders.1_10 <- car::recode(Data2$ders.1_10 , "0=3; 1=2; 2=1; 3=0")
Data2$ders.1_17 <- car::recode(Data2$ders.1_17 , "0=3; 1=2; 2=1; 3=0")
Data2$ders.2_14 <- car::recode(Data2$ders.2_14 , "0=3; 1=2; 2=1; 3=0")
Data2$ders.2_2  <- car::recode(Data2$ders.2_2  , "0=3; 1=2; 2=1; 3=0")
Data2$ders.1_1  <- car::recode(Data2$ders.1_1  , "0=3; 1=2; 2=1; 3=0")
Data2$ders.1_7  <- car::recode(Data2$ders.1_7  , "0=3; 1=2; 2=1; 3=0")

Data2$PANAS_P   <-(as.numeric(Data2$panas.1_1) + as.numeric(Data2$panas.1_3) + as.numeric(Data2$panas.1_4) + as.numeric(Data2$panas.1_9) + as.numeric(Data2$panas.1_10) + 
                   as.numeric(Data2$panas.2_1) + as.numeric(Data2$panas.2_3) + as.numeric(Data2$panas.2_5) + as.numeric(Data2$panas.2_6) + as.numeric(Data2$panas.2_8))
Data2$PANAS_N   <-(as.numeric(Data2$panas.1_2) + as.numeric(Data2$panas.1_5) + as.numeric(Data2$panas.1_6) + as.numeric(Data2$panas.1_7) + as.numeric(Data2$panas.1_8 )+
                   as.numeric(Data2$panas.2_2) + as.numeric(Data2$panas.2_4) + as.numeric(Data2$panas.2_7) + as.numeric(Data2$panas.2_9) + as.numeric(Data2$panas.2_10))
Data2$SDQ_E     <-(as.numeric(Data2$sdq.1_3  ) + as.numeric(Data2$sdq.1_8  ) + as.numeric(Data2$sdq.1_13 ) + as.numeric(Data2$sdq.2_3  ) + as.numeric(Data2$sdq.2_11))
Data2$SDQ_C     <-(as.numeric(Data2$sdq.1_5  ) + as.numeric(Data2$sdq.1_7  ) + as.numeric(Data2$sdq.1_12 ) + as.numeric(Data2$sdq.2_5  ) + as.numeric(Data2$sdq.2_9 ))
Data2$SDQ_H     <-(as.numeric(Data2$sdq.1_2  ) + as.numeric(Data2$sdq.1_10 ) + as.numeric(Data2$sdq.2_2  ) + as.numeric(Data2$sdq.2_8  ) + as.numeric(Data2$sdq.2_12))
Data2$SDQ_PP    <-(as.numeric(Data2$sdq.1_6  ) + as.numeric(Data2$sdq.1_11 ) + as.numeric(Data2$sdq.2_1  ) + as.numeric(Data2$sdq.2_6  ) + as.numeric(Data2$sdq.2_10))
Data2$SDQ_P     <-(as.numeric(Data2$sdq.1_1  ) + as.numeric(Data2$sdq.1_4  ) + as.numeric(Data2$sdq.1_9  ) + as.numeric(Data2$sdq.2_4  ) + as.numeric(Data2$sdq.2_7 ))
Data2$SDQ_Diff  <-(Data2$SDQ_E    + Data2$SDQ_C    + Data2$SDQ_H    + Data2$SDQ_PP)
Data2$SDQ_Int   <-(Data2$SDQ_E    + Data2$SDQ_PP)
Data2$SDQ_Ext   <-(Data2$SDQ_C    + Data2$SDQ_H)     
Data2$DERS_NAcc <-(as.numeric(Data2$ders.1_11) + as.numeric(Data2$ders.1_12) + as.numeric(Data2$ders.2_1 ) + as.numeric(Data2$ders.2_3 ) + as.numeric(Data2$ders.2_5)   + as.numeric(Data2$ders.2_9))
Data2$DERS_Goal <-(as.numeric(Data2$ders.1_13) + as.numeric(Data2$ders.1_18) + as.numeric(Data2$ders.1_20) + as.numeric(Data2$ders.2_6 ) + as.numeric(Data2$ders.2_14))
Data2$DERS_Impu <-(as.numeric(Data2$ders.1_3 ) + as.numeric(Data2$ders.1_14) + as.numeric(Data2$ders.1_19) + as.numeric(Data2$ders.2_4 ) + as.numeric(Data2$ders.2_7  ) + as.numeric(Data2$ders.2_13))
Data2$DERS_Awar <-(as.numeric(Data2$ders.1_2 ) + as.numeric(Data2$ders.1_6 ) + as.numeric(Data2$ders.1_8 ) + as.numeric(Data2$ders.1_10) + as.numeric(Data2$ders.1_17 ) + as.numeric(Data2$ders.2_15))
Data2$DERS_Star <-(as.numeric(Data2$ders.1_15) + as.numeric(Data2$ders.1_16) + as.numeric(Data2$ders.2_2 ) + as.numeric(Data2$ders.2_8 ) + as.numeric(Data2$ders.2_10 ) + as.numeric(Data2$ders.2_11)+ 
                   as.numeric(Data2$ders.2_16) + as.numeric(Data2$ders.2_17))
Data2$DERS_Clar <-(as.numeric(Data2$ders.1_1 ) + as.numeric(Data2$ders.1_4 ) + as.numeric(Data2$ders.1_5 ) + as.numeric(Data2$ders.1_7 ) + as.numeric(Data2$ders.1_9))
Data2$SCS       <-(as.numeric(Data2$b.scs_1  ) + as.numeric(Data2$b.scs_2  ) + as.numeric(Data2$b.scs_3  ) + as.numeric(Data2$b.scs_4  ) + as.numeric(Data2$b.scs_5) + as.numeric(Data2$b.scs_6)   + 
                   as.numeric(Data2$b.scs_7  ) + as.numeric(Data2$b.scs_8  ) + as.numeric(Data2$b.scs_9  ) + as.numeric(Data2$b.scs_10 ) + as.numeric(Data2$b.scs_11)+ as.numeric(Data2$b.scs_12)  + 
                   as.numeric(Data2$b.scs_13))

Data3[] <- lapply(Data3, gsub, pattern = "A little like me", replacement = "1", fixed = TRUE)

Data3[] <- lapply(Data3, gsub, pattern = "Very slightly or not at all", replacement = "0", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "A little", replacement = "1", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Modertaly", replacement = "2", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Quite a bit", replacement = "3", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Extremely", replacement = "4", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Not true", replacement = "0", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Somewhat true", replacement = "1", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Certainly true", replacement = "2", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Almost never (0-10%)", replacement = "0", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Sometimes (11-35%)", replacement = "1", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "About half of the time (36-65%)", replacement = "2", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Most of the time (66-90%)", replacement = "3", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Almost always (91-100%)", replacement = "4", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Not at all like me", replacement = "0", fixed = TRUE)

Data3[] <- lapply(Data3, gsub, pattern = "Somewhat like me", replacement = "2", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Mostly like me", replacement = "3", fixed = TRUE)
Data3[] <- lapply(Data3, gsub, pattern = "Very much like me", replacement = "4", fixed = TRUE)



Data3$DERS.post.1_20 <-car::recode(Data3$DERS.post.1_20 , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.2_4  <-car::recode(Data3$DERS.post.2_4  , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.1_2  <-car::recode(Data3$DERS.post.1_2  , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.1_6  <-car::recode(Data3$DERS.post.1_6  , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.1_8  <-car::recode(Data3$DERS.post.1_8  , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.1_10 <-car::recode(Data3$DERS.post.1_10 , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.1_17 <-car::recode(Data3$DERS.post.1_17 , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.2_14 <-car::recode(Data3$DERS.post.2_14 , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.2_2  <-car::recode(Data3$DERS.post.2_2  , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.1_1  <-car::recode(Data3$DERS.post.1_1  , "0=3; 1=2; 2=1; 3=0")
Data3$DERS.post.1_7  <-car::recode(Data3$DERS.post.1_7  , "0=3; 1=2; 2=1; 3=0")

Data3$PANAS.post_P   <-(as.numeric(Data3$panas.post.1_1) + as.numeric(Data3$panas.post.1_3)  + as.numeric(Data3$panas.post.1_4) + as.numeric(Data3$panas.post.1_9) + as.numeric(Data3$panas.post.1_10) + 
                        as.numeric(Data3$panas.post.2_1) + as.numeric(Data3$panas.post.2_3)  + as.numeric(Data3$panas.post.2_5) + as.numeric(Data3$panas.post.2_6) + as.numeric(Data3$panas.post.2_8))
Data3$PANAS.post_N   <-(as.numeric(Data3$panas.post.1_2) + as.numeric(Data3$panas.post.1_5)  + as.numeric(Data3$panas.post.1_6) + as.numeric(Data3$panas.post.1_7) + as.numeric(Data3$panas.post.1_8 )+
                        as.numeric(Data3$panas.post.2_2) + as.numeric(Data3$panas.post.2_4)  + as.numeric(Data3$panas.post.2_7) + as.numeric(Data3$panas.post.2_9) + as.numeric(Data3$panas.post.2_10))
Data3$SDQ.post_E     <-(as.numeric(Data3$sdq.post.1_3  ) + as.numeric(Data3$sdq.post.1_8  )  + as.numeric(Data3$sdq.post.1_13 ) + as.numeric(Data3$sdq.post.2_3  ) + as.numeric(Data3$sdq.post.2_11))
Data3$SDQ.post_C     <-(as.numeric(Data3$sdq.post.1_5  ) + as.numeric(Data3$sdq.post.1_7  )  + as.numeric(Data3$sdq.post.1_12 ) + as.numeric(Data3$sdq.post.2_5  ) + as.numeric(Data3$sdq.post.2_9 ))
Data3$SDQ.post_H     <-(as.numeric(Data3$sdq.post.1_2  ) + as.numeric(Data3$sdq.post.1_10 )  + as.numeric(Data3$sdq.post.2_2  ) + as.numeric(Data3$sdq.post.2_8  ) + as.numeric(Data3$sdq.post.2_12))
Data3$SDQ.post_PP    <-(as.numeric(Data3$sdq.post.1_6  ) + as.numeric(Data3$sdq.post.1_11 )  + as.numeric(Data3$sdq.post.2_1  ) + as.numeric(Data3$sdq.post.2_6  ) + as.numeric(Data3$sdq.post.2_10))
Data3$SDQ.post_P     <-(as.numeric(Data3$sdq.post.1_1  ) + as.numeric(Data3$sdq.post.1_4  )  + as.numeric(Data3$sdq.post.1_9  ) + as.numeric(Data3$sdq.post.2_4  ) + as.numeric(Data3$sdq.post.2_7 ))
Data3$SDQ.post_Diff  <-(as.numeric(Data3$SDQ.post_E    ) + as.numeric(Data3$SDQ.post_C    )  + as.numeric(Data3$SDQ.post_H    ) + as.numeric(Data3$SDQ.post_PP))
Data3$SDQ.post_INT   <-(as.numeric(Data3$SDQ.post_E    ) + as.numeric(Data3$SDQ.post_PP))
Data3$SDQ.post_EXT   <-(as.numeric(Data3$SDQ.post_C    ) + as.numeric(Data3$SDQ.post_H))
Data3$DERS.post_NAcc <-(as.numeric(Data3$DERS.post.1_11) + as.numeric(Data3$DERS.post.1_12)  + as.numeric(Data3$DERS.post.2_1 ) + as.numeric(Data3$DERS.post.2_3 ) + as.numeric(Data3$DERS.post.2_5 )  + as.numeric(Data3$DERS.post.2_9 ))
Data3$DERS.post_Goal <-(as.numeric(Data3$DERS.post.1_13) + as.numeric(Data3$DERS.post.1_18)  + as.numeric(Data3$DERS.post.1_20) + as.numeric(Data3$DERS.post.2_6 ) + as.numeric(Data3$DERS.post.2_14))
Data3$DERS.post_Impu <-(as.numeric(Data3$DERS.post.1_3 ) + as.numeric(Data3$DERS.post.1_14)  + as.numeric(Data3$DERS.post.1_19) + as.numeric(Data3$DERS.post.2_4 ) + as.numeric(Data3$DERS.post.2_7 )  + as.numeric(Data3$DERS.post.2_13))
Data3$DERS.post_Awar <-(as.numeric(Data3$DERS.post.1_2 ) + as.numeric(Data3$DERS.post.1_6 )  + as.numeric(Data3$DERS.post.1_8 ) + as.numeric(Data3$DERS.post.1_10) + as.numeric(Data3$DERS.post.1_17)  + as.numeric(Data3$DERS.post.2_15))
Data3$DERS.post_Star <-(as.numeric(Data3$DERS.post.1_15) + as.numeric(Data3$DERS.post.1_16)  + as.numeric(Data3$DERS.post.2_2 ) + as.numeric(Data3$DERS.post.2_8 ) + as.numeric(Data3$DERS.post.2_10)  + as.numeric(Data3$DERS.post.2_11) + 
                          as.numeric(Data3$DERS.post.2_16) + as.numeric(Data3$DERS.post.2_17))
Data3$DERS.post_Clar <-(as.numeric(Data3$DERS.post.1_1 ) + as.numeric(Data3$DERS.post.1_4 )  + as.numeric(Data3$DERS.post.1_5 ) + as.numeric(Data3$DERS.post.1_7 ) + as.numeric(Data3$DERS.post.1_9))
Data3$SCS.post       <-(as.numeric(Data3$b.scs.post.1  ) + as.numeric(Data3$b.scs.post.2  )  + as.numeric(Data3$b.scs.post.3  ) + as.numeric(Data3$b.scs.post.4  ) + as.numeric(Data3$b.scs.post.5 )   + as.numeric(Data3$b.scs.post.6)   + 
                        as.numeric(Data3$b.scs.post.7)   + as.numeric(Data3$b.scs.post.8)    + as.numeric(Data3$b.scs.post.9)   + as.numeric(Data3$b.scs.post.10)  + as.numeric(Data3$b.scs.post.11)   + as.numeric(Data3$b.scs.post.12)  + as.numeric(Data3$b.scs.post.13))


Data4[] <- lapply(Data4, gsub, pattern = "A little like me", replacement = "1", fixed = TRUE)

Data4[] <- lapply(Data4, gsub, pattern = "Very slightly or not at all", replacement = "0", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "A little", replacement = "1", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Modertaly", replacement = "2", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Quite a bit", replacement = "3", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Extremely", replacement = "4", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Not true", replacement = "0", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Somewhat true", replacement = "1", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Certainly true", replacement = "2", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Almost never (0-10%)", replacement = "0", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Sometimes (11-35%)", replacement = "1", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "About half of the time (36-65%)", replacement = "2", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Most of the time (66-90%)", replacement = "3", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Almost always (91-100%)", replacement = "4", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Not at all like me", replacement = "0", fixed = TRUE)

Data4[] <- lapply(Data4, gsub, pattern = "Somewhat like me", replacement = "2", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Mostly like me", replacement = "3", fixed = TRUE)
Data4[] <- lapply(Data4, gsub, pattern = "Very much like me", replacement = "4", fixed = TRUE)



Data4$DERS.1MFU.1_20 <-car::recode(Data4$DERS.1MFU.1_20 , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.2_4  <-car::recode(Data4$DERS.1MFU.2_4  , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.1_2  <-car::recode(Data4$DERS.1MFU.1_2  , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.1_6  <-car::recode(Data4$DERS.1MFU.1_6  , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.1_8  <-car::recode(Data4$DERS.1MFU.1_8  , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.1_10 <-car::recode(Data4$DERS.1MFU.1_10 , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.1_17 <-car::recode(Data4$DERS.1MFU.1_17 , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.2_14 <-car::recode(Data4$DERS.1MFU.2_14 , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.2_2  <-car::recode(Data4$DERS.1MFU.2_2  , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.1_1  <-car::recode(Data4$DERS.1MFU.1_1  , "0=3; 1=2; 2=1; 3=0")
Data4$DERS.1MFU.1_7  <-car::recode(Data4$DERS.1MFU.1_7  , "0=3; 1=2; 2=1; 3=0")

Data4$PANAS.1MFU_P   <-(as.numeric(Data4$panas.1MFU.1_1) + as.numeric(Data4$panas.1MFU.1_3 ) + as.numeric(Data4$panas.1MFU.1_4)+ as.numeric(Data4$panas.1MFU.1_9)+ as.numeric(Data4$panas.1MFU.1_10) + 
                        as.numeric(Data4$panas.1MFU.2_1) + as.numeric(Data4$panas.1MFU.2_3 ) + as.numeric(Data4$panas.1MFU.2_5)+ as.numeric(Data4$panas.1MFU.2_6)+ as.numeric(Data4$panas.1MFU.2_8))
Data4$PANAS.1MFU_N   <-(as.numeric(Data4$panas.1MFU.1_2) + as.numeric(Data4$panas.1MFU.1_5 ) + as.numeric(Data4$panas.1MFU.1_6)+ as.numeric(Data4$panas.1MFU.1_7)+ as.numeric(Data4$panas.1MFU.1_8 )+
                        as.numeric(Data4$panas.1MFU.2_2) + as.numeric(Data4$panas.1MFU.2_4 ) + as.numeric(Data4$panas.1MFU.2_7)+ as.numeric(Data4$panas.1MFU.2_9)+ as.numeric(Data4$panas.1MFU.2_10))
Data4$SDQ.1MFU_E     <-(as.numeric(Data4$sdq.1MFU.1_3  ) + as.numeric(Data4$sdq.1MFU.1_8   ) + as.numeric(Data4$sdq.1MFU.1_13 )+ as.numeric(Data4$sdq.1MFU.2_3  )+ as.numeric(Data4$sdq.1MFU.2_11))
Data4$SDQ.1MFU_C     <-(as.numeric(Data4$sdq.1MFU.1_5  ) + as.numeric(Data4$sdq.1MFU.1_7   ) + as.numeric(Data4$sdq.1MFU.1_12 )+ as.numeric(Data4$sdq.1MFU.2_5  )+ as.numeric(Data4$sdq.1MFU.2_9 ))
Data4$SDQ.1MFU_H     <-(as.numeric(Data4$sdq.1MFU.1_2  ) + as.numeric(Data4$sdq.1MFU.1_10  ) + as.numeric(Data4$sdq.1MFU.2_2  )+ as.numeric(Data4$sdq.1MFU.2_8  )+ as.numeric(Data4$sdq.1MFU.2_12))
Data4$SDQ.1MFU_PP    <-(as.numeric(Data4$sdq.1MFU.1_6  ) + as.numeric(Data4$sdq.1MFU.1_11  ) + as.numeric(Data4$sdq.1MFU.2_1  )+ as.numeric(Data4$sdq.1MFU.2_6  )+ as.numeric(Data4$sdq.1MFU.2_10))
Data4$SDQ.1MFU_P     <-(as.numeric(Data4$sdq.1MFU.1_1  ) + as.numeric(Data4$sdq.1MFU.1_4   ) + as.numeric(Data4$sdq.1MFU.1_9  )+ as.numeric(Data4$sdq.1MFU.2_4  )+ as.numeric(Data4$sdq.1MFU.2_7 ))
Data4$SDQ.1MFU_Diff  <-(as.numeric(Data4$SDQ.1MFU_E    ) + as.numeric(Data4$SDQ.1MFU_C     ) + as.numeric(Data4$SDQ.1MFU_H    )+ as.numeric(Data4$SDQ.1MFU_PP))
Data4$DERS.1MFU_NAcc <-(as.numeric(Data4$DERS.1MFU.1_11) + as.numeric(Data4$DERS.1MFU.1_12 ) + as.numeric(Data4$DERS.1MFU.2_1 )+ as.numeric(Data4$DERS.1MFU.2_3 ) + as.numeric(Data4$DERS.1MFU.2_5  ) + as.numeric(Data4$DERS.1MFU.2_9 ))
Data4$DERS.1MFU_Goal <-(as.numeric(Data4$DERS.1MFU.1_13) + as.numeric(Data4$DERS.1MFU.1_18 ) + as.numeric(Data4$DERS.1MFU.1_20)+ as.numeric(Data4$DERS.1MFU.2_6 ) + as.numeric(Data4$DERS.1MFU.2_14))
Data4$DERS.1MFU_Impu <-(as.numeric(Data4$DERS.1MFU.1_3 ) + as.numeric(Data4$DERS.1MFU.1_14 ) + as.numeric(Data4$DERS.1MFU.1_19)+ as.numeric(Data4$DERS.1MFU.2_4 ) + as.numeric(Data4$DERS.1MFU.2_7  ) + as.numeric(Data4$DERS.1MFU.2_13))
Data4$DERS.1MFU_Awar <-(as.numeric(Data4$DERS.1MFU.1_2 ) + as.numeric(Data4$DERS.1MFU.1_6  ) + as.numeric(Data4$DERS.1MFU.1_8 )+ as.numeric(Data4$DERS.1MFU.1_10) + as.numeric(Data4$DERS.1MFU.1_17 ) + as.numeric(Data4$DERS.1MFU.2_15))
Data4$DERS.1MFU_Star <-(as.numeric(Data4$DERS.1MFU.1_15) + as.numeric(Data4$DERS.1MFU.1_16 ) + as.numeric(Data4$DERS.1MFU.2_2 )+ as.numeric(Data4$DERS.1MFU.2_8 ) + as.numeric(Data4$DERS.1MFU.2_10 ) + as.numeric(Data4$DERS.1MFU.2_11) + 
                        as.numeric(Data4$DERS.1MFU.2_16) + as.numeric(Data4$DERS.1MFU.2_17))
Data4$DERS.1MFU_Clar <-(as.numeric(Data4$DERS.1MFU.1_1 ) + as.numeric(Data4$DERS.1MFU.1_4  ) + as.numeric(Data4$DERS.1MFU.1_5 )+ as.numeric(Data4$DERS.1MFU.1_7 ) + as.numeric(Data4$DERS.1MFU.1_9))
Data4$SCS.1MFU       <-(as.numeric(Data4$b.scs.1MFU.1  ) + as.numeric(Data4$b.scs.1MFU.2   ) + as.numeric(Data4$b.scs.1MFU.3  )+ as.numeric(Data4$b.scs.1MFU.4  ) + as.numeric(Data4$b.scs.1MFU.5   ) + as.numeric(Data4$b.scs.1MFU.6 )  + 
                        as.numeric(Data4$b.scs.1MFU.7  )  +as.numeric(Data4$b.scs.1MFU.8   ) + as.numeric(Data4$b.scs.1MFU.9  )+ as.numeric(Data4$b.scs.1MFU.10 ) + as.numeric(Data4$b.scs.1MFU.11  ) + as.numeric(Data4$b.scs.1MFU.12)  + 
                        as.numeric(Data4$b.scs.1MFU.13))

Data1<-merge(Data1,Data2, by = "ID")
DataT2<-merge(Data1,Data3, by = "ID")
DataT2H<-DataT2[which(DataT2$SDQ_Diff < 15),]

DataT2H$TDays<-(DataT2H$total_Tnumber_of_days_1_session_only + DataT2H$total_Tnumber_of_days_2plus_sessions)
hist(DataT2H$TDays)
DataT2HPP<-DataT2H[which(DataT2H$TDays >6),]

DataT3<-merge(Data1, Data4, by = "ID")
Data1$age<-as.numeric(Data1$age)
DataT2$age<-as.numeric(DataT2$age)
DataT2H$age<-as.numeric(DataT2H$age)


DataT2$SDQ_Diff
Data1_rescale<-Data1

Data1_rescale$Scores.neutColNumRandErrRT_T5_1<-Data1$Scores.neutColNumRandErrRT_T5_1/1000
Data1_rescale$Scores.emoColNumRandErrRT_T5_1 <-Data1$Scores.emoColNumRandErrRT_T5_1/1000         
Data1_rescale$Scores.RTSadIncong_T3_1        <-Data1$Scores.RTSadIncong_T3_1/1000
Data1_rescale$Scores.RTSadCong_T3_1          <-Data1$Scores.RTSadCong_T3_1/1000           
Data1_rescale$Scores.RTHappyCong_T3_1        <-Data1$Scores.RTHappyCong_T3_1/1000
Data1_rescale$Scores.RTHappyIncong_T3_1      <-Data1$Scores.RTHappyIncong_T3_1/1000
Data1_rescale$Scores.RTSadNeut_T3_1          <-Data1$Scores.RTSadNeut_T3_1/1000
Data1_rescale$Scores.RTHappyNeut_T3_1        <-Data1$Scores.RTHappyNeut_T3_1/1000

DataT2_rescale<-DataT2

DataT2_rescale$Scores.neutColNumRandErrRT_T5_1<-DataT2$Scores.neutColNumRandErrRT_T5_1/1000
DataT2_rescale$Scores.emoColNumRandErrRT_T5_1 <-DataT2$Scores.emoColNumRandErrRT_T5_1/1000         
DataT2_rescale$Scores.RTSadIncong_T3_1        <-DataT2$Scores.RTSadIncong_T3_1/1000
DataT2_rescale$Scores.RTSadCong_T3_1          <-DataT2$Scores.RTSadCong_T3_1/1000           
DataT2_rescale$Scores.RTHappyCong_T3_1        <-DataT2$Scores.RTHappyCong_T3_1/1000
DataT2_rescale$Scores.RTHappyIncong_T3_1      <-DataT2$Scores.RTHappyIncong_T3_1/1000
DataT2_rescale$Scores.RTSadNeut_T3_1          <-DataT2$Scores.RTSadNeut_T3_1/1000
DataT2_rescale$Scores.RTHappyNeut_T3_1        <-DataT2$Scores.RTHappyNeut_T3_1/1000

DataT2_rescale$Scores.neutColNumRandErrRT_T5_2<-DataT2$Scores.neutColNumRandErrRT_T5_2/1000
DataT2_rescale$Scores.emoColNumRandErrRT_T5_2 <-DataT2$Scores.emoColNumRandErrRT_T5_2/1000         
DataT2_rescale$Scores.RTSadIncong_T3_2        <-DataT2$Scores.RTSadIncong_T3_2/1000
DataT2_rescale$Scores.RTSadCong_T3_2          <-DataT2$Scores.RTSadCong_T3_2/1000           
DataT2_rescale$Scores.RTHappyCong_T3_2        <-DataT2$Scores.RTHappyCong_T3_2/1000
DataT2_rescale$Scores.RTHappyIncong_T3_2      <-DataT2$Scores.RTHappyIncong_T3_2/1000
DataT2_rescale$Scores.RTSadNeut_T3_2          <-DataT2$Scores.RTSadNeut_T3_2/1000
DataT2_rescale$Scores.RTHappyNeut_T3_2        <-DataT2$Scores.RTHappyNeut_T3_2/1000


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
setwd("Z:/Data/WT_TrainingStudy")

###Training
describeBy(DataT2$no_of_Tsessions_over_10_mins, group = DataT2$Group)
DataT2ACT<-DataT2[which(DataT2$Group == "1"),]
DataT2P<-DataT2[which(DataT2$Group == "0"),]

gr<-table(DataT2$Group)
gr

gr<-table(DataT2$no_of_Tsessions_over_10_mins)
gr
DataT2_rescale$TDays<-(DataT2_rescale$total_Tnumber_of_days_1_session_only + DataT2_rescale$total_Tnumber_of_days_2plus_sessions)
hist(DataT2_rescale$TDays)

DataT2PP<-DataT2_rescale[which(DataT2_rescale$no_of_Tsessions_over_10_mins > 6),]

describeBy(DataT2$no_of_Tsessions_over_10_mins, group = DataT2$Group)
DataT2_rescale5<-DataT2_rescale[which(DataT2_rescale$TDays >6),]


DataT2_rescale$Group[which(DataT2_rescale$Group == "999")]<-NA


###H1
DataT2_rescale$Scores.dPrimeEmo_T4_1<-as.numeric(DataT2_rescale$Scores.dPrimeEmo_T4_1)
DataT2_rescale$Scores.dPrimeEmo_T4_2<-as.numeric(DataT2_rescale$Scores.dPrimeEmo_T4_2)

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.dPrimeEmo_T4_1", "Scores.dPrimeEmo_T4_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                 measured = c("Scores.dPrimeEmo_T4_1",
                                              "Scores.dPrimeEmo_T4_2"))
ACT_Training_long$dNBack<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 139, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dNBack ~ Time*Group  + (1|ID),
                  data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.maxFaceN_T4_1", "Scores.maxFaceN_T4_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                  measured = c("Scores.maxFaceN_T4_1",
                                               "Scores.maxFaceN_T4_2"))
ACT_Training_long$dNBack<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 144, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dNBack ~ Time*Group  + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)

describeBy(DataT2_rescale$Scores.maxFaceN_T4_1, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$Scores.maxFaceN_T4_2, group = DataT2_rescale$Group)

###H1 secondary (RT)
ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.meanRTCorrHitsEmo_T4_1", "Scores.meanRTCorrHitsScrambled_T4_1", "no_of_Tsessions_over_10_mins")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID", "no_of_Tsessions_over_10_mins"),
                                  measured = c("Scores.dPrimeEmo_T4_1",
                                               "Scores.dPrimeEmo_T4_2"))
ACT_Training_long$dNBack<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 140, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dNBack ~ Time*Group  + no_of_Tsessions_over_10_mins + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)

ACT_Training6<-DataT2_rescale5[,c("ID","Group", "Scores.dPrimeEmo_T4_1", "Scores.dPrimeEmo_T4_2")]
ACT_Training6<-na.omit(ACT_Training6)
ACT_Training6_long<-reshape2::melt(ACT_Training6, id = c("Group", "ID"),
                                  measured = c("Scores.dPrimeEmo_T4_1",
                                               "Scores.dPrimeEmo_T4_2"))
ACT_Training6_long$dNBack<-ACT_Training6_long$value
ACT_Training6_long$Time<-gl(2, 67, labels = c("Pre", "Post"))
ACT_Training6_long$ID<-as.factor(ACT_Training6_long$ID)

ACT_Train<-lmer(dNBack ~ Time*Group  + (1|ID),
                data = ACT_Training6_long, REML = FALSE)
summary(ACT_Train)

ACT_TrainingPP<-DataT2PP[,c("ID","Group", "Scores.dPrimeEmo_T4_1", "Scores.dPrimeEmo_T4_2")]
ACT_TrainingPP<-na.omit(ACT_TrainingPP)
ACT_TrainingPP_long<-reshape2::melt(ACT_TrainingPP, id = c("Group", "ID"),
                                   measured = c("Scores.dPrimeEmo_T4_1",
                                                "Scores.dPrimeEmo_T4_2"))
ACT_TrainingPP_long$dNBack<-ACT_TrainingPP_long$value
ACT_TrainingPP_long$Time<-gl(2, 76, labels = c("Pre", "Post"))
ACT_TrainingPP_long$ID<-as.factor(ACT_TrainingPP_long$ID)

ACT_Train<-lmer(dNBack ~ Time*Group  + (1|ID),
                data = ACT_TrainingPP_long, REML = FALSE)
summary(ACT_Train)

###H2

###Shifting

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.emoColNumRandErr_T5_1", "Scores.emoColNumRandErr_T5_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                  measured = c("Scores.emoColNumRandErr_T5_1",
                                               "Scores.emoColNumRandErr_T5_2"))


ACT_Training_long$dShift<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 103, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dShift ~ Time*Group  + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)

describeBy(DataT2_rescale$Scores.emoColNumRandErr_T5_1, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$Scores.emoColNumRandErr_T5_2, group = DataT2_rescale$Group)

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.neutColNumRandErr_T5_1", "Scores.neutColNumRandErr_T5_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                  measured = c("Scores.neutColNumRandErr_T5_1",
                                               "Scores.neutColNumRandErr_T5_2"))


ACT_Training_long$dShift<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 109, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dShift ~ Time*Group  + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)

describeBy(DataT2_rescale$Scores.neutColNumRandErr_T5_1, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$Scores.neutColNumRandErr_T5_2, group = DataT2_rescale$Group)


ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.emoColNumRandErr_T5_1", "Scores.emoColNumRandErr_T5_2", "no_of_Tsessions_over_10_mins")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID", "no_of_Tsessions_over_10_mins"),
                                  measured = c("Scores.emoColNumRandErr_T5_1",
                                               "Scores.emoColNumRandErr_T5_2"))


ACT_Training_long$dShift<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 102, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dShift ~ Time*Group +no_of_Tsessions_over_10_mins + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)


###H2 secondary (RT)

###Shifting
ACT_AC<-DataT2_rescale[,c("ID","Group", "Scores.emoColNumRandErrRT_T5_1", "Scores.emoColNumRandErrRT_T5_2") ]
ACT_AC<-na.omit(ACT_AC)
ACT_AC_long<-reshape2::melt(ACT_AC, id = c("Group", "ID"),
                     measured = c("Scores.emoColNumRandErrRT_T5_1",
                                  "Scores.emoColNumRandErrRT_T5_2"))
ACT_AC_long$dShiftRT<-ACT_AC_long$value
ACT_AC_long$Time<-gl(2, 103, labels = c("Pre", "Post"))
ACT_AC_long$ID<-as.factor(ACT_AC_long$ID)

ACT_Train<-lmer(dShiftRT ~ Time*Group   + (1|ID),
                data = ACT_AC_long, REML = FALSE)
summary(ACT_Train)

###WM

DataT2_rescale$Scores.spanEmo_T2_1

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.spanEmo_T2_1", "Scores.spanEmo_T2_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                  measured = c("Scores.spanEmo_T2_1",
                                               "Scores.spanEmo_T2_2"))


ACT_Training_long$dSpan<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 127, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dSpan ~ Time*Group  + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.spanNeu_T2_1", "Scores.spanNeu_T2_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                  measured = c("Scores.spanNeu_T2_1",
                                               "Scores.spanNeu_T2_2"))


ACT_Training_long$dSpan<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 123, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dSpan ~ Time*Group  + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)


###Inhibit

DataT2_rescale$Scores.RTSadIncong_T3_1

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.accSadIncongRaw_T3_1", "Scores.accSadIncongRaw_T3_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                  measured = c("Scores.accSadIncongRaw_T3_1",
                                               "Scores.accSadIncongRaw_T3_2"))


ACT_Training_long$dSIncon<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 144, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dSIncon ~ Time*Group  + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)

ACT_Training<-DataT2_rescale[,c("ID","Group", "Scores.RTSadIncong_T3_1", "Scores.RTSadIncong_T3_2")]
ACT_Training<-na.omit(ACT_Training)
ACT_Training_long<-reshape2::melt(ACT_Training, id = c("Group", "ID"),
                                  measured = c("Scores.RTSadIncong_T3_1",
                                               "Scores.RTSadIncong_T3_2"))


ACT_Training_long$dSIncon<-ACT_Training_long$value
ACT_Training_long$Time<-gl(2, 144, labels = c("Pre", "Post"))
ACT_Training_long$ID<-as.factor(ACT_Training_long$ID)

ACT_Train<-lmer(dSIncon ~ Time*Group  + (1|ID),
                data = ACT_Training_long, REML = FALSE)
summary(ACT_Train)


##H4
DataT2_rescale$cogcon = ((DataT2_rescale$Scores.RTSadNeut_T3_1 + DataT2_rescale$Scores.RTHappyNeut_T3_1 +DataT2_rescale$Scores.emoColNumRandErrRT_T5_1)/3)

DataT2_rescale$affcon = ((DataT2_rescale$Scores.RTSadIncong_T3_1+DataT2_rescale$Scores.RTSadCong_T3_1+DataT2_rescale$Scores.RTHappyIncong_T3_1
                        +DataT2_rescale$Scores.RTHappyCong_T3_1+DataT2_rescale$Scores.emoColNumRandErrRT_T5_1)/5)

cor.test(DataT2_rescale$cogcon, DataT2_rescale$SDQ_Int)
cor.test(DataT2_rescale$affcon, DataT2_rescale$SDQ_Int)

cor.test(DataT2_rescale$cogcon, DataT2_rescale$PANAS_N)
cor.test(DataT2_rescale$affcon, DataT2_rescale$PANAS_N)

####Correlations

Data1_rescale$Scores.neutColNumRandErrRT_T5_1<-as.numeric(Data1_rescale$Scores.neutColNumRandErrRT_T5_1)
Data1_rescale$Scores.emoColNumRandErrRT_T5_1 <-as.numeric(Data1_rescale$Scores.emoColNumRandErrRT_T5_1 )
Data1_rescale$Scores.RTSadIncong_T3_1        <-as.numeric(Data1_rescale$Scores.RTSadIncong_T3_1        )
Data1_rescale$Scores.RTSadCong_T3_1          <-as.numeric(Data1_rescale$Scores.RTSadCong_T3_1          )
Data1_rescale$Scores.RTHappyCong_T3_1        <-as.numeric(Data1_rescale$Scores.RTHappyCong_T3_1        )
Data1_rescale$Scores.RTHappyIncong_T3_1      <-as.numeric(Data1_rescale$Scores.RTHappyIncong_T3_1      )
Data1_rescale$Scores.RTSadNeut_T3_1          <-as.numeric(Data1_rescale$Scores.RTSadNeut_T3_1          )
Data1_rescale$Scores.RTHappyNeut_T3_1        <-as.numeric(Data1_rescale$Scores.RTHappyNeut_T3_1        )
  
Data1_rescale$Scores.spanNeu_T2_1<-as.numeric(Data1_rescale$Scores.spanNeu_T2_1)
Data1_rescale$Scores.spanEmo_T2_1<-as.numeric(Data1_rescale$Scores.spanEmo_T2_1)
Data1_rescale$Scores.neutColNumRandErr_T5_1<-as.numeric(Data1_rescale$Scores.neutColNumRandErr_T5_1)
Data1_rescale$iq.sum<-as.numeric(Data1_rescale$iq.sum)  

Data1_rescale$Scores.emoColNumRandErr_T5_1<-as.numeric(Data1_rescale$Scores.emoColNumRandErr_T5_1)  
CorM<-subset(Data1_rescale,select=c(  Scores.spanNeu_T2_1     , Scores.spanEmo_T2_1          , 
                                      Scores.neutColNumRandErrRT_T5_1 , Scores.RTSadNeut_T3_1        ,  
                                      Scores.RTSadIncong_T3_1         , Scores.RTSadCong_T3_1        ,
                                      Scores.neutColNumRandErr_T5_1   , Scores.emoColNumRandErr_T5_1 ,
                                      Scores.emoColNumRandErrRT_T5_1,  SDQ_Int, SDQ_Diff, SDQ_Ext, PANAS_N, PANAS_P, age ))

CorM<-na.omit(CorM)

cor(CorM)

###Dificulty

###ITT

ACT_DiffITT<-DataT2PP[,c("ID","Group", "SDQ_Diff", "SDQ.post_Diff","age")]
ACT_DiffITT<-na.omit(ACT_DiffITT)
ACT_Diff_longITT<-reshape2::melt(ACT_DiffITT, id = c("Group", "ID","age"),
                              measured = c("SDQ_Diff", "SDQ.post_Diff"))
ACT_Diff_longITT$dSDQ_Diff<-ACT_Diff_longITT$value
ACT_Diff_longITT$Time<-gl(2, 78, labels = c("Pre", "Post"))
ACT_Diff_longITT$ID<-as.factor(ACT_Diff_longITT$ID)

ACT_DiffITT<-lmer(dSDQ_Diff ~ Time*Group + age  + (1|ID),
                  data = ACT_Diff_longITT, REML = FALSE)
summary(ACT_DiffITT)

dDiffITT<-ezANOVA(data = ACT_Diff_longITT, dv = .(dSDQ_Diff), wid = .(ID), within = .(Time), between = .(Group), within_covariates = .(age), type = 3, detailed = TRUE)
dDiffITT

###Training data



ACT_Diff<-DataT2H[,c("ID","Group", "SDQ_Diff", "SDQ.post_Diff", "no_of_Tsessions_over_10_mins","age")]
ACT_Diff<-na.omit(ACT_Diff)
ACT_Diff_long<-reshape2::melt(ACT_Diff, id = c("Group", "ID", "no_of_Tsessions_over_10_mins","age"),
                    measured = c("SDQ_Diff", "SDQ.post_Diff"))
ACT_Diff_long$dSDQ_Diff<-ACT_Diff_long$value
ACT_Diff_long$Time<-gl(2, 65, labels = c("Pre", "Post"))
ACT_Diff_long$ID<-as.factor(ACT_Diff_long$ID)

ACT_Diff<-lmer(dSDQ_Diff ~ Time*Group + no_of_Tsessions_over_10_mins+ age  + (1|ID),
                  data = ACT_Diff_long, REML = FALSE)
summary(ACT_Diff)

dDiff<-ezANOVA(data = ACT_Diff_long, dv = .(dSDQ_Diff), wid = .(ID), within = .(Time), between = .(Group), within_covariates = .(no_of_Tsessions_over_10_mins, age), type = 3, detailed = TRUE)
dDiff

#ACT_Diff_long$Group<-as.factor(ACT_Diff_long$Group)
#ACT_Neg_long$Group<-as.factor(ACT_Neg_long$Group)

baseline_dDiff     <-lme(dSDQ_Diff ~ 1, random = ~1| ID/Time , data = ACT_Diff_long, method = "ML")
model_Cov_dDiff   <-update(baseline_dDiff, .~. + age + no_of_Tsessions_over_10_mins)
model_Time_dDiff   <-update(model_Cov_dDiff, .~. + Time  )
#model_Group_dDiff  <-update(model_Time_dDiff, .~. + (1|Group))
model_TxG_dDiff    <-update(model_Time_dDiff,   .~. + Time:Group)

anova(baseline_dDiff, model_Cov_dDiff, model_Time_dDiff,  model_TxG_dDiff)
summary(model_TxG_dDiff)

dDiff     <-lmer(dSDQ_Diff ~ Time * Group + age + no_of_Tsessions_over_10_mins +(1|ID), data = ACT_Diff_long)
anova(dDiff)

dDiff     <-lmer(dPANAS_N ~ Time * Group*no_of_Tsessions_over_10_mins + age  +(1|ID), data = ACT_Neg_long)
anova(dDiff)
DataT2$SDQ.post_INT
###SDQ Internalizing
ACT_DiffITT<-DataT2PP[,c("ID","Group", "SDQ_Int", "SDQ.post_INT","age")]
ACT_DiffITT<-na.omit(ACT_DiffITT)
ACT_Diff_longITT<-reshape2::melt(ACT_DiffITT, id = c("Group", "ID","age"),
                                 measured = c("SDQ_Diff", "SDQ.post_Diff"))
ACT_Diff_longITT$dSDQ_Diff<-ACT_Diff_longITT$value
ACT_Diff_longITT$Time<-gl(2, 83, labels = c("Pre", "Post"))
ACT_Diff_longITT$ID<-as.factor(ACT_Diff_longITT$ID)

ACT_DiffITT<-lmer(dSDQ_Diff ~ Time*Group + age  + (1|ID),
                  data = ACT_Diff_longITT, REML = FALSE)
summary(ACT_DiffITT)

dDiffITT<-ezANOVA(data = ACT_Diff_longITT, dv = .(dSDQ_Diff), wid = .(ID), within = .(Time), between = .(Group), within_covariates = .(age), type = 3, detailed = TRUE)
dDiffITT

Data5$SDQ_Int<-(Data5$SDQ_E + Data5$SDQ_PP)
Data5$SDQ.post_INT<-(Data5$SDQ.post_E + Data5$SDQ.post_PP)

ACT_Diff<-DataT2[,c("ID","Group", "SDQ_Int", "SDQ.post_INT", "no_of_Tsessions_over_10_mins","age")]
ACT_Diff<-na.omit(ACT_Diff)
ACT_Diff_long<-reshape2::melt(ACT_Diff, id = c("Group", "ID", "no_of_Tsessions_over_10_mins","age"),
                              measured = c("SDQ_Int", "SDQ.post_INT"))
ACT_Diff_long$dSDQ_Diff<-ACT_Diff_long$value
ACT_Diff_long$Time<-gl(2, 149, labels = c("Pre", "Post"))
ACT_Diff_long$ID<-as.factor(ACT_Diff_long$ID)

ACT_Diff<-lmer(dSDQ_Diff ~ Time*Group + age +no_of_Tsessions_over_10_mins + (1|ID),
               data = ACT_Diff_long, REML = FALSE)
summary(ACT_Diff)

dDiff<-ezANOVA(data = ACT_Diff_long, dv = .(dSDQ_Diff), wid = .(ID), within = .(Time), between = .(Group), within_covariates = .(no_of_Tsessions_over_10_mins, age), type = 3, detailed = TRUE)
dDiff

baseline_dDiff     <-lme(dSDQ_Diff ~ 1, random = ~1| ID/Time , data = ACT_Diff_long, method = "ML")
model_Cov_dDiff    <-update(baseline_dDiff, .~. + age + no_of_Tsessions_over_10_mins)
model_Time_dDiff   <-update(model_Cov_dDiff, .~. + Time  )
#model_Group_dDiff <-update(model_Time_dDiff, .~. + (1|Group))
model_TxG_dDiff    <-update(model_Time_dDiff,   .~. + Time:Group)

anova(baseline_dDiff, model_Cov_dDiff, model_Time_dDiff,  model_TxG_dDiff)
summary(model_TxG_dDiff)

###PANAS_N



ACT_Neg<-DataT2PP[,c("ID","Group", "PANAS_N", "PANAS.post_N",  "no_of_Tsessions_over_10_mins","age")]
ACT_Neg<-na.omit(ACT_Neg)
ACT_Neg_long<-reshape2::melt(ACT_Neg, id = c("Group", "ID", "no_of_Tsessions_over_10_mins","age"),
                   measured = c("PANAS_N", "PANAS.post_N"))
ACT_Neg_long$dPANAS_N<-ACT_Neg_long$value
ACT_Neg_long$Time<-gl(2, 85, labels = c("Pre", "Post"))
ACT_Neg_long$ID<-as.factor(ACT_Neg_long$ID)

dNeg<-ezANOVA(data = ACT_Neg_long, dv = .(dPANAS_N), wid = .(ID), within = .(Time), between = .(Group), within_covariates = .(no_of_Tsessions_over_10_mins, age),type = 3, detailed = TRUE)
dNeg

model_PANAS.N <- lmer(dPANAS_N ~ Time*Group+(1|ID),
                      data = ACT_Neg_long, REML = FALSE)
summary(model_PANAS.N)

baseline_dNeg     <-lme(dPANAS_N ~ 1, random = ~1| ID/Time, data = ACT_Neg_long, method = "ML")
model_Time_dNeg   <-update(baseline_dNeg, .~. + Time + age + no_of_Tsessions_over_10_mins)
model_Group_dNeg  <-update(model_Time_dNeg, .~. + Group)
model_TxG_dNeg    <-update(model_Group_dNeg,   .~. + Time:Group)

anova(baseline_dNeg, model_Time_dNeg, model_Group_dNeg,  model_TxG_dNeg)
summary(model_TxG_dNeg)


###PANAS_P

ACT_Pos<-DataTT[,c("ID","Group", "PANAS_P", "PANAS.post_P")]
ACT_Pos<-na.omit(ACT_Pos)
ACT_Pos_long<-melt(ACT_Pos, id = c("Group", "ID"),
                   measured = c("PANAS_P", "PANAS.post_P"))
ACT_Pos_long$dPANAS_P<-ACT_Pos_long$value
ACT_Pos_long$Time<-gl(2, 165, labels = c("Pre", "Post"))
ACT_Pos_long$ID<-as.factor(ACT_Pos_long$ID)

dPos<-ezANOVA(data = ACT_Pos_long, dv = .(dPANAS_P), wid = .(ID), within = .(Time), between = .(Group), type = 3, detailed = TRUE)
dPos


baseline_dPos     <-lme(dPANAS_P ~ 1, random = ~1| ID/Time, data = ACT_Pos_long, method = "ML")
model_Time_dPos   <-update(baseline_dPos, .~. + Time)
model_Group_dPos  <-update(model_Time_dPos, .~. + Group)
model_TxG_dPos    <-update(model_Group_dPos,   .~. + Time:Group)

anova(baseline_dPos, model_Time_dPos, model_Group_dPos,  model_TxG_dPos)
summary(model_TxG_dPos)

###SCS

ACT_SCS<-DataT2[,c("ID","Group", "SCS", "SCS.post", "age", "no_of_Tsessions_over_10_mins")]
ACT_SCS<-na.omit(ACT_SCS)
ACT_SCS_long<-melt(ACT_SCS, id = c("Group", "ID", "age","no_of_Tsessions_over_10_mins"),
                   measured = c("SCS", "SCS.post"))
ACT_SCS_long$dSCS<-ACT_SCS_long$value
ACT_SCS_long$Time<-gl(2, 159, labels = c("Pre", "Post"))
ACT_SCS_long$ID<-as.factor(ACT_SCS_long$ID)

dSCS<-ezANOVA(data = ACT_SCS_long, dv = .(dSCS), wid = .(ID), within = .(Time), between = .(Group), type = 3, detailed = TRUE)
dSCS

model_SCS <- lmer(dSCS ~ Time*Group + age + no_of_Tsessions_over_10_mins+(1|ID),
                      data = ACT_SCS_long, REML = FALSE)
summary(model_SCS)

baseline_dSCS     <-lme(dSCS ~ 1, random = ~1| ID/Time, data = ACT_SCS_long, method = "ML")
model_Time_dSCS       <-update(baseline_dSCS, .~. + Time)
model_Group_dSCS     <-update(model_Time_dSCS, .~. + Group)
model_TxG_dSCS    <-update(model_Group_dSCS,   .~. + Time:Group)

anova(baseline_dSCS, model_Time_dSCS, model_Group_dSCS,  model_TxG_dSCS)
summary(model_TxG_dSCS)


Data1_rescale$cogcon=(Data1_rescale$Scores.RTSadNeut_T3_1+Data1_rescale$Scores.RTHappyNeut_T3_1 +Data1_rescale$Scores.emoColNumRandErrRT_T5_1)
Data1_rescale$affcon=(Data1_rescale$Scores.RTSadIncong_T3_1+Data1_rescale$Scores.RTSadCong_T3_1   +Data1_rescale$Scores.RTHappyIncong_T3_1+
Data1_rescale$Scores.RTHappyCong_T3_1+Data1_rescale$Scores.emoColNumRandErrRT_T5_1)
DataT2_rescale$cogcon.pre=(DataT2_rescale$Scores.RTSadNeut_T3_1  +DataT2_rescale$Scores.RTHappyNeut_T3_1 +DataT2_rescale$Scores.emoColNumRandErrRT_T5_1)
DataT2_rescale$affcon.pre=(DataT2_rescale$Scores.RTSadIncong_T3_1+DataT2_rescale$Scores.RTSadCong_T3_1   +DataT2_rescale$Scores.RTHappyIncong_T3_1+
                              DataT2_rescale$Scores.RTHappyCong_T3_1+DataT2_rescale$Scores.emoColNumRandErrRT_T5_1)
DataT2_rescale$cogcon.post=(DataT2_rescale$Scores.RTSadNeut_T3_2  +DataT2_rescale$Scores.RTHappyNeut_T3_2 +DataT2_rescale$Scores.emoColNumRandErrRT_T5_2)
DataT2_rescale$affcon.post=(DataT2_rescale$Scores.RTSadIncong_T3_2+DataT2_rescale$Scores.RTSadCong_T3_2   +DataT2_rescale$Scores.RTHappyIncong_T3_2+
                       DataT2_rescale$Scores.RTHappyCong_T3_2+DataT2_rescale$Scores.emoColNumRandErrRT_T5_2)

ACT_RT<-DataT2_rescale[,c("ID","Group", "cogcon.pre", "affcon.pre","cogcon.post", "affcon.post","age","no_of_Tsessions_over_10_mins")]
ACT_RT<-na.omit(ACT_RT)
ACT_RT_longITT<-reshape2::melt(ACT_RT, id = c("Group", "ID","age","no_of_Tsessions_over_10_mins"),
                         measured = c("cogcon.pre", "affcon.pre","cogcon.post", "affcon.post"))
ACT_RT_longITT$dRT<-ACT_RT_longITT$value
ACT_RT_longITT$Time<-gl(2, 202, labels = c("Pre", "Post"))
ACT_RT_longITT$Valence<-gl(2, 101, labels = c("Neu", "Neg"))
ACT_RT_longITT$ID<-as.factor(ACT_RT_longITT$ID)

ACT_RTITT<-lmer(dRT ~ Time*Valence*Group + age  +no_of_Tsessions_over_10_mins+ (1|ID),
                  data = ACT_RT_longITT, REML = FALSE)
summary(ACT_RTITT)

dRTITT<-ezANOVA(data = ACT_RT_longITT, dv = .(dRT), wid = .(ID), within = .(Time, Valence), between = .(Group), within_covariates = .(age,no_of_Tsessions_over_10_mins), type = 3, detailed = TRUE)
dRTITT

DataT2_rescale$dcon<-DataT2_rescale$cogcon.pre-DataT2_rescale$cogcon.post
DataT2_rescale$daff<-DataT2_rescale$affcon.pre-DataT2_rescale$affcon.post

describeBy(DataT2_rescale$cogcon.pre, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$affcon.pre, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$cogcon.post, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$affcon.post, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$dcon, group = DataT2_rescale$Group)
describeBy(DataT2_rescale$daff, group = DataT2_rescale$Group)

ACT_RT<-DataT2_rescale[,c("ID","Group", "cogcon.pre", "affcon.pre","cogcon.post", "affcon.post")]
ACT_RT<-na.omit(ACT_RT)
ACT_RT_longITT<-reshape2::melt(ACT_RT, id = c("Group", "ID"),
                               measured = c("cogcon.pre", "affcon.pre","cogcon.post", "affcon.post"))
ACT_RT_longITT$dRT<-ACT_RT_longITT$value
ACT_RT_longITT$Time<-gl(2, 212, labels = c("Pre", "Post"))
ACT_RT_longITT$Valence<-gl(2, 106, labels = c("Neu", "Neg"))
ACT_RT_longITT$ID<-as.factor(ACT_RT_longITT$ID)

ACT_RTITT<-lmer(dRT ~ Time*Valence*Group + (1|ID),
                data = ACT_RT_longITT, REML = FALSE)
summary(ACT_RTITT)

ACT_RT<-DataT2_rescale[,c("ID","Group", "cogcon.pre", "affcon.pre","cogcon.post", "affcon.post","age","no_of_Tsessions_over_10_mins")]
ACT_RT<-na.omit(ACT_RT)
ACT_RT_longITT<-reshape2::melt(ACT_RT, id = c("Group", "ID","age","no_of_Tsessions_over_10_mins"),
                               measured = c("cogcon.pre", "affcon.pre","cogcon.post", "affcon.post"))
ACT_RT_longITT$dRT<-ACT_RT_longITT$value
ACT_RT_longITT$Time<-gl(2, 164, labels = c("Pre", "Post"))
ACT_RT_longITT$Valence<-gl(2, 82, labels = c("Neu", "Neg"))
ACT_RT_longITT$ID<-as.factor(ACT_RT_longITT$ID)

ACT_RTITT<-lmer(dRT ~ Time*Valence*Group + age  +no_of_Tsessions_over_10_mins+ (1|ID),
                data = ACT_RT_longITT, REML = FALSE)
summary(ACT_RTITT)
