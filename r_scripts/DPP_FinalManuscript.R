library(nlme)
library(plyr)
library(reshape)
library(weightr)
library(cowplot)
library(tidyr)
library(Hmisc)
library(psych)
library(foreign)
library(ggplot2)
library(haven)
library(psych)
library(Rcpp)
library(rcompanion)
library(base)
library(mgcv)
library(car)
library(ez)
library(fastR)
library(lavaan)
library(RColorBrewer)
library(ggpubr)
library(emmeans)
library(semPlot)
library(ART)
library("corrplot")

setwd("Z:/Data/DSF Project/UCL study/Data/Updated")

Data13<-read.csv("RevisionDPP.csv")

#Baseline characteristics
IQ<-lm(WASItscore~Group, data = Data13)
summary(IQ)
IQ.contrasts = summary(pairs(emmeans(IQ, ~ Group)))
IQ.contrasts$d = IQ.contrasts$estimate / sigmaHat(IQ)
IQ.contrasts


###Mental health, affective control and emotion regulation capacity across age groups paragraph

#1. Mental health

DifGroup<-lm(Difficulty ~ Group, data = Data13)
summary(DifGroup)
GroupDif.contrasts = summary(pairs(emmeans(DifGroup, ~ Group)))
GroupDif.contrasts$d = GroupDif.contrasts$estimate / sigmaHat(DifGroup)
GroupDif.contrasts

#2. Affective control

ACGroup<-lm(ProporaionalFmSdS_C ~ Group, data = Data13)
summary(ACGroup)
GroupAC.contrasts = summary(pairs(emmeans(ACGroup, ~ Group)))
GroupAC.contrasts$d = GroupAC.contrasts$estimate / sigmaHat(ACGroup)
GroupAC.contrasts

###reviewer question differential association across neutral / affective
AGroup<-lm(Data13$F_totalPerc_noneffective   ~ Group, data = Data13)
summary(AGroup)

NGroup<-lm(Data13$S_totalPerc_noneffective   ~ Group, data = Data13)
summary(NGroup)


####Exploratory analyses of different errors - based on reviewer suggestions
AC_Effective_Group<-lm(ProporaionalFmSdS_effective ~ Group, data = Data13)
summary(AC_Effective_Group)
GroupE.contrasts = summary(pairs(emmeans(AC_Effective_Group, ~ Group)))
GroupE.contrasts$d = GroupE.contrasts$estimate / sigmaHat(AC_Effective_Group)
GroupE.contrasts

AC_Rand_Group<-lm(Proporaional_RandComb_FmSdS_C ~ Group, data = Data13)
summary(AC_Rand_Group)
GroupAC.contrasts = summary(pairs(emmeans(AC_Rand_Group, ~ Group)))
GroupAC.contrasts$d = GroupAC.contrasts$estimate / sigmaHat(AC_Rand_Group)
GroupAC.contrasts

AC_Pers_Group<-lm(Proporaional_Persev_FmSdS_C ~ Group, data = Data13)
summary(AC_Pers_Group)
GroupAC.contrasts = summary(pairs(emmeans(AC_Pers_Group, ~ Group)))
GroupAC.contrasts$d = GroupAC.contrasts$estimate / sigmaHat(AC_Pers_Group)
GroupAC.contrasts

rAGroup<-lm(Data13$F_P_RandComb_CN_C   ~ Group, data = Data13)
summary(rAGroup)

rNGroup<-lm(Data13$S_P_RandComb_CN_C   ~ Group, data = Data13)
summary(rNGroup)

####end exploratory analyses

#3. Emotion regualtion

RegGroup<-lm(Regulation ~ Group, data = Data13)
summary(RegGroup)
GroupERT.contrasts = summary(pairs(emmeans(RegGroup, ~ Group)))
GroupERT.contrasts$d = GroupERT.contrasts$estimate / sigmaHat(RegGroup)
GroupERT.contrasts

DERSGroup<-lm(DRES ~ Group, data = Data13)
summary(DERSGroup)
GroupDERS.contrasts = summary(pairs(emmeans(DERSGroup, ~ Group)))
GroupDERS.contrasts$d = GroupDERS.contrasts$estimate / sigmaHat(DERSGroup)
GroupDERS.contrasts


##Affective control and mental health paragraph


DiffCC<-lm(Difficulty ~ ProporaionalFmSdS_C,data = Data13)
summary(DiffCC)
cor.test(Data13$Difficulty, Data13$ProporaionalFmSdS_C)


####exploratory analyses looking at the different error types

Diff_R_CC<-lm(Difficulty ~ Proporaional_RandComb_FmSdS_C,data = Data13)
summary(Diff_R_CC)
cor.test(Data13$Difficulty, Data13$Proporaional_RandComb_FmSdS_C)

Diff_P_CC<-lm(Difficulty ~ Proporaional_Persev_FmSdS_C,data = Data13)
summary(Diff_P_CC)
cor.test(Data13$Difficulty, Data13$Proporaional_Persev_FmSdS_C)

####end exploratory analyses

Data13$Group<-as.numeric(Data13$Group)


SetShiftingGroup <-
  '# regressions
Difficulty           ~ c*Group 
ProporaionalFmSdS_C ~ a*Group  
Difficulty           ~ b*ProporaionalFmSdS_C

# indirect effect (a*b)
ab := a*b

#direct effect (c)

direct := c

# total effect
total := c + (a*b)'

fit <- sem(SetShiftingGroup, data=Data13, std.lv=T, std.ov=T, 
           missing='fiml', se='robust', estimator='mlr', auto.var = T) # robust estimators, proper missing data method etc.

summary (fit, standardized=T, rsquare=T) # use standardized variables and request R2

fitMeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled",
                                  "rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                  "cfi.scaled","srmr","aic")) # request only most pertinent measures

####Creating data subsets for each age group
Data13_EA<-Data13[which(Data13$Group == 1),]

Data13_Mid<-Data13[which(Data13$Group == 2),]

Data13_Adult<-Data13[which(Data13$Group == 3),]


Data13$Group <- factor(Data13$Group,
                       levels = c(1,2,3),
                       labels = c("Early adolescent", "Mid-adolescent", "Z_Adult"))

DiffAC_EA<-lm   (Data13_EA$Difficulty ~ Data13_EA$ProporaionalFmSdS_C)
summary         (DiffAC_EA)
cor.test        (Data13_EA$Difficulty,  Data13_EA$ProporaionalFmSdS_C)

DiffAC_Mid<-lm  (Data13_Mid$Difficulty ~ Data13_Mid$ProporaionalFmSdS_C)
summary         (DiffAC_Mid)
cor.test        (Data13_Mid$Difficulty, Data13_Mid$ProporaionalFmSdS_C)

DiffAC_Adult<-lm(Data13_Adult$Difficulty ~ Data13_Adult$ProporaionalFmSdS_C)
summary(DiffAC_Adult)
cor.test(Data13_Adult$Difficulty, Data13_Adult$ProporaionalFmSdS_C)

##Affective control and emotion regulation paragraph

RegCC<-lm(Regulation ~ ProporaionalFmSdS_C, data = Data13)
summary(RegCC)
cor.test(Data13$Regulation, Data13$ProporaionalFmSdS_C)

DERSCC<-lm(DRES ~ ProporaionalFmSdS_C,data = Data13)
summary(DERSCC)
cor.test(Data13$DRES, Data13$ProporaionalFmSdS_C)


####exploratory analyses looking at the different error types

DERS_R_CC<-lm(DRES ~ Proporaional_RandComb_FmSdS_C,data = Data13)
summary(DERS_R_CC)
cor.test(Data13$DRES, Data13$Proporaional_RandComb_FmSdS_C)

DERS_P_CC<-lm(DRES ~ Proporaional_Persev_FmSdS_C,data = Data13)
summary(DERS_P_CC)
cor.test(Data13$DRES, Data13$Proporaional_Persev_FmSdS_C)

####end exploratory analyses


Data13$Group<-as.numeric(Data13$Group)

DiffDERSAC <-
  '# regressions
Difficulty           ~ c*ProporaionalFmSdS_C 
DRES                 ~ a*ProporaionalFmSdS_C  
Difficulty           ~ b*DRES

# indirect effect (a*b)
ab := a*b

#direct effect (c)

direct := c

# total effect
total := c + (a*b)'

fit <- sem(DiffDERSAC, data=Data13, std.lv=T, std.ov=T, 
           missing='fiml', se='robust', estimator='mlr', auto.var = T) # robust estimators, proper missing data method etc.

summary (fit, standardized=T, rsquare=T) # use standardized variables and request R2

fitMeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled",
                                  "rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                  "cfi.scaled","srmr","aic")) # request only most pertinent measures

#quantile(Data13$DRES, c(.25, .50, .75)) 
table_DRES<- subset(Data13, select=c("DRES"))
summary(table_DRES)

Data13$DRESGroup<-cut(Data13$DRES,
                    breaks=c(-Inf, 72, 96.5, Inf),
                    labels=c("Low","Average","High"))

Data13_hERD<-Data13[which(Data13$DRESGroup == "High"),]

Data13_aERD<-Data13[which(Data13$DRESGroup == "Average"),]

Data13_lERD<-Data13[which(Data13$DRESGroup == "Low"),]

describeBy(Data13$ProporaionalFmSdS_C, group = Data13$DRESGroup)

ERgroup<-lm(ProporaionalFmSdS_C ~ DRESGroup, data = Data13)
summary(ERgroup)
ERGroup.contrasts = summary(pairs(emmeans(ERgroup, ~ DRESGroup)))
ERGroup.contrasts$d = ERGroup.contrasts$estimate / sigmaHat(ERgroup)
ERGroup.contrasts


highERD<-lm(Difficulty ~ ProporaionalFmSdS_C, data = Data13_hERD)
summary(highERD)
cor.test(Data13_hERD$ProporaionalFmSdS_C, Data13_hERD$Difficulty)

averageERD<-lm(Difficulty ~ ProporaionalFmSdS_C, data = Data13_aERD)
summary(averageERD)
cor.test(Data13_aERD$ProporaionalFmSdS_C, Data13_aERD$Difficulty)

lowERD<-lm(Difficulty ~ ProporaionalFmSdS_C, data = Data13_lERD)
summary(lowERD)
cor.test(Data13_lERD$ProporaionalFmSdS_C, Data13_lERD$Difficulty)


##No mediation possible with the experimental measure of emotion regulation 
##as it was unrelated to age (see above), affective control (above) 
##or mental health difficulties



####ALL FIGURES IN THE PAPER WITH EXCEPTION OF FIG 1 WHICH IS A TASK DESIGN FIGURE
####FIGURES 3A AND 4A WHICH ARE PATH MODELS


###Figure 2. Association of age group with mental health (A), affective control (B) and emotion regulation (C&D)
my_comparisons <- list( c("Early adolescent", "Mid-adolescent"), c("Mid-adolescent", "Z_Adult"), c("Early adolescent", "Z_Adult") )
Fig2A<-ggplot(Data13, aes(x=Group, y=Difficulty, group = Group, fill=Group) ) + geom_boxplot(alpha=0.5)+
  theme(legend.position = "none")+
  ylab ("Mental health problems")+
  scale_x_discrete(labels=c("Early adolescent" = "Early adolescents" , "Mid-adolescent" = "Mid-adolescents",
                        "Z_Adult" = "Adults"))+
  font("x.text", size = 8.5)+
  font("y.text", size = 8.5)+
  font("xlab", size = 10)+
  font("ylab", size = 10)+
  scale_fill_brewer(palette="PuRd")+
  # scale_colour_brewer (palette = 'PuBuGn')+
  # scale_fill_brewer (type="seq", palette = 1)+
  # ylim(10, 40)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.signif" ) + # Add pairwise comparisons p-value
  #geom_label(label.size = 0.1)
  # stat_compare_means(label.y = 47)     # Add global p-valu
  ggsave("Fig2A_R2.png", width=4, height=4)


Fig2B<-ggplot(Data13, aes(x=Group, y=ProporaionalFmSdS_C, group = Group, fill=Group) ) + geom_boxplot(alpha=0.5)+
  theme(legend.position = "none")+
  ylab ("Affective Control")+
  scale_x_discrete(labels=c("Early adolescent" = "Early adolescents" , "Mid-adolescent" = "Mid-adolescents",
                            "Z_Adult" = "Adults"))+
   font("x.text", size = 8.5)+
   font("y.text", size = 8.5)+
   font("xlab", size = 10)+
   font("ylab", size = 10)+
  scale_fill_brewer(palette="PuRd")+
  # scale_colour_brewer (palette = 'PuBuGn')+
  # scale_fill_brewer (type="seq", palette = 1)+
  # ylim(10, 40)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.signif")+ # Add pairwise comparisons p-value
  #  stat_compare_means(label.y = 0.9)     # Add global p-valu
  ggsave("Fig2B_rev.png", width=4, height=4)

Fig2C<-ggplot(Data13, aes(x=Group, y=Regulation, group = Group, fill=Group) ) + geom_boxplot(alpha=0.5)+
  theme(legend.position = "none")+
  ylab ("Emotion regulation capacity (experimental)")+
   font("x.text", size = 8.5)+
   font("y.text", size = 8.5)+
   font("xlab", size = 10)+
   font("ylab", size = 10)+
  scale_fill_brewer(palette="PuRd")+
  # scale_colour_brewer (palette = 'PuBuGn')+
  # scale_fill_brewer (type="seq", palette = 1)+
  # ylim(10, 40)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.signif")+ # Add pairwise comparisons p-value
  #font("label", size = 6)
  # stat_compare_means(label.y = 7.4)     # Add global p-valu
  ggsave("Fig2C.png", width=4, height=4)

Fig2D<-ggplot(Data13, aes(x=Group, y=DRES, group = Group, fill=Group) ) + geom_boxplot(alpha=0.5)+
  theme(legend.position = "none")+
  ylab ("Difficulties regulating emotions (self-report)")+
   font("x.text", size = 8.5)+
   font("y.text", size = 8.5)+
   font("xlab", size = 10)+
   font("ylab", size = 10)+
  scale_fill_brewer(palette="PuRd")+
  # scale_colour_brewer (palette = 'PuBuGn')+
  # scale_fill_brewer (type="seq", palette = 1)+
  # ylim(10, 40)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.signif")+ # Add pairwise comparisons p-value
  # stat_compare_means(label.y = 190)     # Add global p-valu
  ggsave("Fig2D.png", width=4, height=4)


plot_grid(Fig2A, Fig2B,labels = c("A", "B"), ncol = 2)+
ggsave("cow1t_rev.png")
plot_grid(Fig2C, Fig2D, labels = c("C", "D"))+
ggsave("cow2t_rev.png")


###Figure 3. Differential association between affective control and mental health across age groups
###please note there is jitter figures will never look identical from one run of the script to the next

my_subtitleEA<-expression(paste(italic("r")," (27) = -.44, 95%CI [-.69, -.08]"))
my_subtitleMid<-expression(paste(italic("r")," (31) = -.14, 95%CI [-.48, .23]"))
my_subtitleAdult<-expression(paste(italic("r")," (28) = -.13, 95%CI [-.48, .27]"))

Scores<-c(Data13_EA$Difficulty)
CC<-c(Data13_EA$ProporaionalFmSdS_C)
measure<-c(rep('Diff',times=nrow(Data13_EA)))
DFe<-data.frame(Scores,CC,measure)
Fig3B<-ggplot(DFe,aes(CC,Scores,fill=measure,col=measure))+
  geom_point(alpha=.5,aes(fill=measure) )+ 
  #geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Mental health difficulties')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Early adolescent group", subtitle = my_subtitleEA)+
  theme(plot.title = element_text(size=12))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  ggsave('Fig3B_rev_R2.png',width=4,height=4)

Scores<-c(Data13_Mid$Difficulty)
CC<-c(Data13_Mid$ProporaionalFmSdS_C)
measure<-c(rep('Diff',times=nrow(Data13_Mid)))
DFe<-data.frame(Scores,CC,measure)
Fig3C<-ggplot(DFe,aes(CC,Scores,fill=measure,col=measure))+  
  geom_point(alpha=.5,aes(fill=measure) )+ 
 # geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Mental health difficulties')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Mid-adolescent group", subtitle = my_subtitleMid )+
  theme(plot.title = element_text(size=12))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  ggsave('Fig3C_rev_2.png',width=4,height=4)

Scores<-c(Data13_Adult$Difficulty)
CC<-c(Data13_Adult$ProporaionalFmSdS_C)
measure<-c(rep('Diff',times=nrow(Data13_Adult)))
DFe<-data.frame(Scores,CC,measure)
Fig3D<-ggplot(DFe,aes(CC,Scores,fill=measure,col=measure))+
  geom_point(alpha=.5,aes(fill=measure) )+ 
 # geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Mental health difficulties')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Adult group", subtitle = my_subtitleAdult)+
  theme(plot.title = element_text(size=12))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  ggsave('Fig3D_rev_R2.png',width=4,height=4)

plot_grid(Fig3B, Fig3C,Fig3D, labels = c("B", "C","D"), ncol = 3)
ggsave("cow5t2_rev.png")

###Figure 4 - note low difficulty = good ER capacity

my_subtitleHighERD<-expression(paste(italic("r")," (20) = -.57, 95%CI [-.80, -.19]"))
my_subtitleAverageERD<-expression(paste(italic("r")," (39) = -.01, 95%CI [-.32, .30]"))
my_subtitleLowERD<-expression(paste(italic("r")," (21) = -.47, 95%CI [-.74, -.08]"))

Scores<-c(Data13_lERD$Difficulty)
CC<-c(Data13_lERD$ProporaionalFmSdS_C)
measure<-c(rep('Diff',times=nrow(Data13_lERD)))
DFx<-data.frame(Scores,CC,measure)
Fig4B<-ggplot(DFx,aes(CC,Scores,fill=measure,col=measure))+
  geom_point(alpha=.5,aes(fill=measure) )+ 
 # geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Mental health difficulties')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Good emotion regulation", subtitle = my_subtitleLowERD)+
  theme(plot.title = element_text(size=11))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
ggsave('Fig4B_rev_R2.png',width=4,height=4)

Scores<-c(Data13_aERD$Difficulty)
CC<-c(Data13_aERD$ProporaionalFmSdS_C)
measure<-c(rep('Diff',times=nrow(Data13_aERD)))
DFy<-data.frame(Scores,CC,measure)
Fig4C<-ggplot(DFy,aes(CC,Scores,fill=measure,col=measure))+  
  geom_point(alpha=.5,aes(fill=measure) )+ 
  #geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Mental health difficulties')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Average emotion regulation", subtitle = my_subtitleAverageERD )+
  theme(plot.title = element_text(size=11))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
ggsave('Fig4C_rev_R2.png',width=4,height=4)

Scores<-c(Data13_hERD$Difficulty)
CC<-c(Data13_hERD$ProporaionalFmSdS_C)
measure<-c(rep('Diff',times=nrow(Data13_hERD)))
DFz<-data.frame(Scores,CC,measure)
Fig4D<-ggplot(DFz,aes(CC,Scores,fill=measure,col=measure))+
  geom_point(alpha=.5,aes(fill=measure) )+ 
  #geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Mental health difficulties')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Poor emotion regulation", subtitle = my_subtitleHighERD)+
  theme(plot.title = element_text(size=11))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
ggsave('Fig4Drev_R2.png',width=4,height=4)

plot_grid(Fig4B, Fig4C,Fig4D, labels = c("B", "C","D"), ncol = 3)
ggsave("cow6_rev_R2.png")


###Figure S1

my_subtitleExp<-expression(paste(italic("r")," (87) = -.05, 95%CI [-.26, .16]"))
my_subtitleSR<-expression(paste(italic("r")," (87) = .34, 95%CI [.14, .52]"))

Scores<-c(Data13$Regulation)
CC<-c(Data13$FmS_ColNum_Rand_Prob)
measure<-c(rep('Diff',times=nrow(Data13)))
DFf<-data.frame(Scores,CC,measure)
FigS1A<-ggplot(DFf,aes(CC,Scores,fill=measure,col=measure))+
  geom_point(alpha=.5,aes(fill=measure) )+ 
  geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Emotion regulation capacity (experimental)')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Experimental", subtitle = my_subtitleExp)+
  theme(plot.title = element_text(size=11))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
ggsave('FigS1A.png',width=4,height=4)



Scores<-c(Data13$DRES)
CC<-c(Data13$FmS_ColNum_Rand_Prob)
measure<-c(rep('Diff',times=nrow(Data13)))
DFf<-data.frame(Scores,CC,measure)
FigS1B<-ggplot(DFf,aes(CC,Scores,fill=measure,col=measure))+
  geom_point(alpha=.5,aes(fill=measure) )+ 
  geom_jitter(width=.5,height=.048)+
  stat_smooth(method=lm,col='grey10',fill='grey10')+
  scale_colour_brewer (palette = 'PRGn')+
  scale_fill_brewer (type="seq", palette = 1)+
  theme(text=element_text(size=11))+
  xlab('Affective control')+
  ylab('Difficulties regulating emotions (self-report)')+
  theme(legend.position="none")+
  xlim(-0.8,0.8)+
  font("x.text", size = 8) +
  font("y.text", size = 8)+
  labs(title = "Self-report", subtitle = my_subtitleSR)+
  theme(plot.title = element_text(size=11))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  ggsave('FigS1B.png',width=4,height=4)

plot_grid(FigS1A, FigS1B, labels = c("A", "B"), ncol = 2)+
ggsave("cowS1.png")

##########################################################
####SUPPLEMENTAL ANALYSES
##########################################################

###Table S2
#clinical
describeBy(Data13$Difficulty, group = Data13$Group)
describeBy(Data13$DRES, group = Data13$Group)

#non-effective errors
describeBy(Data13$F_totalPerc_noneffective, group = Data13$Group)
describeBy(Data13$S_totalPerc_noneffective, group = Data13$Group)
describeBy(Data13$ProporaionalFmSdS_C, group = Data13$Group)

#effective errors
describeBy(Data13$F_totalPerc_effective, group = Data13$Group)
describeBy(Data13$S_totalPerc_effective, group = Data13$Group)

#non-effective errors split
describeBy(Data13$F_P_P_CN, group = Data13$Group)
describeBy(Data13$S_P_P_CN, group = Data13$Group)
describeBy(Data13$F_P_RandComb_CN, group = Data13$Group)
describeBy(Data13$S_P_RandComb_CN, group = Data13$Group)

#Reaction time
describeBy(Data13$F_ColNum_Rand_RT, group = Data13$Group)
describeBy(Data13$S_ColNum_Rand_RT, group = Data13$Group)
describeBy(Data13$ProporaionalFmSdS_RT, group = Data13$Group)

###Table S3 - Correlation matrix

CorrMat<-Data13[c("ProporaionalFmSdS_C", "F_totalPerc_noneffective", 
                  "S_totalPerc_noneffective", "Difficulty", "DRES", "Regulation", "WASItscore")]
CorrMat<-na.omit(CorrMat)

M<-cor(CorrMat)

corrplot.mixed(M)

res2 <- rcorr(as.matrix(CorrMat))
res2

cor.test(Data13$ProporaionalFmSdS_C, Data13$SDQ_QProSoc)

pcor(M, method = "pearson")

CorrMat_EA<-Data13_EA[c("ProporaionalFmSdS_C", "F_totalPerc_noneffective", 
                  "S_totalPerc_noneffective", "Difficulty", "DRES", "Regulation")]
CorrMat_EA<-na.omit(CorrMat_EA)

M_EA<-cor(CorrMat_EA)

corrplot.mixed(M_EA)

res2_EA <- rcorr(as.matrix(CorrMat_EA))
res2_EA

CorrMat_Mid<-Data13_Mid[c("ProporaionalFmSdS_C", "F_totalPerc_noneffective", 
                        "S_totalPerc_noneffective", "Difficulty", "DRES", "Regulation")]
CorrMat_Mid<-na.omit(CorrMat_Mid)

M_Mid<-cor(CorrMat_Mid)

corrplot.mixed(M_Mid)

res2_Mid <- rcorr(as.matrix(CorrMat_Mid))
res2_Mid

CorrMat_Adult<-Data13_Adult[c("ProporaionalFmSdS_C", "F_totalPerc_noneffective", 
                          "S_totalPerc_noneffective", "Difficulty", "DRES", "Regulation")]
CorrMat_Adult<-na.omit(CorrMat_Adult)

M_Mid<-cor(CorrMat_Mid)

corrplot.mixed(M_Mid)

res2_Adult <- rcorr(as.matrix(CorrMat_Adult))
res2_Adult



####GROUP ANALYSES INCLUDING IQ AS COVARIATE

DifGroup<-lm(Difficulty ~ Group + WASItscore, data = Data13)
summary(DifGroup)

ACGroup<-lm(ProporaionalFmSdS_C ~ Group + WASItscore, data = Data13)
summary(ACGroup)

RegGroup<-lm(Regulation ~ Group +WASItscore, data = Data13)
summary(RegGroup)

RegSRGroup<-lm(DRES ~ Group +WASItscore, data = Data13)
summary(RegSRGroup)

####The mediation analyses including the covariate was run in PROCESS



####SUPPLEMENTAL ANALYSES WITH AGE AS A CONTINUOUS VARIABLE
DifGroup<-lm(Difficulty ~ Age, data = Data13)
summary(DifGroup)

ACGroup<-lm(ProporaionalFmSdS_C ~ Age, data = Data13)
summary(ACGroup)

RegGroup<-lm(Regulation ~ Age, data = Data13)
summary(RegGroup)

RegSRGroup<-lm(DRES ~ Age, data = Data13)
summary(RegSRGroup)

ACAge <-
  '# regressions
Difficulty           ~ c*Age 
ProporaionalFmSdS_C ~ a*Age  
Difficulty           ~ b*ProporaionalFmSdS_C

# indirect effect (a*b)
ab := a*b

#direct effect (c)

direct := c

# total effect
total := c + (a*b)'

fit <- sem(ACAge, data=Data13, std.lv=T, std.ov=T, 
           missing='fiml', se='robust', estimator='mlr', auto.var = T) # robust estimators, proper missing data method etc.

summary (fit, standardized=T, rsquare=T) # use standardized variables and request R2

fitMeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled",
                                  "rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                  "cfi.scaled","srmr","aic")) # request only most pertinent measures
# check http://davidakenny.net/cm/fit.htm to see whether model fits well
# let me know if it doesn't, because then we migth have to change strategy


###############################################################
###Addtional exploratory analyses based on reviewer comments
################################################################

ACGroup<-lm(ProporaionalFmSdS_RT ~ Group, data = Data13)
summary(ACGroup)
describeBy(Data13$ProporaionalFmSdS_RT, group = Data13$Group)
ACUni <- emmeans(ACGroup, "Group")
pairs(ACUni)

DiffCC<-lm(Difficulty ~ ProporaionalFmSdS_RT,data = Data13)
summary(DiffCC)
cor.test(Data13$Difficulty, Data13$ProporaionalFmSdS_RT)

Data13$Group<-as.numeric(Data13$Group)


SetShiftingGroup <-
  '# regressions
Difficulty           ~ c*Group 
ProporaionalFmSdS_RT ~ a*Group  
Difficulty           ~ b*ProporaionalFmSdS_RT

# indirect effect (a*b)
ab := a*b

#direct effect (c)

direct := c

# total effect
total := c + (a*b)'

fit <- sem(SetShiftingGroup, data=Data13, std.lv=T, std.ov=T, 
           missing='fiml', se='robust', estimator='mlr', auto.var = T) # robust estimators, proper missing data method etc.

summary (fit, standardized=T, rsquare=T) # use standardized variables and request R2

fitMeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled",
                                  "rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                  "cfi.scaled","srmr","aic")) # request only most pertinent measures
# check http://davidakenny.net/cm/fit.htm to see whether model fits well
# let me know if it doesn't, because then we migth have to change strategy

#semPaths(fit, title = FALSE,layout = "spring", whatLabels = "std", intercepts = FALSE, style = "ram", bg = "black")

RegCC<-lm(Regulation ~ ProporaionalFmSdS_RT, data = Data13)
summary(RegCC)
cor.test(Data13$Regulation, Data13$ProporaionalFmSdS_RT)

DERSCC<-lm(DRES ~ ProporaionalFmSdS_RT,data = Data13)
summary(DERSCC)
cor.test(Data13$DRES, Data13$ProporaionalFmSdS_RT)

DiffDERSAC <-
  '# regressions
Difficulty           ~ c*ProporaionalFmSdS_RT 
DRES                 ~ a*ProporaionalFmSdS_RT  
Difficulty           ~ b*DRES

# indirect effect (a*b)
ab := a*b

#direct effect (c)

direct := c

# total effect
total := c + (a*b)'

fit <- sem(DiffDERSAC, data=Data13, std.lv=T, std.ov=T, 
           missing='fiml', se='robust', estimator='mlr', auto.var = T) # robust estimators, proper missing data method etc.

summary (fit, standardized=T, rsquare=T) # use standardized variables and request R2

fitMeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled",
                                  "rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                  "cfi.scaled","srmr","aic")) # request only most pertinent measures
# check http://davidakenny.net/cm/fit.htm to see whether model fits well
# let me know if it doesn't, because then we migth have to change strategy
#semPaths(fit, title = FALSE,layout = "spring", whatLabels = "std", intercepts = FALSE, style = "ram")


###DERS reliability in each group
Data13_DERS<-subset(Data13,                      select=c(EmoReg_1,  EmoReg_2,  Emoreg_3,  EmoReg_4,  EmoReg_5,
                                                          EmoReg_6,  EmoReg_7,  EmoReg_8,  EmoReg_9,  EmoReg_10,
                                                          EmoReg_10, EmoReg_11, EmoReg_12, EmoReg_13, EmoReg_14, EmoReg_15,
                                                          EmoReg_16, EmoReg_17, EmoReg_18, EmoReg_19, EmoReg_20,
                                                          EmoReg_21, EmoReg_22, EmoReg_23, EmoReg_24, EmoReg_25,
                                                          EmoReg_26, EmoReg_27, EmoReg_28, EmoReg_29, EmoReg_30,
                                                          EmoReg_31, EmoReg_32, EmoReg_33, EmoReg_34, EmoReg_35, EmoReg_36))
Data13_EA_DERS<-subset(Data13_EA, select=c(EmoReg_1,  EmoReg_2,  Emoreg_3,  EmoReg_4,  EmoReg_5,
                                           EmoReg_6,  EmoReg_7,  EmoReg_8,  EmoReg_9,  EmoReg_10,
                                           EmoReg_10, EmoReg_11, EmoReg_12, EmoReg_13, EmoReg_14, EmoReg_15,
                                           EmoReg_16, EmoReg_17, EmoReg_18, EmoReg_19, EmoReg_20,
                                           EmoReg_21, EmoReg_22, EmoReg_23, EmoReg_24, EmoReg_25,
                                           EmoReg_26, EmoReg_27, EmoReg_28, EmoReg_29, EmoReg_30,
                                           EmoReg_31, EmoReg_32, EmoReg_33, EmoReg_34, EmoReg_35, EmoReg_36))
Data13_Mid_DERS<-subset(Data13_Mid, select=c(EmoReg_1,  EmoReg_2,  Emoreg_3,  EmoReg_4,  EmoReg_5,
                                             EmoReg_6,  EmoReg_7,  EmoReg_8,  EmoReg_9,  EmoReg_10,
                                             EmoReg_10, EmoReg_11, EmoReg_12, EmoReg_13, EmoReg_14, EmoReg_15,
                                             EmoReg_16, EmoReg_17, EmoReg_18, EmoReg_19, EmoReg_20,
                                             EmoReg_21, EmoReg_22, EmoReg_23, EmoReg_24, EmoReg_25,
                                             EmoReg_26, EmoReg_27, EmoReg_28, EmoReg_29, EmoReg_30,
                                             EmoReg_31, EmoReg_32, EmoReg_33, EmoReg_34, EmoReg_35, EmoReg_36))

Data13_Adult_DERS<-subset(Data13_Adult, select=c(EmoReg_1,  EmoReg_2,  Emoreg_3,  EmoReg_4,  EmoReg_5,
                                                 EmoReg_6,  EmoReg_7,  EmoReg_8,  EmoReg_9,  EmoReg_10,
                                                 EmoReg_10, EmoReg_11, EmoReg_12, EmoReg_13, EmoReg_14, EmoReg_15,
                                                 EmoReg_16, EmoReg_17, EmoReg_18, EmoReg_19, EmoReg_20,
                                                 EmoReg_21, EmoReg_22, EmoReg_23, EmoReg_24, EmoReg_25,
                                                 EmoReg_26, EmoReg_27, EmoReg_28, EmoReg_29, EmoReg_30,
                                                 EmoReg_31, EmoReg_32, EmoReg_33, EmoReg_34, EmoReg_35, EmoReg_36))

Data13_SDQ_Diff<-subset(Data13, select=c(SDQ_Q2, SDQ_Q3, SDQ_Q5, SDQ_Q6, SDQ_Q7, SDQ_Q8, SDQ_Q10,
                                         SDQ_Q11, SDQ_Q12, SDQ_Q13, SDQ_Q14, SDQ_Q15, SDQ_Q16, SDQ_Q18, SDQ_Q19,
                                         SDQ_Q21, SDQ_Q22, SDQ_Q24, SDQ_Q25))
Data13_EA_SDQ_Diff<-subset(Data13_EA, select=c(SDQ_Q2, SDQ_Q3, SDQ_Q5, SDQ_Q6, SDQ_Q7, SDQ_Q8, SDQ_Q10,
                                               SDQ_Q11, SDQ_Q12, SDQ_Q13, SDQ_Q14, SDQ_Q15, SDQ_Q16, SDQ_Q18, SDQ_Q19,
                                               SDQ_Q21, SDQ_Q22, SDQ_Q24, SDQ_Q25))

Data13_Mid_SDQ_Diff<-subset(Data13_Mid, select=c(SDQ_Q2, SDQ_Q3, SDQ_Q5, SDQ_Q6, SDQ_Q7, SDQ_Q8, SDQ_Q10,
                                                 SDQ_Q11, SDQ_Q12, SDQ_Q13, SDQ_Q14, SDQ_Q15, SDQ_Q16, SDQ_Q18, SDQ_Q19,
                                                 SDQ_Q21, SDQ_Q22, SDQ_Q24, SDQ_Q25))

Data13_Adult_SDQ_Diff<-subset(Data13_Adult, select=c(SDQ_Q2, SDQ_Q3, SDQ_Q5, SDQ_Q6, SDQ_Q7, SDQ_Q8, SDQ_Q10,
                                                     SDQ_Q11, SDQ_Q12, SDQ_Q13, SDQ_Q14, SDQ_Q15, SDQ_Q16, SDQ_Q18, SDQ_Q19,
                                                     SDQ_Q21, SDQ_Q22, SDQ_Q24, SDQ_Q25))


alpha(Data13_DERS)
alpha(Data13_SDQ_Diff,check.keys = TRUE)



