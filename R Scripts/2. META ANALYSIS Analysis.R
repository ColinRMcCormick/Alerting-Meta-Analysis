###############
#META-ANALYSIS#
###############
#Last edited November 12th, 2024 (CM)
#Associated with the manuscript ASSESSING POSNER’S THEORY OF ALERTING: A META-ANALYSIS
#OF SPEED-ACCURACY EFFECTS

#Required Packages
library(tidyverse)
library(metafor)
library(readODS)
library(tidyverse)

#LOAD PROVIDED TABLE HERE
setwd("/Users/colinmccormick/iCloud Drive (Archive) - 1/Documents/WORK/Research Projects/Dissertation Work/ChapterTwo_MetaAnalysis/ANALYSIS/TABLES:FIGURES")
x<-read.csv2("Alerting_MetaAnalysis_Table.csv", na.strings = c("#VALUE!", "NA", "Unavailable", "#NUM!"), sep=',')

#######################################
#RENAMING AND CONVERTING DATA FORMATS #
#######################################
#Renaming YI (effect) and VI (variance) for metafor package analysis
#yi = ER effects, yi_RT = RT effects, y_logis = log odds effects
#ER effects
x$yi<-x$ER_EFFECT_1_50_NoCue
x$yi_200<-x$ER_EFFECT_2_200_NoCue
x$yi_400<-x$ER_EFFECT_400
#RT effects
x$yi_RT<-x$RT_EFFECT_1_50_NoCue
x$yi_200RT<-x$RT_EFFECT_2_200_NoCue
x$yi_400RT<-x$RT_EFFECT_400
#Log Odd effects
x$yi_logis50<-x$LOGIT_EFFECT_50
x$yi_logis200<-x$LOGIT_EFFECT_200
x$yi_logis400<-x$LOGIT_EFFECT_400

#Effect Variance for ER, logis, and RT.
x <- x %>%
  mutate(
    vi = (as.numeric(ER_EFFECT_1_SD_50_NoCue)^2) / N,
    vi_200 = (as.numeric(ER_EFFECT_2_SD_200_NoCue)^2) / N,
    vi_400 = (as.numeric(ER_400_EFFECT_SD)^2) / N,
    vi_logis50 = (as.numeric(logit_effect_50SD)^2) / N,
    vi_logis200 = (as.numeric(logit_effect_200SD)^2) / N,
    vi_logis400 = (as.numeric(logit_effect_400SD)^2) / N,
    vi_RT = (as.numeric(RT_EFFECT_1_SD_50_NoCue)^2)/N,
    vi_200RT = (as.numeric(RT_EFFECT_2_SD_200_NoCue)^2)/N,
    vi_400RT = (as.numeric(RT_EFFECT_400_SD)^2)/N
  )

#CONFIRMING ALL EFFECTS ARE NUMERIC
x<-mutate(x,
          yi = as.numeric(yi)
          ,yi_200 = as.numeric(yi_200)
          ,yi_400 = as.numeric(yi_400)
          ,yi_logis50=as.numeric(yi_logis50)
          ,yi_logis200=as.numeric(yi_logis200)
          ,yi_logis400=as.numeric(yi_logis400)
          ,yi_RT = as.numeric(yi_RT)
          ,yi_200RT = as.numeric(yi_200RT)
          ,yi_400RT = as.numeric(yi_400RT)
          ,vi = as.numeric(vi)
          ,vi_200 = as.numeric(vi_200)
          ,vi_400 = as.numeric(vi_400)
          ,vi_RT = as.numeric(vi_RT)
          ,vi_200RT = as.numeric(vi_200RT)
          ,vi_400RT = as.numeric(vi_400RT)
)


###########
#Moderating Factors
###########
x$Block_Foreperiod_Duration<-as.factor(x$Block_Foreperiod_Duration)
x$SIGNAL_MODALITY<-as.factor(x$SIGNAL_MODALITY)
x$FEEDBACK<-as.factor(x$FEEDBACK)
x$TPC<-as.numeric(x$TPC)
x$STUDY_num<-1:16
#Create three separate bins for each foreperiod that only includes the relevant
#studies
x %>% drop_na(yi) -> x_50
x %>% drop_na(yi_200) -> x_200
x %>% drop_na(yi_400) -> x_400

#####
#Generate models with moderators based on block structure (fixed or intermixed FPs),
#signal modality (aud or vis)  RT feedback (Yes or No), and TPC
#This is used to observe differences in hetero
####

#######################
#Error Rate mod models#
#######################
#50msec (v no signal) Foreperiod Models
model_50mod<-rma.uni(data = x_50, yi=yi, vi=vi, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK + TPC )
model_50mod_dTPC<-rma.uni(data = x_50, yi=yi, vi=vi, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK )
model_50mod_dFB<-rma.uni(data = x_50, yi=yi, vi=vi, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC )
model_50mod_dBlo<-rma.uni(data = x_50, yi=yi, vi=vi, method = 'REML', mods = ~  FEEDBACK + TPC )

anova.rma(model_50mod, model_50mod_dTPC, refit = TRUE)
model_50mod$I2 - model_50mod_dTPC$I2

anova.rma(model_50mod, model_50mod_dFB, refit = TRUE)
model_50mod$I2 - model_50mod_dFB$I2

anova(model_50mod, model_50mod_dBlo, refit = TRUE)
model_50mod$I2 - model_50mod_dBlo$I2

#200msec (v no signal) Foreperiod Models
model_200mod<-rma.uni(data = x_200, yi=yi_200, vi=vi_200, method = 'REML', mods = ~ Block_Foreperiod_Duration + SIGNAL_MODALITY + FEEDBACK + TPC )
model_200mod_dTPC<-rma.uni(data = x_200, yi=yi_200, vi=vi_200, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK +SIGNAL_MODALITY)
model_200mod_dFB<-rma.uni(data = x_200, yi=yi_200, vi=vi_200, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC + SIGNAL_MODALITY)
model_200mod_dBlo<-rma.uni(data = x_200, yi=yi_200, vi=vi_200, method = 'REML', mods = ~  FEEDBACK + TPC + SIGNAL_MODALITY)
model_200mod_dSig<-rma.uni(data = x_200, yi=yi_200, vi=vi_200, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK + TPC )

anova.rma(model_200mod, model_200mod_dTPC, refit = TRUE)
model_200mod$I2 - model_200mod_dTPC$I2

anova.rma(model_200mod, model_200mod_dFB, refit = TRUE)
model_200mod$I2 - model_200mod_dFB$I2

anova(model_200mod, model_200mod_dBlo, refit = TRUE)
model_200mod$I2 - model_200mod_dBlo$I2

anova(model_200mod, model_200mod_dSig, refit = TRUE)
model_200mod$I2 - model_200mod_dSig$I2

#400msec (v no signal) Foreperiod Models
model_400mod<-rma.uni(data = x_400, yi=yi_400, vi=vi_400, method = 'REML', mods = ~  Block_Foreperiod_Duration + SIGNAL_MODALITY + FEEDBACK  + TPC )
model_400mod_dTPC<-rma.uni(data = x_400, yi=yi_400, vi=vi_400,method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK +SIGNAL_MODALITY)
model_400mod_dFB<-rma.uni(data = x_400, yi=yi_400, vi=vi_400, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC + SIGNAL_MODALITY)
model_400mod_dBlo<-rma.uni(data = x_400, yi=yi_400, vi=vi_400, method = 'REML', mods = ~  FEEDBACK + TPC + SIGNAL_MODALITY)
model_400mod_dSig<-rma.uni(data = x_400, yi=yi_400, vi=vi_400, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK + TPC )

anova.rma(model_400mod, model_400mod_dFB, refit = TRUE)
model_400mod$I2 - model_400mod_dFB$I2

anova(model_400mod, model_400mod_dBlo, refit = TRUE)
model_400mod$I2 - model_400mod_dBlo$I2

anova(model_400mod, model_400mod_dSig, refit = TRUE)
model_400mod$I2 - model_400mod_dSig$I2

anova.rma(model_400mod, model_400mod_dTPC, refit = TRUE)
model_400mod$I2 - model_400mod_dTPC$I2

###############
#Logit models:#
###############

#50
model_logis50mod<-rma.uni(data = x_50, yi=yi_logis50, vi=vi_logis50, method = 'REML', mods = ~  Block_Foreperiod_Duration + FEEDBACK + TPC )
model_logis50mod_dTPC<-rma.uni(data = x_50, yi=yi_logis50, vi=vi_logis50, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK )
model_logis50mod_dFB<-rma.uni(data = x_50, yi=yi_logis50, vi=vi_logis50, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC )
model_logis50mod_dBlo<-rma.uni(data = x_50, yi=yi_logis50, vi=vi_logis50, method = 'REML', mods = ~  FEEDBACK + TPC )

anova.rma(model_logis50mod, model_logis50mod_dTPC, refit = TRUE)
model_logis50mod$I2 - model_logis50mod_dTPC$I2

anova.rma(model_logis50mod, model_logis50mod_dFB, refit = TRUE)
model_logis50mod$I2 - model_logis50mod_dFB$I2

anova.rma(model_logis50mod, model_logis50mod_dBlo, refit = TRUE)
model_logis50mod$I2 - model_logis50mod_dBlo$I2

#200
model_logis200mod<-rma.uni(data = x_200, yi=yi_logis200, vi=vi_logis200, method = 'REML', mods = ~ Block_Foreperiod_Duration + SIGNAL_MODALITY + FEEDBACK + TPC )
model_logis200mod_dTPC<-rma.uni(data = x_200, yi=yi_logis200, vi=vi_logis200, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK +SIGNAL_MODALITY)
model_logis200mod_dFB<-rma.uni(data = x_200, yi=yi_logis200, vi=vi_logis200, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC+SIGNAL_MODALITY )
model_logis200mod_dBlo<-rma.uni(data = x_200, yi=yi_logis200, vi=vi_logis200, method = 'REML', mods = ~  FEEDBACK + TPC+ SIGNAL_MODALITY )
model_logis200mod_dSig<-rma.uni(data = x_200, yi=yi_logis200, vi=vi_logis200, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK + TPC )

anova.rma(model_logis200mod, model_logis200mod_dTPC, refit = TRUE)
model_logis200mod$I2 - model_logis200mod_dTPC$I2

anova.rma(model_logis200mod, model_logis200mod_dFB, refit = TRUE)
model_logis200mod$I2 - model_logis200mod_dFB$I2

anova(model_logis200mod, model_logis200mod_dBlo, refit = TRUE)
model_logis200mod$I2 - model_logis200mod_dBlo$I2

anova(model_logis200mod, model_logis200mod_dSig, refit = TRUE)
model_logis200mod$I2 - model_logis200mod_dSig$I2

#400
model_logis400mod<-rma.uni(data = x_400, yi=yi_logis400, vi=vi_logis400, method = 'REML', mods = ~ Block_Foreperiod_Duration + SIGNAL_MODALITY+ FEEDBACK + TPC )
model_logis400mod_dTPC<-rma.uni(data = x_400, yi=yi_logis400, vi=vi_logis400, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK +SIGNAL_MODALITY )
model_logis400mod_dFB<-rma.uni(data = x_400, yi=yi_logis400, vi=vi_logis400, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC +SIGNAL_MODALITY )
model_logis400mod_dBlo<-rma.uni(data = x_400, yi=yi_logis400, vi=vi_logis400, method = 'REML', mods = ~  FEEDBACK + TPC + SIGNAL_MODALITY)
model_logis400mod_dSig<-rma.uni(data = x_400, yi=yi_logis400, vi=vi_logis400, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK + TPC )

anova.rma(model_logis400mod, model_logis400mod_dTPC, refit = TRUE)
model_logis400mod$I2 - model_logis400mod_dTPC$I2

anova.rma(model_logis400mod, model_logis400mod_dFB, refit = TRUE)
model_logis400mod$I2 - model_logis400mod_dFB$I2

anova(model_logis400mod, model_logis400mod_dBlo, refit = TRUE)
model_logis400mod$I2 - model_logis400mod_dBlo$I2

anova(model_logis400mod, model_logis400mod_dSig, refit = TRUE)
model_logis400mod$I2 - model_logis400mod_dSig$I2

###########
#RT MODELS#
###########
#50
model_50RTmod<-rma.uni(data = x_50, yi=yi_RT, vi=vi_RT, method = 'REML', mods = ~  Block_Foreperiod_Duration+ FEEDBACK +TPC )
model_50RTmod_dTPC<-rma.uni(data = x_50, yi=yi_RT, vi=vi_RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK )
model_50RTmod_dFB<-rma.uni(data = x_50, yi=yi_RT, vi=vi_RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC )
model_50RTmod_dBlo<-rma.uni(data = x_50, yi=yi_RT, vi=vi_RT, method = 'REML', mods = ~  FEEDBACK + TPC )

anova.rma(model_50RTmod, model_50RTmod_dTPC, refit = TRUE)
model_50RTmod$I2 - model_50RTmod_dTPC$I2

anova.rma(model_50RTmod, model_50RTmod_dFB, refit = TRUE)
model_50RTmod$I2 - model_50RTmod_dFB$I2

anova.rma(model_50RTmod, model_50RTmod_dBlo, refit = TRUE)
model_50RTmod$I2 - model_50RTmod_dBlo$I2

#200
model_200RTmod<-rma.uni(data = x_200, yi=yi_200RT, vi=vi_200RT, method = 'REML', mods = ~  Block_Foreperiod_Duration + SIGNAL_MODALITY+ FEEDBACK + TPC )
model_200RTmod_dTPC<-rma.uni(data = x_200, yi=yi_200RT, vi=vi_200RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK +SIGNAL_MODALITY)
model_200RTmod_dFB<-rma.uni(data = x_200, yi=yi_200RT, vi=vi_200RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC + SIGNAL_MODALITY)
model_200RTmod_dBlo<-rma.uni(data = x_200, yi=yi_200RT, vi=vi_200RT, method = 'REML', mods = ~  FEEDBACK + TPC + SIGNAL_MODALITY)
model_200RTmod_dSig<-rma.uni(data = x_200, yi=yi_200RT, vi=vi_200RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK + TPC )

anova.rma(model_200RTmod, model_200RTmod_dTPC, refit = TRUE)
model_200RTmod$I2 - model_200RTmod_dTPC$I2

anova.rma(model_200RTmod, model_200RTmod_dFB, refit = TRUE)
model_200RTmod$I2 - model_200RTmod_dFB$I2

anova(model_200RTmod, model_200RTmod_dBlo, refit = TRUE)
model_200RTmod$I2 - model_200RTmod_dBlo$I2

anova(model_200RTmod, model_200RTmod_dSig, refit = TRUE)
model_200RTmod$I2 - model_200RTmod_dSig$I2

#400
model_altRTmod<-rma.uni(data = x_400, yi=yi_400RT, vi=vi_400RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + SIGNAL_MODALITY+ FEEDBACK + TPC )
model_400mod_dTPC<-rma.uni(data = x_400, yi=yi_400RT, vi=vi_400RT,method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK +SIGNAL_MODALITY)
model_400mod_dFB<-rma.uni(data = x_400, yi=yi_400RT, vi=vi_400RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + TPC + SIGNAL_MODALITY)
model_400mod_dBlo<-rma.uni(data = x_400, yi=yi_400RT, vi=vi_400RT, method = 'REML', mods = ~  FEEDBACK + TPC + SIGNAL_MODALITY)
model_400mod_dSig<-rma.uni(data = x_400, yi=yi_400RT, vi=vi_400RT, method = 'REML', mods = ~ Block_Foreperiod_Duration + FEEDBACK + TPC )

anova.rma(model_altRTmod, model_400mod_dFB, refit = TRUE)
model_altRTmod$I2 - model_400mod_dFB$I2

anova(model_altRTmod, model_400mod_dBlo, refit = TRUE)
model_altRTmod$I2 - model_400mod_dBlo$I2

anova(model_altRTmod, model_400mod_dSig, refit = TRUE)
model_altRTmod$I2 - model_400mod_dSig$I2

anova.rma(model_altRTmod, model_400mod_dTPC, refit = TRUE)
model_altRTmod$I2 - model_400mod_dTPC$I2

####
#Separately, run models without moderator parameters to use RE model in the overall plots
####
#Error Rate models
model_50<-rma.uni(data = x_50, yi=yi, vi=vi, method = 'REML')
model_200<-rma.uni(data = x_200, yi=yi_200, vi=vi_200, method = 'REML')
model_400<-rma.uni(data = x_400, yi=yi_400, vi=vi_400, method = 'REML')
#Logit models:
model_logis50<-rma.uni(data = x_50, yi=yi_logis50, vi=vi_logis50, method = 'REML')
model_logis200<-rma.uni(data = x_200, yi=yi_logis200, vi=vi_logis200, method = 'REML')
model_logis400<-rma.uni(data = x_400, yi=yi_logis400, vi=vi_logis400, method = 'REML')
#RT models
model_50RT<-rma.uni(data = x_50, yi=yi_RT, vi=vi_RT, method = 'REML')
model_200RT<-rma.uni(data = x_200, yi=yi_200RT, vi=vi_200RT, method = 'REML')
model_altRT<-rma.uni(data = x_400, yi=yi_400RT, vi=vi_400RT, method = 'REML')


##############
#FUNNEL PLOTS#
##############
#ERROR RATE FUNNELS
fun1<-funnel(model_50mod, label = TRUE, slab= x_50$Lab, font = 1, cex=1.2, main= 'Error Rate Effects at 50 msec')
fun4<-funnel(model_200mod, label = TRUE, slab= x_200$Lab,   font = 1, cex=1.2, main= 'Error Rate Effects at 200 msec' )
fun7<-funnel(model_400mod, label = TRUE, slab= x_400$Lab,  font = 1, cex=1.2, main= 'Error Rate Effects at 400 msec ')

#REACTION TIME FUNNELS
fun2<-funnel(model_50RTmod, label = TRUE,slab= x_50$STUDY_Short,  font = 1,  cex=1.2, main= 'Reaction Time Effects at 50 msec')
fun5<-funnel(model_200RTmod, label = TRUE,  slab= x_200$Lab, font = 1, cex=1.2, main= 'Reaction Time Effects at 200 msec')
fun8<-funnel(model_altRTmod, label = TRUE,slab= x_400$Lab, legend = FALSE, font = 1, cex=1.2, main= 'Reaction Time Effects at 400 msec')

#LOGIS FUNNELS
fun3<-funnel(model_logis50mod, label = TRUE,slab= x_50$Lab, font = 1, cex=1.2, main= 'Log Odd Effects at 50 msec')
fun6<-funnel(model_logis200mod, label =TRUE, slab= x_200$Lab, font = 1, cex=1.2, main= 'Log Odd Effects at 200 msec')
fun9<-funnel(model_logis400mod, label = TRUE, slab= x_400$Lab, font = 1, cex=1.2, main= 'Log Odd Effects at 400 msec')

##############
#FOREST PLOTS#
##############
#Creating nicely-formatted labels
slab50<-c("Posner et al., 1973", "Han & Proctor, 2022 [No Feedback]", "Han & Proctor, 2022 [Feedback]", "McCormick et al., 2019, Intense Signal", "McCormick et al., 2019, Isointense Signal",  "Han & Proctor, 2023")
slab200<-c("Posner et al., 1973", "Han & Proctor, 2022 [No Feedback]", "Han & Proctor, 2022 [Feedback]", "McCormick et al., 2019, Intense Signal", "McCormick et al., 2019, Isointense Signal", "Dietze, Recker, & Poth, 2023", 'Dietze & Poth, 2023 [Visual Signal]', 'Dietze & Poth, 2023 [Auditory Signal]', 'Kazen-Saad, 1983 [E1]', 'Kazen-Saad, 1983 [E2]', 'He et al., 2020 [Older Sample]', 'He et al., 2020 [Younger Sample]', "Han & Proctor, 2023")
slab400<-c("Posner et al., 1973", "Han & Proctor, 2022 [No Feedback]", "Han & Proctor, 2022 [Feedback]", "Dietze, Recker, & Poth, 2023", 'Dietze & Poth, 2023 [Visual Signal]', 'Dietze & Poth, 2023 [Auditory Signal]', 'Kazen-Saad, 1983 [E1]', 'Kazen-Saad, 1983 [E2]', "Dietze & Poth, 2022 [E1]","Dietze & Poth, 2022 [E2]","Dietze & Poth, 2022 [E3]")

#########################
#ERROR RATE FOREST PLOTS#
#########################
forest(model_50mod,  cex=.8, order = x_50$yi, header = TRUE, slab= slab50, main= 'Error Rate Effect at 50 msec', xlim = c(-20,22), at=seq(-6,18,2), ylim = c(-2,9), addfit = FALSE)
sav1 <- predict(model_50)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_50mod, newmods = colMeans(model.matrix(model_50mod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_50mod)

forest(model_200mod, header = TRUE,  cex=.8, order = x_200$yi_200,  slab= slab200, main= 'Error Rate Effect at 200 msec', xlim = c(-20,22), at=seq(-6,18,2), ylim = c(-2,16), addfit=FALSE)
sav1 <- predict(model_200)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_200mod, newmods = colMeans(model.matrix(model_200mod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_200mod)

forest(model_400mod,  cex=.8, header = TRUE, slab= slab400, order = x_400$yi_400, main= 'Error Rate Effect at 400 msec ', xlim = c(-20,22), at=seq(-6,18,2), ylim = c(-2,14), addfit=FALSE)
sav1 <- predict(model_400)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_400mod, newmods = colMeans(model.matrix(model_400mod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_400mod)

############################
#REACTION TIME FOREST PLOTS#
############################
forest(model_50RTmod, digits = 0, cex=.8, order = x_50$yi_RT, header = TRUE, slab= slab50, main= 'Reaction Time Effect at 50 msec',xlim = c(-160,60), at=seq(-100,20,40), ylim = c(-2,9), addfit = FALSE)
sav1 <- predict(model_50RT)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_50RTmod, newmods = colMeans(model.matrix(model_50RTmod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_50RTmod)

forest(model_200RTmod, digits = 0, header = TRUE, cex=.8, order = x_200$yi_200RT,slab= slab200, main= 'Reaction Time Effect at 200 msec',xlim = c(-200,60), at=seq(-140,20,40), ylim = c(-2,16), addfit = FALSE)
sav1 <- predict(model_200RT)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_200RTmod, newmods = colMeans(model.matrix(model_200RTmod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_200RTmod)

forest(model_altRTmod,  digits = 0, cex=.8,header = TRUE, order = x_400$yi_400RT, slab= slab400, main= 'Reaction Time Effect at 400 msec',xlim = c(-160,60), at=seq(-100,30,40), ylim = c(-2,14), addfit = FALSE)
sav1 <- predict(model_altRT)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_altRTmod, newmods = colMeans(model.matrix(model_altRTmod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_altRTmod)

#######################
#LOG ODD FOREST PLOTS#
######################
forest(model_logis50mod, header = TRUE, order = x_50$yi_logis50, cex=.8, slab= slab50, main= 'Log Odd Effects at 50 msec', xlim = c(-6,5),ylim = c(-2,9), at=seq(-2,4,.5), addfit = FALSE)
sav1 <- predict(model_logis50)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_logis50mod, newmods = colMeans(model.matrix(model_logis50mod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_logis50mod)

forest(model_logis200mod,header = TRUE, cex=.8,order = x_200$yi_logis200, slab= slab200, main= 'Log Odd Effects at 200 msec', xlim = c(-6,5), at=seq(-2,4,.5),ylim = c(-2,16), addfit = FALSE)
sav1 <- predict(model_logis200)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_logis200mod, newmods = colMeans(model.matrix(model_logis200mod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_logis200mod)

forest(model_logis400mod,  cex=.8, order = x_400$yi_logis400, header = TRUE,
       slab= slab400, main= 'Log Odd Effect at 400 msec', xlim = c(-8,7), at=seq(-2,4,.5),
       ylim = c(-2,14), addfit = FALSE)
sav1 <- predict(model_logis400)
addpoly(sav1,mlab="Random-Effects Model", row = -1)
sav2<-predict(model_logis400mod, newmods = colMeans(model.matrix(model_logis400mod))[-1], digits=2)
addpoly.predict.rma(sav2, row=-2, mlab="Meta-Regression Model (Adjusted Effect)")
confint(model_logis400mod)

#NO MODERATOR FUNNELS
#ERROR RATE FUNNELS
fun1<-funnel(model_50, label = TRUE, slab= x_50$STUDY_Short, font = 1, cex=.5, main= 'Error Rate Effects at 50 msec')
fun4<-funnel(model_200, label = TRUE, slab= x_200$STUDY_Short,   font = 1, cex=.5, main= 'Error Rate Effects at 200 msec' )
fun7<-funnel(model_400, label = TRUE, slab= x_400$STUDY_Short,  font = 1, cex=.5, main= 'Error Rate Effects at 400 msec ')

#REACTION TIME FUNNELS
fun2<-funnel(model_50RT, label = TRUE,slab= x_50$STUDY_Short,  font = 1,  cex=.5, main= 'Reaction Time Effects at 50 msec')
fun5<-funnel(model_200RT, label = TRUE,  slab= x_200$STUDY_Short, font = 1, cex=.5, main= 'Reaction Time Effects at 200 msec')
fun8<-funnel(model_altRT, label = TRUE,slab= x_400$STUDY_Short, legend = FALSE, font = 1, cex=.5, main= 'Reaction Time Effects at 400 msec')

#LOGIS FUNNELS
fun3<-funnel(model_logis50, label = TRUE,slab= x_50$STUDY_Short, font = 1, cex=.5, main= 'Log Odd Effects at 50 msec')
fun6<-funnel(model_logis200, label =TRUE, slab= x_200$STUDY_Short, font = 1, cex=.5, main= 'Log Odd Effects at 200 msec')
fun9<-funnel(model_logis400, label = TRUE, slab= x_400$STUDY_Short, font = 1, cex=.5, main= 'Log Odd Effects at 400 msec')




