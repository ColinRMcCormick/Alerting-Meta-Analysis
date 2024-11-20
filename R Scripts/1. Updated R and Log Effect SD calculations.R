###################################
#####     DATA LOAD-IN       ######
# CALCULATING LOG EFFECT VARIANCE##
###################################
#Associated with the manuscript ASSESSING POSNER’S THEORY OF ALERTING: A META-ANALYSIS
#OF SPEED-ACCURACY EFFECTS

#DESCRIPTION:
#This script changes the data type of the columns from the excel file, as well as
#uses some theoretical equations to generate effect SDs, etc, when these values were
#not available in the paper/when the raw-data was unavailable.
#Last edited November 13th (CM)

#Required packages:
library(dplyr)
#SETWD##
setwd("/Users/colinmccormick/iCloud Drive (Archive) - 1/Documents/WORK/Research Projects/Dissertation Work/ChapterTwo_MetaAnalysis/ANALYSIS/TABLES:FIGURES")

##LOAD IN DATA##
#Load in the meta-analysis table, define NA (otherwise it's coded as zeros)
x<-read.csv2("Meta_analysis_2024_finalApril10csv.csv", na.strings = c("#VALUE!", "NA", "Unavailable", "#NUM!"), sep=',')

#SET UP DATAFRAME#
x<-mutate(x,
          ER_EFFECT_1_SD_50_NoCue = as.numeric(ER_EFFECT_1_SD_50_NoCue)
          ,ER_EFFECT_2_SD_200_NoCue = as.numeric(ER_EFFECT_2_SD_200_NoCue)
          ,ER_400_EFFECT_SD = as.numeric(ER_400_EFFECT_SD)
          ,LOGIT_EFFECT_50_SD=as.numeric(LOGIT_EFFECT_50_SD)
          ,LOGIT_EFFECT_200_SD=as.numeric(LOGIT_EFFECT_200_SD)
          ,LOGIT_EFFECT_400_SD=as.numeric(LOGIT_EFFECT_400_SD)
          ,RT_EFFECT_1_SD_50_NoCue=as.numeric(RT_EFFECT_1_SD_50_NoCue)
          ,RT_EFFECT_2_SD_200_NoCue=as.numeric(RT_EFFECT_2_SD_200_NoCue)
          ,RT_EFFECT_400_SD=as.numeric(RT_EFFECT_400_SD)
          ,NO_TONE_Error_SD=as.numeric(NO_TONE_Error_SD)
          ,TONE_50_Error_SD=as.numeric(TONE_50_Error_SD)
          ,TONE_200_Error_SD=as.numeric(TONE_200_Error_SD)
          ,TONE_400_Error_SD=as.numeric(TONE_400_Error_SD)
          ,ER_EFFECT_1_SD_50_NoCue=as.numeric(ER_EFFECT_1_SD_50_NoCue)
          ,ER_EFFECT_2_SD_200_NoCue=as.numeric(ER_EFFECT_2_SD_200_NoCue)
          ,ER_400_EFFECT_SD=as.numeric(ER_400_EFFECT_SD)
          ,TPC=as.numeric(TPC))

############################################
#Create Functions and Important Variables#
############################################
#THEORETICAL VAR of each condition
ThVAR<-function(p,n) p*(1-p)/n
#Theoretical variance for logit, based on simulations
var_logit<-3.29 #3.29/n for conditions (TPC)

#Since: VarCond1 + VarCond2 - 2*COR(VarCond1&2) = EffVar
#SDx is the standard deviation of the condition mean, ESD is effect standard deviation
correlation <- function(SDone, SDtwo, ESD) {
  CoVar <- -(ESD^2 - SDone^2 - SDtwo^2)/2
  r <- CoVar /(SDone * SDtwo)
  return(r)
}

#Correlation and trials per condition
LogEffectSD <- function(correlation, TPC) {
  logeffvar<-((var_logit/TPC)*2) - (2*correlation*(var_logit/TPC))
  logESD<-sqrt(logeffvar)
  return(logESD)
}

###Calculating missing COV/COR for effect variance###
#adding correlations to the dataframe
x$r_50 <- mapply(correlation, x$NO_TONE_Error_SD, x$TONE_50_Error_SD, x$ER_EFFECT_1_SD_50_NoCue)
x$r_200 <- mapply(correlation, x$NO_TONE_Error_SD, x$TONE_200_Error_SD, x$ER_EFFECT_2_SD_200_NoCue)
x$r_400 <- mapply(correlation, x$NO_TONE_Error_SD, x$TONE_400_Error_SD, x$ER_400_EFFECT_SD)
#Value for Leonhard et al studies
x$r_50[9:13]<-.2
x$r_200[9:13]<-.2
x$r_400[9:13]<-.2

#Adding logit effect SD to the data frame
x$logit_effect_50SD <- mapply(LogEffectSD, x$r_50, x$TPC)
x$logit_effect_200SD <- mapply(LogEffectSD, x$r_200, x$TPC)
x$logit_effect_400SD <- mapply(LogEffectSD, x$r_400, x$TPC)
#delete old column of logit SD values (not needed due to new calculation in script)
x <- x[, -49]
x <- x[, -47]
x <- x[, -45]

###################################
#Experiments without condition SDs#
###################################

#PAPER:
#POSNER et al, 1973#
#########POSNER 50################
ERE1<-11.6/100 #(tone)
ERE2<-5.2/100 #(No tone)
ESD<-6.87/100
EVAR<-ESD^2
TPC<-40
#THEORETICAL VAR of each condition
S21<-ThVAR(ERE1, TPC)
S22<-ThVAR(ERE2, TPC)
#Find covaraince bt cond1 and 2, rearrange:
#S21 + S22 - 2*COV(s21 + S22) = EVar

#COVAR
covar<--(EVAR - S21 - S22)/2
#cor = r
r = covar/sqrt(S21 * S22)
x$r_50[1]<-r

#FINAL EQUATION IS BASED ON ABOVE EQUATION
Var_of_EffLog<-(var_logit/TPC)*2 - 2*r*(var_logit/TPC) #logit effect variance
value_adding<-sqrt(Var_of_EffLog)
#Add to analysis table
x$logit_effect_50SD[1]<-value_adding

#########POSNER 200################
ERE1<-11.1/100 #(tone)
ERE2<-5.2/100 #(No tone)
ESD<-6.34/100
EVAR<-ESD^2
TPC<-40

#THEORETICAL VAR of each condition
S21<-ThVAR(ERE1, TPC)
S22<-ThVAR(ERE2, TPC)

#COVAR
covar<--(EVAR - S21 - S22)/2

#cor = r
r = covar/sqrt(S21 * S22)
x$r_200[1]<-r

#FINAL EQUATION IS BASED ON ABOVE EQUATION
Var_of_EffLog<-(var_logit/TPC)*2 - 2*r*(var_logit/TPC) #logit effect variance
value_adding<-sqrt(Var_of_EffLog)
#Add to analysis table
x$logit_effect_200SD[1]<-value_adding

#########POSNER 400################
ERE1<-9.8/100 #(tone)
ERE2<-5.2/100 #(No tone)
ESD<-4.94/100
EVAR<-ESD^2
TPC<-40

#THEORETICAL VAR of each condition
S21<-ThVAR(ERE1, TPC)
S22<-ThVAR(ERE2, TPC)
#Find covaraince bt cond1 and 2

#COVAR
covar<--(EVAR - S21 - S22)/2

#cor = r
r = covar/sqrt(S21 * S22)
x$r_400[1]<-r

#FINAL EQUATION IS BASED ON ABOVE EQUATION
Var_of_EffLog<-(var_logit/TPC)*2 - 2*r*(var_logit/TPC) #logit effect variance
value_adding<-(Var_of_EffLog)
#Add to analysis table
x$logit_effect_400SD[1]<-value_adding


#########Kazen-Saad E1 200################
ERE1<-1.2/100 #(tone)
ERE2<-1.2/100 #(No tone)
ESD<-6.34/100
EVAR<-ESD^2
TPC<-60

#THEORETICAL VAR of each condition
S21<-ThVAR(ERE1, TPC)
S22<-ThVAR(ERE2, TPC)


#Find covaraince bt cond1 and 2
#COVAR
covar<--(EVAR - S21 - S22)/2
#cor = r
r = covar/sqrt(S21 * S22)
x$r_200[14]<-r
#FINAL EQUATION IS BASED ON ABOVE EQUATION
#S21 + S22 - 2*COV(s21 + S22) = EVar
Var_of_EffLog<-(var_logit/TPC)*2 - 2*r*(var_logit/TPC) #logit effect variance
value_adding<-sqrt(Var_of_EffLog)
#Add to table
x$logit_effect_200SD[14]<-value_adding

#########Kazen-Saad E1400################
ERE1<-2.2/100 #(tone)
ERE2<-1.2/100 #(No tone)
ESD<-4.94/100
EVAR<-ESD^2
TPC<-60

#THEORETICAL VAR of each condition
S21<-ThVAR(ERE1, TPC)
S22<-ThVAR(ERE2, TPC)
#Find covaraince bt cond1 and 2
#S21 + S22 - 2*COV(s21 + S22) = EVar

#COVAR
covar<--(EVAR - S21 - S22)/2
#cor = r
r = covar/sqrt(S21 * S22)
#FINAL EQUATION IS BASED ON ABOVE EQUATION
#S21 + S22 - 2*COV(s21 + S22) = EVar
Var_of_EffLog<-(var_logit/TPC)*2 - 2*r*(var_logit/TPC) #logit effect variance
value_adding<-sqrt(Var_of_EffLog)
#Add to analysis table
x$logit_effect_400SD[14]<-value_adding


#########Kazen-Saad E2 200################
ERE1<-.4/100 #(tone)
ERE2<-1/100 #(No tone)
ESD<-6.34/100
EVAR<-ESD^2
TPC<-60

#THEORETICAL VAR of each condition
S21<-ThVAR(ERE1, TPC)
S22<-ThVAR(ERE2, TPC)
#Find covaraince bt cond1 and 2

#S21 + S22 - 2*COV(s21 + S22) = EVar
#COVAR
covar<--(EVAR - S21 - S22)/2
#cor = r
r = covar/sqrt(S21 * S22)
x$r_200[15]<-r
#FINAL EQUATION IS BASED ON ABOVE EQUATION
#S21 + S22 - 2*COV(s21 + S22) = EVar
Var_of_EffLog<-(var_logit/TPC)*2 - 2*r*(var_logit/TPC) #logit effect variance
value_adding<-sqrt(Var_of_EffLog)
#Add to analysis table
x$logit_effect_200SD[15]<-value_adding


#########Kazen-Saad E2 400################
ERE1<-.3/100 #(tone)
ERE2<-1/100 #(No tone)
ESD<-4.94/100
EVAR<-ESD^2
TPC<-60

#THEORETICAL VAR of each condition
S21<-ThVAR(ERE1, TPC)
S22<-ThVAR(ERE2, TPC)
#Find covaraince bt cond1 and 2
#S21 + S22 - 2*COV(s21 + S22) = EVar

#COVAR
covar<--(EVAR - S21 - S22)/2
#cor = r
r = covar/sqrt(S21 * S22)
x$r_400[15]<-r
#FINAL EQUATION IS BASED ON ABOVE EQUATION
#S21 + S22 - 2*COV(s21 + S22) = EVar
Var_of_EffLog<-(var_logit/TPC)*2 - 2*r*(var_logit/TPC) #logit effect variance
value_adding<-sqrt(Var_of_EffLog)
#Add to analysis table
x$logit_effect_400SD[15]<-value_adding

################################################
#Dietze and Poth Manual log SD calculation (200 and 400 SOAs had different TPCs)
#########Dietze & Poth 200 400 vis################
x[7,]$logit_effect_200SD<- mapply(LogEffectSD, x[7,]$r_200, 424)
x[7,]$logit_effect_400SD <- mapply(LogEffectSD, x[7,]$r_400, 145)
#########Dietze & Poth 200 400 aud################
x[8,]$logit_effect_200SD<- mapply(LogEffectSD, x[8,]$r_200, 424)
x[8,]$logit_effect_400SD <- mapply(LogEffectSD, x[8,]$r_400, 145)

#Remove any columns that used a GG correction (can't use data properly)
x<-subset(x, GG_correction == 'N')

##################
#FINAL EXCLUSIONS#
##################
#Choosing to cut this study because log-analysis not possible due to perfect performance
#in 200 and 400 msec conditions
x<-subset(x,STUDY_Short != "Kazen83E3")
#Choosing to cut this study because there is no true 0 condition, and all the ANOVAs
#reported corrected (GG) F values
final_study_trim<-c("Leonhard 2012 E1_DIM",
                    "Leonhard 2012_E1_BRIGHT",
                    "Leonhard 2012 E2_DIM",
                    "Leonhard 2012_E2_BRIGHT",
                    "Leonhard 2012_E3_Intact")
x<-subset(x, STUDY_Short != final_study_trim)

View(x)
#PRINT ANALYSIS SCRIPT
write.csv(x, 'Alerting_MetaAnalysis_Table.csv')
####PROCEED TO SCRIPT 2###

