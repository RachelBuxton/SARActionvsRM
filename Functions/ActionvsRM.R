rm(list=ls())
# install.packages("plyr")

library(plyr)
library(betareg)
library(ggplot2)
library(fastDummies)
library(reshape2)
library(tidyr)
library(dplyr)
library(betareg)
library(car)
library(MuMIn)

###New files from Josh - Feb 2021, reviewed by RTB June 2021
Directory<-"C:\\Users\\rbuxton\\Documents\\Post doc_Carleton\\Canada_RMvsAction\\Data\\JGeauvreau"
Files<-list.files(Directory, full.names = TRUE)

##Action RM
ActionData=read.csv(Files[1])

#Clean the action data set
source("C:\\Users\\rbuxton\\Documents\\Post doc_Carleton\\Canada_RMvsAction\\Functions\\CleanSpreadsheet_01.R")
FinalList<-CleanSpreadsheet(ActionData)

#Summarize by species
Summary_APs<-ddply(FinalList[[1]], c("SpeciesPopulation","Scientific.Name", "Common.Name", "Taxon.Group", "Consol_Taxa","SARA.Status","COSEWIC.Status",
                                     "Endangered","No.Status","Threatened","Special.Concern","Extirpated",
                                     "ProvincialRange","Yukon","NorthwestTerritories","Nunavut","BritishColumbia","Alberta" ,
                                     "Saskatchewan","Manitoba","Ontario","Quebec","NewBrunswick","PEI" ,"NovaScotia","Newfoundland", "Aquatic"),
                   summarize, Actions=sum(Action), RMs=sum(RM), BothARM=sum(Both), TotalRMHighPri=sum(RMHighPri),Numsp=mean(Number.of.Species),Cost=mean(Cost..per.year.),
                   Freq=length(Action))

###Join in THREATS
Threats<-read.csv("C:\\Users\\rbuxton\\Documents\\Post doc_Carleton\\Canada_RMvsAction\\Data\\JGeauvreau\\Threat Codes - Updated Feb 2021.csv")

Threats$SpeciesPopulation<-paste(Threats$Common.Name, Threats$Scientific.Name, Threats$Population, sep="_")
Threats$NumberofThreats<-sapply(strsplit(Threats$Threats, ","), length)
Threats<-Threats[,c(15:16)]

Summary_APs<-left_join(Summary_APs, Threats, by="SpeciesPopulation")

#Split 'both' into half action, half rm
Summary_APs$Actions<-ifelse(Summary_APs$BothARM!=0, Summary_APs$Actions+(Summary_APs$BothARM/2), Summary_APs$Actions)
Summary_APs$RMs<-ifelse(Summary_APs$BothARM!=0, Summary_APs$RMs+(Summary_APs$BothARM/2), Summary_APs$RMs)

Summary_APs$PropRM<-Summary_APs$RMs/Summary_APs$Freq

##########################################################################################################################################
##SUMMARIES FOR METHODS

#
ActionData$SpeciesPopulation<-paste(ActionData$Common.Name, ActionData$Scientific.Name, ActionData$Population, sep="_")
length(unique(ActionData$SpeciesPopulation))
length(unique(subset(ActionData, On.Schedule.1..Yes.No..=="Yes"|On.Schedule.1..Yes.No..=="Schedule 1")$SpeciesPopulation))

#Number of designatable units
nrow(Summary_APs)

#Species in multispecies plans
length(which(Summary_APs$Numsp>1))
length(which(Summary_APs$Numsp>1))/nrow(Summary_APs)

#Number of management tasks both RM and action
sum(FinalList[[1]]$Both)/nrow(FinalList[[1]])

#Number of management tasks not scored
length(which(rowSums(FinalList[[1]][,9:11])==0))/nrow(FinalList[[1]])

##SUMMARIES FOR RESULTS
mean(Summary_APs$PropRM)
length(which(Summary_APs$PropRM>=0.95))
length(which(Summary_APs$PropRM>=0.95))/nrow(Summary_APs)

length(which(Summary_APs$PropRM>.017))/nrow(Summary_APs)
length(which(Summary_APs$PropRM>=.1))/nrow(Summary_APs)

##########################################################################################################################################
##COST
Cost_summary<-subset(FinalList[[1]], Cost..per.year.>=1)%>%
ddply(c("SpeciesPopulation", "Cost..per.year."), summarize, Actions=sum(Action)/length(Action), RMs=sum(RM)/length(Action), BothARM=sum(Both)/length(Action))%>%
subset(Cost..per.year.>1&Cost..per.year.<46100000)

mRM<-lm(Cost_summary$RMs~Cost_summary$Cost..per.year.)
mA<-lm(Cost_summary$Actions~Cost_summary$Cost..per.year.)
mBoth<-lm(Cost_summary$BothARM~Cost_summary$Cost..per.year.)

summary(mRM)
summary(mA)
summary(mBoth)

##########################################################################################################################################

#Construct a model

##Fix 1s and 0s for beta model
Summary_APs$PropRM<-ifelse(Summary_APs$PropRM==0, Summary_APs$PropRM+0.0001,
                           Summary_APs$PropRM)
Summary_APs$PropRM<-ifelse(Summary_APs$PropRM==1, Summary_APs$PropRM-0.0001,
                           Summary_APs$PropRM)

##Combine some threat status
Summary_APs$SARA.Status<-ifelse(Summary_APs$SARA.Status=="Extirpated", "Endangered", Summary_APs$SARA.Status)
Summary_APs$COSEWIC.Status<-ifelse(Summary_APs$COSEWIC.Status=="Extirpated", "Endangered", Summary_APs$COSEWIC.Status)

Summary_APs$COSEWIC.Status<-ifelse(Summary_APs$COSEWIC.Status=="Extirpated", "Endangered", Summary_APs$COSEWIC.Status)
Summary_APs$COSEWIC.Status<-ifelse(Summary_APs$COSEWIC.Status=="Extinct", "Endangered", Summary_APs$COSEWIC.Status)
Summary_APs$COSEWIC.Status<-ifelse(Summary_APs$COSEWIC.Status=="Non-active", "Not at Risk", Summary_APs$COSEWIC.Status)

#Center scale number of threats and Provincial range
Summary_APs$NumberofThreats_s<-scale(Summary_APs$NumberofThreats, center = TRUE, scale=TRUE)[,1]
Summary_APs$ProvincialRange_s<-scale(Summary_APs$ProvincialRange, center = TRUE, scale=TRUE)[,1]
Summary_APs$Numsp[is.na(Summary_APs$Numsp)]<-1
Summary_APs$Numsp_s<-scale(Summary_APs$Numsp, center = TRUE, scale=TRUE)[,1]
Summary_APs$Multispecies<-ifelse(Summary_APs$Numsp==1,1,0)
  
Summary_APs<-cbind(Summary_APs,dummy_cols(Summary_APs$Consol_Taxa))

#Make fish the reference
Summary_APs$Consol_Taxa<-gsub("Fishes", "A_fishes", Summary_APs$Consol_Taxa)

#Make not at risk reference
Summary_APs$SARA.Status<-gsub("No Status", "A_No Status", Summary_APs$SARA.Status)

#Correlation between covariates
Summary_APs_CC<-Summary_APs[complete.cases(Summary_APs$NumberofThreats),]
cor(Summary_APs[,which(colnames(Summary_APs)%in%c("Aquatic","NumberofThreats", "ProvincialRange","Endangered","Extirpated","No Status","Special Concern","Threatened",
                                                  ".data_Amphibians",".data_Birds",".data_Fishes",".data_Invertebrates",".data_Mammals", ".data_Nonvascular Plants",
                                                  ".data_Reptiles" ,  ".data_Vascular Plants" ,"Numsp"))]   , method="spearman")           
cor(Summary_APs_CC[,which(colnames(Summary_APs_CC)%in%c("Aquatic","NumberofThreats", "ProvincialRange","Endangered","Extirpated","No Status","Special Concern","Threatened",
                                                  ".data_Amphibians",".data_Birds",".data_Fishes",".data_Invertebrates",".data_Mammals", ".data_Nonvascular Plants",
                                                  ".data_Reptiles" ,  ".data_Vascular Plants" ))]   , method="spearman")  

m1<-betareg(PropRM~SARA.Status+Consol_Taxa+Aquatic+ProvincialRange_s+Multispecies, data=Summary_APs, na.action=na.fail) #lower AIC
m2<-betareg(PropRM~SARA.Status+Consol_Taxa+Aquatic+ProvincialRange_s+Numsp_s, data=Summary_APs, na.action=na.fail)
AIC(m1,m2)

# DredgeTest<-dredge(m1) #remove aquatic and provincial range
m3<-betareg(PropRM~SARA.Status+Consol_Taxa+Multispecies, data=Summary_APs, na.action=na.fail) #lower AIC

#Try adding in threat
m4<-betareg(PropRM~SARA.Status+Consol_Taxa+NumberofThreats_s+Multispecies, data=Summary_APs_CC, na.action=na.fail)
DredgeTest<-dredge(m4) #Take out COSEWIC status

m5<-betareg(PropRM~Consol_Taxa+NumberofThreats_s+Multispecies, data=Summary_APs_CC, na.action=na.fail)

##JOIN IN RANGE AREA
# SARRange<-read.csv("C:\\Users\\rbuxton\\Documents\\Post doc_Carleton\\Canada_RMvsAction\\Data\\Species-at-Risk-RangeMap-Extents.tbl.csv")
# colnames(SARRange)[3]<-colnames(Summary_APs)[2]
# colnames(SARRange)[4]<-colnames(Summary_APs)[3]
# 
# SARRange<-SARRange[,c(3,4,25)]
# 
# test<-left_join(Summary_APs, SARRange, by=c("Scientific.Name", "Common.Name"))
# test<-test[complete.cases(test$Shape_Area),]
# 
# cor(test[,which(colnames(test)%in%c("Endangered","Extirpated","No Status","Special Concern","Threatened",
#                                                         ".data_Amphibians",".data_Birds",".data_Fishes",".data_Invertebrates",".data_Mammals", ".data_Nonvascular Plants",
#                                                         ".data_Reptiles" ,  ".data_Vascular Plants", "Shape_Area" ))]   , method="spearman")  
# 
# #Try adding in range area
# test$Shape_Area_s<-scale(test$Shape_Area, center = TRUE, scale=TRUE)[,1]
# m7<-betareg(PropRM~SARA.Status+Consol_Taxa+Shape_Area_s+Multispecies, data=test, na.action=na.fail)
# DredgeTest<-dredge(m7) #Range size makes AIC worse

##TRY ADDING IN COST
Summary_APs_cost<-Summary_APs[which(Summary_APs$Cost!=0),]
m6<-betareg(PropRM~SARA.Status+Consol_Taxa+Multispecies+Cost, data=Summary_APs_cost, na.action=na.fail) #lower AIC
# DredgeTest<-dredge(m6)#Cost makes AIC worse

##MAKE A NICE TABLE

CI<-data.frame(confint(m3)[-nrow(data.frame(confint(m3))),])
CI$Estimate<-summary(m3)[[1]]$mean[,1]
CI$SE<-summary(m3)[[1]]$mean[,2]
# CI<-CI[-grep("Intercept", row.names(CI)),]
CI$Covariates<-row.names(CI)
CI$Covariates<-gsub("Consol_Taxa", "", CI$Covariates)
CI$Covariates<-gsub("SARA.Status", "", CI$Covariates)
colnames(CI)[1:2]<-c("CI2.5", "CI97.5")
CI[,1:4]<-round(CI[,1:4], 2)
CI[,3]<-paste(CI[,3], "\u00b1", CI[,4], sep=" ")
CI<-CI[,c(5,3,1,2)]

CI_threats<-data.frame(confint(m5)[-nrow(data.frame(confint(m5))),])
CI_threats$Estimate<-summary(m5)[[1]]$mean[,1]
CI_threats$SE<-summary(m5)[[1]]$mean[,2]
# CI<-CI[-grep("Intercept", row.names(CI)),]
CI_threats$Covariates<-row.names(CI_threats)
CI_threats$Covariates<-gsub("Consol_Taxa", "", CI_threats$Covariates)
colnames(CI_threats)[1:2]<-c("CI2.5", "CI97.5")
CI_threats[,1:4]<-round(CI_threats[,1:4], 2)
CI_threats[,3]<-paste(CI_threats[,3], "\u00b1", CI_threats[,4], sep=" ")
CI_threats<-CI_threats[,c(5,3,1,2)]