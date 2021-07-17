
rm(list=ls())
# install.packages("plyr")

library(plyr)
library(betareg)
library(ggplot2)
library(fastDummies)
library(reshape2)
library(tidyr)
library(wesanderson)

###New files from Josh - Feb 2021
Directory<-"C:\\Users\\rbuxton\\Documents\\GitHub\\SARActionvsRM\\Data"
Files<-list.files(Directory, full.names = TRUE)

##Action RM
ActionData=read.csv(Files[1])

#Clean the action data set
source("C:\\Users\\rbuxton\\Documents\\GitHub\\SARActionvsRM\\Functions\\CleanSpreadsheet_01.R")
FinalList<-CleanSpreadsheet(ActionData)

################### PLOTS OF IUCN ACTION AND RM CODES
FullCodes<-cbind(FinalList[[1]]$SpeciesPopulation, FinalList[[2]], FinalList[[4]])
SummarizedCodes<-cbind(FinalList[[1]]$SpeciesPopulation, FinalList[[3]], FinalList[[5]])
colnames(FullCodes)[1]<-"SpeciesPopulation"
colnames(SummarizedCodes)[1]<-"SpeciesPopulation"


FullCodes_summary<-ddply(FullCodes, c("SpeciesPopulation"), colwise(sum, colnames(FullCodes)[c(grep("Action_", colnames(FullCodes)),grep("Actions", colnames(FullCodes)), grep("ResearchMonitoring", colnames(FullCodes)))]))
SummarizedCodes_summary<-ddply(SummarizedCodes, c("SpeciesPopulation"), colwise(sum, colnames(SummarizedCodes)[c(grep("Action_", colnames(SummarizedCodes)),grep("Actions", colnames(SummarizedCodes)), grep("ResearchMonitoring", colnames(SummarizedCodes)))]))

#long format for graphing
FullCodes_summary_long <- gather(FullCodes_summary, IUCNCode, PresAbs, colnames(FullCodes_summary)[2:ncol(FullCodes_summary)], factor_key=TRUE)
FullCodes_summary_long<-subset(FullCodes_summary_long, PresAbs==1)
FullCodes_summary_long$ActionvRM<-sapply(strsplit(as.character(FullCodes_summary_long$IUCNCode),'_'), "[", 1)
FullCodes_summary_long$IUCN<-sapply(strsplit(as.character(FullCodes_summary_long$IUCNCode),'_'), "[", 2)

#####Research and monitoring
IUCNCodes_long_summary_RM<-subset(FullCodes_summary_long, ActionvRM=="ResearchMonitoring")
IUCNCodes_long_summary_RM$Broadcategory<-ifelse(grepl("^1",IUCNCodes_long_summary_RM$IUCN),"1_Research",
                                             ifelse(grepl("^2",IUCNCodes_long_summary_RM$IUCN),"2_Planning", ifelse(grepl("^3",IUCNCodes_long_summary_RM$IUCN), "3_Monitoring", "4_DevelopDatabases")))

#Labels
IUCNCodes_long_summary_RM$IUCNfull<-ifelse(IUCNCodes_long_summary_RM$IUCN=="1","General",
       ifelse(IUCNCodes_long_summary_RM$IUCN=="1.1", "Taxonomic",
              ifelse(IUCNCodes_long_summary_RM$IUCN=="1.2", "Population",
                     ifelse(IUCNCodes_long_summary_RM$IUCN=="1.3", "Ecology",
                            ifelse(IUCNCodes_long_summary_RM$IUCN=="1.4", "Harvest",
                                   ifelse(IUCNCodes_long_summary_RM$IUCN=="1.5", "Threats",
                                          ifelse(IUCNCodes_long_summary_RM$IUCN=="1.6", "Actions",
                                                 ifelse(IUCNCodes_long_summary_RM$IUCN=="1.7", "Public perception",
                                                        ifelse(IUCNCodes_long_summary_RM$IUCN=="1.8", "Coordinating/prioritizing",
                                                               ifelse(IUCNCodes_long_summary_RM$IUCN=="1.9", "Cost-effectiveness",
                                                                      ifelse(IUCNCodes_long_summary_RM$IUCN=="2.1", "Species",
                                                                             ifelse(IUCNCodes_long_summary_RM$IUCN=="2.2", "Area",
                                                                                    ifelse(IUCNCodes_long_summary_RM$IUCN=="2.3", "Harvest",
                                                                                           ifelse(IUCNCodes_long_summary_RM$IUCN=="2.4", "Communication",
                                                                                                  ifelse(IUCNCodes_long_summary_RM$IUCN=="3.1", "Population monitoring",
                                                                                                         ifelse(IUCNCodes_long_summary_RM$IUCN=="3.4", "Habitat",
                                                                                                                ifelse(IUCNCodes_long_summary_RM$IUCN=="3.5", "Threats and Actions",
                                                                                                                       ifelse(IUCNCodes_long_summary_RM$IUCN=="4.1", "Databases", "Archiving data"))))))))))))))))))
IUCNCodes_long_summary_RM$IUCNfull<-paste(IUCNCodes_long_summary_RM$IUCN, IUCNCodes_long_summary_RM$IUCNfull, sep="_")

pal <- c(wes_palette("Zissou1"), wes_palette("Darjeeling2"), wes_palette("GrandBudapest2"), wes_palette("Darjeeling1"))
RM<-ggplot(IUCNCodes_long_summary_RM, aes(fill=IUCNfull, y=PresAbs, x=Broadcategory)) + 
  geom_bar(position="stack", stat="identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0, 0))+
  # scale_x_continuous(expand = c(0, 0))+
  labs(x = "Broad IUCN category", y = "Number of proposed actions")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12,face="bold"),axis.text.x = element_text(angle = 45, hjust=1))+ 
  labs(fill = "IUCN RM category")+
  scale_fill_manual(values=pal)


#####Action
IUCNCodes_long_summary_Action<-subset(FullCodes_summary_long, ActionvRM=="Action")
IUCNCodes_long_summary_Action$Broadcategory<-ifelse(grepl("^1",IUCNCodes_long_summary_Action$IUCN),"1_Land/water protection",
                                                ifelse(grepl("^2",IUCNCodes_long_summary_Action$IUCN),"2_Land/water management", 
                                                       ifelse(grepl("^3",IUCNCodes_long_summary_Action$IUCN), "3_Species management", 
                                                              ifelse(grepl("^4",IUCNCodes_long_summary_Action$IUCN), "4_Education/Awareness",
                                                                     ifelse(grepl("^5",IUCNCodes_long_summary_Action$IUCN), "5_Law/policy",
                                                                            ifelse(grepl("^6",IUCNCodes_long_summary_Action$IUCN), "6_Incentives","7_Capacity building"))))))


#Labels
IUCNCodes_long_summary_Action$IUCNfull<-ifelse(IUCNCodes_long_summary_Action$IUCN=="1.1","Site/area protection",
                                               ifelse(IUCNCodes_long_summary_Action$IUCN=="1.2","Resource/habitat protection",
                                           ifelse(IUCNCodes_long_summary_Action$IUCN=="2.1", "Area management",
                                                  ifelse(IUCNCodes_long_summary_Action$IUCN=="2.2", "Invasive/problematic species control",
                                                         ifelse(IUCNCodes_long_summary_Action$IUCN=="2.3", "Restoration",
                                                                ifelse(IUCNCodes_long_summary_Action$IUCN=="3.1", "Species management",
                                                                       ifelse(IUCNCodes_long_summary_Action$IUCN=="3.2", "Species recovery",
                                                                              ifelse(IUCNCodes_long_summary_Action$IUCN=="3.3", "Species reintroduction",
                                                                                     ifelse(IUCNCodes_long_summary_Action$IUCN=="3.4", "Ex-situ conservation",
                                                                                            ifelse(IUCNCodes_long_summary_Action$IUCN=="4.1", "Formal education",
                                                                                                   ifelse(IUCNCodes_long_summary_Action$IUCN=="4.2", "Training",
                                                                                                          ifelse(IUCNCodes_long_summary_Action$IUCN=="4.3", "Awareness/communication",
                                                                                                                 ifelse(IUCNCodes_long_summary_Action$IUCN=="4.4", "Outreach",
                                                                                                                        ifelse(IUCNCodes_long_summary_Action$IUCN=="4.5", "Indigenous engagement",
                                                                                                                               ifelse(IUCNCodes_long_summary_Action$IUCN=="5.1", "Legislation",
                                                                                                                                      ifelse(IUCNCodes_long_summary_Action$IUCN=="5.2", "Policies/regulations",
                                                                                                                                             ifelse(IUCNCodes_long_summary_Action$IUCN=="5.3", "Private sector standards/codes",
                                                                                                                                                    ifelse(IUCNCodes_long_summary_Action$IUCN=="5.4", "Compliance/enforcement",
                                                                                                                                                           ifelse(IUCNCodes_long_summary_Action$IUCN=="6", "Incentives",
                                                                                                                                                           ifelse(IUCNCodes_long_summary_Action$IUCN=="6.4", "Conservation payments", 
                                                                                                                                                                  ifelse(IUCNCodes_long_summary_Action$IUCN=="7.1", "Institutional development",
                                                                                                                                                                         ifelse(IUCNCodes_long_summary_Action$IUCN=="7.2", "Alliance and partnership","Conservation finance"))))))))))))))))))))))
IUCNCodes_long_summary_Action$IUCNfull<-paste(IUCNCodes_long_summary_Action$IUCN, IUCNCodes_long_summary_Action$IUCNfull, sep="_")

pal <- c(wes_palette("FantasticFox1"),wes_palette("Zissou1"), wes_palette("Darjeeling1"),wes_palette("Chevalier1"), wes_palette("GrandBudapest1"))
Action<-ggplot(IUCNCodes_long_summary_Action, aes(fill=IUCNfull, y=PresAbs, x=Broadcategory)) + 
  geom_bar(position="stack", stat="identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_y_continuous(expand = c(0, 0))+
  # scale_x_continuous(expand = c(0, 0))+
  labs(x = "Broad IUCN category", y = "Number of proposed actions")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12,face="bold"),axis.text.x = element_text(angle = 45, hjust=1))+ 
  labs(fill = "IUCN Action category")+
  scale_fill_manual(values=pal)

source("C:\\Users\\rbuxton\\Documents\\GitHub\\SARActionvsRM\\Functions\\Multiplot.R")
multiplot(RM, Action, cols=2)

##Numbers for the results section
max(ddply(IUCNCodes_long_summary_RM, c("IUCNfull"),summarize, Total=sum(PresAbs))$Total)/nrow(IUCNCodes_long_summary_RM)

ddply(IUCNCodes_long_summary_Action, c("Broadcategory"),summarize, Total=sum(PresAbs))
ddply(IUCNCodes_long_summary_Action, c("IUCNfull"),summarize, Total=sum(PresAbs))[order(ddply(IUCNCodes_long_summary_Action, c("IUCNfull"),summarize, Total=sum(PresAbs))$Total),]



#######################################
################### PLOTS OF High, Med, Low priority

Priority_summary<-ddply(FinalList[[1]], c("Priority"), summarize, Action=sum(Action), RM=sum(RM), Both=sum(Both))%>%
  subset(Priority!="")

Priority_summary_long <- gather(Priority_summary, ActionRM, Count, colnames(Priority_summary)[2:ncol(Priority_summary)], factor_key=TRUE)
Priority_summary_long$Priority<-gsub("Low", "N_Low", Priority_summary_long$Priority)
Priority_summary_long$ActionRM<-gsub("RM", "ab_RM", Priority_summary_long$ActionRM)

ggplot(Priority_summary_long, aes(fill=Priority, y=Count, x=ActionRM)) + 
  geom_bar(position="stack", stat="identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0, 0))+
  # scale_x_continuous(expand = c(0, 0))+
  labs(x = NULL, y = "Recovery measures")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12,face="bold"),axis.text.x = element_text(angle = 45, hjust=1))+ 
  labs(fill = "Priority")+
  scale_fill_manual(values=wes_palette("Darjeeling2"))

Priority_summary$Action<-Priority_summary$Action/sum(Priority_summary$Action)
Priority_summary$RM<-Priority_summary$RM/sum(Priority_summary$RM)
Priority_summary$Both<-Priority_summary$Both/sum(Priority_summary$Both)

##By Species
Summary_HighPri<-ddply(FinalList[[1]], c("SpeciesPopulation"),summarize, Actions=sum(Action), RMs=sum(RM), BothARM=sum(Both), 
                       TotalRMHighPri=sum(RMHighPri),TotalActionHighPri=sum(ActionHighPri),TotalBothHighPri=sum(BothHighPri),
                   Freq=length(Action))
#Split 'both' into half action, half rm
Summary_HighPri$Actions<-ifelse(Summary_HighPri$BothARM!=0, Summary_HighPri$Actions+(Summary_HighPri$BothARM/2), Summary_HighPri$Actions)
Summary_HighPri$RMs<-ifelse(Summary_HighPri$BothARM!=0, Summary_HighPri$RMs+(Summary_HighPri$BothARM/2), Summary_HighPri$RMs)

Summary_HighPri$TotalActionHighPri<-ifelse(Summary_HighPri$TotalBothHighPri!=0, Summary_HighPri$TotalActionHighPri+(Summary_HighPri$TotalBothHighPri/2), Summary_HighPri$TotalActionHighPri)
Summary_HighPri$TotalRMHighPri<-ifelse(Summary_HighPri$TotalBothHighPri!=0, Summary_HighPri$TotalRMHighPri+(Summary_HighPri$TotalBothHighPri/2), Summary_HighPri$TotalRMHighPri)

Summary_HighPri$PropRM_HighPri<-Summary_HighPri$TotalRMHighPri/Summary_HighPri$RMs
Summary_HighPri$PropAc_HighPri<-Summary_HighPri$TotalActionHighPri/Summary_HighPri$Actions

Summary_HighPri$PropMgmtTasks_HighPri<-Summary_HighPri$TotalRMHighPri/Summary_HighPri$Freq
Summary_HighPri$PropMgmtTasks_AcHighPri<-Summary_HighPri$TotalActionHighPri/Summary_HighPri$Freq

Summary_HighPri$PropRM_HighPri[which(is.nan(Summary_HighPri$PropRM_HighPri))]<-0
Summary_HighPri$PropAc_HighPri[which(is.nan(Summary_HighPri$PropAc_HighPri))]<-0

mean(Summary_HighPri$PropRM_HighPri)
mean(Summary_HighPri$PropAc_HighPri)
mean(Summary_HighPri$PropMgmtTasks_HighPri)
mean(Summary_HighPri$PropMgmtTasks_AcHighPri)

#######################################
################### PLOTS OF Recovery Measure status and timeline

FinalList[[1]]$StatusAndTimeline<-ifelse(FinalList[[1]]$Completed==1, "Completed", 
                                         ifelse(FinalList[[1]]$Annual==1,"Annual",
                                                                ifelse(FinalList[[1]]$Ongoing==1,"Ongoing",
                                                                                      ifelse(FinalList[[1]]$Developmental==1, "Developmental",
                                                                                                      ifelse(FinalList[[1]]$Opportunistic==1,"Opportunistic", "")))))
StatusAndTimeline_summary<-ddply(FinalList[[1]], c("StatusAndTimeline"), summarize, Action=sum(Action), RM=sum(RM), Both=sum(Both))%>%
  subset(StatusAndTimeline!="")

StatusAndTimeline_summary_long <- gather(StatusAndTimeline_summary, ActionRM, Count, colnames(StatusAndTimeline_summary)[2:ncol(StatusAndTimeline_summary)], factor_key=TRUE)
# StatusAndTimeline_summary$Priority<-gsub("Low", "N_Low", Priority_summary_long$Priority)
# Priority_summary_long$ActionRM<-gsub("RM", "ab_RM", Priority_summary_long$ActionRM)

StatusAndTimeline_summary_long$ActionRM<-gsub("RM","Research and monitoring",StatusAndTimeline_summary_long$ActionRM)
StatusAndTimeline_summary_long$ActionRM<-gsub("Both","Action, research, and monitoring",StatusAndTimeline_summary_long$ActionRM)


ggplot(StatusAndTimeline_summary_long, aes(fill=StatusAndTimeline, y=Count, x=ActionRM)) + 
  geom_bar(position="stack", stat="identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0, 0))+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12))+
  labs(x = NULL, y = "Number of management tasks")+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(angle = 45, hjust=1))+ 
   labs(fill = "Status and timeline")+
  scale_fill_manual(values=wes_palette("IsleofDogs1"))

StatusAndTimeline_summary$Action<-StatusAndTimeline_summary$Action/sum(StatusAndTimeline_summary$Action)
StatusAndTimeline_summary$RM<-StatusAndTimeline_summary$RM/sum(StatusAndTimeline_summary$RM)
StatusAndTimeline_summary$Both<-StatusAndTimeline_summary$Both/sum(StatusAndTimeline_summary$Both)
#######################################

#Summarize by species
Summary_APs<-ddply(FinalList[[1]], c("SpeciesPopulation","Scientific.Name", "Common.Name", "Taxon.Group", "Consol_Taxa","SARA.Status","COSEWIC.Status",
                                     "Endangered","No.Status","Threatened","Special.Concern","Extirpated",
                                     "ProvincialRange","Yukon","NorthwestTerritories","Nunavut","BritishColumbia","Alberta" ,
                                     "Saskatchewan","Manitoba","Ontario","Quebec","NewBrunswick","PEI" ,"NovaScotia","Newfoundland", "Aquatic"),
                   summarize, Actions=sum(Action), RMs=sum(RM), BothARM=sum(Both), TotalRMHighPri=sum(RMHighPri),Numsp=mean(Number.of.Species),Cost=mean(Cost..per.year.),
                   Freq=length(Action))
#Split 'both' into half action, half rm
Summary_APs$Actions<-ifelse(Summary_APs$BothARM!=0, Summary_APs$Actions+(Summary_APs$BothARM/2), Summary_APs$Actions)
Summary_APs$RMs<-ifelse(Summary_APs$BothARM!=0, Summary_APs$RMs+(Summary_APs$BothARM/2), Summary_APs$RMs)

Summary_APs$PropRM<-Summary_APs$RMs/Summary_APs$Freq

#Histogram
ggplot(Summary_APs, aes(x=PropRM)) +
  geom_histogram(breaks=seq(0,1, by=0.05), fill="grey81", colour="black")+
  # geom_hline(yintercept=10, linetype="dashed", color = "black")+
  # scale_fill_grey(start = 0.2, end = 0.6)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(hist(Summary_APs$PropRM, plot=FALSE, breaks=seq(0,1, by=0.05))$counts)))+
  # scale_x_continuous(expand = c(0, 0))+
  labs(x = "Proportion of management tasks research/monitoring", y = "Number of species")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=14,face="bold"))

##Taxon
ggplot(Summary_APs, aes(x = reorder(Consol_Taxa, PropRM, FUN = median), y = PropRM)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2, height=0), size=1.5)+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1.02))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Taxon", y = "Proportion of tasks research/monitoring")+ #here is where you can change the x and y labels
  scale_y_continuous(expand = c(0, 0.05))+
  theme(axis.text=element_text(size=14, colour="black"), #here's where you change the size of the x and y label text
        axis.title=element_text(size=14,face="bold"))

##Combine some threat status
Summary_APs$SARA.Status<-ifelse(Summary_APs$SARA.Status=="Extirpated", "Endangered", Summary_APs$SARA.Status)

Summary_APs$COSEWIC.Status<-ifelse(Summary_APs$COSEWIC.Status=="Extirpated", "Endangered", Summary_APs$COSEWIC.Status)
Summary_APs$COSEWIC.Status<-ifelse(Summary_APs$COSEWIC.Status=="Extinct", "Endangered", Summary_APs$COSEWIC.Status)
Summary_APs$COSEWIC.Status<-ifelse(Summary_APs$COSEWIC.Status=="Non-active", "Not at Risk", Summary_APs$COSEWIC.Status)

#Sara status
ggplot(Summary_APs, aes(x = reorder(SARA.Status, PropRM, FUN = median), y = PropRM)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2, height=0), size=1.5)+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1.02))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Threat status", y = "Proportion of tasks research/monitoring")+
  scale_y_continuous(expand = c(0, 0.05))+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14,face="bold"))
