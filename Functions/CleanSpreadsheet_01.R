



CleanSpreadsheet<-function(ActionData){
#Narrow to those with complete action plans
Data_ActionPlansOnly<-subset(ActionData, Action.Plan..Y.N.Proposed.=="Y")
Data_ActionPlansOnly<-Data_ActionPlansOnly[-which(Data_ActionPlansOnly$Recovery.Measure==""),]

###############################################################################################################
#####                                     SUMMARY INFORMATION                                             #####
###############################################################################################################

# #Total populations - 1179
# length(unique(paste(Data$Common.Name, Data$Scientific.Name, Data$Population, sep="_")))
# 
# #Total species - 887
# length(unique(Data$Scientific.Name))
# 
# #Populations with action plans - 258
# length(unique(paste(Data_ActionPlansOnly$Common.Name, Data_ActionPlansOnly$Scientific.Name, Data_ActionPlansOnly$Population, sep="_")))
# 
# #Species with action plans - 239
# length(unique(Data_ActionPlansOnly$Scientific.Name))

###############################################################################################################
#####                                          CLEAN DATASETS                                             #####
###############################################################################################################


######################Cost
Data_ActionPlansOnly$Cost..per.year.<-gsub("\\$", "", Data_ActionPlansOnly$Cost..per.year.)
Data_ActionPlansOnly$Cost..per.year.<-gsub(",", "", Data_ActionPlansOnly$Cost..per.year.)
Data_ActionPlansOnly$Cost..per.year.<-gsub("\\.", "", Data_ActionPlansOnly$Cost..per.year.)

#Make ranges into mean
Data_ActionPlansOnly$Cost..per.year.<-ifelse(Data_ActionPlansOnly$Cost..per.year.=="70000 to 100000",
                                             mean(c(70000,100000)), as.character(Data_ActionPlansOnly$Cost..per.year.))
Data_ActionPlansOnly$Cost..per.year.<-ifelse(Data_ActionPlansOnly$Cost..per.year.=="321500 for the period 2015-2020",
                                             321500/5, as.character(Data_ActionPlansOnly$Cost..per.year.))
Data_ActionPlansOnly$Cost..per.year.<-ifelse(Data_ActionPlansOnly$Cost..per.year.=="400000 over the ten year analysis period",
                                             400000/5, as.character(Data_ActionPlansOnly$Cost..per.year.))

#Weird ones
#Low: 0-1 million (make 500,000)
Data_ActionPlansOnly$Cost..per.year.<-ifelse(Data_ActionPlansOnly$Cost..per.year.=="Low: 0 - 1 million",
                                             500000, as.character(Data_ActionPlansOnly$Cost..per.year.))
#less than 1 million = 500,000
Data_ActionPlansOnly$Cost..per.year.<-ifelse(Data_ActionPlansOnly$Cost..per.year.=="< 1 million",
                                             500000, as.character(Data_ActionPlansOnly$Cost..per.year.))
#less than 50000 = 25,000
Data_ActionPlansOnly$Cost..per.year.<-ifelse(Data_ActionPlansOnly$Cost..per.year.=="< 50000/year",
                                             25000, as.character(Data_ActionPlansOnly$Cost..per.year.))

Data_ActionPlansOnly$Cost..per.year.<-as.numeric(Data_ActionPlansOnly$Cost..per.year.)

###################Priority
#Priority
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="L", "Low", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Lwo", "Low", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-gsub(" ", "", Data_ActionPlansOnly$Priority)
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="low", "Low", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Mediu", "Medium", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Moderate", "Medium", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="M", "Medium", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="high", "High", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="HIgh", "High", as.character(Data_ActionPlansOnly$Priority))
#Urgent = High
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Urgent", "High", as.character(Data_ActionPlansOnly$Priority))
#Necessary = Medium
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Necessary", "Medium", as.character(Data_ActionPlansOnly$Priority))
#Beneficial = low
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Beneficial", "Low", as.character(Data_ActionPlansOnly$Priority))
#1 = High, 2 = Medium, 3 = Low?, 0 = none
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="1", "High", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="2", "Medium", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="3", "Low", as.character(Data_ActionPlansOnly$Priority))
#Medium-High = Medium, HighMedium = High
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Medium-High", "Medium", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-gsub("\n", "", Data_ActionPlansOnly$Priority)
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="HighMedium", "High", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="Highifhistoricallypresent,mediumotherwise", "Medium", as.character(Data_ActionPlansOnly$Priority))
Data_ActionPlansOnly$Priority<-ifelse(Data_ActionPlansOnly$Priority=="0", "", as.character(Data_ActionPlansOnly$Priority))

#Make Dummy Cols
Priority<-dummy_cols(Data_ActionPlansOnly$Priority)
colnames(Priority)<-gsub(".data_", "", colnames(Priority))
Data_ActionPlansOnly<-cbind(Data_ActionPlansOnly,Priority[,3:5])

########################ActionvsRM
Data_ActionPlansOnly[which(Data_ActionPlansOnly$ActionCode2=="3.4.2"),]$ActionCode2<-"3.4"
Data_ActionPlansOnly$ActionCode2<-as.numeric(ifelse(Data_ActionPlansOnly$ActionCode2=="", NA, as.character(Data_ActionPlansOnly$ActionCode2)))

Data_ActionPlansOnly$Action<-ifelse(is.na(Data_ActionPlansOnly$RMCode1)&!is.na(Data_ActionPlansOnly$ActionCode1),1,0)
Data_ActionPlansOnly$RM<-ifelse(!is.na(Data_ActionPlansOnly$RMCode1)&is.na(Data_ActionPlansOnly$ActionCode1),1,0)
Data_ActionPlansOnly$Both<-ifelse(!is.na(Data_ActionPlansOnly$RMCode1)&!is.na(Data_ActionPlansOnly$ActionCode1),1,0)

#########################Number of provinces
Data_ActionPlansOnly$ProvincialRange<-sapply(strsplit(as.character(Data_ActionPlansOnly$Range), ","), length)

#########################Consolidate some taxa
Data_ActionPlansOnly$Consol_Taxa<-ifelse(Data_ActionPlansOnly$Taxon.Group=="Fishes (freshwater)", "Fishes", ifelse(Data_ActionPlansOnly$Taxon.Group=="Fishes (marine)", "Fishes",
                                                                                                                   ifelse(Data_ActionPlansOnly$Taxon.Group=="Lichens", "Nonvascular Plants",
                                                                                                                          ifelse(Data_ActionPlansOnly$Taxon.Group=="Mammals (marine)", "Mammals",
                                                                                                                                 ifelse(Data_ActionPlansOnly$Taxon.Group=="Mammals (terrestrial)", "Mammals",
                                                                                                                                        ifelse(Data_ActionPlansOnly$Taxon.Group=="Mosses", "Nonvascular Plants",
                                                                                                                                               ifelse(Data_ActionPlansOnly$Taxon.Group=="Arthropods", "Invertebrates",
                                                                                                                                                      ifelse(Data_ActionPlansOnly$Taxon.Group=="Molluscs", "Invertebrates",Data_ActionPlansOnly$Taxon.Group))))))))


########################Split range of provinces
Data_ActionPlansOnly$Yukon<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][1], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$NorthwestTerritories<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][2], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$Nunavut<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][3], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$BritishColumbia<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][4], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$Alberta<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][5], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$Saskatchewan<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][6], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$Manitoba<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][7], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$Ontario<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][8], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$Quebec<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][9], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$NewBrunswick<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][10], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$PEI<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][11], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$NovaScotia<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][12], as.character(Data_ActionPlansOnly$Range)),1,0)
Data_ActionPlansOnly$Newfoundland<-ifelse(grepl(strsplit(as.character(Data_ActionPlansOnly[which(Data_ActionPlansOnly$ProvincialRange==13),]$Range[1]), ",")[[1]][13], as.character(Data_ActionPlansOnly$Range)),1,0)

###############################SARA status
Status<-dummy_cols(Data_ActionPlansOnly$SARA.Status)
colnames(Status)<-gsub(".data_", "", colnames(Status))

Data_ActionPlansOnly<-cbind(Data_ActionPlansOnly,Status[,2:6])

##############Status and timeline
Data_ActionPlansOnly$Status.and.Timeline <- tolower(as.character(Data_ActionPlansOnly$Status.and.Timeline)) 
Data_ActionPlansOnly$Completed<-ifelse(grepl("completed|achieved|documented|finalized|established|gained|understood|in place|presented|implemented|produced|prepared|revised monitoring protocol by june|clarified|established|accepted", as.character(Data_ActionPlansOnly$Status.and.Timeline)),1,0)
Data_ActionPlansOnly$Completed<-ifelse(grepl("ongoing", as.character(Data_ActionPlansOnly$Status.and.Timeline)),0,Data_ActionPlansOnly$Completed)

Data_ActionPlansOnly$Annual<-ifelse(grepl("annual|annually|per year|at least once every 2 years|biennially", as.character(Data_ActionPlansOnly$Status.and.Timeline)),1,0)
Data_ActionPlansOnly$Ongoing<-ifelse(grepl("ongoing|on-going|continuous|continously|[0-9][0-9][0-9][0-9]|short-term|long-term|long term|medium-term|5 years|1 year|underway|three years|following receipt of range plans|for at least 5|five ye|five years|within one year of confirmation of breeding", as.character(Data_ActionPlansOnly$Status.and.Timeline)),1,0)
#Some just have years - include with ongoing, bur remove 'new and completed ones'
Data_ActionPlansOnly$Ongoing<-ifelse(grepl("developmental|not started|new|annual|annually|completed|achieved|documented|finalized|established|gained|understood|in place|presented|implemented|begin dialogue|opportunistic|revised monitoring protocol by june|prepared|clarified|established|as needs be", as.character(Data_ActionPlansOnly$Status.and.Timeline)),0,Data_ActionPlansOnly$Ongoing)

Data_ActionPlansOnly$Developmental<-ifelse(grepl("developmental|not started|new|achieved|pending outcome|begin dialogue", as.character(Data_ActionPlansOnly$Status.and.Timeline)),1,0)
Data_ActionPlansOnly$Opportunistic<-ifelse(grepl("as opportunities arise|as required|opportunistic|as new stakeholders are identified|uncertain|as necessary|as needs be|when possible|dependent on|where applicable|if deemed necessary", as.character(Data_ActionPlansOnly$Status.and.Timeline)),1,0)

#Num of research/mon high priority
Data_ActionPlansOnly$RMHighPri<-ifelse(Data_ActionPlansOnly$RM==1&Data_ActionPlansOnly$High==1,1,0)
Data_ActionPlansOnly$ActionHighPri<-ifelse(Data_ActionPlansOnly$Action==1&Data_ActionPlansOnly$High==1,1,0)
Data_ActionPlansOnly$BothHighPri<-ifelse(Data_ActionPlansOnly$Both==1&Data_ActionPlansOnly$High==1,1,0)
colnames(Data_ActionPlansOnly)<-gsub(" ", ".", colnames(Data_ActionPlansOnly))

Data_ActionPlansOnly[which(Data_ActionPlansOnly$Common.Name=="Eastern Musk Turtle"),]$Terrestrial<-0
Data_ActionPlansOnly[which(Data_ActionPlansOnly$Common.Name=="Eastern Musk Turtle"),]$Aquatic<-1

Data_ActionPlansOnly[which(Data_ActionPlansOnly$Common.Name=="Eastern Ribbonsnake"),]$Terrestrial<-1
Data_ActionPlansOnly[which(Data_ActionPlansOnly$Common.Name=="Eastern Ribbonsnake"),]$Aquatic<-0

Data_ActionPlansOnly$SpeciesPopulation<-paste(Data_ActionPlansOnly$Common.Name, Data_ActionPlansOnly$Scientific.Name, Data_ActionPlansOnly$Population, sep="_")
Data_ActionPlansOnly<-subset(Data_ActionPlansOnly, Multispecies.Quality.Control==1)

#####Make dummy variables for actions and RM
ActionCols<-c("ActionCode1", "ActionCode2", "ActionCode3")
ResearchMonCols<-c("RMCode1", "RMCode2", "RMCode3", "RMCode4")

##Function to make dummy columns
DummyFunc<-function(ActionCols, ColName, Summary){
  UniqueActions<-unique(unlist(Data_ActionPlansOnly[,ActionCols]))
  UniqueActions<-UniqueActions[which(!is.na(UniqueActions))]
  
  if(Summary==T){
    ActionDummies<-lapply(1:length(unique(floor(UniqueActions))), function(n){
      rowSums(sapply(floor(Data_ActionPlansOnly[,ActionCols]), function (x) ifelse(x == unique(floor(UniqueActions))[n],1,0)), na.rm = TRUE)
    })
    ActionDummies<-data.frame(do.call("cbind", ActionDummies))
    colnames(ActionDummies)<-paste(ColName, unique(floor(UniqueActions)), sep="_")
    ActionDummies[ActionDummies != 0] <- 1
    
  }else{
    ActionDummies<-lapply(1:length(UniqueActions), function(n){
      rowSums(sapply(Data_ActionPlansOnly[,ActionCols], function (x) ifelse(x == UniqueActions[n],1,0)), na.rm = TRUE)
    })
    ActionDummies<-data.frame(do.call("cbind", ActionDummies))
    colnames(ActionDummies)<-paste(ColName, UniqueActions, sep="_")
    ActionDummies[ActionDummies != 0] <- 1
  }
  ActionDummies
}

ActionDummies<-DummyFunc(ActionCols, "Action", F)
RMDummies<-DummyFunc(ResearchMonCols, "ResearchMonitoring", F)
ActionDummies_summary<-DummyFunc(ActionCols, "Action", T)
RMDummies_summary<-DummyFunc(ResearchMonCols, "ResearchMonitoring", T)

FinalDataframe<-Data_ActionPlansOnly[,which(colnames(Data_ActionPlansOnly)%in%c("Common.Name","Scientific.Name", "Population", "SpeciesPopulation", "Taxon.Group",
                                                                                      "Consol_Taxa","ProvincialRange","Yukon","NorthwestTerritories","Nunavut","BritishColumbia","Alberta",
                                                                                      "Saskatchewan","Manitoba","Ontario","Quebec","NewBrunswick","PEI","NovaScotia","Newfoundland", 
                                                                                      "SARA.Status", "COSEWIC.Status","Endangered","Extirpated","No.Status","Special.Concern","Threatened",              
                                                                                      "Year", "Cost..per.year.","Number.of.Species","Action", "RM", "Both","High","Low","Medium", "Priority",
                                                                                      "RMHighPri", "ActionHighPri","BothHighPri","Annual","Ongoing","Developmental", "Opportunistic", "Completed",              
                                                                                        "Terrestrial","Aquatic"))]

FinalList<-list(FinalDataFrame=FinalDataframe, ActionDummies=ActionDummies, ActionDummies_summary=ActionDummies_summary, RMDummies=RMDummies, RMDummies_summary=RMDummies_summary)
return(FinalList)

}