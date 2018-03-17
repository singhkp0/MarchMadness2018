# MarchMadness2018
Predicting the winners of all NCAA games of 2018


##########kaggle submiddion
rm(list=ls())
library(tidyverse)
setwd("C:/Users/Kamal/Desktop/Kaggle_NCAA")
a<-read.csv("NCAATourneyDetailedResults.csv")
seed<-read.csv("NCAATourneySeeds.csv")
seed$Seed<-as.numeric(substr(as.character(seed$Seed),2,3))
a$round<-ifelse(a$DayNum<=135,0,ifelse(a$DayNum<=137,1,ifelse(a$DayNum<=139,2,ifelse(a$DayNum<=144,3,ifelse(a$DayNum<=146,4,ifelse(a$DayNum<=152,5,ifelse(a$DayNum<=154,6)))))))
round<-data.frame(cbind(a$Season,ifelse(a$WTeamID>a$LTeamID,a$LTeamID,a$WTeamID),ifelse(a$WTeamID>a$LTeamID,a$WTeamID,a$LTeamID),a$round))
colnames(round)<-c("Season","T1_TeamID","T2_TeamID","Round")
c<-read.csv("RegularSeasonDetailedResults.csv")
ken<-read.csv("KenPomMapped.csv",as.is=T)

########Conference Metrics Function########
#i<-1
#h<-Pappu[65,]
#k<-a
conf_metric<-function(h,k=a)
{
  k<-k[k$Season==(h$Season-1),]
  cround_int<-as.data.frame(rbind(cbind(k$Season,k$WTeamID,k$round+1),cbind(k$Season,k$LTeamID,k$round)))
  colnames(cround_int)<-c("Season","TeamID","round")
  f_cround<-cround_int%>% group_by(Season,TeamID) %>% summarize(round=max(round))
  h$T1_last_round<-unlist(ifelse(nrow(f_cround[f_cround$TeamID==h$T1_TeamID,"round"])==0,-10,f_cround[f_cround$TeamID==h$T1_TeamID,"round"]))
  h$T2_last_round<-unlist(ifelse(nrow(f_cround[f_cround$TeamID==h$T2_TeamID,"round"])==0,-10,f_cround[f_cround$TeamID==h$T2_TeamID,"round"]))
  return(h)
}
######## Season Metrics Function ############

regseasonmetrics<-function(x,b=c)
{
  x<-data.frame(t(x))
  #print(dim(x))
  thatseason<-b[b$Season==x$Season,]
  thatseason<-thatseason %>% mutate(Wstlto_ratio= WStl/WTO,
                        Lstlto_ratio= LStl/LTO,
                        Wconv_2p=(WFGM-WFGM3)/(WFGA-WFGA3),
                        Wconv_3p=WFGM3/WFGA3,
                        WConv_ft=WFTM/WFTA,
                        Wconv=WFGM/WFGA,
                        Lconv_2p=(LFGM-LFGM3)/(LFGA-LFGA3),
                        Lconv_3p=LFGM3/LFGA3,
                        LConv_ft=LFTM/LFTA,
                        Lconv=LFGM/LFGA,
                        Wper_2p=(WFGM-WFGM3)*2/WScore,
                        Wper_3p=WFGM3*3/WScore,
                        Wper_ft=WFTM/WScore,
                        Lper_2p=(LFGM-LFGM3)*2/LScore,
                        Lper_3p=LFGM3*3/LScore,
                        Lper_ft=LFTM/LScore,
                        WAst_ratio=(WAst * 100) / (WFGA + (WFTA * 0.44) + WAst + WTO),
                        LAst_ratio=(LAst * 100) / (LFGA + (LFTA * 0.44) + LAst + LTO),
                        Woff_rtg=(100*WScore)/(WFGA + WTO + (0.44*WFTA) - WOR),
                        Loff_rtg=100*LScore/(LFGA + LTO + (0.44*LFTA) - LOR),
                        Wdef_rtg=100*LScore/(LFGA + LTO + (0.44*LFTA) - LOR),
                        Ldef_rtg=100*WScore/(WFGA + WTO + (0.44*WFTA) - WOR),
                        Wrb_rtg=100*WOR/LDR,
                        Lrb_rtg=100*LOR/WDR,
                        Lposs=0.96*(LFGA + LTO + (0.44*LFTA) - LOR),
                        Wposs=0.96*(WFGA + WTO + (0.44*WFTA) - WOR)
                        )
  WWstat<-thatseason[thatseason$WTeamID==x$WTeamID,substr(colnames(thatseason),1,1)=="W"][,-c(3)] # NCAA winning regular season winning
  WLstat<-thatseason[thatseason$LTeamID==x$WTeamID,substr(colnames(thatseason),1,1)=="L"] # NCAA winning regular season loosing
  colnames(WWstat)<-substring(colnames(WWstat),2)
  colnames(WLstat)<-substring(colnames(WLstat),2)
  Wstat<-apply(rbind(WWstat,WLstat),2,mean)
  Wstat<-c(Wstat,nrow(WWstat)/(nrow(WWstat)+nrow(WLstat))) 
  names(Wstat)[length(Wstat)]<-"Twin_perc"
  
  LWstat<-thatseason[thatseason$WTeamID==x$LTeamID,substr(colnames(thatseason),1,1)=="W"][,-c(3)]
  LLstat<-thatseason[thatseason$LTeamID==x$LTeamID,substr(colnames(thatseason),1,1)=="L"]
  colnames(LWstat)<-substring(colnames(LWstat),2)
  colnames(LLstat)<-substring(colnames(LLstat),2)
  Lstat<-apply(rbind(LWstat,LLstat),2,mean)
  Lstat<-c(Lstat,nrow(LWstat)/(nrow(LWstat)+nrow(LLstat)))
  names(Lstat)[length(Lstat)]<-"Twin_perc"
  #print(Lstat)
  if(Lstat["TeamID"]<Wstat["TeamID"]){
    Fstat<-c(Lstat,Wstat,0)
  }else
  {
    Fstat<-c(Wstat,Lstat,1)
  }
  
  return(Fstat)
  
}

#x<-a[1,]
#b=c
colnames(ken)
ken<-ken[,-c(1,2,4,15)]
colnames(ken)<-paste0("W",colnames(ken))
c<-merge(c,ken,by.x=c("Season","WTeamID"),by.y=c("WSeason","Wmap"),all.x=T)
colnames(ken)<-paste0("L",substring(colnames(ken),2,99))
c<-merge(c,ken,by.x=c("Season","LTeamID"),by.y=c("LSeason","Lmap"),all.x=T)

Pappu=apply(a,1,regseasonmetrics)
p<-Pappu
#Pappu<-p
Pappu<-as.data.frame(t(Pappu))
colnames(Pappu)<-c(paste0("T1_",colnames(Pappu)[1:38]),paste0("T2_",colnames(Pappu)[39:76]),"Result")
Pappu<-cbind(a$Season,Pappu)
colnames(Pappu)[1]<-"Season"

temp<-NULL
for(i in 1:nrow(Pappu))
{
  temp<-rbind(temp,conf_metric(Pappu[i,]))
}
Pappu<-temp





t1_seed<-merge(Pappu,seed,by.x=c("Season","T1_TeamID"),by.y=c("Season","TeamID"))[,c("Season","T2_TeamID","T1_TeamID","Seed")]
colnames(t1_seed)[4]<-"T1_Seed"
t2_seed<-merge(Pappu,seed,by.x=c("Season","T2_TeamID"),by.y=c("Season","TeamID"))[,c("Season","T2_TeamID","T1_TeamID","Seed")]
colnames(t2_seed)[4]<-"T2_Seed"

both_seed<-merge(t1_seed,t2_seed,by=c("Season","T2_TeamID","T1_TeamID"))
head(both_seed)

#both_seed$seed_diff<-both_seed$Seed_1-both_seed$Seed_2
#both_seed$Seed_1<-NULL
#both_seed$Seed_2<-NULL
Pappu<-merge(Pappu,both_seed,by=c("Season","T2_TeamID","T1_TeamID"))






######Massey Rankings######################################
massey <- read.csv("MasseyOrdinals_updates.csv", stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
massey_med <- massey %>% group_by(Season, RankingDayNum, TeamID) %>% summarise(med_rank <- median(OrdinalRank))
colnames(massey_med)[4] <- "MedRank"

massey_med_spread <- massey_med %>% spread(key = RankingDayNum,value = MedRank)

massey_med_spread <- data.frame(massey_med_spread)
colnames(massey_med_spread)
sd_func <- function(x)
{
  #print(x)
  #x<-t(x)
  SdRank <- sd(x,na.rm = TRUE)
  MedRank <- mean(x, na.rm = TRUE)
  #print(names(x))
  LastRank <- x[which(names(x) == "X133")]
  MedRank10 <- mean(x[(which(names(x) == "X118"):which(names(x) == "X133"))], na.rm = TRUE)
  MaxRank<- max(x, na.rm = TRUE)
  MinRank<-min(x, na.rm = TRUE)
  RankImproveAll<-x[which(names(x) == "X133")]-x[which(names(x) == "X0")]
  RankImproveHalf<-x[which(names(x) == "X133")]-x[which(names(x) == "X69")]
  return(c(SdRank, MedRank, LastRank, MedRank10,MaxRank,MinRank,RankImproveAll,RankImproveHalf))
}
rank_vec <- apply(massey_med_spread[,-c(1,2)], 1, sd_func)
#x<-massey_med_spread[1,-c(1,2)]
rank_vec <- data.frame(t(rank_vec))
massey_rank <- cbind(massey_med_spread[,c("Season","TeamID")],rank_vec)
#new_var <- c("SdRank","MedRank","LastRank","MedRank10")
colnames(massey_rank)[3:10] <- c("SdRank","MeanRank","LastRank","MeanRank10","MaxRank","MinRank","RankImproveAll","RankImproveHalf")


Pappu <- merge(Pappu,massey_rank,by.x=c("Season","T1_TeamID"),by.y=c("Season","TeamID"))
colnames(Pappu)[colnames(Pappu) %in% c("SdRank","MeanRank","LastRank","MeanRank10","MaxRank","MinRank","RankImproveAll","RankImproveHalf")]<-paste0("T1_",colnames(Pappu)[colnames(Pappu) %in% c("SdRank","MeanRank","LastRank","MeanRank10","MaxRank","MinRank","RankImproveAll","RankImproveHalf")])
Pappu <- merge(Pappu,massey_rank,by.x=c("Season","T2_TeamID"),by.y=c("Season","TeamID"))
colnames(Pappu)[colnames(Pappu) %in% c("SdRank","MeanRank","LastRank","MeanRank10","MaxRank","MinRank","RankImproveAll","RankImproveHalf")]<-paste0("T2_",colnames(Pappu)[colnames(Pappu) %in% c("SdRank","MeanRank","LastRank","MeanRank10","MaxRank","MinRank","RankImproveAll","RankImproveHalf")])
Pappu<-merge(Pappu,round,by=c("Season","T1_TeamID","T2_TeamID"))

Train<-Pappu[Pappu$Season<=2017,!names(Pappu) %in% c("Season","T1_TeamID","T2_TeamID")]
Train$Result<-as.factor(Train$Result)
Train1<-Train[,substr(colnames(Train),1,3)=="T1_"]-Train[,substr(colnames(Train),1,3)=="T2_"]
Train1$Result<-Train$Result
#Train1$seed_diff<-Train$seed_diff
#Train1$round<-Train$Round

library(h2o)

h2o.init()

Train1.h2o <-as.h2o(Train1)
y<-"Result"
#x<-colnames(Train1)[-c(35:37)]
x<-colnames(Train1)[!colnames(Train1) %in% c("T1_RankImproveAll","T1_RankImproveHalf","Result",
                                             "round")]
#x<-colnames(Train1)[-c(46)]


m2<-h2o.automl(x=x,y=y,training_frame = Train1.h2o,max_runtime_secs = 150,stopping_metric = "logloss")
m2@leaderboard






















Test<-Pappu[Pappu$Season>2017,!names(Pappu) %in% c("Season","T1_TeamID","T2_TeamID")]
Test$Result<-as.factor(Test$Result)
Test1<-Test[,substr(colnames(Test),1,3)=="T1_"]-Test[,substr(colnames(Test),1,3)=="T2_"]
#Test1$Result<-Test$Result
#Train1$seed_diff<-Train$seed_diff
#Test1$round<-Test$Round
Test1.h2o<-as.h2o(Test1)


pred<-h2o.predict(m2@leader,Test1.h2o)
pred1<-as.data.frame(pred)

-sum(Test$Result*log(pred1[,3])+(1-Test$Result)*log(1-pred1[,3]))/length(Test$Result)

my_1st_sub<-as.data.frame(cbind(paste(Pappu$Season[Pappu$Season>2017],Pappu$T1_TeamID[Pappu$Season>2017],Pappu$T2_TeamID[Pappu$Season>2017],sep="_"),pred1[,3]),stringsAsFactors = F)
colnames(my_1st_sub)<-c("ID","Pred")
#sample_sub<-read.csv("SampleSubmissionStage1.csv",as.is=T)
#sub_f<-merge(sample_sub,my_1st_sub,by="ID",all.x=T)
#sub_f$Pred.y<-as.character(sub_f$Pred.y)
#sub_f$Pred<-ifelse(is.na(sub_f$Pred.y),sub_f$Pred.x,sub_f$Pred.y)
#sub_f$Pred.x<-NULL
#sub_f$Pred.y<-NULL
sub_f<-my_1st_sub
sub_f$Pred<-as.numeric(as.chasub_f$Pred)
write.csv(sub_f, "WithKenPom.csv", row.names = FALSE)
