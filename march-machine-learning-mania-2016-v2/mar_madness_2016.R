#set seed 
set.seed(1)

#player rating
#install.packages("PlayerRatings")

#set working directory
setwd("C:/Users/Stephen Harder/Desktop/kaggle 2015/MarchMadness/march-machine-learning-mania-2016-v2")

require("data.table")
require("PlayerRatings")
require("ggplot2")

#Read Files----------
seasonCompact <- fread("RegularSeasonCompactResults.csv")
teams <- fread(file.path("Teams.csv"))
sampleSubmission <- fread("SampleSubmission.csv")

#Elo rating for regular seasons----------
#teams in seasons since 2012 'till 2015
allSeasons <- seq(2012, 2016)

eloEndOfSeasonList <- lapply(allSeasons, function(season2extractInfo){
  #season2extractInfo <- 2015 #here for debbugging only
  seasonDataDt <- seasonCompact[Season == season2extractInfo, .(Daynum, Wteam, Lteam, Wloc)]
  resultVector <- rep(1, nrow(seasonDataDt))
  advantageVector <- as.numeric(seasonDataDt$Wloc == "H")
  seasonDataDf <- data.frame(yearDay = seasonDataDt$Daynum,
                             tid1 = seasonDataDt$Wteam, 
                             tid2 = seasonDataDt$Lteam, 
                             result = resultVector)
  EloRatings <- elo(x = seasonDataDf, gamma = advantageVector)
  EloRatingsDt <- as.data.table(EloRatings$ratings)
  
  return(EloRatingsDt)
})

names(eloEndOfSeasonList) <- allSeasons
print("Elo ratings extracted")

#Matches Information Extraction---------
matches2Predict <- lapply(sampleSubmission$Id, function(submissionIds){
  #submissionIds <- sampleSubmission$Id[1]
  matchesInfo <- strsplit(submissionIds, "_")[[1]]
  return(as.numeric(matchesInfo))
})
matches2PredictDt <- as.data.table(do.call(rbind, matches2Predict))
setnames(matches2PredictDt, names(matches2PredictDt), c("Season", "Team1", "Team2"))

eloMatrix <- apply(matches2PredictDt, 1, function(matchInformation){
  #matchInformation <- matches2PredictDt[2] #here for debugging
  season <- matchInformation[["Season"]]
  team1 <- matchInformation[["Team1"]]
  team2 <- matchInformation[["Team2"]]
  
  #Seeds table search 
  seasonMatrix <- eloEndOfSeasonList[[as.character(season)]]
  eloTeam1 <- as.numeric(seasonMatrix[Player == team1, Rating])
  eloTeam2 <- as.numeric(seasonMatrix[Player == team2, Rating])
  
  return(eloTeam1 - eloTeam2)
})

#Rating difference to probability
elo2Prob <- function(elo, CValue){
  #elo to probability transformation
  probabilityOfVictory <- 1 / (1 + 10 ^ (-(elo)/CValue))
  return(probabilityOfVictory)
}

eloProbabilites <- elo2Prob(eloMatrix, CValue = 200)    #C value and formula based on https://fivethirtyeight.com/datalab/introducing-nfl-elo-ratings/
print("Elo ratings difference transformed to probability")

#Plot the top 10 teams according to Elo's system
top10Elo2012 <- head(as.data.frame(eloEndOfSeasonList[["2012"]][, .(Player, Rating)]), 10)
top10Elo2012 <- merge(top10Elo2012, teams, by.x = "Player", by.y = "Team_Id")

top10Elo2013 <- head(as.data.frame(eloEndOfSeasonList[["2013"]][, .(Player, Rating)]), 10)
top10Elo2013 <- merge(top10Elo2013, teams, by.x = "Player", by.y = "Team_Id")

top10Elo2014 <- head(as.data.frame(eloEndOfSeasonList[["2014"]][, .(Player, Rating)]), 10)
top10Elo2014 <- merge(top10Elo2014, teams, by.x = "Player", by.y = "Team_Id")

top10Elo2015 <- head(as.data.frame(eloEndOfSeasonList[["2015"]][, .(Player, Rating)]), 10)
top10Elo2015 <- merge(top10Elo2015, teams, by.x = "Player", by.y = "Team_Id")

top10Elo2016 <- head(as.data.frame(eloEndOfSeasonList[["2016"]][, .(Player, Rating)]), 10)
top10Elo2016 <- merge(top10Elo2016, teams, by.x = "Player", by.y = "Team_Id")

#Plot top 10 in 2012
ggplot(top10Elo2012, aes(x = Team_Name, y = Rating)) +
  geom_bar(stat = "identity", width = 0.5, fill="#6699FF") + coord_cartesian(ylim = c(2300, 2525)) +
  ggtitle("Top 10 Elo ratings before tourney in NCAA basketball \n 2012")

#Plot top 10 in 2013
ggplot(top10Elo2013, aes(x = Team_Name, y = Rating)) +
  geom_bar(stat = "identity", width = 0.5, fill="#6699FF") + coord_cartesian(ylim = c(2300, 2525)) +
  ggtitle("Top 10 Elo ratings before tourney in NCAA basketball \n 2013")

#Plot top 10 in 2014
ggplot(top10Elo2014, aes(x = Team_Name, y = Rating)) +
  geom_bar(stat = "identity", width = 0.5, fill="#6699FF") + coord_cartesian(ylim = c(2300, 2525)) +
  ggtitle("Top 10 Elo ratings before tourney in NCAA basketball \n 2014")

#Plot top 10 in 2015
ggplot(top10Elo2015, aes(x = Team_Name, y = Rating)) +
  geom_bar(stat = "identity", width = 0.5, fill="#6699FF") + coord_cartesian(ylim = c(2300, 2525)) +
  ggtitle("Top 10 Elo ratings before tourney in NCAA basketball \n 2015")

#Plot top 10 in 2016
ggplot(top10Elo2016, aes(x = Team_Name, y = Rating)) +
  geom_bar(stat = "identity", width = 0.5, fill="#6699FF") + coord_cartesian(ylim = c(2300, 2525)) +
  ggtitle("Top 10 Elo ratings before tourney in NCAA basketball \n 2015")

#Plot distributions of elo differences and elo differences transformed to probabilities
hist(eloMatrix)
hist(eloProbabilites)

#Write a .csv file with results
sampleSubmission$Pred <- eloProbabilites
write.csv(sampleSubmission, "EloBenchmark.csv", row.names = FALSE)



#Different model 
setwd("C:/Users/Stephen Harder/Desktop/kaggle 2015/MarchMadness/march-machine-learning-mania-2016-v2/march-machine-learning-mania-2016-v2")
#Uses Bradley-Terry  
#Eliminate previous work - clear the data tables from memory
rm(list=ls())
#load in data.table dplyr and reshape 
library(data.table); library(dplyr); library(reshape)

#Load in the main data tables 
TourneySeeds <- fread("TourneySeeds.csv")
SampleSubmission <- fread("SampleSubmission.csv")
Seasons <- fread("Seasons.csv")
Teams <- fread("Teams.csv")
TourneySlots <- fread("TourneySlots.csv")
TourneyDetailedResults <- fread("TourneyDetailedResults.csv")
TourneyCompactResults <- fread("TourneyCompactResults.csv")
seasonResults<-fread("RegularSeasonDetailedResults.csv")
seasonCompactResults<-fread("RegularSeasonCompactResults.csv")

#Turn the seasonResults into a stacked database 
#first, choose the main variables
#Separate them into two different data files
winData<-select(seasonResults,Season,Wteam,Wfgm,Wfga,Wfgm3,Wfga3,Wfta,Wor,Wdr,Wast,Wto,Wstl,Wblk,Wpf)
loseData<-select(seasonResults,Season,Lteam,Lfgm,Lfga,Lfgm3,Lfga3,Lfta,Lor,Ldr,Last,Lto,Lstl,Lblk,Lpf)

#Rename all columns in win and lose data 
#Eventually we will rbind these two data sets 
colnames(winData)[2:14] <- c("team","fgm","fga","fgm3","fga3","fta","or","dr","ast","to","stl","blk","pf")
colnames(loseData)[2:14] <- c("team","fgm","fga","fgm3","fga3","fta","or","dr","ast","to","stl","blk","pf")

#group winData by season and team
#summarise all data columns by season and team
#duplicate group and summarise  for loseData
grouped_win<-group_by(winData,Season,team)%>%summarise(fgm=sum(fgm),fga=sum(fga),fgm3=sum(fgm3),fga3=sum(fga3),fta=sum(fta),or=sum(or),dr=sum(dr),ast=sum(ast),to=sum(to),stl=sum(stl),blk=sum(blk),pf=sum(pf),n=n())
grouped_lose<-group_by(loseData,Season,team)%>%summarise(fgm=sum(fgm),fga=sum(fga),fgm3=sum(fgm3),fga3=sum(fga3),fta=sum(fta),or=sum(or),dr=sum(dr),ast=sum(ast),to=sum(to),stl=sum(stl),blk=sum(blk),pf=sum(pf),n=n())
#stack winData and loseData 
totalSeason <- rbind(grouped_win, grouped_lose)

#group and summarise new data set 
totalSeason<-group_by(totalSeason,Season,team)%>%summarise(fgm=sum(fgm),fga=sum(fga),fgm3=sum(fgm3),fga3=sum(fga3),fta=sum(fta),or=sum(or),dr=sum(dr),ast=sum(ast),to=sum(to),stl=sum(stl),blk=sum(blk),pf=sum(pf),n=sum(n))

#Begin setting up final prediction file 
TourneySeeds <- TourneySeeds %>% 
  mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)



games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))   

temp <- left_join(games.to.predict, TourneySeeds, by=c("season"="Season", "team1"="Team"))
games.to.predict <- left_join(temp, TourneySeeds, by=c("season"="Season", "team2"="Team"))
colnames(games.to.predict)[c(1,5:6)] <- c("Id", "team1seed", "team2seed")
games.to.predict <- games.to.predict %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))

#Add totalSeason data into games.to.predict file 
temp <- left_join(games.to.predict, totalSeason, by=c("season"="Season", "team1"="team"))
games.to.predict <- left_join(temp, totalSeason, by=c("season"="Season", "team2"="team"))
#We could rename the columns but we might not have to so well leave this alone
#colnames(games.to.predict)[c(1,5:6)] <- c("Id", "team1seed", "team2seed")
#games.to.predict <- games.to.predict %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))

#Now we make the file for the entire seasons since 2003
temp <- left_join(as.data.frame(TourneyCompactResults), totalSeason, by=c("Season", "Wteam"="team"))
compact.results <- left_join(temp, totalSeason, by=c("Season", "Lteam"="team"))
compact.results<-filter(compact.results,Season>=2003)

#repeat for seeds
temp <- left_join(as.data.frame(compact.results), TourneySeeds, by=c("Season", "Wteam"="Team"))
compact.results <- left_join(temp, TourneySeeds, by=c("Season", "Lteam"="Team"))

#Now
set1 <- compact.results %>% select(9:36) %>% mutate(result=1)
set2 <- compact.results %>% select(9:36) %>% mutate(result=0)
colnames(set1)[27:29] <- c("team1seed", "team2seed", "team1win")
colnames(set2)[27:29] <- c("team1seed", "team2seed", "team1win")
#colnames(set1) <- c("team1seed", "team2seed", "team1win")
colnames(set2) <- c("fgm.y","fga.y","fgm3.y","fga3.y","fta.y","or.y","dr.y","ast.y","to.y","stl.y","blk.y","pf.y","n.y","fgm.x",    
                    "fga.x",     "fgm3.x",    "fga3.x",    "fta.x",     "or.x",      "dr.x",      "ast.x",    
                    "to.x",      "stl.x",     "blk.x",     "pf.x",      "n.x",       "team2seed", "team1seed",
                    "team1win")
full.set <- rbind(set1, set2)
full.set <- full.set %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))
#duplicate full set 
games.to.predict2<-games.to.predict

#review
library(rattle)
rattle()

#build GLM
m.logistic.seed.diff <- glm(team1win~., data=full.set, family=binomial())
coef(m.logistic.seed.diff)
games.to.predict$Pred <- predict(m.logistic.seed.diff, games.to.predict, type="response")
write.csv(games.to.predict %>% select(Id, Pred), 'logistic_ALL_submission.csv', row.names=FALSE)

#build RandomForest
library(randomForest)
m.randomForest<-randomForest(team1win~., data=full.set, importance=TRUE, proximity=TRUE)
games.to.predict2$Pred <- predict(m.randomForest,games.to.predict2, type="response")
write.csv(games.to.predict2 %>% select(Id, Pred), 'logistic_RF_ALL_submission.csv', row.names=FALSE)


#Tree Model 
tree<-rpart(team1win~., data=full.set, method = "class")
games.to.predict2$Pred <- predict(tree,games.to.predict2, type="prob")
write.csv(games.to.predict2 %>% select(Id, Pred), 'tree_submission.csv', row.names=FALSE)






#Random Forest Model 
library(dplyr)
library(data.table)
library(xgboost)
#install.packages("Metrics")
library(Metrics)

# Read CSV files
RegularSeasonDetailedResults <- read.csv('RegularSeasonDetailedResults.csv',stringsAsFactors=FALSE)
SampleSubmission <- read.csv('SampleSubmission.csv',stringsAsFactors=FALSE)
Seasons <- read.csv('Seasons.csv',stringsAsFactors=FALSE)
Teams <- read.csv('Teams.csv',stringsAsFactors=FALSE)
TourneyDetailedResults <- read.csv('TourneyDetailedResults.csv',stringsAsFactors=FALSE)
TourneySeeds <- read.csv('TourneySeeds.csv',stringsAsFactors=FALSE)
TourneySlots <- read.csv('TourneySlots.csv',stringsAsFactors=FALSE)

RegularSeasonDetailedResults = filter(RegularSeasonDetailedResults,Season>=2012)
TourneyDetailedResults = filter(TourneyDetailedResults,Season>=2012)
TourneySeeds = filter(TourneySeeds,Season>=2012)
TourneySlots = filter(TourneySlots,Season>=2012)

RegularSeasonStats.W=
  summarize(
    group_by(RegularSeasonDetailedResults,Season,Team=Wteam),
    matches=n(),
    C_wins=n(),
    C_score=sum(Wscore),
    C_fgm=sum(Wfgm),
    C_fga=sum(Wfga), 
    C_fgm3=sum(Wfgm3), 
    C_fga3=sum(Wfga3), 
    C_ftm=sum(Wftm), 
    C_fta=sum(Wfta), 
    C_or=sum(Wor), 
    C_dr=sum(Wdr), 
    C_ast=sum(Wast), 
    C_to=sum(Wto), 
    C_stl=sum(Wstl), 
    C_blk=sum(Wblk), 
    C_pf=sum(Wpf), 
    O_wins=0,
    O_score=sum(Lscore),
    O_fgm=sum(Lfgm), 
    O_fga=sum(Lfga), 
    O_fgm3=sum(Lfgm3), 
    O_fga3=sum(Lfga3), 
    O_ftm=sum(Lftm), 
    O_fta=sum(Lfta), 
    O_or=sum(Lor), 
    O_dr=sum(Ldr), 
    O_ast=sum(Last),
    O_to=sum(Lto), 
    O_stl=sum(Lstl), 
    O_blk=sum(Lblk), 
    O_pf=sum(Lpf)  
  )

RegularSeasonStats.L=
  summarize(
    group_by(RegularSeasonDetailedResults,Season,Team=Lteam),
    matches=n(),   
    C_wins=0,
    C_score=sum(Lscore),    
    C_fgm=sum(Lfgm),
    C_fga=sum(Lfga), 
    C_fgm3=sum(Lfgm3), 
    C_fga3=sum(Lfga3), 
    C_ftm=sum(Lftm), 
    C_fta=sum(Lfta), 
    C_or=sum(Lor), 
    C_dr=sum(Ldr), 
    C_ast=sum(Last), 
    C_to=sum(Lto), 
    C_stl=sum(Lstl), 
    C_blk=sum(Lblk), 
    C_pf=sum(Lpf), 
    O_wins=n(),
    O_score=sum(Wscore),        
    O_fgm=sum(Wfgm), 
    O_fga=sum(Wfga), 
    O_fgm3=sum(Wfgm3), 
    O_fga3=sum(Wfga3), 
    O_ftm=sum(Wftm), 
    O_fta=sum(Wfta), 
    O_or=sum(Wor), 
    O_dr=sum(Wdr), 
    O_ast=sum(Wast),
    O_to=sum(Wto), 
    O_stl=sum(Wstl), 
    O_blk=sum(Wblk), 
    O_pf=sum(Wpf)  
  )

RegularSeasonStats=rbind(RegularSeasonStats.W,RegularSeasonStats.L)
RegularSeasonStats=summarize(group_by(RegularSeasonStats,Season,Team),
                             matches=sum(matches),  
                             C_winsP=mean(C_wins),
                             C_score=sum(C_score),
                             C_fgm=sum(C_fgm),
                             C_fga=sum(C_fga), 
                             C_fgm3=sum(C_fgm3), 
                             C_fga3=sum(C_fga3), 
                             C_ftm=sum(C_ftm), 
                             C_fta=sum(C_fta), 
                             C_or=sum(C_or), 
                             C_dr=sum(C_dr), 
                             C_ast=sum(C_ast), 
                             C_to=sum(C_to), 
                             C_stl=sum(C_stl), 
                             C_blk=sum(C_blk), 
                             C_pf=sum(C_pf), 
                             O_winsP=mean(O_wins),  
                             O_score=sum(O_score), 
                             O_fgm=sum(O_fgm), 
                             O_fga=sum(O_fga), 
                             O_fgm3=sum(O_fgm3), 
                             O_fga3=sum(O_fga3), 
                             O_ftm=sum(O_ftm), 
                             O_fta=sum(O_fta), 
                             O_or=sum(O_or), 
                             O_dr=sum(O_dr), 
                             O_ast=sum(O_ast),
                             O_to=sum(O_to), 
                             O_stl=sum(O_stl), 
                             O_blk=sum(O_blk), 
                             O_pf=sum(O_pf)    
)

RegularSeasonStats$C_score_M=RegularSeasonStats$C_score/RegularSeasonStats$matches
RegularSeasonStats$C_fgm_M=RegularSeasonStats$C_fgm/RegularSeasonStats$matches
RegularSeasonStats$C_fga_M=RegularSeasonStats$C_fga/RegularSeasonStats$matches
RegularSeasonStats$C_fgm3_M=RegularSeasonStats$C_fgm3/RegularSeasonStats$matches
RegularSeasonStats$C_fga3_M=RegularSeasonStats$C_fga3/RegularSeasonStats$matches
RegularSeasonStats$C_ftm_M=RegularSeasonStats$C_ftm/RegularSeasonStats$matches
RegularSeasonStats$C_fta_M=RegularSeasonStats$C_fta/RegularSeasonStats$matches
RegularSeasonStats$C_or_M=RegularSeasonStats$C_or/RegularSeasonStats$matches
RegularSeasonStats$C_dr_M=RegularSeasonStats$C_dr/RegularSeasonStats$matches
RegularSeasonStats$C_ast_M=RegularSeasonStats$C_ast/RegularSeasonStats$matches
RegularSeasonStats$C_to_M=RegularSeasonStats$C_to/RegularSeasonStats$matches
RegularSeasonStats$C_stl_M=RegularSeasonStats$C_stl/RegularSeasonStats$matches
RegularSeasonStats$C_blk_M=RegularSeasonStats$C_blk/RegularSeasonStats$matches
RegularSeasonStats$C_pf_M=RegularSeasonStats$C_pf/RegularSeasonStats$matches
RegularSeasonStats$O_score_M=RegularSeasonStats$O_score/RegularSeasonStats$matches
RegularSeasonStats$O_fgm_M=RegularSeasonStats$O_fgm/RegularSeasonStats$matches
RegularSeasonStats$O_fga_M=RegularSeasonStats$O_fga/RegularSeasonStats$matches
RegularSeasonStats$O_fgm3_M=RegularSeasonStats$O_fgm3/RegularSeasonStats$matches
RegularSeasonStats$O_fga3_M=RegularSeasonStats$O_fga3/RegularSeasonStats$matches
RegularSeasonStats$O_ftm_M=RegularSeasonStats$O_ftm/RegularSeasonStats$matches
RegularSeasonStats$O_fta_M=RegularSeasonStats$O_fta/RegularSeasonStats$matches
RegularSeasonStats$O_or_M=RegularSeasonStats$O_or/RegularSeasonStats$matches
RegularSeasonStats$O_dr_M=RegularSeasonStats$O_dr/RegularSeasonStats$matches
RegularSeasonStats$O_ast_M=RegularSeasonStats$O_ast/RegularSeasonStats$matches
RegularSeasonStats$O_to_M=RegularSeasonStats$O_to/RegularSeasonStats$matches
RegularSeasonStats$O_stl_M=RegularSeasonStats$O_stl/RegularSeasonStats$matches
RegularSeasonStats$O_blk_M=RegularSeasonStats$O_blk/RegularSeasonStats$matches
RegularSeasonStats$O_pf_M=RegularSeasonStats$O_pf/RegularSeasonStats$matches
RegularSeasonStats$C_fgm_P=RegularSeasonStats$C_fgm/RegularSeasonStats$C_fga
RegularSeasonStats$C_fgm3_P=RegularSeasonStats$C_fgm3/RegularSeasonStats$C_fga3
RegularSeasonStats$C_ftm_P=RegularSeasonStats$C_ftm/RegularSeasonStats$C_fta
RegularSeasonStats$C_or_P2=RegularSeasonStats$C_or/(RegularSeasonStats$C_or+RegularSeasonStats$C_dr)
RegularSeasonStats$C_or_P=RegularSeasonStats$C_or/RegularSeasonStats$C_fga
RegularSeasonStats$C_stl_P=RegularSeasonStats$C_stl/RegularSeasonStats$C_fga
RegularSeasonStats$C_blk_P=RegularSeasonStats$C_blk/RegularSeasonStats$C_fga
RegularSeasonStats$O_fgm_P=RegularSeasonStats$O_fgm/RegularSeasonStats$O_fga
RegularSeasonStats$O_fgm3_P=RegularSeasonStats$O_fgm3/RegularSeasonStats$O_fga3
RegularSeasonStats$O_ftm_P=RegularSeasonStats$O_ftm/RegularSeasonStats$O_fta
RegularSeasonStats$O_or_P2=RegularSeasonStats$O_or/(RegularSeasonStats$O_or+RegularSeasonStats$O_dr)
RegularSeasonStats$O_or_P=RegularSeasonStats$O_or/RegularSeasonStats$O_fga
RegularSeasonStats$O_stl_P=RegularSeasonStats$O_stl/RegularSeasonStats$O_fga
RegularSeasonStats$O_blk_P=RegularSeasonStats$O_blk/RegularSeasonStats$O_fga

matches2Predict <- lapply(SampleSubmission$Id, function(submissionIds){
  matchesInfo <- strsplit(submissionIds, "_")[[1]]
  return(as.numeric(matchesInfo))
})
matches2PredictDt <- as.data.table(do.call(rbind, matches2Predict))
setnames(matches2PredictDt, names(matches2PredictDt), c("Season", "Team1", "Team2"))

matches2PredictDt

RegularSeasonStats=
  RegularSeasonStats[,!names(RegularSeasonStats) %in%
                       c('matches', 'C_score', 'C_fgm', 'C_fga', 'C_fgm3', 'C_fga3', 
                         'C_ftm', 'C_fta', 'C_or', 'C_dr', 'C_ast', 'C_to', 'C_stl', 
                         'C_blk', 'C_pf', 'O_score', 'O_fgm', 'O_fga', 'O_fgm3', 
                         'O_fga3', 'O_ftm', 'O_fta', 'O_or', 'O_dr', 'O_ast', 'O_to',
                         'O_stl', 'O_blk', 'O_pf')]

RegularSeasonStats.T1=RegularSeasonStats
RegularSeasonStats.T2=RegularSeasonStats
names(RegularSeasonStats.T1)=paste0('T1.',names(RegularSeasonStats.T1))
names(RegularSeasonStats.T2)=paste0('T2.',names(RegularSeasonStats.T2))

matches2PredictDt=as.data.frame(matches2PredictDt)
data=left_join(matches2PredictDt,RegularSeasonStats.T1,by=c('Season'='T1.Season','Team1'='T1.Team'))
data=left_join(data,RegularSeasonStats.T2,by=c('Season'='T2.Season','Team2'='T2.Team'))

getSeedDivision <- function(seedsInfo){
  #Seed & Division 
  #This function gets the seed and division of a team in a given season
  #Input class == "numeric" corresponding to the season of the tournament and the team unique ID
  #Returns class == "character" corresponding to the seed in that season and the division assigned in the tourney
  #seedsInfo <- tourneySeeds[1] #here for debugging
  
  seasonFromData <- seedsInfo[["Season"]]
  seedAndDivision <- seedsInfo[["Seed"]]
  teamFromData <- seedsInfo[["Team"]]
  
  seedTeam <- gsub(pattern = "[A-Z+a-z]", replacement = "", x = seedAndDivision)
  divisionTeam <- gsub(pattern = "[0-9]", replacement = "", x = seedAndDivision)
  playin <- gsub(pattern = "[A-Z+0-9]", replacement = "", x = divisionTeam) 
  playin <- ifelse(playin!='',1,0)
  #clean the extra letters
  divisionTeam <- gsub(pattern = "[a-z]", replacement = "", x = divisionTeam)  
  
  
  return(c(seasonFromData, teamFromData, seedTeam, divisionTeam, playin))
}

seedsAndDivisionsMatrix <- as.data.frame(t(apply(TourneySeeds, 1, getSeedDivision)),stringsAsFactors=FALSE)


names(seedsAndDivisionsMatrix)=c('Season','Team','Seed','Division','PlayinMatch')
seedsAndDivisionsMatrix$Season=as.numeric(seedsAndDivisionsMatrix$Season)
seedsAndDivisionsMatrix$Team=as.numeric(seedsAndDivisionsMatrix$Team)
seedsAndDivisionsMatrix$Seed=as.numeric(seedsAndDivisionsMatrix$Seed)
seedsAndDivisionsMatrix$PlayinMatch=as.numeric(seedsAndDivisionsMatrix$PlayinMatch)

seedsAndDivisionsMatrix.T1=seedsAndDivisionsMatrix
seedsAndDivisionsMatrix.T2=seedsAndDivisionsMatrix
names(seedsAndDivisionsMatrix.T1)=paste0('T1.',names(seedsAndDivisionsMatrix.T1))
names(seedsAndDivisionsMatrix.T2)=paste0('T2.',names(seedsAndDivisionsMatrix.T2))

data=left_join(data,seedsAndDivisionsMatrix.T1,by=c('Season'='T1.Season','Team1'='T1.Team'))
data=left_join(data,seedsAndDivisionsMatrix.T2,by=c('Season'='T2.Season','Team2'='T2.Team'))


data$same_div=ifelse(data$T1.Division==data$T2.Division,1,0)
data$Seed_diff=data$T2.Seed-data$T1.Seed


RegularSeasonDetailedResults.scores=
  data.frame(
    Season=RegularSeasonDetailedResults$Season,
    Team1=ifelse(RegularSeasonDetailedResults$Wteam<RegularSeasonDetailedResults$Lteam,RegularSeasonDetailedResults$Wteam,RegularSeasonDetailedResults$Lteam),
    Team2=ifelse(RegularSeasonDetailedResults$Wteam<RegularSeasonDetailedResults$Lteam,RegularSeasonDetailedResults$Lteam,RegularSeasonDetailedResults$Wteam),
    Rwin=ifelse(RegularSeasonDetailedResults$Wteam<RegularSeasonDetailedResults$Lteam,1,0)
  )
RegularSeasonDetailedResults.scores.stats=
  summarize(group_by(RegularSeasonDetailedResults.scores,Season,Team1,Team2),Rwin_P=mean(Rwin))


data=left_join(data,RegularSeasonDetailedResults.scores.stats,by=c('Season'='Season','Team1'='Team1','Team2'='Team2'))
data$Rwin_P[is.na(data$Rwin_P)]=-1


feature.names=names(data)[c(1,4:dim(data)[2])]
feature.names=setdiff(feature.names,c("T1.PlayinMatch","T2.PlayinMatch"))


for (f in feature.names) {
  if (class(data[[f]])=="character") {
    levels <- unique(data[[f]])
    data[[f]] <- as.integer(factor(data[[f]], levels=levels))
  }
}

TourneyDetailedResults.data=
  data.frame(Season=TourneyDetailedResults$Season,
             Team1=ifelse(TourneyDetailedResults$Wteam<TourneyDetailedResults$Lteam,TourneyDetailedResults$Wteam,TourneyDetailedResults$Lteam),
             Team2=ifelse(TourneyDetailedResults$Wteam<TourneyDetailedResults$Lteam,TourneyDetailedResults$Lteam,TourneyDetailedResults$Wteam),
             Win=ifelse(TourneyDetailedResults$Wteam<TourneyDetailedResults$Lteam,1,0)
  )

data=left_join(data,TourneyDetailedResults.data,by=c('Season'='Season','Team1'='Team1','Team2'='Team2'))


#play-in games: result is not of interest in final scoring
data$Win[!is.na(data$Win) & data$T1.PlayinMatch==1 & data$T2.PlayinMatch==1]=NA



train=data[!is.na(data$Win),]
test=data[is.na(data$Win),]

dtrain<-xgb.DMatrix(data=data.matrix(train[,feature.names]),label=train$Win)
dtest<-xgb.DMatrix(data=data.matrix(test[,feature.names]))

# Select model parameters
param<-list(
  eta = 0.005,
  max_depth = 10,
  objective = "binary:logistic", 
  eval_metric = "logloss",
  colsample_bytree=0.21,
  min_child_weight=8,
  max_delta_step=1,
  lambda=0.9,
  alpha=0.05,
  gamma=1,
  base_score=0.37, # Bias for final score
  subsample=0.8
)

# Model training
set.seed(1)
xgb.model.cv<-
  xgb.cv(
    params=param,
    data=dtrain,
    nrounds=889, # Optimum n. of rounds
    nfold=2,    # CV folds - ****increase when doing local testing****
    verbose=1,
    maximize=FALSE,
    prediction=TRUE
  )


##Stage1 LB log-loss:
print(logLoss(
  data$Win[!is.na(data$Win)],
  xgb.model.cv$pred
))

train$predict=xgb.model.cv$pred

# Create submissions for model
set.seed(1)
xgb.model.test<-
  xgb.train(
    params=param,
    data=dtrain,
    nrounds=60,
    verbose=1,
    maximize=TRUE
  )

test$predict=predict(xgb.model.test,dtest)

# Draw graph
plot(density(train$predict[train$Win==1]),col='red',ylim=c(0,3))
lines(density(train$predict[train$Win==0]),col='green')
lines(density(test$predict),col='blue')

submission=rbind(
  data.frame(Id=paste0(train$Season,'_',train$Team1,'_',train$Team2),Pred=train$predict),
  data.frame(Id=paste0(test$Season,'_',test$Team1,'_',test$Team2),Pred=test$predict)
)

# Create submission file
write.csv(submission,'submission.csv',row.names=FALSE)



#Bracket Code
rm(list=ls())
setwd("C:/Users/Stephen Harder/Desktop/kaggle 2015/MarchMadness/march-machine-learning-mania-2016-v2/march-machine-learning-mania-2016-v2")


###############################################################
#These files will change. You can run it as is to see what it will
#look like. The submission should be your submission. The teams and 
#seeds files will be the final files that Jeff releases early next week.
###############################################################
submission<-read.csv("Boost_ALL_Submission.csv")
teams<-read.csv("Teams.csv")
seeds<-read.csv("TourneySeeds.csv")

##################################################
#Comment out the next 3 lines for your submission#
##################################################
# for(i in 1:nrow(submission))
# {submission$Pred[i]<-sample(seq(0.01,0.99,.01),1)}
# submission$Pred<-ifelse(submission$Pred==.5, .51, submission$Pred)


names(seeds)<-c("season", "seed", "team")

pdf("sub1.pdf",width=11,height=8.5)
tgtseason<-"2016"

seeds$region<-substr(seeds$seed,1,1)
seeds$seed<-as.numeric(substr(seeds$seed,2,3))
submission$season<-substr(submission[,1],1,4)
submission$team<-as.numeric(substr(submission[,1],6,9))
submission<-merge(submission,seeds)
names(submission)<-c("season", "team1", "id", "pred1", "seed1", "region1")
names(teams)<-c("team1", "name1")
submission<-merge(submission,teams)
submission$team2<-as.numeric(substr(submission[,3],11,14))
names(seeds)<-c("season","seed2","team2","region2")
submission<-merge(submission,seeds)
names(teams)<-c("team2", "name2")
submission<-merge(submission,teams)
submission<-subset(submission, season==tgtseason)
submission<-submission[,c(3,1,8,11,6,9,7,10,5)]

submission$pred1<-round(submission$pred1,2)
submission$pred2<-round((1-submission$pred1),2)

subw<-subset(submission, region1=="W" & region2=="W")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
subw1<-merge(seed12,subw)
remove<-subw1[which(subw1$seed1==subw1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
subw1<-subset(subw1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
subw1$ord<-ifelse(subw1$seed1<subw1$seed2, subw1$seed1,subw1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subw1<-merge(ord,subw1)
subw1<-subw1[with(subw1, order(ord1)), ]
subw1$str1<-ifelse(subw1$seed1<subw1$seed2, 
                   paste(subw1$seed1,substr(subw1$name1,1,14),substr(subw1$pred1,2,4),sep="  "),
                   paste(subw1$seed2,substr(subw1$name2,1,14),substr(subw1$pred2,2,4),sep="  "))
subw1$str2<-ifelse(subw1$seed1<subw1$seed2, 
                   paste(subw1$seed2,substr(subw1$name2,1,14),substr(subw1$pred2,2,4),sep="  "),
                   paste(subw1$seed1,substr(subw1$name1,1,14),substr(subw1$pred1,2,4),sep="  "))

r2teams<-ifelse(subw1$pred1>subw1$pred2, paste(subw1$name1), paste(subw1$name2))
subw2<-subset(subw,(name1 %in% r2teams) & (name2 %in% r2teams))
subw2<-subset(subw2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
                (seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
                (seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
                (seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
subw2$ord<-ifelse(subw2$seed1<subw2$seed2, subw2$seed1,subw2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subw2<-merge(ord,subw2)
subw2<-subw2[with(subw2, order(ord1)), ]
subw2$str1<-ifelse(subw2$seed1 %in% c(1,16,5,12,6,11,7,10), 
                   paste(subw2$seed1,substr(subw2$name1,1,14),substr(subw2$pred1,2,4),sep="  "),
                   paste(subw2$seed2,substr(subw2$name2,1,14),substr(subw2$pred2,2,4),sep="  "))
subw2$str2<-ifelse(subw2$seed1 %in% c(8,9,4,13,3,14,2,15), 
                   paste(subw2$seed1,substr(subw2$name1,1,14),substr(subw2$pred1,2,4),sep="  "),
                   paste(subw2$seed2,substr(subw2$name2,1,14),substr(subw2$pred2,2,4),sep="  "))


r3teams<-ifelse(subw2$pred1>subw2$pred2, paste(subw2$name1), paste(subw2$name2))
subw3<-subset(subw,(name1 %in% r3teams) & (name2 %in% r3teams))
subw3<-subset(subw3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
                (seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
subw3$ord<-ifelse(subw3$seed1<subw3$seed2, subw3$seed1,subw3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subw3<-merge(ord,subw3)
subw3<-subw3[with(subw3, order(ord1)), ]
subw3$str1<-ifelse(subw3$seed1 %in% c(1,16,8,9,6,11,3,14), 
                   paste(subw3$seed1,substr(subw3$name1,1,14),substr(subw3$pred1,2,4),sep="  "),
                   paste(subw3$seed2,substr(subw3$name2,1,14),substr(subw3$pred2,2,4),sep="  "))
subw3$str2<-ifelse(subw3$seed1 %in% c(5,12,4,13,7,10,2,15), 
                   paste(subw3$seed1,substr(subw3$name1,1,14),substr(subw3$pred1,2,4),sep="  "),
                   paste(subw3$seed2,substr(subw3$name2,1,14),substr(subw3$pred2,2,4),sep="  "))

r4teams<-ifelse(subw3$pred1>subw3$pred2, paste(subw3$name1), paste(subw3$name2))
subw4<-subset(subw,(name1 %in% r4teams) & (name2 %in% r4teams))
subw4$str1<-ifelse(subw4$seed1 %in% c(1,16,8,9,5,12,4,13), 
                   paste(subw4$seed1,substr(subw4$name1,1,14),substr(subw4$pred1,2,4),sep="  "),
                   paste(subw4$seed2,substr(subw4$name2,1,14),substr(subw4$pred2,2,4),sep="  "))
subw4$str2<-ifelse(subw4$seed1 %in% c(6,11,3,14,7,10,2,15), 
                   paste(subw4$seed1,substr(subw4$name1,1,14),substr(subw4$pred1,2,4),sep="  "),
                   paste(subw4$seed2,substr(subw4$name2,1,14),substr(subw4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################

subx<-subset(submission, region1=="X" & region2=="X")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
subx1<-merge(seed12,subx)
remove<-subx1[which(subx1$seed1==subx1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
subx1<-subset(subx1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
subx1$ord<-ifelse(subx1$seed1<subx1$seed2, subx1$seed1,subx1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subx1<-merge(ord,subx1)
subx1<-subx1[with(subx1, order(ord1)), ]
subx1$str1<-ifelse(subx1$seed1<subx1$seed2, 
                   paste(subx1$seed1,substr(subx1$name1,1,14),substr(subx1$pred1,2,4),sep="  "),
                   paste(subx1$seed2,substr(subx1$name2,1,14),substr(subx1$pred2,2,4),sep="  "))
subx1$str2<-ifelse(subx1$seed1<subx1$seed2, 
                   paste(subx1$seed2,substr(subx1$name2,1,14),substr(subx1$pred2,2,4),sep="  "),
                   paste(subx1$seed1,substr(subx1$name1,1,14),substr(subx1$pred1,2,4),sep="  "))

r2teams<-ifelse(subx1$pred1>subx1$pred2, paste(subx1$name1), paste(subx1$name2))
subx2<-subset(subx,(name1 %in% r2teams) & (name2 %in% r2teams))
subx2<-subset(subx2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
                (seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
                (seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
                (seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
subx2$ord<-ifelse(subx2$seed1<subx2$seed2, subx2$seed1,subx2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subx2<-merge(ord,subx2)
subx2<-subx2[with(subx2, order(ord1)), ]
subx2$str1<-ifelse(subx2$seed1 %in% c(1,16,5,12,6,11,7,10), 
                   paste(subx2$seed1,substr(subx2$name1,1,14),substr(subx2$pred1,2,4),sep="  "),
                   paste(subx2$seed2,substr(subx2$name2,1,14),substr(subx2$pred2,2,4),sep="  "))
subx2$str2<-ifelse(subx2$seed1 %in% c(8,9,4,13,3,14,2,15), 
                   paste(subx2$seed1,substr(subx2$name1,1,14),substr(subx2$pred1,2,4),sep="  "),
                   paste(subx2$seed2,substr(subx2$name2,1,14),substr(subx2$pred2,2,4),sep="  "))


r3teams<-ifelse(subx2$pred1>subx2$pred2, paste(subx2$name1), paste(subx2$name2))
subx3<-subset(subx,(name1 %in% r3teams) & (name2 %in% r3teams))
subx3<-subset(subx3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
                (seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
subx3$ord<-ifelse(subx3$seed1<subx3$seed2, subx3$seed1,subx3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subx3<-merge(ord,subx3)
subx3<-subx3[with(subx3, order(ord1)), ]
subx3$str1<-ifelse(subx3$seed1 %in% c(1,16,8,9,6,11,3,14), 
                   paste(subx3$seed1,substr(subx3$name1,1,14),substr(subx3$pred1,2,4),sep="  "),
                   paste(subx3$seed2,substr(subx3$name2,1,14),substr(subx3$pred2,2,4),sep="  "))
subx3$str2<-ifelse(subx3$seed1 %in% c(5,12,4,13,7,10,2,15), 
                   paste(subx3$seed1,substr(subx3$name1,1,14),substr(subx3$pred1,2,4),sep="  "),
                   paste(subx3$seed2,substr(subx3$name2,1,14),substr(subx3$pred2,2,4),sep="  "))

r4teams<-ifelse(subx3$pred1>subx3$pred2, paste(subx3$name1), paste(subx3$name2))
subx4<-subset(subx,(name1 %in% r4teams) & (name2 %in% r4teams))
subx4$str1<-ifelse(subx4$seed1 %in% c(1,16,8,9,5,12,4,13), 
                   paste(subx4$seed1,substr(subx4$name1,1,14),substr(subx4$pred1,2,4),sep="  "),
                   paste(subx4$seed2,substr(subx4$name2,1,14),substr(subx4$pred2,2,4),sep="  "))
subx4$str2<-ifelse(subx4$seed1 %in% c(6,11,3,14,7,10,2,15), 
                   paste(subx4$seed1,substr(subx4$name1,1,14),substr(subx4$pred1,2,4),sep="  "),
                   paste(subx4$seed2,substr(subx4$name2,1,14),substr(subx4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################

suby<-subset(submission, region1=="Y" & region2=="Y")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
suby1<-merge(seed12,suby)
remove<-suby1[which(suby1$seed1==suby1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
suby1<-subset(suby1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
suby1$ord<-ifelse(suby1$seed1<suby1$seed2, suby1$seed1,suby1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
suby1<-merge(ord,suby1)
suby1<-suby1[with(suby1, order(ord1)), ]
suby1$str1<-ifelse(suby1$seed1<suby1$seed2, 
                   paste(suby1$seed1,substr(suby1$name1,1,14),substr(suby1$pred1,2,4),sep="  "),
                   paste(suby1$seed2,substr(suby1$name2,1,14),substr(suby1$pred2,2,4),sep="  "))
suby1$str2<-ifelse(suby1$seed1<suby1$seed2, 
                   paste(suby1$seed2,substr(suby1$name2,1,14),substr(suby1$pred2,2,4),sep="  "),
                   paste(suby1$seed1,substr(suby1$name1,1,14),substr(suby1$pred1,2,4),sep="  "))

r2teams<-ifelse(suby1$pred1>suby1$pred2, paste(suby1$name1), paste(suby1$name2))
suby2<-subset(suby,(name1 %in% r2teams) & (name2 %in% r2teams))
suby2<-subset(suby2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
                (seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
                (seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
                (seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
suby2$ord<-ifelse(suby2$seed1<suby2$seed2, suby2$seed1,suby2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
suby2<-merge(ord,suby2)
suby2<-suby2[with(suby2, order(ord1)), ]
suby2$str1<-ifelse(suby2$seed1 %in% c(1,16,5,12,6,11,7,10), 
                   paste(suby2$seed1,substr(suby2$name1,1,14),substr(suby2$pred1,2,4),sep="  "),
                   paste(suby2$seed2,substr(suby2$name2,1,14),substr(suby2$pred2,2,4),sep="  "))
suby2$str2<-ifelse(suby2$seed1 %in% c(8,9,4,13,3,14,2,15), 
                   paste(suby2$seed1,substr(suby2$name1,1,14),substr(suby2$pred1,2,4),sep="  "),
                   paste(suby2$seed2,substr(suby2$name2,1,14),substr(suby2$pred2,2,4),sep="  "))


r3teams<-ifelse(suby2$pred1>suby2$pred2, paste(suby2$name1), paste(suby2$name2))
suby3<-subset(suby,(name1 %in% r3teams) & (name2 %in% r3teams))
suby3<-subset(suby3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
                (seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
suby3$ord<-ifelse(suby3$seed1<suby3$seed2, suby3$seed1,suby3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
suby3<-merge(ord,suby3)
suby3<-suby3[with(suby3, order(ord1)), ]
suby3$str1<-ifelse(suby3$seed1 %in% c(1,16,8,9,6,11,3,14), 
                   paste(suby3$seed1,substr(suby3$name1,1,14),substr(suby3$pred1,2,4),sep="  "),
                   paste(suby3$seed2,substr(suby3$name2,1,14),substr(suby3$pred2,2,4),sep="  "))
suby3$str2<-ifelse(suby3$seed1 %in% c(5,12,4,13,7,10,2,15), 
                   paste(suby3$seed1,substr(suby3$name1,1,14),substr(suby3$pred1,2,4),sep="  "),
                   paste(suby3$seed2,substr(suby3$name2,1,14),substr(suby3$pred2,2,4),sep="  "))

r4teams<-ifelse(suby3$pred1>suby3$pred2, paste(suby3$name1), paste(suby3$name2))
suby4<-subset(suby,(name1 %in% r4teams) & (name2 %in% r4teams))
suby4$str1<-ifelse(suby4$seed1 %in% c(1,16,8,9,5,12,4,13), 
                   paste(suby4$seed1,substr(suby4$name1,1,14),substr(suby4$pred1,2,4),sep="  "),
                   paste(suby4$seed2,substr(suby4$name2,1,14),substr(suby4$pred2,2,4),sep="  "))
suby4$str2<-ifelse(suby4$seed1 %in% c(6,11,3,14,7,10,2,15), 
                   paste(suby4$seed1,substr(suby4$name1,1,14),substr(suby4$pred1,2,4),sep="  "),
                   paste(suby4$seed2,substr(suby4$name2,1,14),substr(suby4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################

subz<-subset(submission, region1=="Z" & region2=="Z")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
subz1<-merge(seed12,subz)
remove<-subz1[which(subz1$seed1==subz1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
subz1<-subset(subz1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
subz1$ord<-ifelse(subz1$seed1<subz1$seed2, subz1$seed1,subz1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subz1<-merge(ord,subz1)
subz1<-subz1[with(subz1, order(ord1)), ]
subz1$str1<-ifelse(subz1$seed1<subz1$seed2, 
                   paste(subz1$seed1,substr(subz1$name1,1,14),substr(subz1$pred1,2,4),sep="  "),
                   paste(subz1$seed2,substr(subz1$name2,1,14),substr(subz1$pred2,2,4),sep="  "))
subz1$str2<-ifelse(subz1$seed1<subz1$seed2, 
                   paste(subz1$seed2,substr(subz1$name2,1,14),substr(subz1$pred2,2,4),sep="  "),
                   paste(subz1$seed1,substr(subz1$name1,1,14),substr(subz1$pred1,2,4),sep="  "))

r2teams<-ifelse(subz1$pred1>subz1$pred2, paste(subz1$name1), paste(subz1$name2))
subz2<-subset(subz,(name1 %in% r2teams) & (name2 %in% r2teams))
subz2<-subset(subz2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
                (seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
                (seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
                (seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
subz2$ord<-ifelse(subz2$seed1<subz2$seed2, subz2$seed1,subz2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subz2<-merge(ord,subz2)
subz2<-subz2[with(subz2, order(ord1)), ]
subz2$str1<-ifelse(subz2$seed1 %in% c(1,16,5,12,6,11,7,10), 
                   paste(subz2$seed1,substr(subz2$name1,1,14),substr(subz2$pred1,2,4),sep="  "),
                   paste(subz2$seed2,substr(subz2$name2,1,14),substr(subz2$pred2,2,4),sep="  "))
subz2$str2<-ifelse(subz2$seed1 %in% c(8,9,4,13,3,14,2,15), 
                   paste(subz2$seed1,substr(subz2$name1,1,14),substr(subz2$pred1,2,4),sep="  "),
                   paste(subz2$seed2,substr(subz2$name2,1,14),substr(subz2$pred2,2,4),sep="  "))


r3teams<-ifelse(subz2$pred1>subz2$pred2, paste(subz2$name1), paste(subz2$name2))
subz3<-subset(subz,(name1 %in% r3teams) & (name2 %in% r3teams))
subz3<-subset(subz3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
                (seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
subz3$ord<-ifelse(subz3$seed1<subz3$seed2, subz3$seed1,subz3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subz3<-merge(ord,subz3)
subz3<-subz3[with(subz3, order(ord1)), ]
subz3$str1<-ifelse(subz3$seed1 %in% c(1,16,8,9,6,11,3,14), 
                   paste(subz3$seed1,substr(subz3$name1,1,14),substr(subz3$pred1,2,4),sep="  "),
                   paste(subz3$seed2,substr(subz3$name2,1,14),substr(subz3$pred2,2,4),sep="  "))
subz3$str2<-ifelse(subz3$seed1 %in% c(5,12,4,13,7,10,2,15), 
                   paste(subz3$seed1,substr(subz3$name1,1,14),substr(subz3$pred1,2,4),sep="  "),
                   paste(subz3$seed2,substr(subz3$name2,1,14),substr(subz3$pred2,2,4),sep="  "))

r4teams<-ifelse(subz3$pred1>subz3$pred2, paste(subz3$name1), paste(subz3$name2))
subz4<-subset(subz,(name1 %in% r4teams) & (name2 %in% r4teams))
subz4$str1<-ifelse(subz4$seed1 %in% c(1,16,8,9,5,12,4,13), 
                   paste(subz4$seed1,substr(subz4$name1,1,14),substr(subz4$pred1,2,4),sep="  "),
                   paste(subz4$seed2,substr(subz4$name2,1,14),substr(subz4$pred2,2,4),sep="  "))
subz4$str2<-ifelse(subz4$seed1 %in% c(6,11,3,14,7,10,2,15), 
                   paste(subz4$seed1,substr(subz4$name1,1,14),substr(subz4$pred1,2,4),sep="  "),
                   paste(subz4$seed2,substr(subz4$name2,1,14),substr(subz4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################
ffdf<-rbind(subw4,subx4,suby4,subz4)
ffnames<-ifelse(ffdf$pred1>ffdf$pred2,paste(ffdf$name1),paste(ffdf$name2))

subff<-subset(submission,(name1 %in% ffnames) & (name2 %in% ffnames))
subff<-subset(subff, 
              ((region1 %in% c("X","W")) & (region2 %in% c("X","W"))) |
                ((region1 %in% c("Y","Z")) & (region2 %in% c("Y","Z")))
)
subff$ord<-ifelse(subff$region1 %in% c("X","W"),1,2)
subff<-subff[with(subff, order(ord)), ]

subff$str1<-ifelse(subff$region1 %in% c("W","Y"), 
                   paste(subff$seed1,substr(subff$name1,1,14),substr(subff$pred1,2,4),sep="  "),
                   paste(subff$seed2,substr(subff$name2,1,14),substr(subff$pred2,2,4),sep="  "))
subff$str2<-ifelse(subff$region1 %in% c("X","Z"), 
                   paste(subff$seed1,substr(subff$name1,1,14),substr(subff$pred1,2,4),sep="  "),
                   paste(subff$seed2,substr(subff$name2,1,14),substr(subff$pred2,2,4),sep="  "))
######################################################################################
######################################################################################
######################################################################################
finnames<-ifelse(subff$pred1>subff$pred2,paste(subff$name1),paste(subff$name2))

subfinal<-subset(submission,(name1 %in% finnames) & (name2 %in% finnames))


subfinal$str1<-ifelse(subfinal$region1 %in% c("W","X"), 
                      paste(subfinal$seed1,substr(subfinal$name1,1,14),substr(subfinal$pred1,2,4),sep="  "),
                      paste(subfinal$seed2,substr(subfinal$name2,1,14),substr(subfinal$pred2,2,4),sep="  "))
subfinal$str2<-ifelse(subfinal$region1 %in% c("Y","Z"), 
                      paste(subfinal$seed1,substr(subfinal$name1,1,14),substr(subfinal$pred1,2,4),sep="  "),
                      paste(subfinal$seed2,substr(subfinal$name2,1,14),substr(subfinal$pred2,2,4),sep="  "))

winner<-ifelse(subfinal$pred1>subfinal$pred2,paste(subfinal$name1),paste(subfinal$name2))




x<-seq(0,220,(221/67))
y<-0:66

plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
     axes=F, col="white")
segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2))) 
segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
segments(60,c(3,19,37,53),60,c(11,27,45,61))
segments(60,c(7,23,41,57),80,c(7,23,41,57))
segments(80,c(7,41),80,c(23,57))
segments(80,c(15,49),100,c(15,49))
segments(100,c(27,37),120,c(27,37))
segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2))) 
segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
segments(160,c(3,19,37,53),160,c(11,27,45,61))
segments(140,c(7,23,41,57),160,c(7,23,41,57))
segments(140,c(7,41),140,c(23,57))
segments(120,c(15,49),140,c(15,49))

text(9.8,64.5,subw1[1,13],cex=.4)
text(9.8,62.5,subw1[1,14],cex=.4)
text(9.8,60.5,subw1[2,13],cex=.4)
text(9.8,58.5,subw1[2,14],cex=.4)
text(9.8,56.5,subw1[3,13],cex=.4)
text(9.8,54.5,subw1[3,14],cex=.4)
text(9.8,52.5,subw1[4,13],cex=.4)
text(9.8,50.5,subw1[4,14],cex=.4)
text(9.8,48.5,subw1[5,13],cex=.4)
text(9.8,46.5,subw1[5,14],cex=.4)
text(9.8,44.5,subw1[6,13],cex=.4)
text(9.8,42.5,subw1[6,14],cex=.4)
text(9.8,40.5,subw1[7,13],cex=.4)
text(9.8,38.5,subw1[7,14],cex=.4)
text(9.8,36.5,subw1[8,13],cex=.4)
text(9.8,34.5,subw1[8,14],cex=.4)

text(29.8,63.5,subw2[1,13],cex=.4)
text(29.8,59.5,subw2[1,14],cex=.4)
text(29.8,55.5,subw2[2,13],cex=.4)
text(29.8,51.5,subw2[2,14],cex=.4)
text(29.8,47.5,subw2[3,13],cex=.4)
text(29.8,43.5,subw2[3,14],cex=.4)
text(29.8,39.5,subw2[4,13],cex=.4)
text(29.8,35.5,subw2[4,14],cex=.4)

text(49.8,61.5,subw3[1,13],cex=.4)
text(49.8,53.5,subw3[1,14],cex=.4)
text(49.8,45.5,subw3[2,13],cex=.4)
text(49.8,37.5,subw3[2,14],cex=.4)

text(69.8,57.5,subw4[1,11],cex=.4)
text(69.8,41.5,subw4[1,12],cex=.4)

text(9.8,30.5,suby1[1,13],cex=.4)
text(9.8,28.5,suby1[1,14],cex=.4)
text(9.8,26.5,suby1[2,13],cex=.4)
text(9.8,24.5,suby1[2,14],cex=.4)
text(9.8,22.5,suby1[3,13],cex=.4)
text(9.8,20.5,suby1[3,14],cex=.4)
text(9.8,18.5,suby1[4,13],cex=.4)
text(9.8,16.5,suby1[4,14],cex=.4)
text(9.8,14.5,suby1[5,13],cex=.4)
text(9.8,12.5,suby1[5,14],cex=.4)
text(9.8,10.5,suby1[6,13],cex=.4)
text(9.8,8.5,suby1[6,14],cex=.4)
text(9.8,6.5,suby1[7,13],cex=.4)
text(9.8,4.5,suby1[7,14],cex=.4)
text(9.8,2.5,suby1[8,13],cex=.4)
text(9.8,0.5,suby1[8,14],cex=.4)

text(29.8,29.5,suby2[1,13],cex=.4)
text(29.8,25.5,suby2[1,14],cex=.4)
text(29.8,21.5,suby2[2,13],cex=.4)
text(29.8,17.5,suby2[2,14],cex=.4)
text(29.8,13.5,suby2[3,13],cex=.4)
text(29.8,9.5,suby2[3,14],cex=.4)
text(29.8,5.5,suby2[4,13],cex=.4)
text(29.8,1.5,suby2[4,14],cex=.4)

text(49.8,27.5,suby3[1,13],cex=.4)
text(49.8,19.5,suby3[1,14],cex=.4)
text(49.8,11.5,suby3[2,13],cex=.4)
text(49.8,3.5,suby3[2,14],cex=.4)

text(69.8,23.5,suby4[1,11],cex=.4)
text(69.8,7.5,suby4[1,12],cex=.4)


text(209.8,64.5,subx1[1,13],cex=.4)
text(209.8,62.5,subx1[1,14],cex=.4)
text(209.8,60.5,subx1[2,13],cex=.4)
text(209.8,58.5,subx1[2,14],cex=.4)
text(209.8,56.5,subx1[3,13],cex=.4)
text(209.8,54.5,subx1[3,14],cex=.4)
text(209.8,52.5,subx1[4,13],cex=.4)
text(209.8,50.5,subx1[4,14],cex=.4)
text(209.8,48.5,subx1[5,13],cex=.4)
text(209.8,46.5,subx1[5,14],cex=.4)
text(209.8,44.5,subx1[6,13],cex=.4)
text(209.8,42.5,subx1[6,14],cex=.4)
text(209.8,40.5,subx1[7,13],cex=.4)
text(209.8,38.5,subx1[7,14],cex=.4)
text(209.8,36.5,subx1[8,13],cex=.4)
text(209.8,34.5,subx1[8,14],cex=.4)

text(189.8,63.5,subx2[1,13],cex=.4)
text(189.8,59.5,subx2[1,14],cex=.4)
text(189.8,55.5,subx2[2,13],cex=.4)
text(189.8,51.5,subx2[2,14],cex=.4)
text(189.8,47.5,subx2[3,13],cex=.4)
text(189.8,43.5,subx2[3,14],cex=.4)
text(189.8,39.5,subx2[4,13],cex=.4)
text(189.8,35.5,subx2[4,14],cex=.4)

text(169.8,61.5,subx3[1,13],cex=.4)
text(169.8,53.5,subx3[1,14],cex=.4)
text(169.8,45.5,subx3[2,13],cex=.4)
text(169.8,37.5,subx3[2,14],cex=.4)

text(149.8,57.5,subx4[1,11],cex=.4)
text(149.8,41.5,subx4[1,12],cex=.4)


text(209.8,30.5,subz1[1,13],cex=.4)
text(209.8,28.5,subz1[1,14],cex=.4)
text(209.8,26.5,subz1[2,13],cex=.4)
text(209.8,24.5,subz1[2,14],cex=.4)
text(209.8,22.5,subz1[3,13],cex=.4)
text(209.8,20.5,subz1[3,14],cex=.4)
text(209.8,18.5,subz1[4,13],cex=.4)
text(209.8,16.5,subz1[4,14],cex=.4)
text(209.8,14.5,subz1[5,13],cex=.4)
text(209.8,12.5,subz1[5,14],cex=.4)
text(209.8,10.5,subz1[6,13],cex=.4)
text(209.8,8.5,subz1[6,14],cex=.4)
text(209.8,6.5,subz1[7,13],cex=.4)
text(209.8,4.5,subz1[7,14],cex=.4)
text(209.8,2.5,subz1[8,13],cex=.4)
text(209.8,0.5,subz1[8,14],cex=.4)

text(189.8,29.5,subz2[1,13],cex=.4)
text(189.8,25.5,subz2[1,14],cex=.4)
text(189.8,21.5,subz2[2,13],cex=.4)
text(189.8,17.5,subz2[2,14],cex=.4)
text(189.8,13.5,subz2[3,13],cex=.4)
text(189.8,9.5,subz2[3,14],cex=.4)
text(189.8,5.5,subz2[4,13],cex=.4)
text(189.8,1.5,subz2[4,14],cex=.4)

text(169.8,27.5,subz3[1,13],cex=.4)
text(169.8,19.5,subz3[1,14],cex=.4)
text(169.8,11.5,subz3[2,13],cex=.4)
text(169.8,3.5,subz3[2,14],cex=.4)

text(149.8,23.5,subz4[1,11],cex=.4)
text(149.8,7.5,subz4[1,12],cex=.4)

text(89.8,49.5,subff[1,12],cex=.4)
text(129.8,49.5,subff[1,13],cex=.4)
text(89.8,15.5,subff[2,12],cex=.4)
text(129.8,15.5,subff[2,13],cex=.4)


text(109.8,37.5,subfinal[1,11],cex=.4)
text(109.8,27.5,subfinal[1,12],cex=.4)

text(109.8,32.5,winner,cex=2.5)

dev.off()
