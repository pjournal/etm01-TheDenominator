getwd()
setwd("C:/Users/Beach/Documents/GitHub/etm01-TheDenominator/files")
###TASK 1
## A)
# read matches info
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')
#read odd details
odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')
library(data.table)
#Filtering 5 bookmakers that we chose in the previous HW.
odds_bookmakers_filtered = odds[bookmaker == '10Bet' |
                                bookmaker == '12BET' |
                                bookmaker == '188BET' |
                                bookmaker == '1xBet' |
                                bookmaker == 'BetVictor']
#Filtering odds for our bookmakers
odds_10Bet_filtered = odds[bookmaker == '10Bet']
odds_12BET_filtered = odds[bookmaker == '12BET']
odds_188BET_filtered = odds[bookmaker == '188BET']
odds_1xBet_filtered = odds[bookmaker == '1xBet']
odds_BetVictor_filtered = odds[bookmaker == 'BetVictor']
#Let's see what type of bets each of them has been providing.
unique(odds_10Bet_filtered$betType)
unique(odds_12BET_filtered$betType)
unique(odds_188BET_filtered$betType)
unique(odds_1xBet_filtered$betType)
unique(odds_BetVictor_filtered$betType)

#12BET is limitig our approach by just offering 3 types of bets. 1x2, ah and ou which are also present in rest of the bookmakers.
#Theser are going to be the classifiers for our general odds dataset which has all 5 bookmakers.

#We are going to modify the match data now with the previous code that we used in HW1.
#While doin so, we are also going to delete NA cells which represent matches which have not yet been played yet
#And also games that postponed
#Modification will end with adding a new column of YES/NO as a indicator of match resulting over or under 2.5 goals.
matches_mod=matches[complete.cases(matches)]
matches_mod=matches_mod[!grepl("POSTP.", matches_mod$score)]
matches_mod[,c('score_home','score_away'):=tstrsplit(score,':')]
matches_mod[,score_home:=as.numeric(score_home)]
matches_mod[,score_away:=as.numeric(score_away)]
matches_mod[,H25:=score_home+score_away]
matches_mod[,outcome:=ifelse(H25>2.5,"YES","NO")]

#Coming back to odds data...
#We will order the data by date and time to find out the latest odd before the game.
#And going to pivot the table with chosing match_IDs as instances and combination of bookmaker & oddtypes as variables.
odds_bookmakers_filtered_ordered=odds_bookmakers_filtered[order(matchId,betType,oddtype,bookmaker,date)]
odds_bookmakers_filtered_ordered_last=odds_bookmakers_filtered_ordered[,list(final_odd=odd[.N]),by=list(matchId,oddtype,bookmaker)]
odds_bookmakers_filtered_dcast=dcast(odds_bookmakers_filtered_ordered_last,matchId~bookmaker+oddtype,value.var='final_odd')
str(odds_bookmakers_filtered_dcast)

#Now we have a new dataset which has 56 variables that we can explain the outcome by making a PCA.

odds_bookmakers_filtered_ordered_last
odds_bookmakers_filtered_ordered_last_10Bet_filtered = odds_bookmakers_filtered_ordered_last[bookmaker == '10Bet']
odds_bookmakers_filtered_ordered_last_12BET_filtered = odds_bookmakers_filtered_ordered_last[bookmaker == '12BET']
odds_bookmakers_filtered_ordered_last_188BET_filtered = odds_bookmakers_filtered_ordered_last[bookmaker == '188BET']
odds_bookmakers_filtered_ordered_last_1xBet_filtered = odds_bookmakers_filtered_ordered_last[bookmaker == '1xBet']
odds_bookmakers_filtered_ordered_last_BetVictor_filtered = odds_bookmakers_filtered_ordered_last[bookmaker == 'BetVictor']
unique(odds_bookmakers_filtered_ordered_last_10Bet_filtered$oddtype)
unique(odds_bookmakers_filtered_ordered_last_12BET_filtered$oddtype)
unique(odds_bookmakers_filtered_ordered_last_188BET_filtered$oddtype)
unique(odds_bookmakers_filtered_ordered_last_1xBet_filtered$oddtype)
unique(odds_bookmakers_filtered_ordered_last_BetVictor_filtered$oddtype)
unique(odds_bookmakers_filtered_ordered_last_BetVictor_filtered$oddtype)

#Since 12Bet is limiting our PCA model by just offering 7 odtypes we would like to replace this bookmaker with another one; bwin.

odds_bookmakers_filtered_bwin = odds[bookmaker == 'bwin']
unique(odds_bookmakers_filtered_bwin$oddtype)

#Bwin checks out with the rest of our 4 bookmakers in terms of oddtype variety.
#Now we have to redo the filtering for the whole odds dataset by replacing bwind with 12Bet.

odds_bookmakers_filtered = odds[bookmaker == '10Bet' |
                                  bookmaker == 'bwin' |
                                  bookmaker == '188BET' |
                                  bookmaker == '1xBet' |
                                  bookmaker == 'BetVictor']
#Filtering odds for bwin
odds_12BET_filtered = odds[bookmaker == 'bwin']

#Redoing the order by date, chosing last odd, and dcasting...
odds_bookmakers_filtered_ordered=odds_bookmakers_filtered[order(matchId,betType,oddtype,bookmaker,date)]
odds_bookmakers_filtered_ordered_last=odds_bookmakers_filtered_ordered[,list(final_odd=odd[.N]),by=list(matchId,oddtype,bookmaker)]
odds_bookmakers_filtered_dcast=dcast(odds_bookmakers_filtered_ordered_last,matchId~bookmaker+oddtype,value.var='final_odd')
str(odds_bookmakers_filtered_dcast)

#Now we have 61 variables which makes sense because 1 variable is matchId and 5 bookmakers with 12(same) oddtpes makes 60 variables.
#But there are still NA values in the dataset which does not makes so much sense.
#We are going to practice another apporach by removing the NAs in the pure odds data.
odds_complete=odds[complete.cases(odds)]
summary(odds)
summary(odds_complete)
unique(odds_complete$bookmaker)
table(odds_complete$bookmaker)
100*prop.table(table(odds_complete$bookmaker))

odds_bookmakers_filtered_v3 = odds_complete[bookmaker == '10Bet' |
                                            bookmaker == 'Betfair Exchange' |
                                           bookmaker == 'bet365' |
                                  bookmaker == '1xBet' |
                                  bookmaker == 'ComeOn']

odds_complete_ordered=odds_complete[order(matchId,betType,oddtype,bookmaker,date)]
odds_complete_ordered_last=odds_complete_ordered[,list(final_odd=odd[.N]),by=list(matchId,oddtype,bookmaker)]
odds_complete_ordered_last_dcast=dcast(odds_complete_ordered_last,matchId~bookmaker+oddtype,value.var='final_odd')
str(odds_complete_ordered_last)
table(odds_complete_ordered_last$bookmaker)
100*prop.table(table(odds_complete_ordered_last$bookmaker))

odds_complete_ordered_last
odds_complete_ordered_last_10Bet = odds_complete_ordered_last[bookmaker == '10Bet']
odds_complete_ordered_last_12Bet = odds_complete_ordered_last[bookmaker == '12BET']
odds_complete_ordered_last_188Bet = odds_complete_ordered_last[bookmaker == '188BET']
odds_complete_ordered_last_bet365 = odds_complete_ordered_last[bookmaker == 'bet365']
odds_complete_ordered_last_BetVictor = odds_complete_ordered_last[bookmaker == 'BetVictor']
odds_complete_ordered_last_Interwetten = odds_complete_ordered_last[bookmaker == 'Interwetten']
odds_complete_ordered_last_Paddy_Power = odds_complete_ordered_last[bookmaker == 'Paddy Power']
odds_complete_ordered_last_Pinnacle = odds_complete_ordered_last[bookmaker == 'Pinnacle']
odds_complete_ordered_last_SBOBET = odds_complete_ordered_last[bookmaker == 'SBOBET']


unique(odds_complete_ordered_last_10Bet$oddtype)
unique(odds_complete_ordered_last_12Bet$oddtype)
unique(odds_complete_ordered_last_188Bet$oddtype)
unique(odds_complete_ordered_last_bet365$oddtype)
unique(odds_complete_ordered_last_BetVictor$oddtype)
unique(odds_complete_ordered_last_Interwetten$oddtype)
unique(odds_complete_ordered_last_Paddy_Power$oddtype)
unique(odds_complete_ordered_last_Pinnacle$oddtype)
unique(odds_complete_ordered_last_SBOBET$oddtype)

##BTinal Approach
odds_ordered=odds[order(matchId,betType,oddtype,bookmaker,date)]
odds_ordered_last=odds_ordered[,list(final_odd=odd[.N]),by=list(matchId,oddtype,bookmaker)]
odds_ordered_last_dcast=dcast(odds_ordered_last,matchId~bookmaker+oddtype,value.var='final_odd')
matches_mod
matches_mod$leagueId <- NULL
matches_mod$home <- NULL
matches_mod$away <- NULL
matches_mod$score <- NULL
matches_mod$date <- NULL
matches_mod$type <- NULL
matches_mod$score_home <- NULL
matches_mod$score_away <- NULL
matches_mod$H25 <- NULL
Merged=merge(odds_ordered_last_dcast,matches_mod,by='matchId')
str(Merged)
summary(Merged)
##
Merged=Merged[,matchId:=NULL]
#we need to have no N/A's in our feature vectors
Merged_complete=Merged[complete.cases(Merged)]
Merged_complete
##
#Going back to original plan
odds_bookmakers_filtered_dcast
Merged=merge(odds_bookmakers_filtered_dcast,matches_mod,by='matchId')
Merged_complete=Merged[complete.cases(Merged)]
Merged_complete
##Merged_complete_ordered=Merged_complete[order(Merged_complete)]
Merged_complete$Over[Merged_complete$outcome=='YES'] <= 1
Merged_complete[,Over:=ifelse(outcome=="YES","1","0")]
Merged_complete$outcome <- NULL
colnames(Merged_complete)[colnames(Merged_complete)=="Over"] <- "outcome"
Merged_complete$outcome=as.numeric(Merged_complete$outcome)
str(Merged_complete)
#Applying PCA
Merged_complete$matchId <- NULL
pca=princomp(Merged_complete)
plot(pca)

str(pca)
summary(pca)

cov_pca = cov(Merged_complete)
eigenValues = eigen(cov_pca)$values
eigenVectors = eigen(cov_pca)$vectors
eigenValues 

eigenVectors[,1]
eigenVectors[,2]
eigenVectors[,3]

install.packages("plot3D")
library("plot3D")
require(ggplot2)
par("mar")
par(mar=c(1,1,1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))
str(pca)
plot(pca$scores[,1],pca$scores[,2],main='PCA',xlab='pca$scores[,1]', ylab='pca$scores[,2]')

##  B)

#Apprpach for calculating Euclidean Distance
Merged_complete_Euclidean=dist(Merged_complete, method="euclidean") 
Merged_complete_Euclidean[is.na(Merged_complete_Euclidean)]=0
MDS_Euclidean=cmdscale(Merged_complete_Euclidean)
plot(MDS_Euclidean[,1],MDS_Euclidean[,2],main='MDS_Euclidean',xlab='', ylab='')

#Apprpach for calculating Manhattan Distance
Merged_complete_Manhattan=dist(Merged_complete, method="manhattan") 
Merged_complete_Manhattan[is.na(Merged_complete_Manhattan)]=0
MDS_Manhattan=cmdscale(Merged_complete_Manhattan)
plot(MDS_Manhattan[,1],MDS_Manhattan[,2],main='MDS_Manhattan',xlab='', ylab='')

#TASK2

require(data.table)
require(TunePareto)
require(glmnet)
require(caTools)
require (caret)
require (ISLR)

setwd("C:/Users/Beach/Documents/GitHub/etm01-TheDenominator/files")

testStart=as.Date('2018-11-16')
trainStart=as.Date('2012-07-15')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')

matches_data_path='C:/Users/Beach/Documents/GitHub/etm01-TheDenominator/files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_data_path='C:/Users/Beach/Documents/GitHub/etm01-TheDenominator/files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)

# preprocess matches
matches=matches_data_preprocessing(matches_raw)

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)

# divide data based on the provided dates
features=features[complete.cases(features)]
split=sample.split(features$outcome,SplitRatio=0.7)
train_features=subset(features,split==TRUE)
test_features=subset(features,split==FALSE)

# run glm on train data return predictions
predictions=train_glmnet(train_features, test_features)
predictions.df = as.data.frame(predictions)
round(predictions.df$predictions.predicted_probabilities)
predictions.df$outcome[predictions.df$predictions.outcome == "OVER"] <- "1"
predictions.df$outcome[predictions.df$predictions.outcome == "under"] <- "0"
predictions.df$predictions.predicted_probabilities[predictions.df$predictions.predicted_probabilities < 0.5] <- "0"
predictions.df$predictions.predicted_probabilities[predictions.df$predictions.predicted_probabilities > 0.5] <- "1"
predictions.df$outcome=as.factor(predictions.df$outcome)
predictions.df$predictions.predicted_probabilities=as.factor(predictions.df$predictions.predicted_probabilities)
confusionMatrix(predictions.df$predictions.predicted_probabilities,predictions.df$outcome)

#This basic glm model's prediction accuracy is not ver well, only 0.4722
#This model's most powerful attribute is it's Sensitivity.
#Which indicates that the matches with total score of less than 2.5 goals will be predicted with a higher success rate.

