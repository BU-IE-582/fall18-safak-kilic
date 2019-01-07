require(data.table)
require(TunePareto)
require(glmnet)
require(elo)
library(HotDeckImputation)
library(e1071)
library(rpart)
library(verification)
library(caret)

setwd('~/Desktop/IE582/Project/Final')

# Test and Training Starts/Ends
testEnd=as.Date('2019-01-05')
testStart=as.Date('2018-10-29')
trainStart=as.Date('2016-01-01')
###


rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')

matches_data_path='~/Desktop/IE582/Project/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_data_path='~/Desktop/IE582/Project/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)

# preprocess matches
matches=matches_data_preprocessing(matches_raw)
matches_train = matches[Match_Date>=trainStart & Match_Date<testStart] 
matches_test = matches[Match_Date>=testStart & Match_Date<testEnd] 

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

# extract open and close odd type features from multiple bookmakers FOR ODDS for 5 bookmakers with most bets
oddfeatures=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)
# team features obtained by training data
teamfeatures=team_features(matches_train, recnum = 3, byrecentyear = 2017)
teamelo=team_elos(matches_train)

# team + elo
teamfeatures$home = unique(merge(teamfeatures$home, teamelo, by = "Team"))
teamfeatures$away = unique(merge(teamfeatures$away, teamelo, by = "Team"))


# all features combined
features = as.data.table(unique(merge(unique(merge(as.data.frame(oddfeatures), as.data.frame(teamfeatures$home), by.x = "Home", by.y = "Team")), as.data.frame(teamfeatures$away), by.x= "Away", by.y = "Team")))
###

# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart & Match_Date<testEnd] 

# Results of test with Home/Tie/Away formats as 1 or 0
result<-test_features[,c(3,8)]
result[, Home := ifelse(Match_Result=="Home", 1, 0)]
result[, Tie := ifelse(Match_Result=="Tie", 1, 0)]
result[, Away := ifelse(Match_Result=="Away", 1, 0)]
result<-result[,][order(matchId)]


# logistic regression of training data and test prediction
predictions_lg=train_glmnet(train_features, test_features ,not_included_feature_indices=c(1:4, 8), alpha=1,nlambda=50, tune_lambda=T,nofReplications=2,nFolds=10,trace=T)
final_lg = merge(as.data.frame(predictions_lg$predictions), as.data.frame(matches[, c("matchId", "Home", "Away", "Match_Date")]), by = "matchId")

RPS_lg=round(mean(RPS_matrix(final_lg[,3:5],result[,3:5])),3)

# normalizing the features and pca
features_norm<-cbind(features[,5:7],features[,9:80])
features_norm<-apply(features_norm,2,as.numeric)
features_norm<-impute.mean(features_norm)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

features_norm<-apply(features_norm,2,normalize)
pca<-prcomp(features_norm)

# first 14 terms reach 93.6%
train_indices=which(features$Match_Date>=trainStart,features$Match_Date<testStart)
test_indices=which(features$Match_Date>=testStart & features$Match_Date<testEnd)

train_features_norm<-cbind(features[train_indices,c(1:3,8)],pca$x[train_indices,1:14])
test_features_norm<-cbind(features[test_indices,c(1:3,8)],pca$x[test_indices,1:14])

# logistic regression of training data and test prediction (pca & normalization)
predictions_lg_norm=train_glmnet(train_features_norm, test_features_norm ,not_included_feature_indices=c(1:4), alpha=1,nlambda=50, tune_lambda=T,nofReplications=2,nFolds=10,trace=T)
final_lg_norm = merge(predictions_lg_norm$predictions, as.data.frame(features[, c("matchId", "Home", "Away", "Match_Date")]), by = "matchId")
RPS_lg_norm=round(mean(RPS_matrix(final_lg_norm[,3:5],result[,3:5])),3)

# polynomial svm
predictions_poly=train_svm_poly(train_features, test_features ,not_included_feature_indices=c(1:4,8))
final_svm_poly = merge(predictions_poly$predictions, as.data.frame(features[, c("matchId", "Home", "Away", "Match_Date","Match_Result")]), by = c("matchId","Home","Away"))
RPS_svm_poly=round(mean(RPS_matrix(final_svm_poly[,4:6],result[,3:5])),3)

# polynomial svm (pca $ normalization)
predictions_poly_norm=train_svm_poly(train_features_norm, test_features_norm ,not_included_feature_indices=c(1:4))
final_svm_poly_norm = merge(predictions_poly_norm$predictions, as.data.frame(features[, c("matchId", "Home", "Away", "Match_Date","Match_Result")]), by = c("matchId","Home","Away"))
RPS_svm_poly_norm=round(mean(RPS_matrix(final_svm_poly_norm[,4:6],result[,3:5])),3)

# gaussian svm
predictions_gauss=train_svm_gauss(train_features, test_features ,not_included_feature_indices=c(1:4,8))
final_svm_gauss = merge(predictions_gauss$predictions, as.data.frame(features[, c("matchId", "Home", "Away", "Match_Date","Match_Result")]), by = c("matchId","Home","Away"))
RPS_svm_gauss=round(mean(RPS_matrix(final_svm_gauss[,4:6],result[,3:5])),3)

# gaussian svm (pca $ normalization)
predictions_gauss_norm=train_svm_gauss(train_features_norm, test_features_norm ,not_included_feature_indices=c(1:4))
final_svm_gauss_norm = merge(predictions_gauss_norm$predictions, as.data.frame(features[, c("matchId", "Home", "Away", "Match_Date","Match_Result")]), by = c("matchId","Home","Away"))
RPS_svm_gauss_norm=round(mean(RPS_matrix(final_svm_gauss_norm[,4:6],result[,3:5])),3)

