####################################################
#                                                  #
#                 TEXT STATISTICS                  #
#                                                  #
# Author: Ana Valdivia                             #
# Date: May 2017                                   #
#                                                  #
####################################################

## Libraries
library(tm)
library(SnowballC)
library(caret)
library(data.table)
library(xgboost)
library(plyr)
library(pROC)
library(quanteda)

## Read DataSet
DataSet <- read.csv("./data/DataSet_ALL_SAMs.csv")
DataSet$X <- NULL 
summary(DataSet)

DataSet$NumWords <- sapply(gregexpr("\\W+", DataSet$Text), length) + 1
DataSet$NumSentences <- nsentence(as.character(DataSet$Text))

aggregate(DataSet$NumWords ~ DataSet$Corpus, FUN = sum)
aggregate(DataSet$NumWords ~ DataSet$Corpus, FUN = mean)

aggregate(DataSet$NumSentences ~ DataSet$Corpus, FUN = sum)
aggregate(DataSet$NumSentences ~ DataSet$Corpus, FUN = mean)
