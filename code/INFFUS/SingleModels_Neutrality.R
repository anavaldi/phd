####################################################
#                                                  #
#                   IOWA MODEL                     #
#                                                  #
# Author: Ana Valdivia                             #
# Date: April 2017                                 #
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

## Read DataSet
DataSet <- read.csv("./data/DataSet_ALL_SAMs.csv")
DataSet$X <- NULL 
summary(DataSet)

## Transfrom DataSet into Document-Term matrix

# function word.tfidf (select most important words regarding to a sentiment)
word.tfidf <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  docTerm.df <- as.data.frame(temp.tf)
  # construct word frequency df
  freq.df <- colMeans(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  list(Freq = freq.df, Temp = docTerm.df)
}

DataSet <- DataSet[,c(1:2, 9:14)]
totCorpus <- nlevels(DataSet$Corpus) # total number of different Corpus
SentimentTools <- c("Bing", "CoreNLP", "MC", "Microsoft", "SentiStr", "VADER" )
totSentTools <- length(SentimentTools) # total number of tool sentiments
Models <- c("xgboost", "svm")

for(i in 1:totCorpus){
  for(j in 1:totSentTools){ 
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(levels(DataSet$Corpus)[i])
    print(SentimentTools[j])
    print(colnames(DataSet)[j+2])
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    DataSetAux <- DataSet[(DataSet$Corpus == levels(DataSet$Corpus)[i]), c(1, 2 , j+2)]
    
    word.tfidf.pos <- word.tfidf(DataSetAux[DataSetAux[, 3] == "positive", 1])$Freq # selecting positive reviews (First column)
    word.tfidf.neg <- word.tfidf(DataSetAux[DataSetAux[, 3] == "negative", 1])$Freq # selecting negative reviews (Second column)
    
    word.tfidf.pos <- as.data.table(word.tfidf.pos)
    word.tfidf.pos <- word.tfidf.pos[order(freq, decreasing = TRUE),]
    word.tfidf.neg <- as.data.table(word.tfidf.neg)
    word.tfidf.neg <- word.tfidf.neg[order(freq, decreasing = TRUE),]
    
    # Delete STOPWORDS
    wordPos <- word.tfidf.pos[!(word.tfidf.pos$word %in% stopwords("SMART")),]
    wordNeg <- word.tfidf.neg[!(word.tfidf.neg$word %in% stopwords("SMART")),]
    
    # Delete common WORDS
    wordPos <- wordPos[!(wordPos$word %in% wordNeg$word),]
    wordNeg <- wordNeg[!(wordNeg$word %in% wordPos$word),]
    
    rm(word.tfidf.pos)
    rm(word.tfidf.neg)
    
    # Order and select most 25 popular words
    wordPos <- wordPos[order(freq, decreasing = TRUE),]
    wordPos <- wordPos[1:10,]
    
    wordNeg <- wordNeg[order(freq, decreasing = TRUE),]
    wordNeg <- wordNeg[1:10,]
    
    # Merge wordPos and wordNeg
    wordPosNeg <- merge(wordPos, wordNeg, by = c("word", "freq"), all = TRUE)
    
    # Build Document-Term Matrix
    DataSetAux_PosNeg <- DataSetAux[DataSetAux[,3] != "neutral",]
    DataSetAux_DTM <- word.tfidf(DataSetAux_PosNeg$Text)$Temp
    DataSetAux_DTM <- as.data.frame(ifelse(DataSetAux_DTM > 0, 1, 0))
    DataSetAux_DTM <- DataSetAux_DTM[, colnames(DataSetAux_DTM) %in% wordPosNeg$word]
    DataSetAux_DTM <- cbind(DataSetAux_DTM, DataSetAux[DataSetAux[,3] != "neutral",3])
    setnames(DataSetAux_DTM, "DataSetAux[DataSetAux[, 3] != \"neutral\", 3]", paste0(SentimentTools[j], "Sentiment"))
    
    # Write the DataSet
    assign(paste0("DataSet_", levels(DataSet$Corpus)[i], "_", SentimentTools[j]), DataSetAux_DTM)
    # write.csv(get(paste0("DataSet_", levels(DataSet$Corpus)[i], "_", SentimentTools[j])), paste0("./data/Model_1/", paste0("DataSet_", levels(DataSet$Corpus)[i], "_", SentimentTools[j]), "_MaxNeutr.csv"))
    # 
    # Delete datasets
    rm(wordNeg, wordPos, wordPosNeg, DataSetAux_PosNeg, DataSetAux)
    
    # Building the model
    setnames(DataSetAux_DTM, paste0(SentimentTools[j], "Sentiment"), "SentimentClass")
    DataSetAux_DTM$SentimentClass <- factor(DataSetAux_DTM$SentimentClass, levels = c("positive", "negative"))
    DataSetAux_DTM$SentimentClass <- factor(DataSetAux_DTM$SentimentClass)
    DataSetAux_DTM$id <- as.factor(row.names(DataSetAux_DTM))
    colnames(DataSetAux_DTM)[1:(ncol(DataSetAux_DTM)-2)] <- paste0("X", 1:(ncol(DataSetAux_DTM)-2))
    
    # Split data set
    set.seed(15)
    ## 80% of the sample size
    sampleSize <- floor(0.8 * nrow(DataSetAux_DTM))
    
    ## set the seed to make your partition reproductible
    set.seed(15)
    trainIndex <- sample(seq_len(nrow(DataSetAux_DTM)), size = sampleSize)
    
    DataSet_TRAIN <- DataSetAux_DTM[trainIndex, ]
    DataSet_TRAIN_ID <- DataSet_TRAIN$id
    DataSet_TRAIN$id <- NULL
    DataSet_TEST <- DataSetAux_DTM[-trainIndex, ]
    DataSet_TEST_ID <- DataSet_TEST$id
    DataSet_TEST$id <- NULL
    DataSet_TEST_LABELS <- DataSet_TEST$SentimentClass
    DataSet_TEST$SentimentClass <- NULL
    
    for(k in 1:length(Models)){
      print(k)
      if(Models[k]=="xgboost"){
        control <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                                summaryFunction = twoClassSummary, allowParallel = TRUE)
        set.seed(15)
        
        predictors <- DataSet_TRAIN[,-ncol(DataSet_TRAIN)]
        
        # for(n in 1:ncol(predictors)){
        #   predictors[,n] <- as.numeric(as.character(predictors[,n]))
        # }
        
        label <- DataSet_TRAIN$SentimentClass
        
        ModelResults <- caret::train(x=predictors,
                                     y=label,
                                     method="xgbTree",
                                     trControl=control,
                                     metric="ROC")
      }else if(Models[k]=="svm"){
        control <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                                summaryFunction = twoClassSummary, allowParallel = TRUE)
        
        
        set.seed(15)
        ModelResults <- caret::train(SentimentClass ~ .,
                                     data=DataSet_TRAIN,
                                     method="svmLinear",
                                     scale = FALSE,
                                     trControl=control,
                                     metric="ROC")
      }
      
      # Print Results
      pathResults <- paste0("./results/SingleModels/", levels(DataSet$Corpus)[i], "_", SentimentTools[j], "_", Models[k],".txt")
      sink(pathResults)
      
      print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      print(paste0("EXPERIMENT: ", levels(DataSet$Corpus)[i], " ", SentimentTools[j], " ", Models[k]))
      print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      
      
      print("DATA SET DESCRIPTION: ")
      print("Class Sentiment Distribution:")
      print("all:")
      print(summary(DataSetAux_DTM$SentimentClass)[1])
      print(summary(DataSetAux_DTM$SentimentClass)[2])
      print("in train:")
      print(summary(DataSet_TRAIN$SentimentClass)[1])
      print(summary(DataSet_TRAIN$SentimentClass)[2])
      
      print("Rows and features:")
      print(dim(DataSetAux_DTM))
      print(paste0("in train:"))
      print(dim(DataSet_TRAIN))
      
      
      print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      print("MODEL RESULT: ")
      print(paste0("Model: ", Models[k]))
      print("Model summary: ")
      print(ModelResults)
      
      print("AUC TRAIN: ")
      print(max(ModelResults$results$ROC))
      
      confMatrix <- confusionMatrix(ModelResults)
      
      
      print("Train Confusion Matrix: ")
      print(confMatrix)
      
      confTable <- (nrow(DataSet_TRAIN)/100)*(confMatrix$table)
      # Precision: tp/(tp+fp):
      precision <- confTable[2,2]/sum(confTable[2,1:2])
      
      # Recall: tp/(tp + fn):
      spec <- confTable[2,2]/sum(confTable[1:2,2])
      recall <- confTable[1,1]/sum(confTable[1:2,1])
      # # F-Score: 2 * precision * recall /(precision + recall):
      # fscore <- 2 * precision * recall /(precision + recall)
      # 
      # # G-measure: sqrt(precision*recall)
      # gmeasure <- sqrt(precision * recall)
      
      print(paste0("SensTRAIN: ", recall))
      print(paste0("SpecTRAIN: ", spec))
      
      # Prediction
      for(n in 1:ncol(DataSet_TEST)){
        DataSet_TEST[,n] <- as.numeric(as.character(DataSet_TEST[,n]))
      }
      
      
      ModelResults_pred <- predict(ModelResults, DataSet_TEST)
      ModelResults_pred_prob <- predict(ModelResults, DataSet_TEST, type="prob")
      # print(confusionMatrix(xfbResults_pred[,2], LABELTripAdvisorFeaturesTEST))
      # pred <- data.frame(DataSet_TEST_ID, DataSet_TEST_LABELS, ModelResults_pred[,2]) 
      # setnames(pred, old=c("DataSet_TEST_ID", "DataSet_TEST_LABELS", "ModelResults_pred...2."),
      #          new=c("id", "SentimentClass", "SentimentProb"))
      # pred$SentimentPred <- ifelse(pred$SentimentProb > 0.5, "positive", "negative")
      
      # Print confusion matrix test
      confTableTEST <- table(ModelResults_pred, DataSet_TEST_LABELS)
      print("ConfMatrix TEST: ")
      print(confTableTEST)
      
      # Precision: tp/(tp+fp):
      precision <- confTableTEST[2,2]/sum(confTableTEST[2,1:2])
      
      # Recall: tp/(tp + fn):
      spec <- confTableTEST[2,2]/sum(confTableTEST[1:2,2])
      recall <- confTableTEST[1,1]/sum(confTableTEST[1:2,1])
      # # F-Score: 2 * precision * recall /(precision + recall):
      # fscore <- 2 * precision * recall /(precision + recall)
      # 
      # # G-measure: sqrt(precision*recall)
      # gmeasure <- sqrt(precision * recall)
      print(auc(DataSet_TEST_LABELS, ModelResults_pred_prob$positive))
      print(paste0("SensTEST: ", recall))
      print(paste0("SpecTEST: ", spec))
      
      
      sink()
    }
  }
}