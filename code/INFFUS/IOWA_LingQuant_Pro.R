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


# Compute Linguistic Quantifiers
weightsAtLeast <- numeric(6)
weightsMostOf <- numeric(6)
weightsAsPossible <- numeric(6)

aux <- numeric(6)
for(i in 1:length(aux)){
  if(i == 1) aux[i] <- 1/6
  else aux[i] <- aux[i-1] + 1/6
}

Q1 <- numeric(6)
Q2 <- numeric(6)
Q3 <- numeric(6)

for(i in 1:length(aux)){
  # At Least
  if(i == 1) {
    Q1[i] <- aux[i]/(1/2)
    weightsAtLeast[i] <- Q1[i]
    }
  
  else{
    if(aux[i] < 0.5){
      Q1[i] <- aux[i]/(1/2)
      weightsAtLeast[i] <-  Q1[i] -  Q1[i-1]
    }
    else{
      Q1[i] <- 1
      weightsAtLeast[i] <- Q1[i] -  Q1[i-1]
    }
  }
  
  # Most Of
  if(i == 1){
    if(aux[i] < 0.3){
      Q2[i] <- 0
      weightsMostOf[i] <- Q2[i] 
    } 
    else{
      Q[i] <- (aux[i]-0.3)/(1/2)
      weightsMostOf[i] <- Q2[i]
    } 
  }

  else{
    if(aux[i] < 0.3){
      Q2[i] <- 0
      weightsMostOf[i] <- Q2[i] - Q2[i-1]
    }
    else if(aux[i] < 0.8){
      Q2[i] <- (aux[i]-0.3)/(1/2)
      weightsMostOf[i] <- Q2[i] - Q2[i-1]
    }   
    else{
      Q2[i] <- 1
      weightsMostOf[i] <- Q2[i] - Q2[i-1]
    } 
  }
# As Possible
  if(i == 1){
    if(aux[i] < 0.5){
      Q3[i] <- 0
      weightsAsPossible[i] <- Q3[i] 
    } 
    else{
      Q3[i] <- (aux[i]-0.5)/(1/2)
      weightsAsPossible[i] <- Q3[i]
    } 
  }
  
  else{
    if(aux[i] < 0.5){
      Q3[i] <- 0
      weightsAsPossible[i] <- Q3[i] - Q3[i-1]
    }
    else if(aux[i] < 1){
      Q3[i] <- (aux[i]-0.5)/(1/2)
      weightsAsPossible[i] <- Q3[i] - Q3[i-1]
    }   
    else{
      Q3[i] <- 1
      weightsAsPossible[i] <- Q3[i] - Q3[i-1]
    } 
  }
}


# Function for weights
weightAbs <- function(x){1-abs(x-0.5)}

## Compute IOWA weights
for(i in 1:nrow(DataSet)){
  orderWeights <- order(weightAbs(DataSet[i,3:8]), decreasing = TRUE)
  
  weigthAtLeast_aux <- weightsAtLeast*DataSet[i,2+orderWeights]
  DataSet$AtLeast[i] <- sum((weigthAtLeast_aux/sum(weigthAtLeast_aux))*DataSet[i,3:8])
  
  weigthMostOf_aux <- weightsMostOf*DataSet[i,2+orderWeights]
  DataSet$MostOf[i] <- sum((weigthMostOf_aux/sum(weigthMostOf_aux))*DataSet[i,3:8])
  
  weigthAsPossible_aux <- weightsAsPossible*DataSet[i,2+orderWeights]
  DataSet$AsPossible[i] <- sum((weigthAsPossible_aux/sum(weigthAsPossible_aux))*DataSet[i,3:8])
}

DataSet$AtLeastSentiment <- ifelse(DataSet$AtLeast >= 0.6, "positive", ifelse(DataSet$AtLeast <= 0.4, "negative", "neutral"))
DataSet$MostOfSentiment <- ifelse(DataSet$MostOf >= 0.6, "positive", ifelse(DataSet$MostOf <= 0.4, "negative", "neutral"))
DataSet$AsPossibleSentiment <- ifelse(DataSet$AsPossible >= 0.6, "positive", ifelse(DataSet$AsPossible <= 0.4, "negative", "neutral"))

table(DataSet$Corpus, DataSet$MostOfSentiment)


## Transfrom DataSet into Document-Term matrix

# function word.tfidf (select most important words regarding to a sentiment)
word.tfidf <- function(document.vector, sparsity = .999){
  # Prostruct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # Prostruct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  docTerm.df <- as.data.frame(temp.tf)
  # Prostruct word frequency df
  freq.df <- colMeans(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  list(Freq = freq.df, Temp = docTerm.df)
}


totCorpus <- nlevels(DataSet$Corpus) # total number of different Corpus
Models <- c("xgboost", "svm")

for(i in 8:totCorpus){
  for(j in 1:3){
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    print(levels(DataSet$Corpus)[i])

    DataSetAux <- DataSet[DataSet$Corpus == levels(DataSet$Corpus)[i], c(1, 2, ncol(DataSet)-3+j)]
    
    print(colnames(DataSetAux)[3])
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    
    word.tfidf.pos <- word.tfidf(DataSetAux[DataSetAux[, 3] == "positive", 1])$Freq # selecting positive reviews (First column)
    word.tfidf.neg <- word.tfidf(DataSetAux[DataSetAux[, 3] == "negative", 1])$Freq # selecting negative reviews (SeProd column)
    
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
    setnames(DataSetAux_DTM, "DataSetAux[DataSetAux[, 3] != \"neutral\", 3]", "SentimentClass")
    
    # Write the DataSet
    # assign(paste0("DataSet_", levels(DataSet$Corpus)[i], "_SentimentOWA"), DataSetAux_DTM)
    # write.csv(get(paste0("DataSet_", levels(DataSet$Corpus)[i], "_SentimentOWA")), 
    #           paste0("./data/Model_2_2/DataSet_", levels(DataSet$Corpus)[i], "_SentimentOWA.csv"))
    # 
    # # Delete datasets
    rm(wordNeg, wordPos, wordPosNeg, DataSetAux_PosNeg)
    
    # Building the model
    DataSetAux_DTM$SentimentClass <- factor(DataSetAux_DTM$SentimentClass)
    DataSetAux_DTM$id <- as.factor(row.names(DataSetAux_DTM))
    colnames(DataSetAux_DTM)[1:(ncol(DataSetAux_DTM)-2)] <- paste0("X", 1:(ncol(DataSetAux_DTM)-2))
    DataSetAux_DTM <-  DataSetAux_DTM[!(is.na(DataSetAux_DTM$SentimentClass)), ]
    # Split data set
    set.seed(15)
    ## 80% of the sample size
    sampleSize <- floor(0.8 * nrow(DataSetAux_DTM))
    
    ## set the seed to make your partition reproductible
  
    trainIndex <- sample(seq_len(nrow(DataSetAux_DTM)), size = sampleSize)
    
    # if(i == 8){
    #   pos <-  sample(seq_len(nrow(DataSetAux_DTM[DataSetAux_DTM$SentimentClass == "positive",])), size = 264)
    #   neg <-  sample(seq_len(nrow(DataSetAux_DTM[DataSetAux_DTM$SentimentClass == "negative",])), size = 9)
    #   trainIndex <- c(pos, neg)
    #   rm(pos)
    #   rm(neg)
    # }
    
    DataSet_TRAIN <- DataSetAux_DTM[trainIndex, ]
    DataSet_TRAIN_ID <- DataSet_TRAIN$id
    DataSet_TRAIN$id <- NULL
    DataSet_TEST <- DataSetAux_DTM[-trainIndex, ]
    DataSet_TEST_ID <- DataSet_TEST$id
    DataSet_TEST$id <- NULL
    DataSet_TEST_LABELS <- DataSet_TEST$SentimentClass
    DataSet_TEST$SentimentClass <- NULL
    
    
    # Create Data Partition
    trainIndex <- createDataPartition(DataSetAux_DTM$SentimentClass, times = 1, p = 0.8, list = FALSE)
    DataSet_TRAIN_aux <- DataSetAux_DTM[trainIndex, ]
    
    
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
                                     trControl = control,
                                     metric="ROC")
      }else if(Models[k]=="svm"){
        control <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                                summaryFunction = twoClassSummary, allowParallel = TRUE)
        
        
        set.seed(15)
        ModelResults <- caret::train(SentimentClass ~ .,
                                     data=DataSet_TRAIN,
                                     method="svmLinear",
                                     scale = FALSE,
                                     trControl= control,
                                     metric="ROC")
      }
      # Print Results
      pathResults <- paste0("./results/QuanLingProNeutrality/", levels(DataSet$Corpus)[i], "_", colnames(DataSetAux)[3], "_", Models[k],".txt")
      sink(pathResults)
      
      print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      print(paste0("EXPERIMENT: ", levels(DataSet$Corpus)[i], " ", "ProNeutrality", " ", Models[k]))
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
      
      ConfMatrix <- confusionMatrix(ModelResults)
      
      print("Train Confusion Matrix: ")
      print(ConfMatrix)
      
      ConfTable <- (nrow(DataSet_TRAIN)/100)*(ConfMatrix$table)
      # Precision: tp/(tp+fp):
      precision <- ConfTable[2,2]/sum(ConfTable[2,1:2])
      
      # Recall: tp/(tp + fn):
      spec <- ConfTable[2,2]/sum(ConfTable[1:2,2])
      recall <- ConfTable[1,1]/sum(ConfTable[1:2,1])
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
      ModelResults_pred_Conb <- predict(ModelResults, DataSet_TEST, type="prob")
      # print(ConfusionMatrix(xfbResults_pred[,2], LABELTripAdvisorFeaturesTEST))
      # pred <- data.frame(DataSet_TEST_ID, DataSet_TEST_LABELS, ModelResults_pred[,2]) 
      # setnames(pred, old=c("DataSet_TEST_ID", "DataSet_TEST_LABELS", "ModelResults_pred...2."),
      #          new=c("id", "SentimentClass", "SentimentConb"))
      # pred$SentimentPred <- ifelse(pred$SentimentConb > 0.5, "positive", "negative")
      
      # Print Confusion matrix test
      ConfTableTEST <- table(ModelResults_pred, DataSet_TEST_LABELS)
      print("ConfMatrix TEST: ")
      print(ConfTableTEST)
      
      
      # Precision: tp/(tp+fp):
      precision <- ConfTableTEST[2,2]/sum(ConfTableTEST[2,1:2])
      
      # Recall: tp/(tp + fn):
      spec <- ConfTableTEST[2,2]/sum(ConfTableTEST[1:2,2])
      recall <- ConfTableTEST[1,1]/sum(ConfTableTEST[1:2,1])
      # # F-Score: 2 * precision * recall /(precision + recall):
      # fscore <- 2 * precision * recall /(precision + recall)
      # 
      # # G-measure: sqrt(precision*recall)
      # gmeasure <- sqrt(precision * recall)
      print(auc(DataSet_TEST_LABELS, ModelResults_pred_Conb$positive))
      print(paste0("SensTEST: ", recall))
      print(paste0("SpecTEST: ", spec))
      
      
      sink()
    }
  }
}