####################################################
#                                                  #
#             Applying SAM to New                  #
#                                                  #
# Author: Ana Valdivia                             #
# Date: 22/12/2016                                 #
# Summary: This script applies SAM to new          #
# opinions.                                        #
#                                                  #
####################################################

# Packages & Libraries
library(dplyr)
library(data.table)

# Read DataaSet
pathData <- "./data/"
DataSet <-  read.csv(paste0(pathData, "DataSetVickyNew.csv"))

###################################### Stanford CoreNLP ######################################
devtools::install_github("statsmaths/coreNLP")
coreNLP::downloadCoreNLP()

library(coreNLP)
initCoreNLP()

for(i in 1:nrow(DataSet)){
  print(i)
  pos <- 0
  neg <- 0
  opinion <- as.character(DataSet$Text[i])
  opinion.df <- getSentiment(annotateString(opinion))
  
  for(j in 1:nrow(opinion.df)){
    if(opinion.df$sentiment[j]=="Verypositive"){
      pos = pos + 2
    } else if(opinion.df$sentiment[j]=="Positive"){
      pos = pos + 1
    } else if(opinion.df$sentiment[j]=="Negative"){
      neg = neg + 1
      
    } else if(opinion.df$sentiment[j]=="Verynegative"){
      neg = neg + 2
    }
  }
  DataSet$CoreNLPSentiment[i] <- ifelse(pos > neg, "positive", 
                                         ifelse(neg > pos, "negative", "neutral"))
  
  DataSet$CoreNLPMean[i] <- mean(as.matrix(opinion.df$sentimentValue))
}


###################################### Bing ######################################
devtools::install_github("mjockers/syuzhet")
library(syuzhet)
library("openNLPdata")

for(i in 1:nrow(DataSet)){
  print(i)
  vectorBingSentiment <- get_sentiment(get_sentences(as.character(DataSet$Text[i])), method = "bing")
  pos <- 0
  neg <- 0
  for(j in 1: length(vectorBingSentiment)){
    if(vectorBingSentiment[j] > 0){
      pos <- pos + 1
    }else{
      neg <- neg + 1
    }
  }
  DataSet$BingSentiment[i] <- ifelse(pos == neg, "neutral", ifelse(pos > neg, "positive", "negative"))
  DataSet$BingMean[i] <- mean(vectorBingSentiment)
}



###################################### Microsoft Azure ######################################

if ("mscstexta4r" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("mscstexta4r")
}

if ("mscstexta4r" %in% installed.packages()[,"Package"] == FALSE) {
  if ("devtools" %in% installed.packages()[,"Package"] == FALSE) {
    install.packages("devtools")
  }
  devtools::install_github("philferriere/mscstexta4r")
}
library(mscstexta4r)
Sys.setenv(MSCS_TEXTANALYTICS_URL ="https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/",
           MSCS_TEXTANALYTICS_KEY ="0ec41d1eb7a74679abe1ebb1a75d302c")

textaInit()

DataSet$Text <- gsub("£", " ", DataSet$Text)

last <- 1972
for(i in last:nrow(DataSet)){
  print(i)
  DataSet$Microsoft[i] <- textaSentiment(documents = as.character(DataSet$Text[i]))$result[,2]
}

######################################################################################

write.csv(DataSet, paste0(pathData, "DataSet.csv"), row.names = FALSE)

DataSet$MicrosoftSentiment <- ifelse(DataSet$MicrosoftScale <= 0.4, "negative", ifelse(DataSet$MicrosoftScale >= 0.6, "positive", "neutral"))

setnames(DataSet, c("CoreNLPSentiment", "BingSentiment"), c("CoreNLPSentiment2", "BingSentiment2"))

scaleFunction <- function(x){(x-min(x))/(max(x)-min(x))}

DataSet$CoreNLPScale <- ave(DataSet$CoreNLPMean, DataSet$Corpus, FUN = scaleFunction)
DataSet$BingScale <- ave(DataSet$BingMean, DataSet$Corpus, FUN = scaleFunction)

DataSet$CoreNLPSentiment <- ifelse(DataSet$CoreNLPScale <= 0.4, "negative", ifelse(DataSet$CoreNLPScale >= 0.6, "positive", "neutral"))
DataSet$BingSentiment <- ifelse(DataSet$BingScale <= 0.4, "negative", ifelse(DataSet$BingScale >= 0.6, "positive", "neutral"))


######################################################################################



