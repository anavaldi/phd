# ###########################################################################
#
# ASPECT WORD2VEC
#
# Description: Compute word2vec aspects.
# 
# Author: Ana Valdivia
# Date: April 2018
###########################################################################

# packages
library(beepr)
library(dplyr)
library(tidyr)
library(readr)

# set directory
setwd("C:/Users/Ana/Dropbox/AlhambraAnalytics/COMPUTINTELL2018")

# set monument
monument <- "Alhambra"
# monuments = c(Alhambra, Mezquita, SagradaFamilia)
mon <- ifelse(monument == "Alhambra", "Alh", ifelse(monument == "Mezquita", "Mez", "Safa"))
  
# read deep words
load("./data/depsWords.Rdata")

# read aspects
print("Loading data...")
datasetAspects <- data.frame(read_delim(paste0("./data/Iti/", mon, "_sent2_out_all"), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE))
colnames(datasetAspects) <- c("ReviewID_SentenceID", "Sentence", "Aspects")
# a <- sample(1:nrow(datasetAspects), 10000)
# datasetAspects <- datasetAspects[a,]
datasetAspects$Review_ID <- str_split_fixed(datasetAspects$ReviewID_SentenceID, "_", 2)[,1]
datasetAspects$SentenceID <- str_split_fixed(datasetAspects$ReviewID_SentenceID, "_", 2)[,2]

# clean aspects (delete duplicates, nulls)
datasetAspects <- datasetAspects[datasetAspects$Aspects != "-",]

datasetAspects <-  datasetAspects %>%
  mutate(Aspects = strsplit(as.character(Aspects), ",")) %>%
  unnest() %>%
  filter(Aspects != "") 

datasetAspects$Aspects <- tolower(datasetAspects$Aspects)
aspects <- datasetAspects[!(duplicated(datasetAspects$Aspects)), "Aspects"]
print(paste0("The total number of aspects is: ", length(aspects), "."))

rm(datasetAspects)

# compute word2vec of aspects
iterations = length(aspects)
variables = ncol(deps.words)

word2vec_aspects <- data.frame()

for(i in 1:length(aspects)){
  print(i)
  if(grepl(" ", aspects[i])){
    words <- unlist(strsplit(as.character(aspects[i]), " "))
    word2vec_aspects_aux <- deps.words[deps.words$V1 %in% words, ]
    word2vec_aspects_aux <- as.data.frame.list(colMeans(word2vec_aspects_aux[,2:ncol(deps.words)]))
    word2vec_aspects_aux <- cbind(V1 = aspects[i], word2vec_aspects_aux)
    if(i == 1){
      word2vec_aspects <- word2vec_aspects_aux
    }else{
      word2vec_aspects <- rbind(word2vec_aspects, word2vec_aspects_aux)
    }
    rm(word2vec_aspects_aux)
  }
  else{
    if(i == 1){
      word2vec_aspects <- deps.words[deps.words$V1 == aspects[i],]
    }else{
      word2vec_aspects <- rbind(word2vec_aspects, deps.words[deps.words$V1 == aspects[i],])
    }
  }
}

rm(deps.words)

sink(paste0("./data/Results/summary_aspects_", monument, ".txt"))
aspects_not_word2vec <- aspects[!(aspects %in% word2vec_aspects$V1)]
print(paste0("There are ", length(aspects_not_word2vec) + length( word2vec_aspects[!(complete.cases(word2vec_aspects)),]$V1), " of ", length(aspects)," aspects not in word2vec."))

# Delete aspects not in deps.words
word2vec_aspects <- word2vec_aspects[complete.cases(word2vec_aspects),]
print(paste0("There are ", nrow(word2vec_aspects), " total aspects."))
sink()

# Save aspects with word2vecs
save(word2vec_aspects, file = paste0("./data/word2vecAspects_", monument, ".Rdata"))
