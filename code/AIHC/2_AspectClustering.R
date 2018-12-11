# ###########################################################################
#
# ASPECT CLUSTERING
#
# Description: Compute aspects clustering in order to apply subgroup discovery.
# 
# Author: Ana Valdivia
# Date: April 2018
###########################################################################

# packages
library(beepr)
library(readr)
library(stringr)


# set directory
setwd("C:/Users/Ana/Dropbox/AlhambraAnalytics/COMPUTINTELL2018")

# set monument
monument <- "SagradaFamilia"
# monuments = c(Alhambra, Mezquita, SagradaFamilia)
mon <- ifelse(monument == "Alhambra", "Alh", ifelse(monument == "Mezquita", "Mez", "Safa"))

numClusters <- 200

# read word2vecs
load(paste0("./data/word2vecAspects_", monument, ".Rdata"))

# compute kmeans
km <- kmeans(word2vec_aspects[, c(2:ncol(word2vec_aspects))], numClusters, iter.max = 15)

km.df <- data.frame(word2vec_aspects$V1, km$cluster)
colnames(km.df) <- c("Aspects", "Cluster_Num")
km.df$Cluster_ID <- chartr("0123456789", "ABCDEFGHIJ", km.df$Cluster_Num)

write.csv(km.df, paste0("./data/clusters/clusters_", numClusters, "_", monument, ".csv"))

rm(km)
# merge clusters to opinion dataframe
datasetAspects <- data.frame(read_delim(paste0("./data/Iti/", mon, "_sent2_out_all"), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE))
datasetOpinions <- read.csv(paste0("./data/Ana/", monument, "NEUROCOMPUTING.csv")) 
# a <- sample(1:nrow(datasetAspects), 1000)
# datasetAspects <- datasetAspects[a,]
# merge datasets
colnames(datasetAspects) <- c("ReviewID_SentenceID", "Sentence", "Aspects")
datasetAspects$X <- str_split_fixed(datasetAspects$ReviewID_SentenceID, "_", 2)[,1]
datasetAspects_aux <- aggregate(Aspects ~ X, data = datasetAspects, toString)
datasetOpinions$userSentiment <- ifelse(datasetOpinions$rating < 4, "negative", "positive")

datasetAspects <- datasetAspects[datasetAspects$Aspects != "-",]

dataset <- merge(datasetOpinions, datasetAspects_aux[,c("X", "Aspects")], by = "X")

dataset$Aspects <- gsub(" ", "", dataset$Aspects, fixed = TRUE)
dataset$Aspects <- gsub("-,", "", dataset$Aspects, fixed = TRUE)
dataset$Aspects <- gsub(",-", "", dataset$Aspects, fixed = TRUE)
dataset <- dataset[dataset$Aspects!= "-",]

rm(datasetAspects_aux)
rm(datasetOpinions)
rm(datasetAspects)


# Create new column with clusters
dataset$Clusters <- NA
for(i in 1:nrow(dataset)){
  print(i)
  aspects <- as.vector(unlist(strsplit(dataset$Aspects[i], ",")))
  aspects <- tolower(aspects)
  aspects <- aspects[!duplicated(aspects)]
  aspects_in <- aspects[aspects %in% km.df$Aspects]
  aspects_out <- aspects[!(aspects %in% km.df$Aspects)]
  clusters <- c()

  if(length(aspects) > 0){
    if(length(aspects_in) > 0){
      for(j in 1:length(aspects_in)){
        if(j == 1){
          clusters <- km.df[km.df$Aspects == aspects_in[j], ]$Cluster_ID
        }else{
          clusters <- paste0(clusters, ",", km.df[km.df$Aspects == aspects_in[j], ]$Cluster_ID)
        }
      }
    }
    if(length(aspects_out) > 0){
      for(k in 1:length(aspects_out)){
        if(k == 1 & length(aspects_in) == 0){
          clusters <- aspects_out[k]
        }else{
          clusters <- paste0(clusters, ",", aspects_out[k])
        }
      }
    }
    dataset$Clusters[i] <- clusters
  }
}

save(dataset, file = paste0("./data/dataset_", monument, "_numClusters_", numClusters,".Rdata"))
beep(2)

# dataset$Clusters <- NA
# for(i in 1:nrow(dataset)){
#   print(i)
#   aspects <- as.vector(unlist(strsplit(dataset$Aspects[i], ",")))
#   aspects <- tolower(aspects)
#   aspects <- aspects[!duplicated(aspects)]
#   aspects <- aspects[aspects %in% km.df$Aspects]
#   clusters <- c()
#   
#   if(length(aspects) > 0){
#     for(j in 1:length(aspects_in)){
#       if(j == 1){
#         clusters <- km.df[km.df$Aspects == aspects_in[j], ]$Cluster
#       }else{
#         clusters <- paste0(clusters, ",", km.df[km.df$Aspects == aspects_in[j], ]$Cluster)
#       }
#     }
# 
#     dataset$Clusters[i] <- clusters
#   }
# }



# Shiloute
library(cluster)


wss <- sapply(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), 
              function(k){kmeans(word2vec_aspects[, c(2:ncol(word2vec_aspects))], k, iter.max = 15)$tot.withinss})

plot(c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", 
            ylab="Total within-clusters sum of squares")


