# ###########################################################################
#
# Subgroup Discovery
#
# Description: Apply subgroup discovery in data in order to find patterns
# with a fixed target value.
# Author: Ana Valdivia
# Date: April 2018
###########################################################################

# packages
print("Loading packages...")
library(beepr)
library(readr)
library(tm)
library(splitstackshape)
library(arules)
library(rsubgroup)
library(ggplot2)

options(max.print=1000000)


# set directory
setwd("C:/Users/Ana/Dropbox/AlhambraAnalytics/COMPUTINTELL2018")

# set monument
monument <- "SagradaFamilia"
numClusters <- 200
# monuments = c(Alhambra, Mezquita, SagradaFamilia)
mon <- ifelse(monument == "Alhambra", "Alh", ifelse(monument == "Mezquita", "Mez", "Safa"))


# read word2vecs
load(paste0("./data/word2vecAspects_", monument, ".Rdata"))
load(paste0("./data/dataset_", monument, "_numClusters_", numClusters,".Rdata"))


# build dummy matrix
print("Computing Term Document Matrix...")
tdm <- dataset[, c("Clusters", "userSentiment")]
tdm <- concat.split.expanded(tdm, "Clusters", sep = ",", fill = 0, type = "character", drop = TRUE)

# Plot
# tdm <- tdm[tdm$userSentiment == "negative",]
tdm$userSentiment <- NULL
counts <- as.data.frame(colSums(tdm))
counts$Var <- rownames(counts)
colnames(counts) <- c("Total", "Aspects")
counts <- counts[,c(2,1)]
counts$Aspects <- gsub("Clusters_", "", counts$Aspects)
counts <- counts[order(counts$Total, decreasing = TRUE),]

counts$UpperCase <- grepl("^[[:upper:]]+$", counts$Aspects)
#
write.csv(counts, file = paste0("./data/results/counts_", monument , ".csv"))

ggplot(data=counts, aes(counts$Total), binwidth = 20) +
  geom_histogram(colour = "gray30") + ggtitle(paste0(monument, " dataset")) +
  xlab("Frequency") + ylab("Clustered Aspects") + theme(
    plot.title = element_text(color="black", size=20, face="bold.italic"),
    axis.title.x = element_text(color="black", size=20, face="bold"),
    axis.title.y = element_text(color="black", size=20, face="bold"),
    text = element_text(size=25)
)

# delete aspects which freq is < 2
tdm$userSentiment <- NULL
tdm <- tdm[,colSums(tdm) > 1]
tdm$userSentiment <- dataset$userSentiment
#tdm <- tdm[tdm$userSentiment == "negative",]

sink(paste0("./data/results/aspect_senti_", numClusters, "_s", monument, ".txt"))
for(i in 1:(ncol(tdm)-1)){
  print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  print(colnames(tdm)[i])
  print(table(tdm$userSentiment, tdm[,i]))
  print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  print(" ")
}
sink()

tdm <- tdm[tdm$userSentiment == "negative",]
# tdm$userSentiment <- NULL
# tdm <- tdm[,colSums(tdm) > 1]
# max(colSums(tdm))/nrow(tdm)
tdm$userSentiment <- "negative"

# rules
print("Executing rules...")
names(tdm) <- make.names(names(tdm))
tdm <- as.data.frame(sapply(tdm, function(x) as.factor(as.character(x))))

min.len = 3
max.len = 3
max.time = 5
min.sup = 0.001
min.conf = 0.001

rules <- apriori(tdm, parameter = list(minlen = min.len, maxlen = max.len , maxtime = max.time, supp = min.sup, conf = min.conf),
                 appearance = list(rhs=c("userSentiment=negative"), default="lhs"), control = list(verbose=F))
beep(2)
rules <- sort(rules, by = "lift", decreasing = T)
head(inspect(rules))

sink(paste0("./data/Results/rules_clusters_", numClusters, "_3_", monument, ".txt"))
print("######### PARAMETERS ################")
print(paste0("minLen: ", min.len))
print(paste0("maxLen: ", max.len))
print(paste0("minSup: ", min.sup))
print(paste0("minConf: ", min.conf))
print("#####################################")
print(" ")
print(inspect(rules))
sink()

# discover subgroups
 numAspects <- ncol(tdm)-1
 print("Executing subgroup discovery algorithm...")
 tdm <- as.data.frame(sapply(tdm, function(x) as.factor(as.character(x))))
 task <- CreateSDTask(tdm, as.target("userSentiment", "negative"),
                      new("SDTaskConfig", attributes = colnames(tdm)[1:numAspects]))
# print(task)
# print("Finished.")
# print("Computing subgroups...")
# discoveredSubgroups <- DiscoverSubgroupsByTask(task)
# sink("SD_Alhambra.txt")
# print(discoveredSubgroups)
# sink()
# print("Finished.")

# Term Document Matrix
# word.tfidf <- function(document.vector, sparsity = .999){
#   # construct corpus
#   temp.corpus <- Corpus(VectorSource(document.vector))
#   # construct tf matrix and remove sparse terms
#   temp.tf <- DocumentTermMatrix(temp.corpus, control = list(removeNumbers = FALSE, tolower = FALSE, wordLengths = c(1, Inf), stopwords = FALSE, stemming = FALSE))
#   temp.tf <- removeSparseTerms(temp.tf, sparsity)
#   temp.tf <- as.matrix(temp.tf)
#   docTerm.df <- as.data.frame(temp.tf)
#   # construct word frequency df
#   freq.df <- colMeans(temp.tf)
#   freq.df <- data.frame(word = names(freq.df), freq = freq.df)
#   rownames(freq.df) <- NULL
#   list(Freq = freq.df, Temp = docTerm.df)
# }
# tdm <- ifelse(tdm > 0, 1, 0)
# tdm <- cbind(tdm, dataset$userSentiment)
# colnames(tdm)[ncol(tdm)] <- "userSentiment"
# tdm <- as.data.frame(tdm)