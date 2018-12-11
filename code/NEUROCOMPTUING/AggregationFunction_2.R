###########################################################################
#
# AGGREGATION SYSTEM
#
# Description: Copy Emiliya's Matlab code
# Author: Ana Valdivia
# Date: November 2017
#
###########################################################################


library(ggplot2)
library(NLP)
library(reshape2)

setwd("C:/Users/Ana/Dropbox/AlhambraAnalytics/NEUROCOMPUTING2017")

Alhambra <- read.csv("data/Alhambra_NEUROCOMPUTING.csv")

Alhambra <- read.csv("data/Pantheon_NEUROCOMPUTING.csv")

Alhambra$X <- NULL

SAMS <- c("Afinn", "Syuzhet", "Bing", "CoreNLP", "SenticPatternDL")


SAM <- SAMS[4]

Alhambra$UserScale <- (Alhambra$rating-min(Alhambra$rating))/(max(Alhambra$rating)-min(Alhambra$rating))
Alhambra <- Alhambra[order(-Alhambra$UserScale, -Alhambra[,paste0(SAM, "Scale")]),]
Alhambra$order <- 1:nrow(Alhambra)

# Set parameters & function
alpha <- 1
beta <- 0
dim <- 4

aggregation <- function(x, y, alpha, beta){
  a <- x^alpha
  b <-y^beta
  (a*b)^(1/2)
}
# Compute aggregation models
DataSet <- Alhambra[,c(1, 23, 6, 24, 13:17)]

for(i in 1:dim){
  beta <- beta + 0.25
  assign(paste0("beta_", beta), aggregation(DataSet$UserScale, DataSet[,paste0(SAM, "Scale")], alpha, beta))
  DataSet <- cbind(DataSet, get(paste0("beta_", beta)))
  colnames(DataSet)[ncol(DataSet)] <- paste0("beta_", beta)
}

DataSet2 <- DataSet
DataSet_melted <- melt(DataSet2, id.vars = c("order", "UserScale",  paste0(SAM, "Scale")), measure.vars = grep("^beta", names(DataSet), value = TRUE))
DataSet_melted <- DataSet_melted[order(DataSet_melted$UserScale, DataSet_melted[,paste0(SAM, "Scale")], decreasing = TRUE),]
DataSet_melted$variable <- as.factor(DataSet_melted$variable)
colnames(DataSet_melted)[4] <- "beta"

ggplot(DataSet_melted, aes(x = order, y = value, colour = beta)) +
  geom_line(aes(group = beta), size = 1) +
  geom_line(data = DataSet_melted, aes(x = order, y = UserScale), color = "black", size = 1.2) +
  geom_line(data = DataSet_melted, aes(x = order, y = get(paste0(SAM, "Scale"))), color = "black", linetype = "twodash", size = 1) +
  labs(x = "Ordered reviews (most positive to most negative)", y = "f(x, y)") +
  theme(legend.title = element_blank(), axis.title = element_text(size = 20, face="bold"),
  axis.text = element_text(size = 10),  axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) +
  ggplot2::annotate("text", x = 2700, y = 1.05, label = ("Normalized User Rating"), size = 7) +
  ggplot2::annotate("text", x = 2400, y = 0.65, label = paste0("Normalized ", SAM, " Polarity"), size = 7) + 
  geom_hline(yintercept = 0.6, color = "blue", linetype = "longdash") + 
  ggplot2::annotate("text", x = 50, y = 0.62, label = "Positive", size = 5, color = "blue") +
  ggplot2::annotate("text", x = 50, y = 0.58, label = "Neutral", size = 5, color = "blue") +
  geom_hline(yintercept = 0.4, color = "blue", linetype = "longdash") + 
  ggplot2::annotate("text", x = 50, y = 0.42, label = "Neutral", size = 5, color = "blue") +
  ggplot2::annotate("text", x = 50, y = 0.38, label = "Negative", size = 5, color = "blue") 




# Prueba
# DataSet3 <- DataSet2[,c("order", "UserScale", "CoreNLPScale", "beta_0.25")]
# DataSet3 <- DataSet3[order(DataSet3$beta_0.25, decreasing = TRUE), ]
# 
# DataSet_melted <- melt(DataSet2, id.vars = c("order", "UserScale",  paste0(SAM, "Scale")), measure.vars = "beta_0.25")
# 
# DataSet_melted <- DataSet_melted[order(DataSet_melted$UserScale, DataSet_melted$CoreNLPScale,  decreasing = TRUE),]
# DataSet_melted$order <- 1:nrow(DataSet_melted)
