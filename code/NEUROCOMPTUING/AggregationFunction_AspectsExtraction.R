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
library(readr)
library(stringr)

setwd("C:/Users/Ana/Dropbox/AlhambraAnalytics/NEUROCOMPUTING2017")

Alhambra <- read.csv("data/Alhambra_NEUROCOMPUTING.csv")

Alhambra$X <- NULL

# Calcular tabla Paco



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
  labs(x = SAM, y = "f(x, y)") +
  theme(legend.title = element_blank(), axis.title = element_text(size = 35, face="bold"),
  axis.text = element_text(size = 10),  axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) +
  ggplot2::annotate("text", x = 2700, y = 1.05, label = ("Normalized User Rating Polarity"), size = 7) +
  ggplot2::annotate("text", x = 2400, y = 0.65, label = paste0("Normalized ", SAM, " Polarity"), size = 7) + 
  geom_hline(yintercept = 0.6, color = "blue", linetype = "longdash") + 
  ggplot2::annotate("text", x = 50, y = 0.62, label = "Positive", size = 5, color = "blue") +
  ggplot2::annotate("text", x = 50, y = 0.58, label = "Neutral", size = 5, color = "blue") +
  geom_hline(yintercept = 0.4, color = "blue", linetype = "longdash") + 
  ggplot2::annotate("text", x = 50, y = 0.42, label = "Neutral", size = 5, color = "blue") +
  ggplot2::annotate("text", x = 50, y = 0.38, label = "Negative", size = 5, color = "blue") 

ggplot(DataSet_melted, aes(x = order, y = value, colour = beta)) +
  geom_line(aes(group = beta), size = 1) + ggtitle("Alhambra") +
  geom_line(data = DataSet_melted, aes(x = order, y = UserScale), color = "black", size = 1.2) +
  geom_line(data = DataSet_melted, aes(x = order, y = get(paste0(SAM, "Scale"))), color = "black", linetype = "twodash", size = 1) +
  theme(plot.title = element_text(size=25, face="bold.italic"), legend.title = element_blank(), 
        axis.text = element_text(size = 15),  axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        legend.text=element_text(size=20), 
        axis.title=element_text(size=15)) +
  ggplot2::annotate("text", x = 2700, y = 1.05, label = ("Normalized User Rating Polarity"), size = 7) +
  ggplot2::annotate("text", x = 2400, y = 0.65, label = paste0("Normalized ", SAM, " Polarity"), size = 7) + 
  geom_hline(yintercept = 0.6, color = "blue", linetype = "longdash") + 
  ggplot2::annotate("text", x = 50, y = 0.62, label = "Positive", size = 5, color = "blue") +
  ggplot2::annotate("text", x = 50, y = 0.58, label = "Neutral", size = 5, color = "blue") +
  geom_hline(yintercept = 0.4, color = "blue", linetype = "longdash") + 
  ggplot2::annotate("text", x = 50, y = 0.42, label = "Neutral", size = 5, color = "blue") +
  ggplot2::annotate("text", x = 50, y = 0.38, label = "Negative", size = 5, color = "blue") 

rm(DataSet, DataSet2)

# Aspect Extraction
Alhambra_Aspects <- read_delim("data/Iti/Alh_sent2_out_all", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
Alhambra <- read.csv("data/Ana/AlhambraNEUROCOMPUTING.csv") 

Alhambra$UserScale <- (Alhambra$rating-min(Alhambra$rating))/(max(Alhambra$rating)-min(Alhambra$rating))

beta <- 0.75
assign(paste0("beta_", beta), aggregation(Alhambra$UserScale, Alhambra$CoreNLPScale, 1, beta))
Alhambra <- cbind(Alhambra, get(paste0("beta_", beta)))
colnames(Alhambra)[ncol(Alhambra)] <- paste0("beta_", beta)


colnames(Alhambra_Aspects) <- c("ReviewID_SentenceID", "Sentence", "Aspects")

Alhambra_Aspects$X <- str_split_fixed(Alhambra_Aspects$ReviewID_SentenceID, "_", 2)[,1]
Alhambra_Aspects <- Alhambra_Aspects[Alhambra_Aspects$Aspects != "-",]
s <- strsplit(Alhambra_Aspects$Aspects, split = ",")
Alhambra_Aspects_2 <- data.frame(X = rep(Alhambra_Aspects$X, sapply(s, length)), Aspects = unlist(s))
Alhambra_Aspects_2 <- merge(Alhambra_Aspects_2, Alhambra[, c(1, 17, 22, 23)], by = "X")

# Merge both datasets (Section 5.3 of paper)
Alhambra_Aspects_3 <- merge(Alhambra_Aspects_2, Alhambra[, c(1, 5, 6, 7, 9, 11)], by = "X")
write.csv(Alhambra_Aspects_3, "data/Alhambra_Opinion_Aspects.csv")
rm(Alhambra_Aspects_3)

DataSet_Words <- aggregate(Alhambra_Aspects_2[,c(3, 4, 5)], list(Alhambra_Aspects_2$Aspects), mean)
colnames(DataSet_Words) <- c("lemma", "CoreNLPMeanScale", "UserMeanScale", "beta_0.75_Scale")
DataSet_Words_2 <- as.data.frame(table(Alhambra_Aspects_2$Aspects))
colnames(DataSet_Words_2) <- c("lemma", "count")

DataSet_Words <- merge(DataSet_Words, DataSet_Words_2, by = "lemma")
rm(DataSet_Words_2)

DataSet_Words <- DataSet_Words[DataSet_Words$count >= 2,]

ggplot(DataSet_Words, aes(DataSet_Words$count, DataSet_Words$UserMeanScale, colour = DataSet_Words$CoreNLPMeanScale)) + 
  geom_text(size= 5, aes(label = DataSet_Words$lemma), check_overlap = TRUE) + 
  scale_x_log10() + scale_colour_gradientn(name="Normalized CoreNLP Polarity\n", colours= c("navyblue", "darkorange2")) + 
  theme(plot.title = element_text(size=20), axis.title = element_text(size=15)) + labs(x = "Times Aspect Detected", y = "Normalized User Rating Polarity") +
  theme(axis.title = element_text(size = 35, face="bold"),
        axis.text = element_text(size = 10),  axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) +
theme(legend.title = element_text(size=15, face="bold")) + theme(legend.position="top")

ggplot(DataSet_Words, aes(DataSet_Words$count, DataSet_Words$UserMeanScale, colour = DataSet_Words$beta_0.75_Scale)) + 
  geom_text(size= 7, aes(label = DataSet_Words$lemma), check_overlap = TRUE) + 
  scale_x_log10() + scale_colour_gradientn(name="CoreNLP Polarity", colours= c("goldenrod1", "springgreen4")) + 
  theme(plot.title = element_text(size=20), axis.title = element_text(size=15)) + labs(x = "Times Aspect Detected", y = "Normalized User Rating Polarity") +
  theme(axis.title = element_text(size = 35, face="bold"),
        axis.text = element_text(size = 10),  axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) +
  theme(legend.title = element_text(size=15, face="bold")) + theme(legend.position="top") + theme(legend.position="top") +  theme(panel.background = element_blank(), axis.line = element_line(colour = "azure4"))



ggplot(DataSet_Words, aes(DataSet_Words$count, DataSet_Words$UserMeanScale, colour = DataSet_Words$beta_0.75_Scale)) + 
  geom_text(size= 7, aes(label = DataSet_Words$lemma), check_overlap = TRUE) + 
  scale_x_log10() + scale_colour_gradientn(name="Polarity Aggregation Model (beta = 0.75)\n", colours= c("goldenrod1", "springgreen4")) + 
  theme(plot.title = element_text(size=20), axis.title = element_text(size=15)) + labs(x = "Times Aspect Detected", y = "Normalized User Rating Polarity") +
  theme(axis.title = element_text(size = 35, face="bold"),
        axis.text = element_text(size = 10),  axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) +
  theme(legend.title = element_text(size=15, face="bold")) + theme(legend.position="top") +  theme(panel.background = element_blank(), axis.line = element_line(colour = "azure4"))


DataSet_Words
ggplot(DataSet_Words, aes(DataSet_Words$UserMeanScale, DataSet_Words$CoreNLPMeanScale, colour = DataSet_Words$beta_0.75_Scale)) +  
  geom_text(size= 7, aes(label = DataSet_Words$lemma), check_overlap = TRUE) + 
  scale_colour_gradientn(name="Polarity Aggregation Model (beta = 0.75)\n", colours= c("navyblue", "darkorange2")) + 
  theme(plot.title = element_text(size=20), axis.title = element_text(size=15)) + labs(x = "Normalized User Rating Polarity", y = "Normalized CoreNLP Polarity") +
  theme(axis.title = element_text(size = 35, face="bold"),
        axis.text = element_text(size = 10),  axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) +
  theme(legend.title = element_text(size=15, face="bold")) + theme(legend.position="top") + 
  scale_x_continuous(limits = c(-0.1, 1.1)) +  theme(panel.background = element_blank(), axis.line = element_line(colour = "azure4"))


DataSet_Words$diff <- abs(DataSet_Words$CoreNLPMeanScale-DataSet_Words$UserMeanScale)
