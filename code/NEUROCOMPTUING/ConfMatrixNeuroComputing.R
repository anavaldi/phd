###########################################################################
#
# CONFUSIO MATRIX FOR NEUROCOMPTUING
#
# Description: Print confusion matrix for Neurocomputing
# Author: Ana Valdivia
# Date: November 2017
#
###########################################################################


library(ggplot2)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(lemon)


# myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
b <- c(0,0.5,1)

# Example
z <- cor(mtcars)
z.m <- melt(z)

ggplot(z.m, aes(Var1, Var2, fill = value)) + geom_tile() + scale_fill_gradientn(colours= c("navyblue","darkmagenta","darkorange1", "yellow2"),breaks=b,labels=format(b))


# Afinn
user <- c("Positive", "Positive", "Positive", "Neutral", "Neutral", "Neutral", "Negative", "Negative", "Negative")
sam <- c("Positive", "Neutral", "Negative", "Positive", "Neutral", "Negative", "Positive", "Neutral", "Negative")
values <- c(0.835, 0.135, 0.030, 0.688, 0.208, 0.104, 0.461, 0.344, 0.196)

CorrelationMatrix_afinn <- data.frame(user, sam, values)

CorrelationMatrix_afinn$sam <- as.factor(CorrelationMatrix_afinn$sam)
CorrelationMatrix_afinn$sam <- factor(CorrelationMatrix_afinn$sam,levels(CorrelationMatrix_afinn$sam)[c(3, 2, 1)])

# Bing
values <- c(0.772,	0.149,	0.079,
            0.550,	0.273,	0.177,
            0.275,	0.402,	0.323)

CorrelationMatrix_bing <- data.frame(user, sam, values)

CorrelationMatrix_bing$sam <- as.factor(CorrelationMatrix_bing$sam)
CorrelationMatrix_bing$sam <- factor(CorrelationMatrix_bing$sam,levels(CorrelationMatrix_bing$sam)[c(3, 2, 1)])

# CoreNLP
values <- c(0.476,	0.292,	0.232,
            0.192,	0.271,	0.537,
            0.088,	0.231,	0.681)
            

CorrelationMatrix_corenlp <- data.frame(user, sam, values)

CorrelationMatrix_corenlp$sam <- as.factor(CorrelationMatrix_corenlp$sam)
CorrelationMatrix_corenlp$sam <- factor(CorrelationMatrix_corenlp$sam,levels(CorrelationMatrix_corenlp$sam)[c(3, 2, 1)])

# MeaningCloud
values <- c(0.868,	0.086,	0.046,
            0.595,	0.196,	0.209,
            0.363,	0.254,	0.383)

CorrelationMatrix_meaningcloud <- data.frame(user, sam, values)

CorrelationMatrix_meaningcloud$sam <- as.factor(CorrelationMatrix_meaningcloud$sam)
CorrelationMatrix_meaningcloud$sam <- factor(CorrelationMatrix_meaningcloud$sam,levels(CorrelationMatrix_meaningcloud$sam)[c(3, 2, 1)])




afinn <- ggplot(CorrelationMatrix_afinn, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "Afinn", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size = 18), axis.text.y = element_text(angle=90))

bing <- ggplot(CorrelationMatrix_bing, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "Bing", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size = 18), axis.text.y = element_text(angle=90))

corenlp <- ggplot(CorrelationMatrix_corenlp, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "CoreNLP", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size = 18), axis.text.y = element_text(angle=90))

meaningcloud <- ggplot(CorrelationMatrix_meaningcloud, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "MeaningCloud", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size = 18), axis.text.y = element_text(angle=90))

grid_arrange_shared_legend(afinn, bing, corenlp, meaningcloud, nrow = 2, ncol = 2, position = "top")

grid_arrange(afinn, bing, corenlp, meaningcloud, top = "Title", legend)











# SentiStrength
user <- c("Positive", "Positive", "Positive", "Neutral", "Neutral", "Neutral", "Negative", "Negative", "Negative")
sam <- c("Positive", "Neutral", "Negative", "Positive", "Neutral", "Negative", "Positive", "Neutral", "Negative")
values <- c(0.696,	0.245,	0.059,
            0.467,	0.372,	0.161,
            0.257,	0.414,	0.329)

CorrelationMatrix_sentistrength <- data.frame(user, sam, values)

CorrelationMatrix_sentistrength$sam <- as.factor(CorrelationMatrix_sentistrength$sam)
CorrelationMatrix_sentistrength$sam <- factor(CorrelationMatrix_sentistrength$sam,levels(CorrelationMatrix_sentistrength$sam)[c(3, 2, 1)])

# SenticPattern
values <- c(0.571,	0.255,	0.174,
            0.572,	0.264,	0.165,
            0.522,	0.314,	0.164)

CorrelationMatrix_senticpattern <- data.frame(user, sam, values)

CorrelationMatrix_senticpattern$sam <- as.factor(CorrelationMatrix_senticpattern$sam)
CorrelationMatrix_senticpattern$sam <- factor(CorrelationMatrix_senticpattern$sam,levels(CorrelationMatrix_senticpattern$sam)[c(3, 2, 1)])

# Syuzhet
values <- c(0.863,	0.097,	0.040,
            0.689,	0.201,	0.110,
            0.478,	0.304,	0.218)

CorrelationMatrix_syuzhet <- data.frame(user, sam, values)

CorrelationMatrix_syuzhet$sam <- as.factor(CorrelationMatrix_syuzhet$sam)
CorrelationMatrix_syuzhet$sam <- factor(CorrelationMatrix_syuzhet$sam,levels(CorrelationMatrix_syuzhet$sam)[c(3, 2, 1)])


# VADER
values <- c(0.921,	0.050,	0.029,
            0.701,	0.150,	0.149,
            0.521,	0.141,	0.339)


CorrelationMatrix_vader <- data.frame(user, sam, values)

CorrelationMatrix_vader$sam <- as.factor(CorrelationMatrix_vader$sam)
CorrelationMatrix_vader$sam <- factor(CorrelationMatrix_vader$sam,levels(CorrelationMatrix_vader$sam)[c(3, 2, 1)])




sentistrength <- ggplot(CorrelationMatrix_sentistrength, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "SentiStrength", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size = 18), axis.text.y = element_text(angle=90))

senticpattern <- ggplot(CorrelationMatrix_senticpattern, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "SenticPattern+DL", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size = 18), axis.text.y = element_text(angle=90))

syuzhet <- ggplot(CorrelationMatrix_syuzhet, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "Syuzhet", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size = 18), axis.text.y = element_text(angle=90))

vader <- ggplot(CorrelationMatrix_vader, aes(sam, user, fill = values)) + geom_tile(aes(fill = values)) + 
  scale_fill_gradientn(colours= c("navyblue", "darkorange2")) + 
  geom_text(aes(sam, user, label = sprintf("%1.2f%%", 100*values)), color = "white", size = 10) + 
  labs(x = "Vader", y = "User") + 
  theme(axis.title = element_text(size=35, face="bold"),
        axis.text = element_text(size=18), axis.text.y = element_text(angle=90))

grid_arrange_shared_legend(afinn, bing, corenlp, meaningcloud, nrow = 2, ncol = 2, position = "top")
grid_arrange_shared_legend(sentistrength, senticpattern, syuzhet, vader, nrow = 2, ncol = 2, position = "top")

