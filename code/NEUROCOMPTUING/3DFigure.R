###########################################################################
#
#  3D FIGURE
#
# Description: 3D Figure for NeuroComputing
# Author: Natalia Ana Valdivia
# Date: December 2017
#
###########################################################################

# Libraries
library(ggplot2)
library(NLP)
library(readr)
library(stringr)
library(plotly)
library(rlang)



# aggregation function
aggregation <- Vectorize(function(x, y, alpha, beta){
  a <- x^alpha
  b <-y^beta
  (a*b)^(1/2)
})

# beta 0
beta <- 0
x = c(0, 0.25, 0.5, 0.75, 1)
y = c(0, 0.25, 0.5, 0.75, 1)

z = rep(beta, 25)
df <- data.frame(expand.grid(x,y), z)
colnames(df) <- c("x", "y", "z")
df$aggr <- aggregation(df$x, df$y, 1, beta)

aggr <- df$aggr
dim(aggr) <- c(5, 5)

# beta 0.75
beta <- 0.75
x = c(0, 0.25, 0.5, 0.75, 1)
y = c(0, 0.25, 0.5, 0.75, 1)

z = rep(beta, 25)
df <- data.frame(expand.grid(x,y), z)
colnames(df) <- c("x", "y", "z")
df$aggr <- aggregation(df$x, df$y, 1, beta)

aggr1 <- df$aggr
dim(aggr1) <- c(5, 5)

# beta 1.75
beta <- 1.75
x = c(0, 0.25, 0.5, 0.75, 1)
y = c(0, 0.25, 0.5, 0.75, 1)

z = rep(beta, 25)
df <- data.frame(expand.grid(x,y), z)
colnames(df) <- c("x", "y", "z")
df$aggr <- aggregation(df$x, df$y, 1, beta)

aggr2 <- df$aggr
dim(aggr2) <- c(5, 5)

# beta 2.75
beta <- 2.75
x = c(0, 0.25, 0.5, 0.75, 1)
y = c(0, 0.25, 0.5, 0.75, 1)

z = rep(beta, 25)
df <- data.frame(expand.grid(x,y), z)
colnames(df) <- c("x", "y", "z")
df$aggr <- aggregation(df$x, df$y, 1, beta)

aggr3 <- df$aggr
dim(aggr3) <- c(5, 5)

# beta 3.75
beta <- 3.75
x = c(0, 0.25, 0.5, 0.75, 1)
y = c(0, 0.25, 0.5, 0.75, 1)

z = rep(beta, 25)
df <- data.frame(expand.grid(x,y), z)
colnames(df) <- c("x", "y", "z")
df$aggr <- aggregation(df$x, df$y, 1, beta)

aggr4 <- df$aggr
dim(aggr4) <- c(5, 5)


# colnames(z) <- x
# rownames(z) <- y
# 
# colnames(color) <- x
# rownames(color) <- y

plot_ly(showscale = FALSE, colorscale = colorRamp(c("navyblue", "darkorange2"))) %>%  
  add_surface(z=~aggr) %>% 
  add_surface(z=~aggr1+2) %>% 
  add_surface(z=~aggr2+4) %>% 
  add_surface(z=~aggr2+6) %>% 
  add_surface(z=~aggr2+8) %>% 
  layout(scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'beta')))