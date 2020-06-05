library(ggplot2)
library(GGally)
library(sna)
library(network)
library(R.utils)
library(plyr)
library(scales)
library(reshape2)

df <- read.csv("C:/Users/Mikael Marcondes/Desktop/ex3a.csv")
histogram <- matrix(, nrow = 3, ncol = 5)
inhibitions <- vector()
activations <- vector()
no_connections <- vector()

for(j in 1:5){
  inhibitions[j] <- as.integer(0)
  activations[j] <- as.integer(0)
  no_connections[j] <- as.integer(0)
  for(i in 1:16){
    if(as.integer(df[i, j]) == -1){
      inhibitions[j] <- inhibitions[j] + 1
    }
    else{
      if(as.integer(df[i, j]) == 1){
        activations[j] <- activations[j] + 1
      }
      else{
        if(as.integer(df[i, j]) == 0){
          no_connections[j] <- no_connections[j] + 1
        }
      }
    }
  }
}

histogram[1, ] <- inhibitions
histogram[2, ] <- activations
histogram[3, ] <- no_connections

histogram <- round(histogram/16, digits = 2)
histogram
hist <- data.frame(t(histogram))
rownames(hist) <- c('x1', 'x2', 'x3', 'x4', 'x5')
colnames(hist) <- c('inhibition', 'activation', 'no connection')

hist$category <- rownames(hist)
mhist <- melt(hist, id.vars = "category")

(p <- ggplot(mhist, aes(category, value, fill = variable)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent)
)
