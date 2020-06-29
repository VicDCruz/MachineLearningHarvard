# Q1
x <- matrix(rnorm(100*10), 100, 10)

# Q2
dim(x)
length(x[,1])
length(x[1,])

# Q3
x1 <- sweep(x, 1, 1:nrow(x),"+")
x2 <- x + seq(nrow(x))

# Q4
x3 <- sweep(x, 2, 1:ncol(x), FUN = "+")

# Q5
meansRows <- rowMeans(x)
meansCols <- colMeans(x)

# Q6
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

train_set <- mnist$train

means <- rowMeans(train_set$images)
qplot(means, bins = "30", color = I("black"))

train_set$images[train_set$images <= 50 & train_set$images >= 205] <- 0
train_set$images[train_set$images > 50 & train_set$images < 205] <- 1
length(which(train_set$images == 1)) / length(train_set$images)

y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y)
