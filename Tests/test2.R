library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Q7
set.seed(2)
test_index <- createDataPartition(y, 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Q8

xp = train$Petal.Width
cutoff <- seq(0, max(xp), 0.1)
accuracy <- map_dbl(cutoff, function(x) {
  yHat <- ifelse(xp > x, "virginica", "versicolor")
  mean(yHat == train$Species)
})
max(accuracy)
#Answer: Petal.Length

# Q9
xp = train$Petal.Length
cutoff <- seq(0, max(xp), 0.1)
accuracy <- map_dbl(cutoff, function(x) {
  yHat <- ifelse(test$Petal.Length > x, "virginica", "versicolor")
  mean(yHat == test$Species)
})
plot(cutoff, accuracy, type = "o")
bestCutoff <- cutoff[which.max(accuracy)]
max(accuracy)

# Q10
xp = test$Petal.Width
cutoff <- seq(0, max(xp), 0.1)
accuracy <- map_dbl(cutoff, function(x) {
  yHat <- ifelse(xp > x, "virginica", "versicolor")
  mean(yHat == test$Species)
})
max(accuracy)

#Answer: Petal.Width

# Q11
plot(iris,pch=21,bg=iris$Species)
xp = test$Petal.Length
xq = test$Petal.Width
maxX = max(xp, xq)
cutoffP <- seq(0, max(maxX), 0.1)
cutoffQ <- seq(0, max(maxX), 0.1)
accuracy <- map2_dbl(cutoffP, cutoffQ, function(a, b) {
  yHat <- ifelse(xp > a | xq > b, "virginica", "versicolor")
  mean(yHat == test$Species)
})
max(accuracy)

# Q11 by teacher
petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)
