models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

# Q1
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Q2
pred <- sapply(fits, function(fit) 
  predict(fit, newdata = mnist_27$test))
dim(pred)

# Q3
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

# Q4
generalpred <- rowMeans(pred == 7)
y_hat <- ifelse(generalpred > 0.5, 7, 2)

mean(y_hat == mnist_27$test$y)

# Q5
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

# Q6
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

# Q9
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)


