library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

# Q1
elements = c(3, 4, 7)
map(elements, function(x) {
  length(which(indexes[[1]] == x))
})

# Q2
map(elements, function(x) {
  total = 0
  for (index in indexes) {
    total = total + length(which(index == x))
  }
  total
})

# Q3
y <- rnorm(100, 0, 1)
qnorm(0.75) # Original
quantile(y, 0.75) # Estimated

set.seed(1, sample.kind="Rounding")

B <- 10^4
N <- 250
M <- replicate(B, {
  X <- sample(y, N, replace=TRUE)
  quantile(X, 0.75)
})

mean(M)
sd(M)

# Q4
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)

set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10)

q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

# Q5
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)

set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10^4)

q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

