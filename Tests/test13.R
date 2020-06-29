library(dslabs)
library(caret)
library(purrr)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Q1
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

# Q2
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Q3
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "qda")
fit_lda$results["Accuracy"]

# Q4
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Q5
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

# Q6
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]

confusionMatrix(fit_lda)
