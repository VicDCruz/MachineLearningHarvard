# Q1
library(rpart)
library(tidyverse)
library(dslabs)
data(tissue_gene_expression)
set.seed(1991, sample.kind = "Rounding")
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
ggplot(fit)

# Q2
set.seed(1991, sample.kind = "Rounding")
fit_rpart <- train(tissue_gene_expression$x, tissue_gene_expression$y,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
             control = rpart.control(minsplit = 0))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)

# Q3
plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

# Q4
library(randomForest)
set.seed(1991, sample.kind = "Rounding")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

# Q5
imp <- varImp(fit)
imp

# Q6
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
