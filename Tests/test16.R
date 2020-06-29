library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
library(lubridate)
library(purrr)
library(pdftools)
library(broom)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.20, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]     

nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

# Q2
set.seed(3, sample.kind = 'Rounding') # if R version >= 3.6
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

# Q3a
survival_women <- train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)

survival_men <- train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

# Q3b
sex_model <- ifelse(test_set$Sex == "female", 1, 0)
mean(sex_model == test_set$Survived)

# Q4a
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

# Q4b
class_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(class_model == test_set$Survived)

# Q4c
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

# Q4d
sex_class_model <- ifelse(
  test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)

# Q5a
# Confusion Matrix: sex model
cm1 <- confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
cm1 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate
# Confusion Matrix: class model
cm2 <- confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
cm2 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate
# Confusion Matrix: sex and class model
cm3 <- confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))
cm3 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate

# Q6
F_meas(data = factor(sex_model), reference = test_set$Survived)
F_meas(data = factor(class_model), reference = test_set$Survived)
F_meas(data = factor(sex_class_model), reference = test_set$Survived)

# Q7
set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived ~ Fare, data = train_set, method = "lda")
survived_hat <- predict(fit_lda, test_set)
mean(survived_hat == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived ~ Fare, data = train_set, method = "qda")
survived_hat <- predict(fit_qda, test_set)
mean(survived_hat == test_set$Survived)

# Q8
set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Age, data = train_set, method = "glm")
survived_hat <- predict(fit_glm, test_set)
mean(survived_hat == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Sex + Pclass + Fare + Age, data = train_set, method = "glm")
survived_hat <- predict(fit_glm, test_set)
mean(survived_hat == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ ., data = train_set, method = "glm")
survived_hat <- predict(fit_glm, test_set)
mean(survived_hat == test_set$Survived)

# Q9a
set.seed(6, sample.kind = "Rounding")
train_knn <- train(Survived ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(3, 51, 2)), 
             data = train_set)
train_knn$bestTune

# Q9b
ggplot(train_knn)

# Q9c
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

# Q10
set.seed(8, sample.kind = "Rounding")
train_knn <- train(Survived ~ .,  method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 51, 2)), 
                   data = train_set,
                   trControl = trainControl(method = "cv", number=10, p=0.9))
train_knn$bestTune

knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

# Q11a
set.seed(10, sample.kind = "Rounding")
fit_rpart <- train(Survived ~ ., 
                   data=train_set, 
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
fit_rpart$bestTune

rpart_preds <- predict(fit_rpart, test_set)
mean(rpart_preds == test_set$Survived)

# Q11b
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel, cex = 0.75)

# Q12
set.seed(14, sample.kind = "Rounding")
fit_rf <- train(Survived ~ ., 
             data = train_set,
             method = "rf", 
             tuneGrid = data.frame(mtry = seq(1, 7)), 
             ntree = 100)
fit_rf$bestTune
rf_predics <- predict(fit_rf, test_set)
mean(rf_predics == test_set$Survived)
varImp(fit_rf)

