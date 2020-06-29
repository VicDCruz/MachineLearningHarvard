set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

pHealthy = length(which(disease == 0)) / length(disease)
pDisease = 1 - pHealthy

pTestNegativeHealthy = length(which(test[disease == 0] == 0)) / length(test[disease == 0])
pTestPositiveHealthy = 1 - pTestNegativeHealthy
pTestPositiveDisease = length(which(test[disease == 1] == 1)) / length(test[disease == 1])
pTestNegativeDisease = 1 - pTestPositiveDisease

pTestPositive = mean(test)
pTestNegative = 1 - pTestPositive

pDiseaseTestPositive = mean(disease[test==1]==1)

pDiseaseTestNegative = mean(disease[test==0])

pDiseaseTestPositive / pDisease

