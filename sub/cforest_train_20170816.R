
tc <- trainControl(method="cv",
#                   index=indexList,
                   savePredictions=T,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary)
createCfGrid <- function(len, data) {
  g = createGrid("cforest", len, data)
  g = expand.grid(.controls = cforest_unbiased(ntree = 1000))
  return(g)
}
set.seed(1)
(cfMatFit <- train(CRRP ~ .,
                   data=train_imp,
                   method="cforest",
                   metric="ROC",
                   trControl=tc,
                   tuneGrid = createCfGrid))


tmp <- iris
names(tmp)[5] <- ".outcome"
head(tmp)
createGrid("cforest", data = tmp, len = 4)


