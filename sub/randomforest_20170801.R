## Random Forest with grid search

imbRF_greed <- function(method,weightRF=1){
  if(method=='WRF'){
    prefix <- sprintf('%s_%s',method,weightRF)
    treemodel_rf <- tuneRF(
      x=ads_imp %>% dplyr::select(-CRRP),
      y=ads_imp$CRRP,
      mtryStart=4,
      ntreeTry=500,
      stepFactor=1.5,
      improve=0.5,
      trace=TRUE, 
      plot=TRUE,
      doBest=TRUE,
      classwt=c(weightRF,1-weightRF)
    )
  }
  if(method=='BRF'){
    weightRF <- 1
    prefix <- sprintf('%s_',method)
    treemodel_rf <- tuneRF(
      x=ads_imp %>% dplyr::select(-CRRP),
      y=ads_imp$CRRP,
      mtryStart=4,
      ntreeTry=500,
      stepFactor=1.5,
      improve=0.5,
      trace=TRUE, 
      plot=TRUE,
      doBest=TRUE,
      sampsize=min(table(data_train$CRRP))
    )
  }
  pdf(sprintf('%s_%s',prefix,'randomForest_output.pdf'))
  print(treemodel_rf)
  plot(treemodel_rf)
  varImpPlot(treemodel_rf)
  varImp_list <- as.data.frame(importance(treemodel_rf))
  
  dev.off()
  
  treeList <- RF2List(treemodel_rf) #getTree()
  exec <- extractRules(treeList,data_train,ntree=500)
  ruleMetric <- getRuleMetric(exec,data_train,data_train$CRRP)
  ruleMetric <- pruneRule(ruleMetric,data_train,data_train$CRRP) # Pruning
  ruleMetric <- selectRuleRRF(ruleMetric,data_train,data_train$CRRP)
  learner <- buildLearner(ruleMetric,data_train,data_train$CRRP,minFreq=0.01)
  readableRules <- presentRules(learner,colnames(data_train)) #ルールを読みやすく加工
  readableRules#結果の表示
  
  result_predict_RF  <-predict(
    treemodel_rf, 
    pred_imp
  )
  result_predict_RF_ads  <-predict(
    treemodel_rf, 
    ads_imp
  )
  return(
    list(
      weight    = weightRF,
      RF_result = result_predict_RF_ads,
      readableRules = as.data.frame(readableRules),
      prefix    = prefix,
      direct.pred = as.matrix(table(result_predict_RF_ads,data_train$CRRP)),
      true.pred   = as.matrix(table(result_predict_RF,data_pred$CRRP)),
      sens        = as.matrix(table(result_predict_RF,data_pred$CRRP))[2,2]/
        sum(as.matrix(table(result_predict_RF,data_pred$CRRP))[,2])
    )
  )
  
}





## caret package ##
# https://www.slideshare.net/sfchaos/ss-33703018
# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# Create model with default paramters
control <- trainControl(
  method="repeatedcv", 
  number=10, repeats=30,
  classProbs = TRUE,
  sampling = "smote",
  
  search = 'grid')
seed <- 7
metric <- "ROC"
set.seed(seed)
tuneGrid <- expand.grid(
  mtry = sqrt(ncol(data_train)) + seq(-2,2,1)
)
disc.RF <- train(
  CRRP ~ ., data=data_train[complete.cases(data_train),],
  method="rf",
  metric="ROC", 
  tuneGrid=tuneGrid, 
  trControl=control
)
plot(disc.RF)
predict(disc.RF,data_pred)
importance(disc.RF)

confusionMatrix(
  data = predict(disc.RF,data_pred), 
  reference = data_pred[complete.cases(data_pred),'CRRP'],
  positive = '1'
)