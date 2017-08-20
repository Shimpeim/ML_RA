
imbRF_greed <- function(method,weightRF=1){
  if(method=='WRF'){
    prefix <- sprintf('%s_%s',method,log(weightRF,10))
    treemodel_rf <- tuneRF(
      x=train_imp %>% dplyr::select(-CRRP),
      y=train_imp$CRRP,
      mtryStart=3,
      ntreeTry=5000,
      stepFactor=1.5,
      improve=0.5,
      trace=TRUE, 
      plot=TRUE,
      doBest=TRUE,
      keep.inbag=TRUE,
      importance=TRUE,
      classwt=c(weightRF,1-weightRF)
    )
  }
  if(method=='BRF'){
    weightRF <- 1
    prefix <- sprintf('%s_',method)
    treemodel_rf <- tuneRF(
      x=train_imp %>% dplyr::select(-CRRP),
      y=train_imp$CRRP,
      mtryStart=3,
      ntreeTry=5000,
      stepFactor=1.5,
      improve=0.5,
      trace=TRUE, 
      plot=TRUE,
      doBest=TRUE,
      sampsize=min(table(data_train$CRRP))
    )
  }
  pdf(
    sprintf(
      './%s/%s_%s_%s',
      'Output',
      format(
        Sys.time(),
        "%b%d%H%M%S%Y"),
      prefix,
      'randomForest_output.pdf'
      )
    )
  plot(treemodel_rf,xlim=c(1,500))
  varImpPlot(treemodel_rf,type='2')
  dev.off()
  
  print(treemodel_rf)
  
  result_predict_RF  <-predict(
    treemodel_rf, 
    pred_imp
  )
  result_train_RF <-predict(
    treemodel_rf, 
    train_imp
  )
  return(
    list(
      weight    = weightRF,
      RF_result = treemodel_rf,
      mtry      = treemodel_rf$mtry,
      varImp_list = as.data.frame(randomForest::importance(treemodel_rf),type='2'),
      prefix    = prefix,
      train.comp = as.matrix(table(result_train_RF,train_imp$CRRP)),
      valid.comp = as.matrix(table(result_predict_RF,pred_imp$CRRP)),
      
      sens.valid = as.matrix(table(result_predict_RF,pred_imp$CRRP))[2,2]/
        sum(as.matrix(table(result_predict_RF,pred_imp$CRRP))[,2]),
      spec.valid = as.matrix(table(result_predict_RF,pred_imp$CRRP))[1,1]/
        sum(as.matrix(table(result_predict_RF,pred_imp$CRRP))[,1]),
      
      sens.train = as.matrix(table(result_train_RF,train_imp$CRRP))[2,2]/
        sum(as.matrix(table(result_train_RF,train_imp$CRRP))[,2]),
      spec.train = as.matrix(table(result_train_RF,train_imp$CRRP))[1,1]/
        sum(as.matrix(table(result_train_RF,train_imp$CRRP))[,1])
    )
  )
  
}


Rules_fromRF <- function(treemodel_rf){
  treeList <- RF2List(treemodel_rf) #getTree()
  exec <- extractRules(treeList,data_train,ntree=500)
  ruleMetric <- getRuleMetric(exec,data_train%>%dplyr::select(-CRRP),data_train$CRRP)
  ruleMetric <- pruneRule(ruleMetric,data_train%>%dplyr::select(-CRRP),data_train$CRRP) # Pruning
  ruleMetric <- selectRuleRRF(ruleMetric,data_train%>%dplyr::select(-CRRP),data_train$CRRP)
  learner <- buildLearner(ruleMetric,data_train,data_train$CRRP,minFreq=0.0001)
  readableRules <- presentRules(learner,colnames(data_train)) #ルールを読みやすく加工
  return(readableRules)#結果の表示
}
