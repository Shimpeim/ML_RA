
##== SVM ==

## caret package ##
# https://www.slideshare.net/sfchaos/ss-33703018

disc.svm <- train(
  CRRP ~ ., data=data_train[complete.cases(data_train),],
  method="svmRadial",
  tuneGrid=expand.grid(
    C = 10**seq(-10,-5,1),
    sigma = 10**seq(5,10,1)
  ),
  trControl=trainControl(
    method='cv',
    number=10
  )
)



## tune.svm()
## http://d.hatena.ne.jp/hoxo_m/20110325/p1

# the first search 

gammaRange = 10^(-5:5)
costRange = 10^(-5:5)

train.RBF <- tune.svm(CRRP ~., data=data_train, class.weights = 100/table(data_train$CRRP),
                      gamma=gammaRange, cost=costRange,
                      tunecontrol = tune.control(sampling="cross", cross=8))

cat("- best parameters:\n")
cat(
  "gamma =", 
  train.RBF$best.parameters$gamma, 
  "; cost =",
  train.RBF$best.parameters$cost,
  ";\n")
cat(
  "accuracy:", 
  100 - train.RBF$best.performance * 100, 
  "%\n\n"
)
plot(
  train.RBF, transform.x=log10, transform.y=log10)

# the second search

gamma <- 10^(-1.5)
cost  <- 10^(1.5)
gammaRange <- 10^seq(log10(gamma)-1,log10(gamma)+1,length=11)[2:10]
costRange  <- 10^seq(log10(cost)-1 ,log10(cost)+1 ,length=11)[2:10]

train.RBF <- tune.svm(CRRP ~., data=data_train, gamma=gammaRange, cost=costRange,
                      tunecontrol = tune.control(sampling="cross", cross=10))

cat("- best parameters:\n")
cat(
  "gamma =", 
  train.RBF$best.parameters$gamma, 
  "; cost =",
  train.RBF$best.parameters$cost,
  ";\n")
cat(
  "accuracy:", 
  100 - train.RBF$best.performance * 100, 
  "%\n\n"
)
plot(
  train.RBF, transform.x=log10, transform.y=log10)

## prediction

gamma = 0.05011872 ; cost = 5.011872 ;

SVM.model_RBF <- svm(CRRP ~ ., data = data_training, gamma=gamma, cost=cost)

result_predict[
  complete.cases(result_predict), "RBF"] <- predict(SVM.model_RBF, data_predicting)
table(result_predict$RBF, result_predict$CRRP)




data_svm_poly <-ksvm(CRRP ~., data=data_training, kernel="polydot" )
data_svm_lin  <-ksvm(CRRP ~., data=data_training, kernel="vanilladot" )
data_svm_tanh <-ksvm(CRRP ~., data=data_training, kernel="tanhdot" )
data_svm_LaP  <-ksvm(CRRP ~., data=data_training, kernel="laplacedot" )
data_svm_besseldot <-ksvm(CRRP ~., data=data_training, kernel="besseldot" )
data_svm_anovadot  <-ksvm(CRRP ~., data=data_training, kernel="anovadot" )
data_svm_splinedot <-ksvm(CRRP ~., data=data_training, kernel="splinedot" )


