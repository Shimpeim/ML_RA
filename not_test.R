

dir.sub  <- "./Prog/sub"
ROC.func <- "functions20170410.R"

#```{r Load libraries}


## LIBRARIES
rm()

packages <- c(
  'dplyr', # progress bar
  'plyr',  # progress bar
  'dplyr', # progress bar
  'tidyr',
  'xlsx',
  'ggplot2',
  'gplots',
  'GMD',
  'pvclust',
  'reshape2',
  'pander',
  'matrixcalc' ,
  'kernlab' ,
  'e1071' ,
  'rpart',
  'rpart.plot',
  'Formula',
  'partykit',
  'randomForest',
  'tree'
  #  'scaleboot' 
  #  'biomaRt'
) 

new.packages <-
  packages[!(packages %in% installed.packages())] 
# installed.packages() returns installed packages 

if(length(new.packages) > 0){ 
  install.packages(new.packages, repos='http://cran.us.r-project.org')
}
require('plyr')  # progress bar
require('dplyr') # progress bar
require('tidyr')
require('xlsx')
require('ggplot2')
require('gplots')
require('GMD')
require('pvclust')
require('reshape2')
require('pander')
require('matrixcalc')
require('kernlab')
require('e1071')
require('rpart')
require('rpart.plot')
require('partykit')
require('randomForest')
require('tree')

write(toBibtex(citation()),file="CRAN")
for(i in 1:length(packages)){
  write(toBibtex(citation(packages[i])),file=sprintf("%s%s.bib",packages[i],"_CRAN"))
}

##== subroutines ==##

# ROC #
source(sprintf("%s/%s",dir.sub, ROC.func))

###
data <- read.csv('../Analysis/Data/170725/data170725.csv') %>%
  mutate(
    CRRP      = as.factor(CRRP),
    cm_bio    = as.factor(cm_bio),
    PSL       = as.factor(PSL),
    sex       = as.factor(sex),
    age       = as.numeric(age),
    MTX_use   = as.factor(MTX_use),
    dose_MTX_base = as.numeric(ifelse(!is.na(dose_MTX_base),dose_MTX_base,0)),
    MTX_1yr       = as.numeric(ifelse(!is.na(MTX_1yr),MTX_1yr,0)),
    autoantibody  = as.factor(ifelse(!is.na(autoantibody),autoantibody,3)),
    duration      = as.factor(ifelse(duration>=3,1,0))
    ) %>%
  dplyr::select(
    -id,
    -MTX_base,
    -csDMARD_change,
    -csDMARD_increase,
    -csDMARD_strength,
    -TSS.1,
    -DAS28ESR_auc.1#,
#    -ESR,
#   -CRP
    )
is.na(data)
summary(data)

data_tidy <-  data %>%
  gather(var,val,-CRRP) %>%
  filter(!is.na(val))
data_summ <- data_tidy %>%
  group_by(CRRP,var) %>%
  summarise(
    n=n(),
    mean=mean(as.numeric(val)),
    sd=sd(as.numeric(val)))

data_summ

## data split (train./ pred.)##

rowdata<-nrow(data)

random_ids<-sample(rowdata,rowdata*0.5)
data_training<-data[random_ids, ]
data_predicting<-data[-random_ids, ]

summary(data_training)
summary(data_predicting)

## RF ##
##

seq_weightRF <- seq(1,10,by=0.5) 
weightRF     <- 1*10**(-seq_weightRF) 

ads_imp <- rfImpute(CRRP ~ . ,
                    data_training,
                    iter=10,
                    ntree=500)
pred_imp <- rfImpute(CRRP ~ . ,
                     data_predicting,
                     iter=10,
                     ntree=500)

RF_weightGreed <- function(weightRF){
  prefix <- weightRF
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
  pdf(sprintf('%s_%s',prefix,'randomForest_output.pdf'))
  print(treemodel_rf)
  plot(treemodel_rf)
  varImpPlot(treemodel_rf)
  varImp_list <- as.data.frame(importance(treemodel_rf))
  
  dev.off()
  
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
      weightRF,
      table(result_predict_RF_ads,data_training$CRRP),
      table(result_predict_RF,data_predicting$CRRP)
    )
  )

}

RF_results <- llply(weightRF,RF_weightGreed)

dev.off()

##== Tree (rpart) ==##

# use RF result (important variables)

treemodel_data <- rpart(
  CRRP ~ CRP+erosion+DAS28ESR_auc+ESR+duration+TSS+est1yrTSS+JSN+age+HAQ,
  data = data_training,
  control = rpart.control(minsplit = 10, minbucket = round(10/3), cp = 0.00001, 
                maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                surrogatestyle = 0, maxdepth = 30)
  )

check_predict_rpart   <-predict(treemodel_data, data_training)
check_tree_prediction <-data.frame(check_predict_rpart,data_training$CRRP) %>%
  mutate(pred = ifelse(X1 > 0.4 ,1 ,0))
hist(check_tree_prediction$X0)
hist(check_tree_prediction$X1)
check_tree_prediction


summary(treemodel_data)
result_predict_rpart  <-predict(treemodel_data, data_predicting)
tree_prediction <-data.frame(result_predict_rpart,data_predicting$CRRP) %>%
  mutate(pred = ifelse(X1 > 0.4 ,1 ,0))
tree_prediction

pdf('rpart_with_varImp.pdf')

rpart.plot(treemodel_data)
plotcp(treemodel_data)
plot(as.party(treemodel_data))

hist(tree_prediction$X0)
hist(tree_prediction$X1)

table(tree_prediction$data_predicting.CRRP, tree_prediction$pred)

dev.off()

# without usage of RF result (all variables)

treemodel_full_data <- rpart(
  CRRP ~ .,
  data = data_training,
  control = rpart.control(minsplit = 10, minbucket = round(10/3), cp = 0.00001, 
                          maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                          surrogatestyle = 0, maxdepth = 30)
)

check_predict_full_rpart   <-predict(treemodel_full_data, data_training)
check_tree_full_prediction <-data.frame(check_predict_full_rpart,data_training$CRRP) %>%
  mutate(pred = ifelse(X1 > 0.4 ,1 ,0))
hist(check_tree_full_prediction$X0)
hist(check_tree_full_prediction$X1)
check_tree_full_prediction

summary(treemodel_full_data)
result_predict_full_rpart  <-predict(treemodel_full_data, data_predicting)
tree_full_prediction <-data.frame(result_predict_full_rpart,data_predicting$CRRP) %>%
  mutate(pred = ifelse(X1 > 0.4 ,1 ,0))
tree_full_prediction

rpart.plot(treemodel_full_data)
plotcp(treemodel_full_data)
plot(as.party(treemodel_full_data))

hist(tree_full_prediction$X0)
hist(tree_full_prediction$X1)

table(tree_prediction$data_predicting.CRRP, tree_prediction$pred)

##
## Tree (C5.0) ##

## NOT WORK ##

treemodel_data <- C5.0(
  x = data_training %>%
    dplyr::select(CRP,erosion,DAS28ESR_auc),
  y = data_training$CRRP
)
summary(treemodel_data)
result_predict_C50  <-predict(treemodel_data, data_predicting)
table(result_predict_C50,data_predicting$CRRP)
plot(treemodel_data)
##


##== SVM ==

## tune.svm()
## http://d.hatena.ne.jp/hoxo_m/20110325/p1

# the first search 

gammaRange = 10^(-5:5)
costRange = 10^(-5:5)

train.RBF <- tune.svm(CRRP ~., data=data_training, class.weights = 100/table(data_training$CRRP),
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

train.RBF <- tune.svm(CRRP ~., data=data_training, gamma=gammaRange, cost=costRange,
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




##== ROC ==##

# ROC Curve
#```{r ROC curve}

#pdf(file=sprintf(
#  "%s/%s.pdf",
#  wd,
#  paste(ROC.File.output,"")
#)#,
#width=30,
#height = 70
#)
#ROC(
#  ads$X, 
#  ads$Y,
#  col.area=gray(0.9), type="o", pch=16
#)
#dev.off()



