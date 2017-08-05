

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
  'inTrees',
  'tree',
  'caret',
  'DMwR'
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
require('inTrees')
require('tree')
require('caret')
require('DMwR')

write(toBibtex(citation()),file="CRAN")
for(i in 1:length(packages)){
  write(toBibtex(citation(packages[i])),file=sprintf("../Biblio/%s%s.bib",packages[i],"_CRAN"))
}

##== subroutines ==##

# ROC #
source(sprintf("%s/%s",dir.sub, ROC.func))

##== ANALYSIS DATA SET IMPORT ==##

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

##== Scaling ==##

data_scaled <- data %>%
  mutate(
    age = scale(age),
    DAS28ESR_auc = scale(DAS28ESR_auc),
    TSS = scale(TSS),
    HAQ = scale(HAQ),
    dose_MTX_base = scale(dose_MTX_base),
    MTX_1yr = scale(MTX_1yr),
    CRP = scale(CRP),
    ESR = scale(ESR),
    erosion = scale(erosion),
    JSN = scale(JSN),
    est1yrTSS = scale(est1yrTSS)
    )

summary(data_scaled)

##== data split (train./ pred.) ==##

## caret package ##
# https://www.slideshare.net/sfchaos/ss-33703018

row_in_train <- createDataPartition(
  data$CRRP,
  p=.8,
  list=FALSE
  )
data_train <- data[row_in_train,  ]
data_pred  <- data[-row_in_train, ]

summary(data_train)
summary(data_pred)

##== RF ==##


source('./Prog/sub/randomforest_20170801.R')

 seq_weightRF <- seq(1,10,by=0.5) 
 weightRF     <- 1*10**(-seq_weightRF) 

ads_imp <- rfImpute(CRRP ~ . ,
                    data_train,
                    iter=10,
                    ntree=500)
pred_imp <- rfImpute(CRRP ~ . ,
                     data_pred,
                     iter=10,
                     ntree=500)

WRF_results <- llply(weightRF,imbRF_greed,method='WRF')
BRF_results <- imbRF_greed(method='BRF')

weight_sens <- data.frame()
for(i in 1:length(seq_weightRF)){
  weight_sens[i,1] <- WRF_results[[i]]$sens
  weight_sens[i,2] <- WRF_results[[i]]$weight
}
max_PPV <- 
#library(ROCR)

RF_to_ROC <- function(list_of_rfResults){
  predictions=as.vector(list_of_rfResults$votes[,2])
  pred=prediction(list_of_rfResults,data_pred)
  
  perf_AUC=performance(pred,"auc") #Calculate the AUC value
  AUC=perf_AUC@y.values[[1]]
  
  perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
  plot(perf_ROC, main="ROC plot")
  text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
}

a <- llply(WRF_results,RF_to_ROC)

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



