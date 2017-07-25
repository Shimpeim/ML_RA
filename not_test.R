
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
  'rpart',
  'rpart.plot',
  'Formula',
  'partykit',
  'randomForest'
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
require('rpart')
require('rpart.plot')
require('partykit')
require('randomForest')

###
data <- read.csv('../Analysis/Data/170725/data170725.csv') %>%
  mutate(
    CRRP      = as.factor(CRRP),
    cm_bio    = as.factor(cm_bio),
    PSL       = as.factor(PSL),
    sex       = as.factor(sex),
    age       = as.numeric(age),
    MTX_use   = as.factor(MTX_use),
    dose_MTX_base = as.numeric(dose_MTX_base),
    MTX_1yr       = as.numeric(MTX_1yr) ,
    autoantibody  = as.factor(autoantibody)
    ) %>%
  dplyr::select(
    -id,
    -MTX_base,
    -csDMARD_change,
    -csDMARD_increase,
    -csDMARD_strength,
    -TSS.1,
    -DAS28ESR_auc.1
    )

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

## RF ##
##

ads_imp <- rfImpute(CRRP ~ . ,
                    data_training,
                    iter=10,
                    ntree=5000)
pred_imp <- rfImpute(CRRP ~ . ,
                     data_predicting,
                     iter=10,
                     ntree=5000)

treemodel_rf <- tuneRF(
  x=ads_imp %>% dplyr::select(-CRRP),
  y=ads_imp$CRRP,
  mtryStart=2,
  ntreeTry=5000,
  stepFactor=0.00005,
  improve=0.00005,
  trace=TRUE, 
  plot=TRUE,
  doBest=TRUE
)

print(treemodel_rf)
plot(treemodel_rf)
varImpPlot(treemodel_rf)

result_predict_RF  <-predict(
  treemodel_rf, 
  pred_imp
  )
table(result_predict_RF,data_predicting$CRRP)



## Tree (rpart) ##

treemodel_data <- rpart(
  CRRP ~ CRP+erosion+DAS28ESR_auc+ESR+duration+TSS+est1yrTSS+JSN+age+HAQ,
  data = data_training,
  control = rpart.control(minsplit = 10, minbucket = round(10/3), cp = 0.00001, 
                maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                surrogatestyle = 0, maxdepth = 30)
  )

summary(treemodel_data)
result_predict_rpart  <-predict(treemodel_data, data_predicting)
tree_prediction <-data.frame(result_predict_rpart,data_predicting$CRRP) %>%
  mutate(pred = ifelse(X1 > 0.4 ,1 ,0))
tree_prediction


rpart.plot(treemodel_data)
plotcp(treemodel_data)
plot(as.party(treemodel_data))

hist(tree_prediction$X0)
hist(tree_prediction$X1)

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

## SVM ##
##http://yut.hatenablog.com/entry/20120827/1346024147#

# kernel functions (help("ksvm"))

# rbfdot Radial Basis kernel "Gaussian"
# polydot Polynomial kernel
# vanilladot Linear kernel
# tanhdot Hyperbolic tangent kernel
# laplacedot Laplacian kernel
# besseldot Bessel kernel
# anovadot ANOVA RBF kernel
# splinedot Spline kernel
# stringdot String kernel

data_svm_RBF  <-ksvm(CRRP ~., data=data_training, kernel="rbfdot" )
data_svm_poly <-ksvm(CRRP ~., data=data_training, kernel="polydot" )
data_svm_lin  <-ksvm(CRRP ~., data=data_training, kernel="vanilladot" )
data_svm_tanh <-ksvm(CRRP ~., data=data_training, kernel="tanhdot" )
data_svm_LaP  <-ksvm(CRRP ~., data=data_training, kernel="laplacedot" )
data_svm_besseldot <-ksvm(CRRP ~., data=data_training, kernel="besseldot" )
data_svm_anovadot  <-ksvm(CRRP ~., data=data_training, kernel="anovadot" )
data_svm_splinedot <-ksvm(CRRP ~., data=data_training, kernel="splinedot" )

data_svm_RBF
data_svm_poly
data_svm_lin
data_svm_tanh
data_svm_LaP
data_svm_besseldot
data_svm_anovadot
data_svm_splinedot

result_predict_RBF  <-predict(data_svm_RBF, data_predicting)
result_predict_poly <-predict(data_svm_poly, data_predicting)
result_predict_lin  <-predict(data_svm_lin, data_predicting)
result_predict_tanh <-predict(data_svm_tanh, data_predicting)
result_predict_LaP  <-predict(data_svm_LaP, data_predicting)
result_predict_anovadot  <-predict(data_svm_anovadot, data_predicting)
result_predict_splinedot <-predict(data_svm_splinedot, data_predicting)

table(result_predict_RBF,data_predicting$CRRP)
table(result_predict_poly,data_predicting$CRRP)
table(result_predict_lin,data_predicting$CRRP)
table(result_predict_tanh,data_predicting$CRRP)
table(result_predict_LaP,data_predicting$CRRP)
table(result_predict_anovadot,data_predicting$CRRP)
table(result_predict_splinedot,data_predicting$CRRP)

##



