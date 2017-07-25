
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

#if( !("biomaRt" %in% installed.packages()) ){
#  source('http://bioconductor.org/biocLite.R')
#  biocLite( 'biomaRt' )
#  require( 'biomaRt' )
#}else  require( 'biomaRt' )

if( !("scaleboot" %in% installed.packages()) ){
  install.packages(
    sprintf(fmt = '%s%s',
            pkgsDirectry,
            "scaleboot_0.3-3.tar.gz"  
            # ver. 16-May-2010 16:21
            # https://cran.r-project.org/src/contrib/Archive/scaleboot/
    ),
    repos = NULL, type = "source")
  require( 'scaleboot' )
}else require( 'scaleboot' )


###

pairs(iris)
summary(iris)

iris_tidy <-  iris %>%
  gather(var,val,-Species)
iris_summ <- iris_tidy %>%
  group_by(Species,var) %>%
  summarise(n=n(),mean=mean(val),sd=sd(val))

iris_summ

## data split (train./ pred.)##

rowdata<-nrow(iris)

random_ids<-sample(rowdata,rowdata*0.5)
iris_training<-iris[random_ids, ]
iris_predicting<-iris[-random_ids, ]

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

iris_svm_RBF  <-ksvm(Species ~., data=iris_training, kernel="rbfdot" )
iris_svm_poly <-ksvm(Species ~., data=iris_training, kernel="polydot" )
iris_svm_lin  <-ksvm(Species ~., data=iris_training, kernel="vanilladot" )
iris_svm_tanh <-ksvm(Species ~., data=iris_training, kernel="tanhdot" )
iris_svm_LaP  <-ksvm(Species ~., data=iris_training, kernel="laplacedot" )
iris_svm_besseldot <-ksvm(Species ~., data=iris_training, kernel="besseldot" )
iris_svm_anovadot  <-ksvm(Species ~., data=iris_training, kernel="anovadot" )
iris_svm_splinedot <-ksvm(Species ~., data=iris_training, kernel="splinedot" )

iris_svm_RBF
iris_svm_poly
iris_svm_lin
iris_svm_tanh
iris_svm_LaP
iris_svm_besseldot
iris_svm_anovadot
iris_svm_splinedot

result_predict_RBF  <-predict(iris_svm_RBF, iris_predicting)
result_predict_poly <-predict(iris_svm_poly, iris_predicting)
result_predict_lin  <-predict(iris_svm_lin, iris_predicting)
result_predict_tanh <-predict(iris_svm_tanh, iris_predicting)
result_predict_LaP  <-predict(iris_svm_LaP, iris_predicting)
result_predict_anovadot  <-predict(iris_svm_anovadot, iris_predicting)
result_predict_splinedot <-predict(iris_svm_splinedot, iris_predicting)

table(result_predict_RBF,iris_predicting$Species)
table(result_predict_poly,iris_predicting$Species)
table(result_predict_lin,iris_predicting$Species)
table(result_predict_tanh,iris_predicting$Species)
table(result_predict_LaP,iris_predicting$Species)
table(result_predict_anovadot,iris_predicting$Species)
table(result_predict_splinedot,iris_predicting$Species)



## Tree ##
##


data(churn)
treemodel <- C5.0(x = churnTrain[,-20],y = churnTrain$churn)
summary(treemodel)

treemodel_iris <- C5.0(x = iris_training[,-20],y = iris_training$Species)
summary(treemodel_iris)
result_predict_C50  <-predict(treemodel_iris, iris_predicting)
table(result_predict_C50,iris_predicting$Species)

##

## RF ##
##

ads_imp <- rfImpute(Species ~ . ,
                    iris_training,
                    ntree=500)

treemodel_rf <- tuneRF(
  x=iris_training %>% select(-Species),
  y=as.factor(iris_training$Species),
  mtryStart=1,
  ntreeTry=2,
  stepFactor=0.0005,
  improve=0.0005,
  trace=TRUE, 
  plot=TRUE,
  doBest=TRUE
)

print(treemodel_rf)
plot(treemodel_rf)
varImpPlot(treemodel_rf)

result_predict_RF  <-predict(treemodel_rf, iris_predicting)
table(result_predict_RF,iris_predicting$Species)






