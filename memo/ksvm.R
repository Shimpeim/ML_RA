## SVM ##
##http://yut.hatenablog.com/entry/20120827/1346024147#
##https://bi.biopapyrus.jp/ai/machine-learning/svm/r/e1071.html

# kernel functions (help("ksvm"))
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

result_predict <- data_predicting

result_predict[
  complete.cases(result_predict), "RBF"] <- predict(data_svm_RBF, data_predicting)
result_predict[
  complete.cases(result_predict), "poly"] <- predict(data_svm_poly, data_predicting)
result_predict[
  complete.cases(result_predict), "lin"] <- predict(data_svm_lin, data_predicting)
result_predict[
  complete.cases(result_predict), "tanh"] <- predict(data_svm_tanh, data_predicting)
result_predict[
  complete.cases(result_predict), "LaP"] <- predict(data_svm_LaP, data_predicting)
result_predict[
  complete.cases(result_predict), "anovadot"] <- predict(data_svm_anovadot, data_predicting)
result_predict[
  complete.cases(result_predict), "splinedot"] <- predict(data_svm_splinedot, data_predicting)


result_predict$poly <-predict(data_svm_poly,data_predicting)
result_predict_lin  <-predict(data_svm_lin, data_predicting)
result_predict_tanh <-predict(data_svm_tanh,data_predicting)
result_predict_LaP  <-predict(data_svm_LaP,  pred_imp)
result_predict_anovadot  <-predict(data_svm_anovadot, pred_imp)
result_predict_splinedot <-predict(data_svm_splinedot,  pred_imp)

table(result_predict$RBF,data_predicting$CRRP)
table(result_predict_poly,data_predicting$CRRP)
table(result_predict_lin,data_predicting$CRRP)
table(result_predict_tanh,data_predicting$CRRP)
table(result_predict_LaP,data_predicting$CRRP)
table(result_predict_anovadot,data_predicting$CRRP)
table(result_predict_splinedot,data_predicting$CRRP)

plot_predict_poly      <-plot(data_svm_poly,  data = pred_imp)
plot_predict_lin       <-plot(data_svm_lin,  data = pred_imp)
plot_predict_tanh      <-plot(data_svm_tanh,  data = pred_imp)
plot_predict_LaP      <-plot(data_svm_LaP,  data = pred_imp)
plot_predict_anovadot  <-plot(data_svm_anovadot, data = pred_imp)
plot_predict_splinedot <-plot(data_svm_splinedot,  data = pred_imp)