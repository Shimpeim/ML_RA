
as.tree_treemodel_data <- tree(CRRP ~ est1yrTSS+TSS,
                               data = data_training)
p <- data %>%
  ggplot(aes(x=est1yrTSS,y=TSS,group=CRRP))
p <- p + geom_point(aes(colour=CRRP),size=0.02)

plot.new()
plot(data_training$est1yrTSS,data_training$TSS,col=data_training$CRRP,cex=0.3)
partition.tree(as.tree_treemodel_data, add = TRUE)


##==パッケージユーザーのための機械学習(5)：ランダムフォレスト==##
## http://tjo.hatenablog.com/entry/2013/12/24/190000

treemodel_rf <- tuneRF(CRRP~.,ads_imp,classwt=c(0.05,0.95))

px<-seq(-10,300,50)
py<-seq(-10,400,50)
pgrid<-expand.grid(px,py)
names(pgrid)<-c("x","y")
# 分離超平面を描くためのグリッドを作る

plot(
  data_training[
    data_training$CRRP==0,
    c("est1yrTSS","TSS")
    ],
  col="blue",pch=19,cex=0.6,xlim=c(-10,300),ylim=c(-10,400)
  )
points(
  data_training[
    data_training$CRRP==1,
    c("est1yrTSS","TSS")],
  col="red",pch=19,cex=0.6)
par(new=T)
contour(
  px,
  py,
  array(
    treemodel_rf,
    dim=c(length(px),length(py))
    ),
  xlim=c(-10,300),
  ylim=c(-10,400),
  col="purple",
  lwd=0.6,drawlabels=F,levels=0.5
  )
# シンプルパターンで分離超平面を描く