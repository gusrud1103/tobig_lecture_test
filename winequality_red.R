rm(list=ls())
# data load
data <- read.csv("C:/R/wq_red.csv", header = T) # 1599 obs, 12 variables
str(data)
head(data)
sum(is.na(data)) # 결측치 없음 
data$quality <- as.factor(data$quality) # 범주화 



#*********************************** 기초 통계량
summary(data)
for(i in 1:11){
  print(round(sd(data[,i]),digits=2))
} # 표준편차 확인 
colMeans(data[,-12]) # 평균 확인
 
X=scale(data[,c(-12)],center=FALSE,scale=TRUE) # quality변수 제외한 표준화
scale_R=cor(X); scale_R # 표준화된 correlation
round(scale_R,digits=2) # 표준화된 correlation,소수점 정리 

# 바이올린 그래프 
ggplot(data=data,aes(x=quality,y=data[,1],fill=quality))+ 
  geom_violin(trim=FALSE)+ 
  geom_boxplot(fill='darkred',width=0.1)+ 
  stat_summary(geom='point',fun.y=mean,shape=23,size=3)+ 
  geom_point(position='jitter')+ 
  scale_fill_brewer(palette='Pastel2')+ 
  theme(legend.position='none')


# 범주형 quality변수 도표와 도넛그래프

table(data[,12]) # 도수 빈도표 

library(ggplot2)
dat = data.frame(count=c(10,53,681,638,199,18), category=c("3","4","5","6","7","8"))
dat$percentage=dat$count/sum(dat$count)*100
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))
# https://gist.github.com/chelsyx/ebc8ff5d7125a79d4297 
p2 = ggplot(dat, aes(label = paste(round(percentage,2),"%"),fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(colour="grey30") +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_bw() +
  scale_fill_manual(values = c("#F361A6","#ED006D","#E69F00", "#D55E00","#993800","#662500"), name="")+
  ggtitle("quality dount plot")+
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) 
p2 # 도넛 그래프 


# 상관행렬 시각화 그래프
library(lattice)
levelplot(as.matrix(scale_R),main="correlation of red wine quality variables")
#install.packages("magrittr")
library(magrittr)
library(corrplot)
data[,-12] %>% cor() %>% corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)


pairs(data, panel=function(x,y,...){
  points(x,y,...)
  abline(lm(y~x),col="blue")},pch=".",cex=1.5) # 산점도 행렬(회귀선을 포함) 



#***********************************  주성분분석
# 표준화된 상관행렬로 주성분 분석 
# prcomp함수
pca_prcomp = prcomp(data[,c(-12)], scale=TRUE) # quality변수 제외
attributes(pca_prcomp)
print(round(pca_prcomp$rotation,digits=2))
summary(pca_prcomp) # 주성분 분석 요약 (주성분 6개로 대략86%까지 설명 가능)
plot(pca_prcomp$sdev^2, xlab="Component number",
     ylab="Component variance", type="l", main="Scree diagram") # 스크리 그림, elbow point number=6
abline(v=6,col="red")

round(eigen(scale_R)$values,digits=3) # 고유값 (<3.1.1>)

biplot(pca_prcomp,col=c("yellow","red")) # 행렬도 




#***********************************  인자 분석
plot(prcomp(scale_R),type="l",main="Scree Plot",sub="number") # 스크리 그림 

(ff1=factanal(covmat=scale_R, factors=4,method="mle",n.obs=1599,rotation="none")) # 6개로 하면 74%설명 가능,none 
(ff2=factanal(covmat=scale_R, factors=4,method="mle",n.obs=1599,rotation="varimax")) # varimax(직교 회전)
(ff3=factanal(covmat=scale_R, factors=4,method="mle",n.obs=1599,rotation="promax")) # promax(사각 회전) 
#############################
par(mfrow=c(2,3))
#dev.off()  # mfrow끄는 코드 
DD2=factanal(covmat=scale_R, factors=4,method="mle",n.obs=1599,rotation="varimax")
plot(DD2$loadings[,1],DD2$loadings[,2],xlab="factor1",ylab = "factor2",type="n") # 직교 회전
text(DD2$loadings[,1],DD2$loadings[,2],colnames(scale_R))
abline(v=0,col="orange");abline(h=0,col="orange")
plot(DD2$loadings[,1],DD2$loadings[,3],xlab="factor1",ylab="factor3",type="n") # 직교 회전
text(DD2$loadings[,1],DD2$loadings[,3],colnames(scale_R))
abline(v=0,col="orange");abline(h=0,col="orange")
plot(DD2$loadings[,1],DD2$loadings[,4],xlab="factor1",ylab="factor4",type="n") # 직교 회전
text(DD2$loadings[,1],DD2$loadings[,4],colnames(scale_R))
abline(v=0,col="orange");abline(h=0,col="orange")
plot(DD2$loadings[,2],DD2$loadings[,3],xlab="factor2",ylab="factor3",type="n") # 직교 회전
text(DD2$loadings[,2],DD2$loadings[,3],colnames(scale_R))
abline(v=0,col="orange");abline(h=0,col="orange")
plot(DD2$loadings[,2],DD2$loadings[,4],xlab="factor2",ylab="factor4",type="n") # 직교 회전
text(DD2$loadings[,2],DD2$loadings[,4],colnames(scale_R))
abline(v=0,col="orange");abline(h=0,col="orange")
plot(DD2$loadings[,3],DD2$loadings[,4],xlab="factor3",ylab="factor4",type="n") # 직교 회전
text(DD2$loadings[,3],DD2$loadings[,4],colnames(scale_R))
abline(v=0,col="orange");abline(h=0,col="orange")


#######################
# DD=factanal(covmat=scale_R, factors=4,method="mle",n.obs=1599,rotation="none")
# plot(DD$loadings[,1],DD$loadings[,2],main="before rotation",type="n") # none
# text(DD$loadings[,1],DD$loadings[,2],colnames(scale_R))
# 
# DD2=factanal(covmat=scale_R, factors=4,method="mle",n.obs=1599,rotation="varimax")
# plot(DD2$loadings[,1],DD2$loadings[,2],main="varimax",type="n") # 직교 회전
# text(DD2$loadings[,1],DD2$loadings[,2],colnames(scale_R))
# 
# DD3=factanal(covmat=scale_R, factors=4,method="mle",n.obs=1599,rotation="promax")
# plot(DD3$loadings[,1],DD3$loadings[,2],main="promax",type="n") # 사각 회전 
# text(DD3$loadings[,1],DD3$loadings[,2],colnames(scale_R))




#***********************************   군집 분석
# Kmeans
# training dataset, test dataset분류 , http://www.dodomira.com/2016/02/20/r%EC%9D%84-%EC%82%AC%EC%9A%A9%ED%95%9C-k-means-%EA%B5%B0%EC%A7%91%EB%B6%84%EC%84%9D-k-means-clustering-in-r/
library(caret)
set.seed(123)
inTrain <- createDataPartition(y=data$quality, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
training.data <- scale(training[-12])
summary(training.data)


library(ggplot2)
library(ggalt)
library(ggfortify)


data.kmeans=kmeans(training.data[,-12], centers = 2, iter.max = 10000)
data.kmeans$centers
training$cluster<-as.factor(data.kmeans$cluster)
qplot(training$alcohol,training$fixed.acidity,colour=cluster,data=training)

table(training$quality, training$cluster)


wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(training.data)


#install.packages("NbClust")
library(NbClust)

nc <- NbClust(training.data, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")



training.data<-as.data.frame(training.data)
modFit <- train(x=training.data[,-12],
                y=training$cluster,
                method="rpart")


testing.data<-as.data.frame(scale(testing[-12]))
testClusterPred <- predict(modFit,testing.data) 
table(testClusterPred ,testing$quality)


# 각 군집별 통계량 
round(aggregate(training[,c(-12,-13)],by=list(clusters=data.kmeans$cluster),mean),2)
round(aggregate(training[,c(-12,-13)],by=list(clusters=data.kmeans$cluster),sd),2)
aggregate(training[,c(-12,-13)],by=list(clusters=data.kmeans$cluster),length)

# 
# # 군집 그림 
# #install.packages("ggfortify")
# #install.packages("ggalt")
# library(ggplot2)
# library(ggalt)
# library(ggfortify)
# theme_set(theme_classic())

# Compute data with principal components ------------------

# # Data frame of principal components ----------------------
# df_pc <- data.frame(pca_prcomp $x, quality=data$quality)  # dataframe of principal components
# df_pc_1 <- df_pc[df_pc$quality == "3", ] 
# df_pc_1 <- rbind(df_pc_1 ,df_pc[df_pc$quality == "4", ]  )
# df_pc_1 <- rbind(df_pc_1 ,df_pc[df_pc$quality == "5", ]  )
# df_pc_2 <- df_pc[df_pc$quality == "6", ]  
# df_pc_2 <- rbind(df_pc_2 ,df_pc[df_pc$quality == "7", ]  )
# df_pc_2 <- rbind(df_pc_2 ,df_pc[df_pc$quality == "8", ]  )
# 
# # Plot ----------------------------------------------------
# ggplot(df_pc, aes(PC1, PC2, col=c("red","orange"))) +
#   geom_point(aes(shape=c("1","2")), size=2) +   # draw points
#   labs(title="Wine Clustering",
#        subtitle="With principal components PC1 and PC2 as X and Y axis",
#        caption="Source: Wine quality") +
#   coord_cartesian(xlim = 1.2 * c(min(df_pc$PC1), max(df_pc$PC1)),
#                   ylim = 1.2 * c(min(df_pc$PC2), max(df_pc$PC2))) +   # change axis limits
#   geom_encircle(data = df_pc_1, aes(x=PC1, y=PC2)) +   # draw circles
#   geom_encircle(data = df_pc_2, aes(x=PC1, y=PC2)) 



#**************************random forest 
# Random Forest
#install.packages("randomForest")
library(caret)
require(randomForest)
set.seed(100)
Wine <- createDataPartition(y = data$quality, p = 0.7, list = F) # cross validation 
training <- data[Wine,]
testing <- data[-Wine,]

fit = randomForest(training$quality~ .,importance=T ,data=training)
summary(fit) # 모형 적합 
pred = predict(fit, newdata = testing[,-12])
confusionMatrix(pred, testing[,12]) # 행렬 
plot(fit)
fit$importance # 변수 중요도
varImpPlot(fit) # 변수 중요도 그림 


