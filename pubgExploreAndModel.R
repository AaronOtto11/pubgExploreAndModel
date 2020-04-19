library(readr)
test_V2 <- read_csv("test_V2.csv")
View(test_V2)
library(readr)
train_V2 <- read_csv("train_V2.csv")
View(train_V2)
load("~/pubg/current.RData")

killsByGroup= aggregate(train_V2$kills, by=list(groupId= train_V2$groupId), FUN=sum)
trainAdded=merge(train_V2,killsByGroup,by="groupId")
killsByGroup= aggregate(train_V2$kills, by=list(groupId= train_V2$groupId), FUN=sum)
trainAdded = rename(trainAdded,c("x"="totalGroupKills"))
mins <- trainAdded$matchDuration/60
kpm <- trainAdded$kills/mins
Groupkpm <- trainAdded$totalGroupKills/mins
trainAdded=cbind(trainAdded,kpm)
trainAdded=cbind(trainAdded,Groupkpm)
library(plyr)
trainAdded = rename(trainAdded,c("x"="totalGroupKills"))
trainAdded=cbind(trainAdded,Groupkpm)
Groupkpm <- trainAdded$totalGroupKills/mins
trainAdded=cbind(trainAdded,Groupkpm)
averageKills= aggregate(train_V2$kills, by=list(groupId= train_V2$groupId), FUN=mean)
averageMovement= aggregate(train_V2$walkDistance+train_V2$swimDistance+train_V2$rideDistance, by=list(groupId= train_V2$groupId), FUN=mean)
trainAdded=merge(trainAdded,averageMovement,by="groupId")
trainAdded = rename(trainAdded,c("x"="averageMovement"))
ggplot(data= trainAdded[sample(nrow(trainAdded),200000),], aes(averageKills,winPlacePerc)) +geom_point()+facet_wrap(~matchType)
ggplot(data= trainAdded[sample(nrow(trainAdded),200000),], aes(averageKills,winPlacePerc)) +geom_point()+facet_wrap(~matchType)
ggplot(data= trainAdded[sample(nrow(trainAdded),200000),], aes(averageMovement,winPlacePerc)) +geom_point()+facet_wrap(~matchType)
avgDamage= aggregate(train_V2$damageDealt, by=list(groupId= train_V2$groupId), FUN=mean)
trainAdded=merge(trainAdded,avgDamage,by="groupId")
trainAdded = rename(trainAdded,c("x"="avgDamage"))
avgKPM= aggregate(train_V2$kills/(train_V2$matchDuration/60), by=list(groupId= train_V2$groupId), FUN=mean)
trainAdded=merge(trainAdded,avgKPM,by="groupId")
trainAdded = rename(trainAdded,c("x"="avgKPM"))
ggplot(data= trainAdded[sample(nrow(trainAdded),200000),], aes(avgKPM,winPlacePerc)) +geom_point()+facet_wrap(~matchType)
avgHeadshotPerc= aggregate(train_V2$kills/train_V2$headshotKills, by=list(groupId= train_V2$groupId), FUN=mean)
trainAdded=merge(trainAdded,avgHeadshotPerc,by="groupId")
trainAdded = rename(trainAdded,c("x"="avgHeadshotPerc"))
ggplot(data= trainAdded[sample(nrow(trainAdded),200000),], aes(avgHeadshotPerc,winPlacePerc)) +geom_point()+facet_wrap(~matchType)
trainAdded$avgHeadshotPerc[!is.finite(trainAdded$avgHeadshotPerc)] <- 0
model1 = lm(winPlacePerc~averageKills+averageMovement+avgDamage+avgKPM,trainAdded)
summary(model1)
install.packages("ggiraphExtra")
install.packages("caTools")
library(ggplot2)
library(ggiraphExtra)
library(caTools)
sampleSize=floor(.8*nrow(trainAdded))
set.seed(123)
trainModel1=sample(seq_len(nrow(trainAdded)),size=sampleSize)
trainData=trainAdded[trainModel1,]
testData=trainAdded[-trainModel1,]
model1 = lm(winPlacePerc~averageKills+averageMovement+avgDamage+avgKPM,trainData)
ggplot(data=trainData,mapping=aes(x=averageMovement,y=winPlacePerc))+geom_point()+stat_smooth(method="lm",col="dodgerblue3")+theme(panel.background = element_rect(fill = "white"),
                                                                                                                                   axis.line.x=element_line(),
                                                                                                                                   axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")
ggplot(data=testData,mapping=aes(x=averageMovement,y=winPlacePerc))+geom_point()+stat_smooth(method="lm",col="dodgerblue3")+ylim(0,1)+theme(panel.background = element_rect(fill = "white"),axis.line.x=element_line(),
                                                                                                                                            axis.line.y=element_line()) +ggtitle("Linear Model Fitted to Data")
testData$linPred=predict(model1,testData)
ggplot(testData,aes(x=winPlacePerc,y=linPred))+geom_point()+geom_abline(slope=1,intercept=0)

rm(train_V2)
rm(test_V2)
rm(averageKills)
rm(averageMovement)
rm(averageDamage)
rm(avgDamage)
rm(avgKPM)
rm(avgHeadshotPerc)
library(FNN)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
set.seed(123)
dat.d <- sample(1:nrow(trainAdded),size=nrow(trainAdded)*0.1,replace = FALSE)
smallerData=trainAdded[dat.d]
smallerData=trainAdded[dat.d,]
dat1.d <- sample(1:nrow(smallerData),size=nrow(smallerData)*0.8,replace = FALSE)
smallerDataSubset=smallerData[c("winPlacePerc","averageKills","averageMovement","avgDamage")]
smallerDataSubset.n=as.data.frame(lapply(smallerDataSubset[,2:3],normalize))
smallerTrain.d <- sample(1:nrow(smallerData),size=nrow(smallerData)*0.8,replace = FALSE)
smallerTrainDataSubset=smallerDataSubset[smallerTrain.d,]
smallerTestDataSubset=smallerDataSubset[-smallerTrain.d,]
smallTrainLable=smallerTrainDataSubset[c("winPlacePerc")]
knn.5=knn.reg(train=smallerTrainDataSubset,test=smallerTestDataSubset,y=smallTrainLable,k=5)
knn.20=knn.reg(train=smallerTrainDataSubset,test=smallerTestDataSubset,y=smallTrainLable,k=20)
knn.30=knn.reg(train=smallerTrainDataSubset,test=smallerTestDataSubset,y=smallTrainLable,k=30)
knn.70=knn.reg(train=smallerTrainDataSubset,test=smallerTestDataSubset,y=smallTrainLable,k=70)
ggplot(data=smallerTestDataSubset,aes(x=knn.5$pred,y=winPlacePerc)+geom_hex(binwidth=c(.25,.25))
       
       