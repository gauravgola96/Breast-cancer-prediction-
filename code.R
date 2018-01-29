require(caTools)
data=read.csv("D:\\study\\Healthcare Project\\Kaggle cancer wiscosin\\data.csv",na.strings = c("",' ',NA,NaN,'NAN','<NAN>','NA'))
colSums(is.na(data))
y=data[,'diagnosis']
data=data[,-which(names(data) %in% c('id','X','diagnosis'))]
names(data)

pca=prcomp(data,scale. = T,center = T)

summary(pca)
#varimax(pca$rotation)
#pca2=princomp(data)
#summary(pca2)
pca$rotation
head(data)
head(predict(pca))
head(pca$x)

data1=pca$x
avg_accuracy=NA
avg_accuracy_list=list()
falseNegativeErrorRate=NA
falseNegativeErrorRate_list=list()
modelPerformance=data.frame('ModelName'=NA,'Accuracy'=NA,'False_Error_Rate'=NA)
table(trainData$y)
#for(j in c('glm','lda','nnet')){

n=1

#glm,gbm,rf,knn,lvq,lda,LMT,nnet
for(j in c('glm','knn','rf','lvq','lda','LMT','gbm','nnet')){

  for(i in 1:n){
data1=data.frame(pca$x,y)
data2=data1[,c(1:10,31)]
data2$y=ifelse(data2$y=='M',yes = 1,0)
index=sample.split(Y = data2$y,SplitRatio = 0.7)
trainData=data2[index,]
trainData$y=as.factor(trainData$y)
test=data2[!index,]
test$y=as.factor(test$y)

require(caret)
model=train(y~.,data=trainData,method = j)

pred=predict(object = model,newdata = trainData)
confusionMatrix(pred,trainData$y)

predTest=predict(object = model,newdata = test)
metric=confusionMatrix(predTest,test$y)
avg_accuracy_list[i]=as.numeric(metric$overall['Accuracy'])
falseNegativeErrorRate_list[i]=1-as.numeric(metric$byClass['Sensitivity'])
  }
  avg_accuracy=sum(unlist(avg_accuracy_list))
  avg_falseNegativeErrorRate=sum(unlist(falseNegativeErrorRate_list))
  print(j)
  modelPerformance[nrow(modelPerformance)+1,]=c('ModelName'=j,'Accuracy'=round(avg_accuracy,digits = 4),'False_Error_Rate'=round(avg_falseNegativeErrorRate,digits = 4))
}
modelPerformance$Accuracy=as.numeric(modelPerformance$Accuracy)/n
modelPerformance$False_Error_Rate=as.numeric(modelPerformance$False_Error_Rate)/n
modelPerformance=data.frame(modelPerformance[-1,],row.names = c(1,2,3,4,5,6,7,8))
print(modelPerformance)

require(ROCR)
RocPred=prediction(predictions = c(predTest),labels = c(test$y))
RocPerf=performance(prediction.obj = RocPred,'tpr','fpr')
plot(RocPerf,colorize=T)
RocPerf2=performance(prediction.obj = RocPred,'tpr','fnr')
plot(RocPerf2,colorize=T)


require(rpart)
require(rpart.plot)

rpartModel=rpart(formula = y~.,data = trainData)
rpart.plot(rpartModel)
predTree=predict(object = rpartModel,newdata = test,type = 'class')
confusionMatrix(predTree,reference = test$y)

require(rattle)
fancyRpartPlot(rpartModel)

modelPerformance$ModelName[9]='Neural Network'
modelPerformance$ModelName[1]='Logistic Regression'
modelPerformance$ModelName[3]='K-nearest neighbor'
modelPerformance$ModelName[4]='Random Forest'
modelPerformance$ModelName[5]='Learning Vector Quantization'
modelPerformance$ModelName[6]='Linear Discriminant Analysis'
modelPerformance$ModelName[7]='Logistic Model Trees'
modelPerformance$ModelName[8]='Stochastic Gradient Boosting'


#avg_accuracy=sum(unlist(avg_accuracy_list))/10
#print(avg_accuracy)
# remove(x)
# sum(unlist(x))/100
# nrow(rob)
# 
# rob=data.frame('E'=NA,'F'=NA)
# k=0
# for(e in c('glm','knn','lvq','lda','LMT')){
#   k=k+1
#   for(f in 1:10){
#     g=2
#   }
# 
#   print(g)
#   print(e)
#   print(k)

#   rob[nrow(rob)+1,]=c('E'=e,'F'=g)
# 
#   print(rob)
# }
# 
# rbind()
# rob
# rob=NA
# g=NA
# rob[2,]=data.frame('E'=NA,'F'=NA)


# what problem are you trying to solve
# why data science is needed to solve
# how you have approached it with data science

