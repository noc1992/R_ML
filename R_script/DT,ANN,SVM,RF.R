library(caret)
idx <- createDataPartition(iris$Species, p=0.7, list=F)
iris_train<-iris[idx, ]
iris_test<-iris[-idx, ]

# Dicision tree
library(rpart)

rpart_rt=rpart(Species~.,iris_train)
rpart_pdt=predict(rpart_rt, iris_test, type='class')
confusionMatrix(iris_test$Species, rpart_pdt)

# ANN
library(nnet)
iris_train_scale=as.data.frame(sapply(iris_train[,-5], scale))
iris_test_scale=as.data.frame(sapply(iris_test[,-5],scale))
iris_train_scale$Species=iris_train$Species
iris_test_scale$Species=iris_test$Species

nnet_rlt=nnet(Species~.,iris_train_scale, size=3)

# SVM
library(kernlab)
svm_rt=ksvm(Species~., iris_train, kernal='rbfdot')
svm_pdt=predict(svm_rt, iris_test, type='response')
kk = table(svm_pdt, iris_test$Species)
confusionMatrix(svm_pdt, iris_test$Species)

accurecy=c(accurecy=(kk[1,1] + kk[2,2] + kk[3,3]) / sum(kk))
accurecy           

# Random forest
install.packages("randomForest")
library(randomForest)
rf_rt=randomForest(Species~., iris_train,ntree=500)
rf_pdt=predict(rf_rt, iris_test, type='response')
kk1 = table(rf_pdt, iris_test$Species)
accurecy1=c(accurecy1=(kk1[1,1] + kk1[2,2] + kk1[3,3]) / sum(kk1))
accurecy1           
