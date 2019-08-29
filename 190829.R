library(Deducer)
library(caret)

idx=createDataPartition(iris$Species, p=0.7,list = F)

iris_train = iris[idx,]
iris_test  = iris[-idx,]
table(iris_train$Species)
table(iris_test$Species)

library(rpart)
library(e1071)
library(randomForest)

rpart_tr = rpart(Species~., data=iris_train)
bayes_tr = naiveBayes(Species~., data=iris_train)
rdf_tr = randomForest(Species~., data=iris_train)

rpart_pdt = predict(rpart_tr, newdata = iris_test, type='class')
bayes_pdt = predict(bayes_tr, newdata = iris_test, type='class')
rdf_pdt = predict(rdf_tr, newdata = iris_test, type='class')

table(iris_test$Species, rpart_pdt)
table(iris_test$Species, bayes_pdt)
table(iris_test$Species, rdf_pdt)

confusionMatrix(rpart_pdt, iris_test$Species)
confusionMatrix(bayes_pdt, iris_test$Species)
confusionMatrix(rdf_pdt, iris_test$Species)

library(leaps)
attitude
