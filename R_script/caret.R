install.packages("caret")
library(caret)

iris
table(iris)

idx = sample(1:nrow(iris), size = nrow(iris)*0.7, replace = F)
idx

iris_train=iris[idx,]
iris_test=iris[-idx,]
str(iris_train)
dim(iris_test)
table(iris_train$Species)

train.idx<-createDataPartition(iris$Species, p=0.7, list=F)
iris_train1 = iris[train.idx,]
iris_test1 = iris[-train.idx,]
