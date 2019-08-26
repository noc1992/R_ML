library(e1071)
library(caret)
library(nnet)

train_rt = naiveBayes(iris_train1, iris_train1$Species, laplace = 1)
naive_pdt= predict(train_rt, iris_test1, type='class')
table(naive_pdt, iris_test1$Species)

confusionMatrix(naive_pdt,iris_test1$Species)

multi_re <- multinom(Species~., iris_test1)
multi_pred <- predict(multi_re, iris_test1)

table(multi_pred, iris_test1$Species)

install.packages("rpart")
library(rpart)


