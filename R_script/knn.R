library(class)
like = read.csv("like.csv",stringsAsFactors = F, header = T)

colnames(like) <- c('talk','book','travel','school','tall','skin','muscle','pref')
head(like)

test <- data.frame(talk=70, book=50, travel=40, school=100, tall=60, skin=80, muscle=70)

train <- like[,-8]
group <- like[,8]
knnpred1<-knn(train, test, group, k=3,prob=T)
knnpred2<-knn(train, test, group, k=4, prob = T)
knnpred1
knnpred2
