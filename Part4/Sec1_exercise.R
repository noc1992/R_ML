library(dplyr)
library("RColorBrewer")
library(wesanderson)
View(iris)
#########1번 입니다.##############
data1 <- filter(iris, Species =="setosa")
x1 <- select(data1, Sepal.Length, Sepal.Width)
plot(x1)

data2 <- filter(iris, Species =="setosa")
x2 <- select(data2, Petal.Length, Petal.Width)
plot(x2)

data3 <- filter(iris, Species =="versicolor")
y1 <- select(data3, Sepal.Length, Sepal.Width)
plot(y1)

data4 <- filter(iris, Species =="versicolor")
y2 <- select(data4, Petal.Length, Petal.Width)
plot(y2)

data5 <- filter(iris, Species =="virginica")
z1 <- select(data5, Sepal.Length, Sepal.Width)
plot(z1)

data6 <- filter(iris, Species =="virginica")
z2 <- select(data6, Petal.Length, Petal.Width)
plot(z2)

########################2번입니다##################
# ---(1) 세로 --- #
all_b1 <- iris %>%
  group_by(Species) %>%
  summarise_each(funs(mean), Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
all_b1 <- subset(all_b1, select = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
all_b1

barp <- data.frame(all_b1)

Tr <- t(barp)
label <- c("Setosa", "Versicolor","virginica")
colnames(Tr) <- label
Tr

barplot(as.matrix(Tr), main = "num2 barplot", beside =T,col=brewer.pal(n=4,name='RdBu'),
        ylim = c(0, 10))
legend(11, 10, c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width"),
       cex = 0.8, fill = brewer.pal(n=nrow(Tr),name = 'RdBu'))
#--(2) 가로---#
t(Tr)
barplot(t(Tr), main = "num2 barplot", 
        col=brewer.pal(n=length(t(Tr)),name="Set3"),ylim = c(0,20),  
        space = 0.1, cex.axis = 1,las=0.2)
legend(3, 18, c("Setosa", "Versicolor", "virginica"),
       cex = 0.8, fill = brewer.pal(n=length(t(Tr)),name = 'Set3'))

########################3번 입니다. ##################

b1 <- boxplot(x1, col = brewer.pal(n=nrow(x1), name = "Pastel1"), main = "Setosa Sepal Box", names=c("Sepal.Length","Sepal width"))
b2 <- boxplot(x2, col = brewer.pal(n=nrow(x2), name = "Pastel1"), main = "Setosa Petal Box", names=c("Petal.Length","Petal width"))
b3 <- boxplot(y1, col = brewer.pal(n=nrow(y1), name = "Pastel1"), main = "versicolor Sepal Box", names=c("Sepal.Length","Sepal width"))
b4 <- boxplot(y2, col = brewer.pal(n=nrow(y2), name = "Pastel1"), main = "versicolor Petal Box", names=c("Petal.Length","Petal width"))
b5 <- boxplot(z1, col = brewer.pal(n=nrow(z1), name = "Pastel1"), main = "virginica Sepal Box", names=c("Sepal.Length","Sepal width"))
b6 <- boxplot(z2, col = brewer.pal(n=nrow(z2), name = "Pastel1"), main = "virginica Petal Box", names=c("Petal.Length","Petal width"))
par(mfrow=c(2,3))
