library(dplyr)
iris
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
seto_b1 <- data1 %>%
  group_by(Species) %>%
  summarise_each(funs(mean), Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
seto_b1
Set <- data.frame(seto_b1)
row.names(Set) <- Set$Species
Set <- subset(Set, select = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
Set

ver_b1 <- data3 %>%
  group_by(Species) %>%
  summarise_each(funs(mean), Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
Ver <- data.frame(ver_b1)
row.names(Ver) <- Ver$Species
Ver <- subset(Ver, select = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
Ver


vir_b1 <- data5 %>%
  group_by(Species) %>%
  summarise_each(funs(mean), Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
Vir <- data.frame(vir_b1)
row.names(Vir) <- Vir$Species
Vir <- subset(Vir, select = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
Vir

barp <- data.frame(Setosa = Set, Versicolor=Ver, Virginica=Vir)

barp
barplot(as.matrix(barp), main = "num2 barplot", col ="green")

#--(2) 가로---#
t(barp)
barplot(t(barp), main = "num2 barplot", col ="green", space = 0.1, cex.axis = 0.8,las=1)



