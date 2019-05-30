mat1 <- matrix(c(1,2,3,4))
mat2 <- matrix(c(1,2,3,4), nrow = 2)
mat3 <- matrix(c(1,2,3,4), nrow = 2, byrow = T)

mat4 <- matrix(seq(1:9), nrow = 3, byrow = T)
mat4 <- rbind(mat4, c(10,11,12))
mat4 <- cbind(mat4, c(13,14,15,16))
mat4

mat5 <- matrix(c('a','b','c','d'), nrow=2, byrow = T)
mat5
mat6 <- matrix(c('a','b','c','d'), nrow=2, byrow = F)
mat6

mat5[,2]
mat5 <- rbind(mat5, c('초봄','초가을'))
mat5
mat5 <- cbind(mat5, c('h','i','j'))
mat5
array1 <- array(c(1:12), dim = c(4,3))
array1
array2 <- array(c(1:12), dim = c(2,2,3))
array2


list1 <- list(name = '홍길동', address = '전국', tel = '010-1234-5678',age = 300)
list1$name
list1$birth <- "1000-10-23"
list1
list1$name <- c("leonard", "smart hong")
list1$age <- NULL
list1
