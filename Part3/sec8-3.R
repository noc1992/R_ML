install.packages("reshape2")
install.packages("stringr")
library(reshape2)

Fruits
melt(Fruits,id='Year')

dcast(mtest, name~var_name,sum)

