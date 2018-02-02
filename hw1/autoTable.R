# Read in txt files

#to use fread instead of read.table
library(data.table)
#setwd("/home/l4luo/biostat-m280-2018-winter/hw1")

filenames <- list.files("/home/l4luo/biostat-m280-2018-winter/hw1", 
                        pattern="*00.txt")
test2 <- lapply(filenames, fread, drop = 1)
test3 <- matrix(unlist(test2), byrow = TRUE, ncol = 2)
#reorder numbers to t1, t5, gaussian
test4 <- test3[c(6:15, 1:5), c(2, 1)]

#create dataframe
tableDF <- as.data.frame(matrix(t(test4), nrow = 10))
#labels
nLabs <- c(100, NA, 200, NA, 300, NA, 400, NA, 500, NA)
methodLabs <- rep(c("PrimeAvg","SampAvg"), times = 5)
MSETable <- cbind(nLabs, methodLabs, tableDF)
colnames(MSETable) <- c("n", "Method", "t1", "t5", "Gaussian")

library(knitr)
kable(MSETable)


#test <- read.table("gaussian_n100.txt", colClasses = c("NULL", NA, NA))



