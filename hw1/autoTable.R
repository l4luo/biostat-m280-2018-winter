library(data.table)

filenames <- list.files("/home/l4luo/biostat-m280-2018-winter/hw1", 
                        pattern = "*00.txt")
results <- lapply(filenames, fread, drop = 1)
results2 <- matrix(unlist(results), byrow = TRUE, ncol = 2)
# reorder output to t1, t5, gaussian
results3 <- results2[c(6:15, 1:5), c(2, 1)]

# create dataframe
tableDF <- as.data.frame(matrix(t(results3), nrow = 10))
# labels
nLabs <- c(100, NA, 200, NA, 300, NA, 400, NA, 500, NA)
methodLabs <- rep(c("PrimeAvg","SampAvg"), times = 5)
MSETable <- cbind(nLabs, methodLabs, tableDF)

library(knitr)
options(knitr.kable.NA = '')
kable(MSETable, col.names = c("$n$", "Method", "$t_1$", "$t_5$", "Gaussian"),
                              align = "l", digits = 3)
