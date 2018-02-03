## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text = arg))
}

## check if a given integer is prime
isPrime <- function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observations with prime indices
estMeanPrimes <- function (x) {
  n <- length(x)
  ind <- sapply(1:n, isPrime) #a vector of T/Fs, T=prime
  return (mean(x[ind])) #mean of x values where ind=T
}

## function to generate distributions
## argument whichMean: specify either estMeanPrime or mean
avgMSE <- function(seed, n, dist, rep, whichMean) {
  set.seed(seed)
  if (dist == "gaussian") {
    x1 <- replicate(rep, whichMean(rnorm(n, mean = 0, sd = 1)))
  } else if (dist == "t1") {
    x1 <- replicate(rep, whichMean(rt(n, df = 1)))
  } else if (dist == "t5") {
    x1 <- replicate(rep, whichMean(rt(n, df = 5)))
  }
  return(sum((x1)^2)/rep)
}

c(avgMSE(seed, n, dist, rep, mean), avgMSE(seed, n, dist, rep, estMeanPrimes))


