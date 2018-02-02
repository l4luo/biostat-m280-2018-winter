
nVals <- seq(100, 500, by=100)
distVals <- c("gaussian", "t1", "t5")
seed <- 280
rep <- 50

for (n in nVals) {
  for (dist in distVals) {
    oFile <- paste(dist, "_n", n, ".txt", sep="")
    arg <- paste(paste("seed=", seed, " n=", n, " 'dist=\"", dist, "\"'", 
                       " rep=", rep, sep=""))
    sysCall <- paste("nohup Rscript runSim.R ", arg, " > ", oFile)
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
}
