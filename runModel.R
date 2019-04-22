source("Model.R")
library(foreach)
library(doParallel)
ncores<-detectCores()
cl<-makeCluster((1), outfile="")
registerDoParallel(cl)
mc=1
writeLines(c(""), "log.txt")

foreach(m=1:mc) %dopar% {
sink("log.txt", append=TRUE)
cat(paste("Starting iteration",m,"\n"))
sink()
library(abind)
library(rootSolve)
source("Initialise.R")
initreturn<-init()
inits<-initreturn$init
params<-initreturn$param
firminits<-initreturn$firminit
runModel(seed=(m),nF=100,Time=2000,nrun=80)
}

stopCluster(cl)