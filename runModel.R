source("MinskyParallel.R")
library(foreach)
library(doParallel)
ncores<-detectCores()
cl<-makeCluster((ncores-3), outfile="")
registerDoParallel(cl)
mc=50
writeLines(c(""), "log.txt")

foreach(m=1:mc) %dopar% {
sink("log.txt", append=TRUE)
cat(paste("Starting iteration",m,"\n"))
sink()
library(abind)
library(rootSolve)
source("MinskyInitsNew4.R")
initreturn<-init()
inits<-initreturn$init
params<-initreturn$param
firminits<-initreturn$firminit
params[1,"iota2"]=1.5
runModel(seed=m,nF=100,Time=2000,nrun=80)
}

stopCluster(cl)