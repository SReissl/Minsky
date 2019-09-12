source("Model.R")
library(foreach)
library(doParallel)
library(tictoc)
#insert desired number of cores here
ncores<-5
cl<-makeCluster((ncores), outfile="")
registerDoParallel(cl)
mc=100
tic()
foreach(m=1:mc) %dopar% {
library(abind)
library(rootSolve)
source("Initialise.R")
initreturn<-init()
inits<-initreturn$init
params<-initreturn$param
firminits<-initreturn$firminit
#create folder "Rundata" before running model!
runModel(seed=m,nF=50,Time=2000,foldername="Rundata/")
}
toc()
stopCluster(cl)


