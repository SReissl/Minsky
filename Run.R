source("Model.R")
library(foreach)
library(doParallel)
library(tictoc)
#insert desired number of cores here
ncores<-5
cl<-makeCluster((ncores), outfile="")
registerDoParallel(cl)
mc=5
tic()
foreach(m=1:mc) %dopar% {
library(abind)
library(rootSolve)
calibration<-as.data.frame(read.csv("Calibration.csv",header=TRUE),header=FALSE)
source("Initialise.R")
initreturn<-init(init=inits,param=params,firminit=firminits,calib=calibration)
inits<-initreturn$init
params<-initreturn$param
firminits<-initreturn$firminit
#create folder "Rundata" before running model!
runModel(seed=m,nF=50,Time=1000,nrun=50,foldername="Rundata/")
}
toc()
stopCluster(cl)


