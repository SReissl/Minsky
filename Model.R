runModel<-function(seed=1,nF=100,Time = 2000,nrun=100,foldername="Testdata/"){
  library(abind)
  library(rootSolve)
  library(tictoc)
  #tic()
  ###Argument list
  args<-as.data.frame(array(data=0,dim=c(1,4),dimnames=list(NULL,c("t","nF","Time","nrun"))))
  args$t=1
  args$nF=nF
  args$Time=Time
  args$nrun=nrun
  
  
  #####Sectoral Structure####
  
  # HH column names
  HNames<-c("V_h","YD","E_f","W","Div_f","sav_h","c_d","D_h")
  Households<-array(data = NA, dim=c(2,length(HNames)),dimnames = list(NULL,HNames))
  
  # C-Firm column names (ABM)
  CFNames<-c("V_fc","K","k","D_f","L","E_fc","i","I","N_dc","W_c","Div_fc","iL","rep_L","sav_fc","p_c","Pr_fc","c","C","share","c_d","r_L","Q","omega_c","p_dkint","u","p_rel","k_rel","int","CF","deltaK","root","bankrupt","mu1","mu2","gamma","share_shock","yield","attitude","index","sentiment","Q_f","Q_e","type","prod","divmax")
  Cfirms<-array(data = NA,dim=c(2,length(CFNames),args$nF),dimnames = list(NULL,CFNames,NULL))
  
  # C-Firm column names (Agg)
  CFNamesagg<-c("V_fc","K","k","D_f","L","E_fc","i","I","N_dc","W_c","Div_fc","iL","rep_L","sav_fc","Pr_fc","c","C","CPI","r_L","Y","y","u","p_c","bankrupt","CF","prod","Prod")
  Cfirmsagg<-array(data = NA, dim=c(2,length(CFNamesagg)),dimnames = list(NULL,CFNamesagg))
  
  # K-Firm column names
  KFNames<-c("V_fk","E_fk","i","i_p","I","I_p","W_k","Div_fk","sav_fk","p_k","Pr_fk","N_dk")
  Kfirms<-array(data = NA, dim=c(2,length(KFNames)),dimnames = list(NULL,KFNames))
  
  # Bank column names
  BNames<-c("V_b","E_b","L","D","D_h","D_f","Div_b","sav_b","Pr_b","CAR","r_0")
  Banks<-array(data = NA, dim=c(2,length(BNames)),dimnames = list(NULL,BNames))
  
  # Government
  GNames<-c("stimulus","Bonds","D_g","V_g","sav_g","cumulative")
  Gov<-array(data=NA,dim=c(2,length(GNames)),dimnames = list(NULL,GNames))
  
  # auxiliary stuff
  AuxNames<-c("SFCcheck1","SFCcheck2","recap","defaults","defaults2","counter","Hedge","Speculative","Ponzi","outputsum","yieldsum","CFsum","prsum")
  Aux<-array(data = NA, dim=c(2,length(AuxNames)),dimnames = list(NULL,AuxNames))
  
  #####Data Frames#####
  if(params[1,"fulloutput"]==0){
  datanames<-c("K","L","i","Y","y","D_f","c","Hedge","Speculative","Ponzi")
  output<-as.data.frame(array(data = NA, dim=c(args$Time/args$nrun+10,length(datanames)),dimnames = list(NULL,datanames)))
  datanamesmicro<-c("i","share","bankrupt","type","c_d")
  outputmicro<-(array(data = NA, dim=c(args$Time/args$nrun+10,length(datanamesmicro),args$nF),dimnames = list(NULL,datanamesmicro,NULL)))
  }else{
  datanames<-c("V_h","YD","W","Div_f","sav_h","c_d","D_h","V_fc","K","k","D_f","L","i","I","W_c","Div_fc","iL","rep_L","sav_fc","Pr_fc","c","C","CPI","r_L","Y","y","u","bankrupt","CF","i","i_p","I","I_p","Div_fk","Div_b","stimulus","Bonds","D_g","V_g","sav_g","cumulative","r_0","SFCcheck1","SFCcheck2","recap","defaults","defaults2","counter","E_b","Hedge","Speculative","Ponzi")
  output<-as.data.frame(array(data = NA, dim=c(args$Time/args$nrun+10,length(datanames)),dimnames = list(NULL,datanames)))
  datanamesmicro<-c("V_fc","K","k","D_f","L","i","I","W_c","Div_fc","iL","rep_L","sav_fc","p_c","Pr_fc","c","C","share","c_d","r_L","Q","omega_c","p_dkint","u","p_rel","k_rel","int","CF","deltaK","root","bankrupt","mu1","mu2","gamma","share_shock","yield","attitude","index","sentiment","Q_f","Q_e","type","E_fc","divmax")
  outputmicro<-(array(data = NA, dim=c(args$Time/args$nrun+10,length(datanamesmicro),args$nF),dimnames = list(NULL,datanamesmicro,NULL)))
  }
  
  set.seed(seed)
    
    #######Initial values####
    Households[1,"D_h"]<-inits[[1,"D_h"]]
    Households[1,"YD"]<-inits[[1,"YD"]]
    Households[1,"V_h"]<-inits[[1,"V_h"]]
    Kfirms[1,"E_fk"]<-inits[[1,"E_fk"]]
    Banks[1,"E_b"]<-inits[[1,"E_b"]]
    Aux[1,"counter"]<-0
    Cfirms[1,"mu2",]<-params[1,"mu2"]
    Banks[1,"r_0"]<-params[1,"r_0"]
    Gov[1,"stimulus"]<-0
    Gov[1,"Bonds"]<-0
    Gov[1,"D_g"]<-0
    Gov[1,"V_g"]<-0
    Gov[1,"cumulative"]<-0
    
    #firminits
    Cfirms[1,"K",]<-firminits[,"K"]
    Cfirms[1,"k",]<-firminits[,"k"]
    Cfirms[1,"omega_c",]<-params[1,"omega"]
    Cfirms[1,"L",]<-firminits[,"L"]
    Cfirms[1,"r_L",]<-firminits[,"r_L"]
    Cfirms[1,"share",]<-1/args$nF
    Cfirms[1,"D_f",]<-firminits[,"D_f"]
    Cfirms[1,"C",]<-firminits[,"C"]
    Cfirms[1,"E_fc",]<-firminits[,"E_fc"]
    Cfirms[1,"p_c",]<-(1+params[1,"omega"])*params[1,"w"]
    Cfirms[1,"bankrupt",]<-0
    Cfirms[1,"gamma",]<-params[1,"gamma"]
    Cfirms[1,"share_shock",]<-1
    outputmicro[1,"bankrupt",]<-0
    outputmicro[1,"share",]<-1/args$nF
    if(params[1,"types1"]==1){
    for(i in 1:args$nF){
    type=runif(1)
    if(type>params[1,"fshare"]){
    Cfirms[1,"type",i]=0
    }else{
    Cfirms[1,"type",i]=1
    }
    }
    }
  
    r=1
    Timer=1
    for(r in 1:args$nrun){
    if(r>1){
    args$t=10
    marker=10
    output[1:10,]<-output[(args$Time/args$nrun+1):(args$Time/args$nrun+10),]
    output[11:(args$Time/args$nrun+10),]<-NA
    outputmicro[1:10,,]<-outputmicro[(args$Time/args$nrun+1):(args$Time/args$nrun+10),,]
    outputmicro[11:(args$Time/args$nrun+10),,]<-NA
    }else{
    args$t=1
    marker=1
    }
    for(t in marker:(args$Time/args$nrun+9)){
      Timer=Timer+1
      args$t=(args$t+1)
      Cfirms<-updateCapital1(Cfirms=Cfirms,params=params,args=args)
      Cfirms<-loanPayments(Cfirms=Cfirms,params=params,args=args)
      bankruptcyReturns<-bankruptcy(Cfirms=Cfirms,Kfirms=Kfirms,Banks=Banks,Aux=Aux,params=params,args=args,Timer=Timer,outputmicro=outputmicro)
      Cfirms<-bankruptcyReturns$Cfirms
      Aux<-bankruptcyReturns$Aux
      Kfirms<-bankruptcyReturns$Kfirms
      outputmicro<-bankruptcyReturns$outputmicro
      Cfirms<-setCprice(Cfirms=Cfirms,params=params,args=args,Timer=Timer,outputmicro=outputmicro)
      Cfirmsagg<-calculateCPI(Cfirmsagg=Cfirmsagg,Cfirms=Cfirms,params=params,args=args)
      Kfirms<-setKprice(Kfirms=Kfirms,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Households<-decideConsumption(Households=Households,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Cfirms<-distributeConsumption(Cfirms=Cfirms,Households=Households,params=params,args=args)
      prodReturn<-cProduction(Households=Households,Cfirms=Cfirms,Aux=Aux,Timer=Timer,params=params,args=args)
      Cfirms<-prodReturn$Cfirms
      Aux<-prodReturn$Aux
      YieldReturn<-estimateYield(Cfirms=Cfirms,Cfirmsagg=Cfirmsagg,Aux=Aux,params=params,args=args,Timer=Timer,firminits=firminits,meanprofitfun=meanprofitfun,meanprofitext=meanprofitext,fitfun=fitfun,fitext=fitext)
      Cfirms<-YieldReturn$Cfirms
      Cfirmsagg<-YieldReturn$Cfirmsagg
      Aux<-YieldReturn$Aux
      Cfirms<-Invest(Cfirms=Cfirms,Banks=Banks,Cfirmsagg=Cfirmsagg,Kfirms=Kfirms,params=params,args=args,Timer=Timer)
      Cfirms<-updateCapital2(Kfirms=Kfirms,Cfirms=Cfirms,params=params,args=args)
      Kfirms<-supplyCapital(Kfirms=Kfirms,Cfirms=Cfirms,params=params,args=args)
      Cfirms<-profitCfirms(Cfirms=Cfirms,Aux=Aux,params=params,args=args,Timer=Timer)
      Cdivreturns<-CDividends(Cfirms=Cfirms,Kfirms=Kfirms,Banks=Banks,Aux=Aux,params=params,args=args,Timer=Timer,outputmicro=outputmicro)
      Cfirms<-Cdivreturns$Cfirms
      Aux<-Cdivreturns$Aux
      Kfirms<-Cdivreturns$Kfirms
      outputmicro<-Cdivreturns$outputmicro
      Cfirms<-Cfsave(Cfirms=Cfirms,params=params,args=args)
      Cfirmsagg<-Cfaggregates(Cfirmsagg=Cfirmsagg,Cfirms=Cfirms,params=params,args=args)
      Banks<-bankStocks1(Banks=Banks,Cfirmsagg=Cfirmsagg,params=params,args=args)
      FPreturns<-FP(Gov=Gov,Cfirmsagg=Cfirmsagg,Banks=Banks,Aux=Aux,params=params,args=args,inits=inits,firminits=firminits,Timer=Timer)
      Gov<-FPreturns$Gov
      Aux<-FPreturns$Aux
      Bdivreturns<-payDividendsB(Banks=Banks,Gov=Gov,Cfirmsagg=Cfirmsagg,Aux=Aux,params=params,args=args,Timer=Timer)
      Banks<-Bdivreturns$Banks
      Aux<-Bdivreturns$Aux
      Kfirms<-Kfprofit(Kfirms=Kfirms,params=params,args=args)
      Kfirms<-payDividendsK(Kfirms=Kfirms,params=params,args=args)
      Kfirms<-Kfsave(Kfirms=Kfirms,params=params,args=args)
      Kfirms<-Kfwealth(Kfirms=Kfirms,params=params,args=args)
      Households<-Hidentities(Households=Households,Gov=Gov,Aux=Aux,Banks=Banks,Kfirms=Kfirms,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Banks<-bankStocks2(Banks=Banks,Households=Households,Gov=Gov,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Aux<-SFCcheck(Aux=Aux,Households=Households,Gov=Gov,Cfirmsagg=Cfirmsagg,Kfirms=Kfirms,Banks=Banks,params=params,args=args)
      if(params[1,"fulloutput"]==0){
      datareturn<-collectData(Households=Households,Kfirms=Kfirms,Cfirms=Cfirms,Cfirmsagg=Cfirmsagg,Banks=Banks,Aux=Aux,args=args,output=output,outputmicro=outputmicro)
      output<-datareturn$output
      outputmicro<-datareturn$outputmicro
      }else{
      datareturn<-collectFullData(Households=Households,Kfirms=Kfirms,Gov=Gov,Cfirms=Cfirms,Cfirmsagg=Cfirmsagg,Banks=Banks,Aux=Aux,args=args,output=output,outputmicro=outputmicro)
      output<-datareturn$output
      outputmicro<-datareturn$outputmicro
      }
      indexreturn<-shiftIndex(Households=Households,Gov=Gov,Kfirms=Kfirms,Cfirmsagg=Cfirmsagg,Banks=Banks,Cfirms=Cfirms,Aux=Aux)
      Households<-indexreturn$Households
      Kfirms<-indexreturn$Kfirms
      Banks<-indexreturn$Banks
      Cfirms<-indexreturn$Cfirms
      Aux<-indexreturn$Aux
      Gov<-indexreturn$Gov
      Cfirmsagg<-indexreturn$Cfirmsagg
    }
    if(r==1){
      Timer=(args$Time/args$nrun+10)
      outputsel<-output
      outputmicrosel<-outputmicro
    }else{
      outputsel<-rbind(outputsel,output[11:(args$Time/args$nrun+10),])
      outputmicrosel2<-outputmicro[11:(args$Time/args$nrun+10),,]
      outputmicrosel<-abind(outputmicrosel,outputmicrosel2,along=1)
    }
    }
  Modelreturns<-list(output=outputsel,outputmicro=outputmicrosel)
  filename<-paste("data","mc",seed,sep="")
  save(Modelreturns,file = paste(foldername,filename, ".Rdata", sep=''))
  #toc()
  }


######Accounting identities#####
#####Households####
Hidentities<-function(Households=stop("need to have households defined!"),Aux=stop("Need to have Aux defined!"),Gov=stop("need to have government defined!"),Banks=stop("need to have Banks defined!"),Kfirms=stop("need to have Kfirms defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Households[[2,"YD"]]=Cfirmsagg[[2,"W_c"]]+Kfirms[[2,"W_k"]]+Cfirmsagg[[2,"Div_fc"]]+Kfirms[[2,"Div_fk"]]+Banks[[2,"Div_b"]]+Gov[[2,"stimulus"]]
  Households[[2,"sav_h"]]=Cfirmsagg[[2,"W_c"]]+Kfirms[[2,"W_k"]]+Cfirmsagg[[2,"Div_fc"]]+Kfirms[[2,"Div_fk"]]+Banks[[2,"Div_b"]]-Cfirmsagg[[2,"C"]]+Gov[[2,"stimulus"]]
  Households[[2,"D_h"]]=Households[[1,"D_h"]]+Households[[2,"sav_h"]]-Aux[[2,"recap"]]
  Households[[2,"V_h"]]=Households[[2,"D_h"]]+Banks[[2,"E_b"]]+Cfirmsagg[[2,"E_fc"]]+Kfirms[[2,"E_fk"]]
  Households[[2,"E_f"]]=Cfirmsagg[[2,"E_fc"]]+Kfirms[[2,"E_fk"]]
  Households[[2,"Div_f"]]=Cfirmsagg[[2,"Div_fc"]]+Kfirms[[2,"Div_fk"]]
  Households[[2,"W"]]=Cfirmsagg[[2,"W_c"]]+Kfirms[[2,"W_k"]]
  return(Households)
}

#####K-Firms####
Kfprofit<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[2,"Pr_fk"]]=Kfirms[[2,"I"]]-Kfirms[[2,"W_k"]]
  return(Kfirms)
}

Kfsave<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[2,"sav_fk"]]=Kfirms[[2,"I"]]-Kfirms[[2,"W_k"]]-Kfirms[[2,"Div_fk"]]
  return(Kfirms)
}

Kfwealth<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[2,"E_fk"]]=Kfirms[[1,"E_fk"]]+Kfirms[[2,"sav_fk"]]
  Kfirms[[2,"V_fk"]]=-Kfirms[[2,"E_fk"]]
  return(Kfirms)
}

#######C-firms####
#ABM#####
Cfsave<-function(Cfirms=stop("need to have Cfirms defined!"),params=params,args=args){
  Cfirms[2,"sav_fc",]=Cfirms[2,"C",]-Cfirms[2,"I",]-Cfirms[2,"W_c",]-Cfirms[2,"iL",]-Cfirms[2,"Div_fc",]
  return(Cfirms)
}

#####sectoral balances####
SFCcheck<-function(Households=stop("need to have households defined!"),Gov=stop("need to have government defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Kfirms=stop("need to have Kfirms defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have auxiliary array defined!"),params=params,args=args){
  Aux[[2,"SFCcheck1"]]=Households[[2,"sav_h"]]+Cfirmsagg[[2,"sav_fc"]]+Banks[[2,"sav_b"]]+Kfirms[[2,"sav_fk"]]+Gov[[2,"sav_g"]]
  Aux[[2,"SFCcheck2"]]=Households[[2,"V_h"]]+Cfirmsagg[[2,"V_fc"]]+Banks[[2,"V_b"]]+Kfirms[[2,"V_fk"]]+Gov[[2,"V_g"]]-Cfirmsagg[[2,"K"]]
  return(Aux)
}

######
####Equations
####Households####
decideConsumption<-function(Households=stop("need to have households defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Households[[2,"c_d"]]=((params[[1,"alpha1"]]*Households[[1,"YD"]]+params[[1,"alpha2"]]*Households[[1,"V_h"]])/Cfirmsagg[[2,"CPI"]])
  return(Households)
}


#####Firms#####
#####K-firms####

setKprice<-function(Kfirms=stop("need to have Kfirms defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Kfirms[[2,"p_k"]]=(1+params[1,"omega"])*params[1,"w"]
  return(Kfirms)
}

supplyCapital<-function(Kfirms=stop("need to have Kfirms defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Kfirms[[2,"i"]]=sum(Cfirms[2,"i",])
  Kfirms[[2,"N_dk"]]=Kfirms[[2,"i"]]
  Kfirms[[2,"W_k"]]=params[[1,"w"]]*Kfirms[[2,"N_dk"]]
  Kfirms[[2,"I"]]=Kfirms[[2,"i"]]*Kfirms[[2,"p_k"]]
  return(Kfirms)
}

payDividendsK<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[2,"Div_fk"]]=Kfirms[[2,"Pr_fk"]]
  return(Kfirms)
}

#####C-firms (agent-based)####

updateCapital1<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[2,"k",]=(1-params[[1,"delta"]])*Cfirms[1,"k",]
  Cfirms[2,"K",]=(1-params[[1,"delta"]])*Cfirms[1,"K",]
  return(Cfirms)
}



setCprice<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args,Timer=Timer,outputmicro=outputmicro){
  rand<-sample(1:args$nF,args$nF/4)
  for(i in 1:args$nF){
  if(Timer<5){
  Cfirms[[2,"omega_c",i]]=Cfirms[[1,"omega_c",i]]
  Cfirms[[2,"p_c",i]]=(1+Cfirms[[2,"omega_c",i]])*params[[1,"w"]]
  }else{
  if(is.element(i,rand)){
  if(outputmicro[[(args$t),"bankrupt",i]]==1||outputmicro[[(args$t-1),"bankrupt",i]]==1 || outputmicro[[(args$t-2),"bankrupt",i]]==1||outputmicro[[(args$t-3),"bankrupt",i]]==1||outputmicro[[(args$t-4),"bankrupt",i]]==1){
  Cfirms[[2,"omega_c",i]]=Cfirms[1,"omega_c",i]
  Cfirms[[2,"p_c",i]]=((1+Cfirms[[1,"omega_c",i]])*params[[1,"w"]])
  }else{
  Cfirms[[2,"omega_c",i]]=params[1,"omega"]*(2/(1+exp(params[1,"epsilon"]*((mean(outputmicro[(args$t-4):(args$t-1),"share",i])/(1/args$nF))-1))))
  Cfirms[[2,"p_c",i]]=((1+Cfirms[[2,"omega_c",i]])*params[[1,"w"]])
  }}else{
  Cfirms[[2,"omega_c",i]]=Cfirms[[1,"omega_c",i]]
  Cfirms[[2,"p_c",i]]=Cfirms[[1,"p_c",i]]
  }}}
  return(Cfirms)
}

estimateYield<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Aux=stop("Need to have auxiliary array defined!"),params=params,args=args,Timer=Timer,firminits=firminits,meanprofitfun=meanprofitfun,meanprofitext=meanprofitext,fitfun=fitfun,fitext=fitext){
  Cfirmsagg[[2,"CF"]]=sum(Cfirms[2,"CF",])
  Cfirmsagg[[2,"K"]]=sum(Cfirms[2,"K",])
  Cfirmsagg[[2,"iL"]]=sum(Cfirms[2,"iL",])
  Cfirms[2,"type",]=Cfirms[1,"type",]
  if(Timer>=200 && Timer<500){
  Aux[[2,"yieldsum"]]=Aux[[1,"yieldsum"]]+mean(((Cfirms[2,"c_d",]*Cfirms[(2),"p_c",]-params[[1,"w"]]*(Cfirms[2,"c_d",]+params[[1,"kappa_L"]]*params[1,"kappa"]*Cfirms[2,"k",]))))
  Aux[[2,"prsum"]]=Aux[[1,"prsum"]]+mean(Cfirms[1,"Pr_fc",]*4/Cfirms[1,"K",])
  }else{
  if(Timer<200){
  Aux[[2,"yieldsum"]]=0
  Aux[[2,"prsum"]]=0
  }
  if(Timer>=500){
  Aux[[2,"yieldsum"]]=Aux[[1,"yieldsum"]]
  Aux[[2,"prsum"]]=Aux[[1,"prsum"]]
  }
  }
  if(params[1,"types1"]==1){
  for(i in 1:args$nF){
  Cfirms[2,"yield",i]=((Cfirms[2,"c_d",i]*Cfirms[(2),"p_c",i]-params[[1,"w"]]*(Cfirms[2,"c_d",i]+params[[1,"kappa_L"]]*params[1,"kappa"]*Cfirms[2,"k",i])))
  if(Timer<500){
  Cfirms[2,"Q_f",i]=(Cfirms[2,"yield",i])/(params[1,"r_d"]+params[1,"delta"])
  }else{
  Cfirms[2,"Q_f",i]=(Aux[[2,"yieldsum"]]/300)/(params[1,"r_d"]+params[1,"delta"])
  }
  Cfirms[2,"Q_e",i]=(Cfirms[2,"yield",i])/(params[1,"r_d"]+params[1,"delta"])
  if(Cfirms[2,"type",i]==1){
  Cfirms[2,"Q",i]=Cfirms[2,"Q_f",i]
  }else{
  Cfirms[2,"Q",i]=Cfirms[2,"Q_e",i]
  }
  Cfirms[2,"mu1",i]=params[1,"mu1"]
  }
  }else{
  for(i in 1:args$nF){
  sprob=runif(1)
  if(Timer>=500){
  Cfirms[[2,"index",i]]=params[1,"rho"]*(Cfirms[1,"Pr_fc",i])*4/Cfirms[1,"K",i]+(1-params[1,"rho"])*(Cfirmsagg[1,"Pr_fc"])*4/Cfirmsagg[1,"K"]
  Cfirms[2,"sentiment",i]=1/(1+exp(params[1,"eta"]*(Cfirms[[2,"index",i]]-Aux[2,"prsum"]/300)))
  if(Cfirms[2,"sentiment",i]>sprob){
  Cfirms[2,"attitude",i]=1
  }else{
  Cfirms[2,"attitude",i]=0
  }
  }
  if(params[1,"expdyn"]==1 && Timer>=500){
  Cfirms[2,"yield",i]=((Cfirms[2,"c_d",i]*Cfirms[(2),"p_c",i]-params[[1,"w"]]*(Cfirms[2,"c_d",i]+params[[1,"kappa_L"]]*params[1,"kappa"]*Cfirms[2,"k",i])))
  if(Cfirms[2,"attitude",i]==1){
  Cfirms[2,"yield",i]=Cfirms[2,"yield",i]*params[1,"opt"]
  }else{
  Cfirms[2,"yield",i]=Cfirms[2,"yield",i]*params[1,"pes"]
  }}
  else{
  Cfirms[2,"yield",i]=Cfirms[2,"c_d",i]*Cfirms[(2),"p_c",i]-params[[1,"w"]]*(Cfirms[2,"c_d",i]+params[[1,"kappa_L"]]*params[1,"kappa"]*Cfirms[2,"k",i])
  }
  Cfirms[2,"Q",i]=(Cfirms[2,"yield",i])/(params[1,"r_d"]+params[1,"delta"])
  Cfirms[2,"mu1",i]=params[1,"mu1"]
  }
  }
  return(list(Cfirms=Cfirms,Cfirmsagg=Cfirmsagg,Aux=Aux))
}


Invest<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Banks=stop("need to have Banks defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Kfirms=stop("need to have Kfirms defined!"),params=params,args=args,Timer=Timer){
  Cfirms[2,"p_dkint",]=pmax(0,(Cfirms[2,"Q",]-Cfirms[2,"mu1",]*(Cfirms[2,"int",])^2)/Cfirms[2,"k",])
  for(i in 1:args$nF){
  if(is.na(Cfirms[[2,"p_dkint",i]])){
  Modelreturns<-Cfirms
  filename<-paste("data","mc",args$m,sep="")
  save(Modelreturns,file = paste("Testdata/",filename, ".Rdata", sep=''))
  break
  }
  if(Cfirms[[2,"p_dkint",i]]<=Kfirms[[2,"p_k"]]){
  if(Cfirms[[2,"Q",i]]-Cfirms[[2,"k",i]]*Kfirms[[2,"p_k"]]>0){
  Cfirms[[2,"i",i]]=Cfirms[[2,"i",i]]+max(0,min(Cfirms[[2,"int",i]],sqrt(Cfirms[[2,"Q",i]]-Cfirms[[2,"k",i]]*Kfirms[[2,"p_k"]])/(sqrt(Cfirms[2,"mu1",i]))))
  }else{
  Cfirms[[2,"i",i]]=Cfirms[[2,"i",i]]
  }
  Cfirms[[2,"i",i]]=Cfirms[[2,"i",i]]/Kfirms[[2,"p_k"]]
  Cfirms[[2,"L",i]]=Cfirms[[1,"L",i]]
  Cfirms[[2,"r_L",i]]=max(Banks[[1,"r_0"]],Banks[[1,"r_0"]]+Cfirms[1,"mu2",i]*(Cfirms[[2,"iL",i]]+Cfirms[[2,"rep_L",i]])/(Cfirms[[2,"CF",i]]))
  if(is.na(Cfirms[2,"r_L",i])){
  Cfirms[[2,"r_L",i]]=Banks[[1,"r_0"]]
  }
  Cfirms[[2,"I",i]]=Cfirms[[2,"i",i]]*Kfirms[[2,"p_k"]]
  Cfirms[[2,"D_f",i]]=Cfirms[[2,"D_f",i]]-Cfirms[[2,"I",i]]
  }else{
  fun<- function(x) Kfirms[[2,"p_k"]]+Kfirms[[2,"p_k"]]*(1-Cfirms[[2,"int",i]]/x)*((Cfirms[[2,"CF",i]]*Banks[[1,"r_0"]]+Cfirms[1,"mu2",i]*params[1,"theta"]*(Cfirms[[1,"L",i]]-params[1,"theta"]*Cfirms[[1,"L",i]]+x-Cfirms[[2,"int",i]]))/(Cfirms[[2,"CF",i]]-Cfirms[1,"mu2",i]*(Cfirms[[1,"L",i]]-params[1,"theta"]*Cfirms[[1,"L",i]]+x-Cfirms[[2,"int",i]])))/(params[1,"theta"]+params[1,"r_d"])-(Cfirms[[2,"Q",i]]-Cfirms[2,"mu1",i]*x^2)/Cfirms[[2,"k",i]]
  root<-uniroot.all(fun,c(0,sum(firminits[,"k"])/25),n=sum(firminits[,"k"])/100)
  if(length(root)<1){
  root=Cfirms[[2,"int",i]]
  }else{
  if(length(root)>1){
  root<-min(root)
  }
  if(Kfirms[[2,"p_k"]]+Kfirms[[2,"p_k"]]*(1-Cfirms[[2,"int",i]]/(root-0.025))*((Cfirms[[2,"CF",i]]*Banks[[1,"r_0"]]+Cfirms[1,"mu2",i]*params[1,"theta"]*(Cfirms[[1,"L",i]]-params[1,"theta"]*Cfirms[[1,"L",i]]+(root-0.025)-Cfirms[[2,"int",i]]))/(Cfirms[[2,"CF",i]]-Cfirms[1,"mu2",i]*(Cfirms[[(1),"L",i]]-params[1,"theta"]*Cfirms[[(1),"L",i]]+(root-0.025)-Cfirms[[2,"int",i]])))/(params[1,"theta"]+params[1,"r_d"])>Kfirms[[2,"p_k"]]+Kfirms[[2,"p_k"]]*(1-Cfirms[[2,"int",i]]/(root+0.025))*((Cfirms[[2,"CF",i]]*Banks[[(1),"r_0"]]+Cfirms[(1),"mu2",i]*params[1,"theta"]*(Cfirms[[1,"L",i]]-params[1,"theta"]*Cfirms[[1,"L",i]]+(root+0.025)-Cfirms[[2,"int",i]]))/(Cfirms[[2,"CF",i]]-Cfirms[(1),"mu2",i]*(Cfirms[[(1),"L",i]]-params[1,"theta"]*Cfirms[[1,"L",i]]+(root+0.025)-Cfirms[[2,"int",i]])))/(params[1,"theta"]+params[1,"r_d"])){
  root<-0
  }else{
  if((Cfirms[[2,"Q",i]]-Cfirms[2,"mu1",i]*root^2)/Cfirms[[2,"k",i]]<0)
  root<-0
  }}
  Cfirms[[2,"root",i]]<-root
  Cfirms[[2,"i",i]]=Cfirms[[2,"i",i]]+max(0,root/Kfirms[[2,"p_k"]])
  Cfirms[[2,"I",i]]=Cfirms[[2,"i",i]]*Kfirms[[2,"p_k"]]
  Cfirms[[2,"L",i]]=Cfirms[[(1),"L",i]]+max(0,Cfirms[[2,"I",i]]-Cfirms[[2,"int",i]])
  Cfirms[[2,"D_f",i]]=Cfirms[[2,"D_f",i]]+Cfirms[[2,"L",i]]-Cfirms[[(1),"L",i]]-Cfirms[[2,"I",i]]
  Cfirms[[2,"r_L",i]]=max(Banks[[1,"r_0"]],(Cfirms[[2,"CF",i]]*Banks[[(1),"r_0"]]+Cfirms[(1),"mu2",i]*Cfirms[[2,"L",i]]*params[1,"theta"]-Cfirms[(1),"mu2",i]*Cfirms[[2,"rep_L",i]]*params[1,"theta"])/(Cfirms[[2,"CF",i]]-Cfirms[[2,"L",i]]*Cfirms[(1),"mu2",i]+Cfirms[(1),"mu2",i]*Cfirms[[2,"rep_L",i]]))
  if(is.na(Cfirms[2,"r_L",i])){
  Cfirms[[2,"r_L",i]]=Banks[[1,"r_0"]] 
  }
  }
  }
  return(Cfirms)
}


distributeConsumption<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Households=stop("Need to have Households defined!"),params=params,args=args){
  for(i in 1:args$nF){
    Cfirms[[2,"p_rel",i]]=(Cfirms[[2,"p_c",i]]/mean(Cfirms[2,"p_c",]))^params[[1,"iota1"]]
    Cfirms[[2,"k_rel",i]]=(Cfirms[[2,"k",i]]/mean(Cfirms[2,"k",]))^params[[1,"iota2"]]
  }
  Cfirms[2,"p_rel",]=Cfirms[2,"p_rel",]/sum(Cfirms[2,"p_rel",])
  Cfirms[2,"k_rel",]=Cfirms[2,"k_rel",]/sum(Cfirms[2,"k_rel",])
  for(i in 1:args$nF){
  Cfirms[[2,"share",i]]=params[[1,"lambda1"]]*Cfirms[1,"share",i]+(1-params[[1,"lambda1"]])*(Cfirms[[2,"p_rel",i]]+Cfirms[[2,"k_rel",i]])/2  
  }
  Cfirms[2,"share",]=Cfirms[2,"share",]/sum(Cfirms[2,"share",])
  for(i in 1:args$nF){
  Cfirms[2,"share_shock",i]=params[1,"lambda2"]*Cfirms[1,"share_shock",i]+(1-params[1,"lambda2"])*max(0.01,rnorm(1,1,Cfirms[(1),"share",i]*params[1,"sigma"]))
  Cfirms[2,"share",i]=Cfirms[2,"share",i]*Cfirms[2,"share_shock",i]
  }
  Cfirms[2,"share",]=Cfirms[2,"share",]/sum(Cfirms[2,"share",])
  return(Cfirms)
}

cProduction<-function(Households=stop("need to have households defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),Aux=stop("need to have auxiliary array defined!"),Timer=Timer,params=params,args=args){
  Cfirms[2,"c_d",]=Cfirms[2,"share",]*Households[[2,"c_d"]]
  for(i in 1:args$nF){
  Cfirms[[2,"prod",i]]=min(Cfirms[[2,"c_d",i]],Cfirms[[2,"k",i]]*params[[1,"kappa"]])
  Cfirms[[2,"c",i]]=min(Cfirms[[2,"c_d",i]],Cfirms[[2,"prod",i]])
  }
  Cfirms[2,"N_dc",]=Cfirms[2,"prod",]+params[[1,"kappa_L"]]*(Cfirms[2,"k",]*params[[1,"kappa"]])
  Cfirms[2,"W_c",]=params[[1,"w"]]*Cfirms[2,"N_dc",]
  Cfirms[2,"C",]=Cfirms[2,"p_c",]*Cfirms[2,"c",]
  Cfirms[2,"D_f",]=Cfirms[2,"D_f",]+Cfirms[2,"C",]-Cfirms[2,"W_c",]
  Cfirms[2,"CF",]=pmax(0,Cfirms[2,"C",]-Cfirms[2,"W_c",])
  if(Timer>=200 && Timer<500){
  Aux[[2,"CFsum"]]=Aux[[1,"CFsum"]]+mean(Cfirms[2,"CF",])
  }else{
  if(Timer<200){
  Aux[[2,"CFsum"]]=0
  }
  if(Timer>=500)
  Aux[[2,"CFsum"]]=Aux[[1,"CFsum"]]
  }
  if(Timer>=500 && params[1,"types2"]==1){
  for(i in 1:args$nF){
  if(Cfirms[1,"type",i]==1){
  Cfirms[2,"Div_fc",i]=pmax(0,Cfirms[1,"gamma",i]*Aux[[2,"CFsum"]]/300)
  }else{
  Cfirms[2,"Div_fc",i]=pmax(0,Cfirms[1,"gamma",i]*Cfirms[2,"CF",i])
  }
  }
  }else{
  Cfirms[2,"Div_fc",]=pmax(0,Cfirms[1,"gamma",]*Cfirms[2,"CF",])
  }
  Cfirms[2,"int",]=pmax(0,Cfirms[2,"D_f",]-Cfirms[2,"Div_fc",]-Cfirms[2,"iL",]-Cfirms[2,"rep_L",])
  Cfirms[2,"share",]=Cfirms[2,"c",]/sum(Cfirms[2,"c",])
  return(list(Cfirms=Cfirms,Aux=Aux))
}

updateCapital2<-function(Kfirms=stop("need to have Kfirms defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[2,"I",]=Kfirms[[2,"p_k"]]*Cfirms[2,"i",]
  Cfirms[2,"k",]=Cfirms[2,"k",]+Cfirms[2,"i",]
  Cfirms[2,"K",]=Cfirms[2,"K",]+Cfirms[2,"I",]
  Cfirms[2,"deltaK",]=Cfirms[2,"K",]-Cfirms[1,"K",]
  Cfirms[2,"u",]=Cfirms[2,"prod",]/(Cfirms[2,"k",]*params[[1,"kappa"]])
  return(Cfirms)
}

loanPayments<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[2,"iL",]=Cfirms[(1),"r_L",]*Cfirms[(1),"L",]
  Cfirms[2,"rep_L",]=params[[1,"theta"]]*Cfirms[(1),"L",]
  return(Cfirms)
}

profitCfirms<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Aux=stop("need to have auxiliary array defined!"),params=params,args=args,Timer=Timer){
  Cfirms[2,"Pr_fc",]=Cfirms[2,"C",]-Cfirms[2,"I",]+Cfirms[2,"deltaK",]-Cfirms[2,"W_c",]-Cfirms[2,"iL",]
  if(Timer>=200 && Timer<500){
  Aux[[2,"outputsum"]]=Aux[[1,"outputsum"]]+sum(Cfirms[2,"prod",])+sum(Cfirms[2,"i",])
  }else{
  if(Timer<200){
  Aux[[2,"outputsum"]]=0
  }
  if(Timer>=500)
  Aux[[2,"outputsum"]]=Aux[[1,"outputsum"]]
  }
  if(params[1,"divpol"]==1 && Timer>=500){
  y_current=sum(Cfirms[2,"prod",])+sum(Cfirms[2,"i",])
  for(i in 1:args$nF){  
  Cfirms[2,"gamma",i]=min(1,max(0,params[1,"gamma"]*2/(1+exp(params[1,"tau"]*(((Cfirms[(2),"iL",i]+Cfirms[(2),"rep_L",i])/Cfirms[(2),"CF",i])-params[1,"beta1"])))))
  Cfirms[2,"divmax",i]=min(params[1,"gamma_max"],max(0,params[1,"gamma_max"]-params[1,"phi_div"]*(y_current-(Aux[[2,"outputsum"]]/300))/(Aux[[2,"outputsum"]]/300)))
  Cfirms[2,"gamma",i]=min(Cfirms[2,"gamma",i],Cfirms[2,"divmax",i])
  }
  }else{
  Cfirms[2,"gamma",]=pmin(1,pmax(0,params[1,"gamma"]*2/(1+exp(params[1,"tau"]*(((Cfirms[(2),"iL",]+Cfirms[(2),"rep_L",])/Cfirms[(2),"CF",])-params[1,"beta1"])))))
  }
  return(Cfirms)
}

bankruptcy<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Kfirms=stop("need to have K-Firms defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have Aux Array defined!"),params=params,args=args,Timer=Timer,outputmicro=outputmicro){
  Cfirms[2,"i",]=0
  Cfirms[2,"D_f",]=Cfirms[1,"D_f",]
  Aux[[2,"counter"]]=Aux[[1,"counter"]]
  Aux[[2,"defaults"]]=0
  Aux[[2,"defaults2"]]=0
  Aux[[2,"Hedge"]]=0
  Aux[[2,"Speculative"]]=0
  Aux[[2,"Ponzi"]]=0
  Aux[[2,"recap"]]=0
  outputmicro[args$t,"bankrupt",]<-0
  Cfirms[2,"bankrupt",]<-0
  for(i in 1:args$nF){
  excess=(Cfirms[[(2),"iL",i]]+Cfirms[[(2),"rep_L",i]]+Cfirms[[1,"Div_fc",i]])/Cfirms[[(1),"CF",i]]-1
  if(is.na(excess)){excess=0}
  if(Cfirms[[(2),"iL",i]]+Cfirms[[(2),"rep_L",i]]+Cfirms[[1,"Div_fc",i]]<Cfirms[[(1),"CF",i]] && Timer>2){
    Aux[[2,"Hedge"]]=Aux[[2,"Hedge"]]+1
  }
  if(Cfirms[[(2),"iL",i]]+Cfirms[[(2),"rep_L",i]]+Cfirms[[1,"Div_fc",i]]>Cfirms[[(1),"CF",i]] && Cfirms[[(2),"iL",i]]+Cfirms[[1,"Div_fc",i]]<Cfirms[[(1),"CF",i]] && Timer>2){
    Aux[[2,"Speculative"]]=Aux[[2,"Speculative"]]+1
  }
  if(Cfirms[[(2),"iL",i]]+Cfirms[[1,"Div_fc",i]]>Cfirms[[(1),"CF",i]] && Timer>2){
    Aux[[2,"Ponzi"]]=Aux[[2,"Ponzi"]]+1
  }
  if(excess>0){
  bprob=tanh(params[[1,"beta2"]]*excess)
  }else{
  bprob=0
  }
  rdraw=runif(1)
  if(bprob>rdraw || Cfirms[1,"E_fc",i]<0){
  Cfirms[[2,"bankrupt",i]]=1
  outputmicro[args$t,"bankrupt",i]=1
  Aux[[2,"counter"]]=Aux[[2,"counter"]]+1
  Aux[[2,"defaults"]]=Aux[[2,"defaults"]]+Cfirms[[1,"L",i]]-Cfirms[[1,"D_f",i]]-params[1,"chi"]*Cfirms[[2,"K",i]]
  Aux[[2,"defaults2"]]=Cfirms[[1,"L",i]]
  Cfirms[[2,"D_f",i]]=0
  Cfirms[[1,"D_f",i]]=0
  Aux[[2,"recap"]]=Aux[[2,"recap"]]+params[1,"chi"]*Cfirms[[2,"K",i]]
  Cfirms[[2,"K",i]]=params[1,"chi"]*Cfirms[[2,"K",i]]
  Cfirms[[2,"k",i]]=params[1,"chi"]*Cfirms[[2,"k",i]]
  Cfirms[[1,"K",i]]=Cfirms[[2,"K",i]]/(1-params[1,"delta"])
  Cfirms[[1,"k",i]]=Cfirms[[2,"k",i]]/(1-params[1,"delta"])
  Cfirms[[1,"L",i]]=0
  Cfirms[2,"iL",i]=0
  Cfirms[2,"rep_L",i]=0
  krel=(Cfirms[(2),"k",]/mean(Cfirms[(2),"k",]))^params[[1,"iota2"]]
  krel[i]=krel[i]/sum(krel)
  Cfirms[[1,"share",i]]=krel[i]
  Cfirms[[1,"c_d",i]]=Cfirms[[1,"share",i]]*sum(outputmicro[(args$t-1),"c_d",])
  Cfirms[[1,"omega_c",i]]=(params[1,"omega"])*(2/(1+exp(params[1,"epsilon"]*((mean(Cfirms[(1),"share",i])/(1/args$nF))-1))))
  Cfirms[[1,"share_shock",i]]=1
  Cfirms[[1,"r_L",i]]=Banks[[1,"r_0"]]
  Cfirms[1,"E_fc",i]=Cfirms[[1,"K",i]]+Cfirms[[1,"D_f",i]]-Cfirms[[1,"L",i]]
  Cfirms[1,"mu2",i]=params[1,"mu2"]
  }}
  return(list(Aux=Aux,Cfirms=Cfirms,Kfirms=Kfirms,outputmicro=outputmicro))
}


CDividends<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Kfirms=stop("need to have K-Firms defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have Aux Array defined!"),params=params,args=args,Timer=Timer,outputmicro=outputmicro){
  outputmicro[args$t,"share",]<-Cfirms[2,"share",]
  Cfirms[2,"E_fc",]=Cfirms[1,"E_fc",]+Cfirms[2,"Pr_fc",]
  Kfirms[[2,"i_p"]]=Kfirms[[2,"i"]]
  Kfirms[[2,"I_p"]]=Kfirms[[2,"I"]]
  for(i in 1:args$nF){
  if((Cfirms[2,"Pr_fc",i])>0){
  Cfirms[[2,"Div_fc",i]]=max(0,min(Cfirms[[2,"D_f",i]],Cfirms[[2,"Div_fc",i]]))
  Cfirms[[2,"L",i]]=Cfirms[[2,"L",i]]-min(0,Cfirms[[2,"D_f",i]]-Cfirms[[2,"Div_fc",i]])
  Cfirms[[2,"D_f",i]]=max(0,Cfirms[[2,"D_f",i]]-Cfirms[[2,"Div_fc",i]])
  Cfirms[[2,"E_fc",i]]=Cfirms[[2,"E_fc",i]]-Cfirms[[2,"Div_fc",i]]
  }else{
  Cfirms[[2,"Div_fc",i]]=0
  Cfirms[[2,"D_f",i]]=Cfirms[[2,"D_f",i]]-Cfirms[[2,"Div_fc",i]]
  Cfirms[[2,"E_fc",i]]=Cfirms[[2,"E_fc",i]]-Cfirms[[2,"Div_fc",i]]
  }
  Cfirms[[2,"L",i]]=Cfirms[[2,"L",i]]-min(0,Cfirms[[2,"D_f",i]]-Cfirms[[2,"iL",i]]-Cfirms[[2,"rep_L",i]])
  Cfirms[[2,"D_f",i]]=max(0,Cfirms[[2,"D_f",i]]-Cfirms[[2,"iL",i]]-Cfirms[[2,"rep_L",i]])
  Cfirms[[2,"L",i]]=Cfirms[[2,"L",i]]-Cfirms[[2,"rep_L",i]]
  Cfirms[[2,"r_L",i]]=max(Banks[[1,"r_0"]],(Cfirms[[2,"CF",i]]*Banks[[1,"r_0"]]+Cfirms[[2,"L",i]]*Cfirms[1,"mu2",i]*params[1,"theta"])/(Cfirms[[2,"CF",i]]-Cfirms[[2,"L",i]]*Cfirms[1,"mu2",i]))
  if(Cfirms[2,"L",i]>=Cfirms[2,"D_f",i] && Cfirms[2,"D_f",i]>=0){
  Cfirms[2,"L",i]=Cfirms[2,"L",i]-Cfirms[2,"D_f",i]
  Cfirms[2,"D_f",i]=0
  }else{
  if(Cfirms[2,"L",i]<Cfirms[2,"D_f",i] && Cfirms[2,"D_f",i]>=0){
  Cfirms[2,"D_f",i]=Cfirms[2,"D_f",i]-Cfirms[2,"L",i]
  Cfirms[2,"L",i]=0
  }
  }
  Cfirms[[2,"r_L",i]]=max(Banks[[1,"r_0"]],(Cfirms[[2,"CF",i]]*Banks[[1,"r_0"]]+Cfirms[[2,"L",i]]*Cfirms[1,"mu2",i]*params[1,"theta"])/(Cfirms[[2,"CF",i]]-Cfirms[[2,"L",i]]*Cfirms[1,"mu2",i]))
  if(is.na(Cfirms[2,"r_L",i])){
  Cfirms[[2,"r_L",i]]=Banks[[1,"r_0"]] 
  }
  }
  Cfirms[2,"V_fc",]=Cfirms[2,"K",]+Cfirms[2,"D_f",]-Cfirms[2,"L",]-Cfirms[2,"E_fc",]
  Cfirms[2,"mu2",]=params[[1,"mu2"]]
  return(list(Aux=Aux,Cfirms=Cfirms,Kfirms=Kfirms,outputmicro=outputmicro))
}

calculateCPI<-function(Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirmsagg[[2,"CPI"]]=sum(Cfirms[2,"p_c",]*(Cfirms[1,"C",]/sum(Cfirms[1,"C",])))
  return(Cfirmsagg)
}

Cfaggregates<-function(Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirmsagg[[2,"c"]]=sum(Cfirms[2,"c",])
  Cfirmsagg[[2,"C"]]=sum(Cfirms[2,"C",])
  Cfirmsagg[[2,"prod"]]=sum(Cfirms[2,"prod",])
  Cfirmsagg[[2,"Prod"]]=sum(Cfirms[2,"prod",]*Cfirms[2,"p_c",])
  Cfirmsagg[[2,"k"]]=sum(Cfirms[2,"k",])
  Cfirmsagg[[2,"K"]]=sum(Cfirms[2,"K",])
  Cfirmsagg[[2,"W_c"]]=sum(Cfirms[2,"W_c",])
  Cfirmsagg[[2,"Div_fc"]]=sum(Cfirms[2,"Div_fc",])
  Cfirmsagg[[2,"iL"]]=sum(Cfirms[2,"iL",])
  Cfirmsagg[[2,"rep_L"]]=sum(Cfirms[2,"rep_L",])
  Cfirmsagg[[2,"L"]]=sum(Cfirms[2,"L",])
  Cfirmsagg[[2,"D_f"]]=sum(Cfirms[2,"D_f",])
  Cfirmsagg[[2,"N_dc"]]=sum(Cfirms[2,"N_dc",])
  Cfirmsagg[[2,"E_fc"]]=sum(Cfirms[2,"E_fc",])
  Cfirmsagg[[2,"V_fc"]]=sum(Cfirms[2,"V_fc",])
  Cfirmsagg[[2,"K"]]=sum(Cfirms[2,"K",])
  Cfirmsagg[[2,"sav_fc"]]=sum(Cfirms[2,"sav_fc",])
  Cfirmsagg[[2,"Pr_fc"]]=sum(Cfirms[2,"Pr_fc",])
  Cfirmsagg[[2,"Div_fc"]]=sum(Cfirms[2,"Div_fc",])
  Cfirmsagg[[2,"I"]]=sum(Cfirms[2,"I",])
  Cfirmsagg[[2,"i"]]=sum(Cfirms[2,"i",])
  Cfirmsagg[[2,"p_c"]]=mean(Cfirms[2,"p_c",])
  Cfirmsagg[[2,"bankrupt"]]=sum(Cfirms[2,"bankrupt",])
  Cfirmsagg[[2,"Y"]]=Cfirmsagg[[2,"Prod"]]+Cfirmsagg[[2,"I"]]
  Cfirmsagg[[2,"y"]]=Cfirmsagg[[2,"prod"]]+Cfirmsagg[[2,"i"]]
  Cfirmsagg[[2,"r_L"]]=sum(Cfirms[2,"r_L",]*(Cfirms[(2),"L",]/sum(Cfirms[(2),"L",])))
  Cfirmsagg[[2,"u"]]=sum(Cfirms[2,"u",]*(Cfirms[(2),"c",]/sum(Cfirms[(2),"c",])))
  return(Cfirmsagg)
}

###BANKS#####
bankStocks1<-function(Banks=stop("need to have Banks defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Banks[[2,"D_f"]]=Cfirmsagg[[2,"D_f"]]
  Banks[[2,"L"]]=Cfirmsagg[[2,"L"]]
  return(Banks)
}

bankStocks2<-function(Banks=stop("need to have Banks defined!"),Gov=stop("need to have government defined!"),Households=stop("Need to have Households defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Banks[[2,"D_h"]]=Households[[2,"D_h"]]
  Banks[[2,"D"]]=Cfirmsagg[[2,"D_f"]]+Households[[2,"D_h"]]
  Banks[[2,"V_b"]]=Banks[[2,"L"]]-Banks[[2,"D"]]-Banks[[2,"E_b"]]-Gov[[2,"D_g"]]+Gov[[2,"Bonds"]]
  return(Banks)
}


payDividendsB<-function(Banks=stop("need to have Banks defined!"),Gov=stop("need to have government defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Aux=stop("Need to have Aux defined!"),params=params,args=args,Timer=Timer){
  Banks[[2,"Pr_b"]]=Cfirmsagg[[2,"iL"]]+Banks[[1,"r_0"]]*Gov[[1,"Bonds"]]
  if(params[1,"bank"]==0){
  Banks[[2,"r_0"]]=params[1,"r_0"]
  }else{
  if(Timer<500){
  Banks[[2,"r_0"]]=params[1,"r_0"]
  }else{
  Banks[[2,"r_0"]]=max(0,params[1,"phi_b2"]*Banks[[1,"r_0"]]+(1-params[1,"phi_b2"])*(params[[1,"r_0"]]+params[1,"phi_b1"]*(Cfirmsagg[2,"y"]-Cfirmsagg[1,"y"])/Cfirmsagg[1,"y"]))
  }}
  Banks[[2,"E_b"]]=Banks[[1,"E_b"]]+Banks[[2,"Pr_b"]]-Aux[[2,"defaults"]]
  Banks[[2,"Div_b"]]=max(0,(Banks[[2,"Pr_b"]]-Aux[[2,"defaults"]]))
  Banks[[2,"E_b"]]=Banks[[2,"E_b"]]-Banks[[2,"Div_b"]]
  div2=(Banks[[2,"E_b"]]-params[[1,"CAR"]]*Banks[[2,"L"]])
  Banks[[2,"E_b"]]=Banks[[2,"E_b"]]-div2
  Banks[[2,"Div_b"]]=max(0,Banks[[2,"Div_b"]]+div2)
  Banks[[2,"CAR"]]=Banks[[2,"E_b"]]/(Banks[[2,"L"]])
  Banks[[2,"sav_b"]]=Cfirmsagg[[2,"iL"]]+Banks[[1,"r_0"]]*Gov[[1,"Bonds"]]-Banks[[2,"Div_b"]]
  return(list(Banks=Banks,Aux=Aux))
}

###GOVERNMENT####
FP<-function(Gov=stop("need to have government defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("Need to have auxiliary array defined!"),params=params,args=args,inits=inits,firminits=firminits,Timer=Timer){
  if(Timer>=200 && Timer<500){
  Aux[[2,"outputsum"]]=Aux[[1,"outputsum"]]+Cfirmsagg[2,"y"]
  }else{
  if(Timer<200){
  Aux[[2,"outputsum"]]=0
  }
  if(Timer>=500)
  Aux[[2,"outputsum"]]=Aux[[1,"outputsum"]]
  }
  if(params[1,"gov"]==0){
  Gov[[2,"stimulus"]]=0
  }else{
  if(Timer>=500){
  Gov[[2,"stimulus"]]=params[1,"phi_g"]*(Aux[[2,"outputsum"]]/300-Cfirmsagg[2,"y"])
  }else{
  Gov[[2,"stimulus"]]=0
  }}
  Gov[[2,"cumulative"]]=Gov[[1,"cumulative"]]+Gov[[2,"stimulus"]]+Banks[[1,"r_0"]]*Gov[[1,"Bonds"]]
  if(Gov[[2,"cumulative"]]>=0){
    Gov[[2,"Bonds"]]=Gov[[2,"cumulative"]]
    Gov[[2,"D_g"]]=0
  }else{
    Gov[[2,"Bonds"]]=0
    Gov[[2,"D_g"]]=-Gov[[2,"cumulative"]]
  }
  Gov[[2,"V_g"]]=Gov[[2,"D_g"]]-Gov[[2,"Bonds"]]
  Gov[[2,"sav_g"]]=-Gov[[2,"stimulus"]]-Banks[[1,"r_0"]]*Gov[[1,"Bonds"]]
  return(list(Gov=Gov,Aux=Aux))
}

collectData<-function(Households=stop("need to have households defined!"),Kfirms=stop("need to have K-firms defined!"),Cfirms=stop("need to have C-firms defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have auxilliary array defined!"),args=args,output=output,outputmicro=outputmicro){
  output[[args$t,"K"]]=Cfirmsagg[2,"K"]
  output[[args$t,"L"]]=Cfirmsagg[2,"L"]
  output[[args$t,"i"]]=Cfirmsagg[2,"i"]
  output[[args$t,"Y"]]=Cfirmsagg[2,"Y"]
  output[[args$t,"y"]]=Cfirmsagg[2,"y"]
  output[[args$t,"D_f"]]=sum(Cfirms[2,"D_f",])
  output[[args$t,"c"]]=sum(Cfirms[2,"c",])
  output[[args$t,"Hedge"]]=(Aux[2,"Hedge"])
  output[[args$t,"Speculative"]]=(Aux[2,"Speculative"])
  output[[args$t,"Ponzi"]]=(Aux[2,"Ponzi"])
  outputmicro[args$t,"i",]=Cfirms[2,"i",]
  outputmicro[args$t,"share",]=Cfirms[2,"share",]
  outputmicro[args$t,"bankrupt",]=Cfirms[2,"bankrupt",]
  outputmicro[args$t,"type",]=Cfirms[2,"type",]
  outputmicro[args$t,"c_d",]=Cfirms[2,"c_d",]
  return(list(output=output,outputmicro=outputmicro))
}

collectFullData<-function(Households=stop("need to have households defined!"),Gov=stop("need to have government defined!"),Kfirms=stop("need to have K-firms defined!"),Cfirms=stop("need to have C-firms defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have auxilliary array defined!"),args=args,output=output,outputmicro=outputmicro){
  output[[args$t,"E_b"]]=Banks[[2,"E_b"]]
  output[[args$t,"V_h"]]=Households[2,"V_h"]
  output[[args$t,"YD"]]=Households[2,"YD"]
  output[[args$t,"W"]]=Households[2,"W"]
  output[[args$t,"Div_f"]]=Households[2,"Div_f"]
  output[[args$t,"sav_h"]]=Households[2,"sav_h"]
  output[[args$t,"c_d"]]=Households[2,"c_d"]
  output[[args$t,"D_h"]]=Households[2,"D_h"]
  output[[args$t,"V_fc"]]=Cfirmsagg[2,"V_fc"]
  output[[args$t,"k"]]=Cfirmsagg[2,"k"]
  output[[args$t,"K"]]=Cfirmsagg[2,"K"]
  output[[args$t,"L"]]=Cfirmsagg[2,"L"]
  output[[args$t,"i"]]=Cfirmsagg[2,"i"]
  output[[args$t,"iL"]]=Cfirmsagg[2,"iL"]
  output[[args$t,"I"]]=Cfirmsagg[2,"I"]
  output[[args$t,"Y"]]=Cfirmsagg[2,"Y"]
  output[[args$t,"y"]]=Cfirmsagg[2,"y"]
  output[[args$t,"D_f"]]=Cfirmsagg[2,"D_f"]
  output[[args$t,"W_c"]]=Cfirmsagg[2,"W_c"]
  output[[args$t,"Div_fc"]]=Cfirmsagg[2,"Div_fc"]
  output[[args$t,"sav_fc"]]=Cfirmsagg[2,"sav_fc"]
  output[[args$t,"rep_L"]]=Cfirmsagg[2,"rep_L"]
  output[[args$t,"Pr_fc"]]=Cfirmsagg[2,"Pr_fc"]
  output[[args$t,"c"]]=Cfirmsagg[2,"c"]
  output[[args$t,"C"]]=Cfirmsagg[2,"C"]
  output[[args$t,"CPI"]]=Cfirmsagg[2,"CPI"]
  output[[args$t,"r_L"]]=Cfirmsagg[2,"r_L"]
  output[[args$t,"u"]]=Cfirmsagg[2,"u"]
  output[[args$t,"bankrupt"]]=Cfirmsagg[2,"bankrupt"]
  output[[args$t,"CF"]]=Cfirmsagg[2,"CF"]
  output[[args$t,"i_p"]]=Kfirms[2,"i_p"]
  output[[args$t,"I"]]=Cfirmsagg[2,"I"]
  output[[args$t,"I_p"]]=Kfirms[2,"I_p"]
  output[[args$t,"Div_fk"]]=Kfirms[2,"Div_fk"]
  output[[args$t,"Div_b"]]=Banks[2,"Div_b"]
  output[[args$t,"stimulus"]]=Gov[2,"stimulus"]
  output[[args$t,"r_0"]]=Banks[2,"r_0"]
  output[[args$t,"SFCcheck1"]]=Aux[2,"SFCcheck1"]
  output[[args$t,"SFCcheck2"]]=Aux[2,"SFCcheck2"]
  output[[args$t,"recap"]]=Aux[2,"recap"]
  output[[args$t,"defaults"]]=Aux[2,"defaults"]
  output[[args$t,"defaults2"]]=Aux[2,"defaults2"]
  output[[args$t,"counter"]]=Aux[2,"counter"]
  output[[args$t,"Hedge"]]=Aux[2,"Hedge"]
  output[[args$t,"Speculative"]]=Aux[2,"Speculative"]
  output[[args$t,"Ponzi"]]=Aux[2,"Ponzi"]
  output[[args$t,"Bonds"]]=Gov[2,"Bonds"]
  output[[args$t,"D_g"]]=Gov[2,"D_g"]
  output[[args$t,"V_g"]]=Gov[2,"V_g"]
  output[[args$t,"sav_g"]]=Gov[2,"sav_g"]
  output[[args$t,"cumulative"]]=Gov[2,"cumulative"]
  outputmicro[args$t,"mu2",]=Cfirms[2,"mu2",]
  outputmicro[args$t,"share",]=Cfirms[2,"share",]
  outputmicro[args$t,"bankrupt",]=Cfirms[2,"bankrupt",]
  outputmicro[args$t,"type",]=Cfirms[2,"type",]
  outputmicro[args$t,"V_fc",]=Cfirms[2,"V_fc",]
  outputmicro[args$t,"K",]=Cfirms[2,"K",]
  outputmicro[args$t,"k",]=Cfirms[2,"k",]
  outputmicro[args$t,"D_f",]=Cfirms[2,"D_f",]
  outputmicro[args$t,"L",]=Cfirms[2,"L",]
  outputmicro[args$t,"i",]=Cfirms[2,"i",]
  outputmicro[args$t,"I",]=Cfirms[2,"I",]
  outputmicro[args$t,"W_c",]=Cfirms[2,"W_c",]
  outputmicro[args$t,"Div_fc",]=Cfirms[2,"Div_fc",]
  outputmicro[args$t,"iL",]=Cfirms[2,"iL",]
  outputmicro[args$t,"rep_L",]=Cfirms[2,"rep_L",]
  outputmicro[args$t,"sav_fc",]=Cfirms[2,"sav_fc",]
  outputmicro[args$t,"p_c",]=Cfirms[2,"p_c",]
  outputmicro[args$t,"Pr_fc",]=Cfirms[2,"Pr_fc",]
  outputmicro[args$t,"c",]=Cfirms[2,"c",]
  outputmicro[args$t,"C",]=Cfirms[2,"C",]
  outputmicro[args$t,"c_d",]=Cfirms[2,"c_d",]
  outputmicro[args$t,"r_L",]=Cfirms[2,"r_L",]
  outputmicro[args$t,"Q",]=Cfirms[2,"Q",]
  outputmicro[args$t,"omega_c",]=Cfirms[2,"omega_c",]
  outputmicro[args$t,"p_dkint",]=Cfirms[2,"p_dkint",]
  outputmicro[args$t,"u",]=Cfirms[2,"u",]
  outputmicro[args$t,"p_rel",]=Cfirms[2,"p_rel",]
  outputmicro[args$t,"k_rel",]=Cfirms[2,"k_rel",]
  outputmicro[args$t,"int",]=Cfirms[2,"int",]
  outputmicro[args$t,"CF",]=Cfirms[2,"CF",]
  outputmicro[args$t,"deltaK",]=Cfirms[2,"deltaK",]
  outputmicro[args$t,"root",]=Cfirms[2,"root",]
  outputmicro[args$t,"mu1",]=Cfirms[2,"mu1",]
  outputmicro[args$t,"gamma",]=Cfirms[2,"gamma",]
  outputmicro[args$t,"share_shock",]=Cfirms[2,"share_shock",]
  outputmicro[args$t,"yield",]=Cfirms[2,"yield",]
  outputmicro[args$t,"attitude",]=Cfirms[2,"attitude",]
  outputmicro[args$t,"index",]=Cfirms[2,"index",]
  outputmicro[args$t,"sentiment",]=Cfirms[2,"sentiment",]
  outputmicro[args$t,"Q_f",]=Cfirms[2,"Q_f",]
  outputmicro[args$t,"Q_e",]=Cfirms[2,"Q_e",]
  outputmicro[args$t,"E_fc",]=Cfirms[2,"E_fc",]
  outputmicro[args$t,"divmax",]=Cfirms[2,"divmax",]
  return(list(output=output,outputmicro=outputmicro))
}
  

shiftIndex<-function(Households=stop("need to have households defined!"),Gov=stop("need to have government defined!"),Kfirms=stop("need to have K-firms defined!"),Cfirmsagg=stop("need to have C-firms (aggregate) defined!"),Cfirms=stop("need to have C-firms defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have auxilliary array defined!")){
  Banks[1,]<-Banks[2,]
  Households[1,]<-Households[2,]
  Kfirms[1,]<-Kfirms[2,]
  Cfirms[1,,]<-Cfirms[2,,]
  Aux[1,]<-Aux[2,]
  Gov[1,]<-Gov[2,]
  Cfirmsagg[1,]<-Cfirmsagg[2,]
  Banks[2,]<-NA
  Households[2,]<-NA
  Kfirms[2,]<-NA
  Cfirms[2,,]<-NA
  Aux[2,]<-NA
  Gov[2,]<-NA
  return(list(Banks=Banks,Cfirmsagg=Cfirmsagg,Households=Households,Kfirms=Kfirms,Cfirms=Cfirms,Aux=Aux,Gov=Gov))
}
  
  
  
  
  
  
  