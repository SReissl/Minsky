runModel<-function(seed=1,nF=100,Time = 2000,nrun=80,foldername="Sensitivitydata/"){
  library(abind)
  library(rootSolve)
  ###Argument list
  args<-as.data.frame(array(data=0,dim=c(1,4),dimnames=list(NULL,c("t","nF","Time","nrun"))))
  args$t=1
  args$nF=nF
  args$Time=Time
  args$nrun=nrun

  #####Sectoral Structure####
  
  # HH column names
  HNames<-c("V_h","V_he","YD","YD_e","E_f","W","Div_f","sav_h","c_d","D_h")
  Households<-array(data = NA, dim=c(args$Time/args$nrun+10,length(HNames)),dimnames = list(NULL,HNames))
  
  # C-Firm column names (ABM)
  CFNames<-c("V_fc","K","k","D_f","L","E_fc","i","I","N_dc","W_c","Div_fc","iL","rep_L","sav_fc","p_c","Pr_fc","c","C","share","c_d","r_L","Q","omega_c","p_dkint","u","p_rel","k_rel","int","CF","deltaK","root","bankrupt","mu1","gamma","share_shock","yield","attitude","index","sentiment")
  Cfirms<-array(data = NA,dim=c(args$Time/args$nrun+10,length(CFNames),args$nF),dimnames = list(NULL,CFNames,NULL))
  
  # C-Firm column names (Agg)
  CFNamesagg<-c("V_fc","K","k","D_f","L","E_fc","i","I","N_dc","W_c","Div_fc","iL","rep_L","sav_fc","p_c","Pr_fc","c","C","CPI","r_L","Y","y","p","u","p_c","bankrupt","CF")
  Cfirmsagg<-array(data = NA, dim=c(args$Time/args$nrun+10,length(CFNamesagg)),dimnames = list(NULL,CFNamesagg))
  
  # K-Firm column names
  KFNames<-c("V_fk","E_fk","i","i_p","I","I_p","W_k","Div_fk","sav_fk","p_k","Pr_fk","N_dk")
  Kfirms<-array(data = NA, dim=c(args$Time/args$nrun+10,length(KFNames)),dimnames = list(NULL,KFNames))
  
  # Bank column names
  BNames<-c("V_b","E_b","L","D","D_h","D_f","Div_b","sav_b","Pr_b","CAR","mu2","stimulus","r_0")
  Banks<-array(data = NA, dim=c(args$Time/args$nrun+10,length(BNames)),dimnames = list(NULL,BNames))
  
  # auxiliary stuff
  AuxNames<-c("SFCcheck1","SFCcheck2","recap","defaults","counter","def_liqu","def_equ")
  Aux<-array(data = NA, dim=c(args$Time/args$nrun+10,length(AuxNames)),dimnames = list(NULL,AuxNames))
  
  #####Data Frames#####
  datanames<-c(HNames,CFNamesagg,KFNames,BNames,AuxNames)
  output<-as.data.frame(array(data = NA, dim=c((args$Time+10),length(datanames)),dimnames = list(NULL,datanames)))
  
    #setting the seed for reproducibility
    set.seed(seed)
    #initialise random numbers
    
    #######Initial values####
    Households[1,"D_h"]<-inits[[1,"D_h"]]
    Households[1,"YD"]<-inits[[1,"YD"]]
    Households[1,"YD_e"]<-inits[[1,"YD"]]
    Households[1,"V_h"]<-inits[[1,"V_h"]]
    Households[1,"V_he"]<-inits[[1,"V_h"]]
    Kfirms[1,"E_fk"]<-inits[[1,"E_fk"]]
    Banks[1,"E_b"]<-inits[[1,"E_b"]]
    Cfirmsagg[1,"Y"]<-inits[[1,"Y"]]
    Aux[1,"counter"]<-0
    Banks[1,"mu2"]<-params[1,"mu2"]
    Banks[1,"r_0"]<-params[1,"r_0"]
    
    #firminits
    Cfirms[1,"K",]<-firminits[,"K"]
    Cfirms[1,"k",]<-firminits[,"k"]
    Cfirms[1,"omega_c",]<-params[1,"omega"]
    Cfirms[1,"L",]<-firminits[,"L"]
    Cfirms[1,"r_L",]<-firminits[,"r_L"]
    Cfirms[1,"share",]<-firminits[,"share"]
    Cfirms[1,"D_f",]<-firminits[,"D_f"]
    Cfirms[1,"C",]<-firminits[,"C"]
    Cfirms[1,"E_fc",]<-firminits[,"E_fc"]
    Cfirms[1,"p_c",]<-(1+params[1,"omega"])*params[1,"w"]
    Cfirms[1,"bankrupt",]<-0
    Cfirms[1,"gamma",]<-firminits[,"gamma"]
    Cfirms[1,"share_shock",]<-1
  
    #####THIS IS THE PERIOD REPETITION WITHIN 1 MONTE CARLO SIMULATION#####
    r=1
    Timer=1
    for(r in 1:args$nrun){
    if(r>1){
    args$t=10
    marker=10
    Households[1:10,]<-Households[(args$Time/args$nrun+1):(args$Time/args$nrun+10),]
    Households[11:(args$Time/args$nrun+10),]<-NA
    Kfirms[1:10,]<-Kfirms[(args$Time/args$nrun+1):(args$Time/args$nrun+10),]
    Kfirms[11:(args$Time/args$nrun+10),]<-NA
    Cfirmsagg[1:10,]<-Cfirmsagg[(args$Time/args$nrun+1):(args$Time/args$nrun+10),]
    Cfirmsagg[11:(args$Time/args$nrun+10),]<-NA
    Cfirms[1:10,,]<-Cfirms[(args$Time/args$nrun+1):(args$Time/args$nrun+10),,]
    Cfirms[11:(args$Time/args$nrun+10),,]<-NA
    Banks[1:10,]<-Banks[(args$Time/args$nrun+1):(args$Time/args$nrun+10),]
    Banks[11:(args$Time/args$nrun+10),]<-NA
    Aux[1:10,]<-Aux[(args$Time/args$nrun+1):(args$Time/args$nrun+10),]
    Aux[11:(args$Time/args$nrun+10),]<-NA
    }else{
    args$t=1
    marker=1
    }
    for(t in marker:(args$Time/args$nrun+9)){
      #update
      Timer=Timer+1
      args$t=(args$t+1)
      #PERIOD SIMULATION
      #call functions here
      Cfirms<-updateCapital1(Cfirms=Cfirms,params=params,args=args)
      Cfirms<-loanPayments(Cfirms=Cfirms,params=params,args=args)
      Cfirms<-setCprice(Cfirms=Cfirms,params=params,args=args,Timer=Timer)
      Cfirmsagg<-calculateCPI(Cfirmsagg=Cfirmsagg,Cfirms=Cfirms,params=params,args=args)
      Kfirms<-setKprice(Kfirms=Kfirms,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Households<-buildExpectationHH(Households=Households,params=params,args=args)
      Households<-decideConsumption(Households=Households,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Cfirms<-distributeConsumption(Cfirms=Cfirms,Households=Households,params=params,args=args)
      Cfirms<-cProduction(Households=Households,Cfirms=Cfirms,params=params,args=args)
      YieldReturn<-estimateYield(Cfirms=Cfirms,Cfirmsagg=Cfirmsagg,params=params,args=args,Timer=Timer)
      Cfirms<-YieldReturn$Cfirms
      Cfirmsagg<-YieldReturn$Cfirmsagg
      Cfirms<-Invest(Cfirms=Cfirms,Banks=Banks,Cfirmsagg=Cfirmsagg,Kfirms=Kfirms,params=params,args=args,Timer=Timer,firminits=firminits)
      Cfirms<-updateCapital2(Kfirms=Kfirms,Cfirms=Cfirms,params=params,args=args)
      Kfirms<-supplyCapital(Kfirms=Kfirms,Cfirms=Cfirms,params=params,args=args)
      Cfirms<-profitCfirms(Cfirms=Cfirms,params=params,args=args)
      bankruptcyReturns<-bankruptcyDividends(Cfirms=Cfirms,Kfirms=Kfirms,Banks=Banks,Aux=Aux,params=params,args=args,Timer=Timer,firminits=firminits)
      Cfirms<-bankruptcyReturns$Cfirms
      Aux<-bankruptcyReturns$Aux
      Kfirms<-bankruptcyReturns$Kfirms
      Cfirms<-Cfsave(Cfirms=Cfirms,params=params,args=args)
      Cfirmsagg<-Cfaggregates(Cfirmsagg=Cfirmsagg,Cfirms=Cfirms,params=params,args=args)
      Banks<-bankStocks1(Banks=Banks,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Banks<-Bprofit(Banks=Banks,Cfirmsagg=Cfirmsagg,Aux=Aux,params=params,args=args)
      Bdivreturns<-payDividendsB(Banks=Banks,Cfirmsagg=Cfirmsagg,Aux=Aux,params=params,args=args)
      Banks<-Bdivreturns$Banks
      Aux<-Bdivreturns$Aux
      Banks<-Bsave(Banks=Banks,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Kfirms<-Kfprofit(Kfirms=Kfirms,params=params,args=args)
      Kfirms<-payDividendsK(Kfirms=Kfirms,params=params,args=args)
      Kfirms<-Kfsave(Kfirms=Kfirms,params=params,args=args)
      Kfirms<-Kfwealth(Kfirms=Kfirms,params=params,args=args)
      Households<-Hidentities(Households=Households,Aux=Aux,Banks=Banks,Kfirms=Kfirms,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Banks<-bankStocks2(Banks=Banks,Households=Households,Cfirmsagg=Cfirmsagg,params=params,args=args)
      Aux<-SFCcheck(Aux=Aux,Households=Households,Cfirmsagg=Cfirmsagg,Kfirms=Kfirms,Banks=Banks,params=params,args=args)
    }
    if(r==1){
      Timer=(args$Time/args$nrun+10)
      Householdssel<-Households
      Kfirmssel<-Kfirms
      Cfirmsaggsel<-Cfirmsagg
      Bankssel<-Banks
      Cfirmssel<-Cfirms
      Cfirmsav<-as.data.frame(apply(Cfirms,c(1,2),mean))
      Auxsel<-Aux
      output<-as.data.frame(cbind(Householdssel,Kfirmssel,Bankssel,Cfirmsaggsel,Auxsel))
    }else{
      Householdssel<-Households[11:(args$Time/args$nrun+10),]
      Kfirmssel<-Kfirms[11:(args$Time/args$nrun+10),]
      Cfirmsaggsel<-Cfirmsagg[11:(args$Time/args$nrun+10),]
      Bankssel<-Banks[11:(args$Time/args$nrun+10),]
      Cfirmssel2<-Cfirms[11:(args$Time/args$nrun+10),,]
      Cfirmssel<-abind(Cfirmssel,Cfirmssel2,along=1)
      Cfirmsav2<-as.data.frame(apply(Cfirmssel2,c(1,2),mean))
      Auxsel<-Aux[11:(args$Time/args$nrun+10),]
      output2<-as.data.frame(cbind(Householdssel,Kfirmssel,Bankssel,Cfirmsaggsel,Auxsel))
      output<-rbind(output,output2)
      Cfirmsav<-rbind(Cfirmsav,Cfirmsav2)
    }
    }
  #DATA COLLECTION####
  for(l in 1:ncol(output)){
        if(is.na(output[1,l])==TRUE){
          output[1,l]<-output[2,l]
        }
      }
  for(l in 1:ncol(Cfirmsav)){
      if(is.na(Cfirmsav[1,l])==TRUE){
      Cfirmsav[1,l]<-Cfirmsav[2,l]
      }
      }
  Modelreturns<-list(output=output,Cfirmsav=Cfirmsav,Cfirms=Cfirmssel,CFNames=CFNames)
  filename<-paste("dataiota21.5","mc",seed,sep="")
  save(Modelreturns,file = paste(foldername,filename, ".Rdata", sep=''))
}



######Define functions here#####

######Accounting identities#####
#####Households####
Hidentities<-function(Households=stop("need to have households defined!"),Aux=stop("Need to have Aux defined!"),Banks=stop("need to have Banks defined!"),Kfirms=stop("need to have Kfirms defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Households[[args$t,"YD"]]=Cfirmsagg[[args$t,"W_c"]]+Kfirms[[args$t,"W_k"]]+Cfirmsagg[[args$t,"Div_fc"]]+Kfirms[[args$t,"Div_fk"]]+Banks[[args$t,"Div_b"]]+Banks[[args$t,"stimulus"]]
  Households[[args$t,"sav_h"]]=Cfirmsagg[[args$t,"W_c"]]+Kfirms[[args$t,"W_k"]]+Cfirmsagg[[args$t,"Div_fc"]]+Kfirms[[args$t,"Div_fk"]]+Banks[[args$t,"Div_b"]]-Cfirmsagg[[args$t,"C"]]+Banks[[args$t,"stimulus"]]
  Households[[args$t,"D_h"]]=Households[[(args$t-1),"D_h"]]+Households[[args$t,"sav_h"]]-Aux[[args$t,"recap"]]
  Households[[args$t,"V_h"]]=Households[[args$t,"D_h"]]+Banks[[args$t,"E_b"]]+Cfirmsagg[[args$t,"E_fc"]]+Kfirms[[args$t,"E_fk"]]
  Households[[args$t,"E_f"]]=Cfirmsagg[[args$t,"E_fc"]]+Kfirms[[args$t,"E_fk"]]
  Households[[args$t,"Div_f"]]=Cfirmsagg[[args$t,"Div_fc"]]+Kfirms[[args$t,"Div_fk"]]
  Households[[args$t,"W"]]=Cfirmsagg[[args$t,"W_c"]]+Kfirms[[args$t,"W_k"]]
  #print(Households[[args$t,"D_h"]])
  return(Households)
}

#####K-Firms####
Kfprofit<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[args$t,"Pr_fk"]]=Kfirms[[args$t,"I"]]-Kfirms[[args$t,"W_k"]]
  return(Kfirms)
}

Kfsave<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[args$t,"sav_fk"]]=Kfirms[[args$t,"I"]]-Kfirms[[args$t,"W_k"]]-Kfirms[[args$t,"Div_fk"]]
  return(Kfirms)
}

Kfwealth<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[args$t,"E_fk"]]=Kfirms[[(args$t-1),"E_fk"]]+Kfirms[[args$t,"sav_fk"]]
  Kfirms[[args$t,"V_fk"]]=-Kfirms[[args$t,"E_fk"]]
  if(Kfirms[[args$t,"E_fk"]]<0){
  #print("Warning: The capital firm has gone bankrupt!")
  }
  return(Kfirms)
}


######Banks####
Bsave<-function(Banks=stop("need to have banks defined!"),Cfirmsagg=stop("need to have Cfirms (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"sav_b"]]=Cfirmsagg[[args$t,"iL"]]-Banks[[args$t,"Div_b"]]-Banks[[args$t,"stimulus"]]
  return(Banks)
}

Bprofit<-function(Banks=stop("need to have banks defined!"),Cfirmsagg=stop("need to have Cfirms (aggregate) defined!"),Aux=stop("need to have Aux defined!"),params=params,args=args){
  Banks[[args$t,"Pr_b"]]=Cfirmsagg[[args$t,"iL"]]
  return(Banks)
}


#######C-firms####
#ABM#####
Cfsave<-function(Cfirms=stop("need to have Cfirms defined!"),params=params,args=args){
  Cfirms[args$t,"sav_fc",]=Cfirms[args$t,"C",]-Cfirms[args$t,"I",]-Cfirms[args$t,"W_c",]-Cfirms[args$t,"iL",]-Cfirms[args$t,"Div_fc",]
  return(Cfirms)
}

#####sectoral balances####
SFCcheck<-function(Households=stop("need to have households defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Kfirms=stop("need to have Kfirms defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have auxiliary array defined!"),params=params,args=args){
  Aux[[args$t,"SFCcheck1"]]=Households[[args$t,"sav_h"]]+Cfirmsagg[[args$t,"sav_fc"]]+Banks[[args$t,"sav_b"]]+Kfirms[[args$t,"sav_fk"]]
  Aux[[args$t,"SFCcheck2"]]=Households[[args$t,"V_h"]]+Cfirmsagg[[args$t,"V_fc"]]+Banks[[args$t,"V_b"]]+Kfirms[[args$t,"V_fk"]]-Cfirmsagg[[args$t,"K"]]
  return(Aux)
}

######
####Equations
####Households####
decideConsumption<-function(Households=stop("need to have households defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Households[[args$t,"c_d"]]=((params[[1,"alpha1"]]*Households[[args$t,"YD_e"]]+params[[1,"alpha2"]]*Households[[args$t,"V_he"]])/Cfirmsagg[[args$t,"CPI"]])
  return(Households)
}

buildExpectationHH<-function(Households=stop("need to have households defined!"),params=params,args=args){
  Households[[args$t,"YD_e"]]=Households[[(args$t-1),"YD_e"]]+params[[1,"psi"]]*(Households[[(args$t-1),"YD"]]-Households[[(args$t-1),"YD_e"]])
  Households[[args$t,"V_he"]]=Households[[(args$t-1),"V_he"]]+params[[1,"psi"]]*(Households[[(args$t-1),"V_h"]]-Households[[(args$t-1),"V_he"]])
  return(Households)
}

#####Firms#####
#####K-firms####

setKprice<-function(Kfirms=stop("need to have Kfirms defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Kfirms[[args$t,"p_k"]]=Cfirmsagg[[args$t,"CPI"]]
  return(Kfirms)
}

supplyCapital<-function(Kfirms=stop("need to have Kfirms defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Kfirms[[args$t,"i"]]=sum(Cfirms[args$t,"i",])
  Kfirms[[args$t,"N_dk"]]=Kfirms[[args$t,"i"]]
  Kfirms[[args$t,"W_k"]]=params[[1,"w"]]*Kfirms[[args$t,"N_dk"]]
  Kfirms[[args$t,"I"]]=Kfirms[[args$t,"i"]]*Kfirms[[args$t,"p_k"]]
  return(Kfirms)
}

payDividendsK<-function(Kfirms=stop("need to have Kfirms defined!"),params=params,args=args){
  Kfirms[[args$t,"Div_fk"]]=Kfirms[[args$t,"Pr_fk"]]
  return(Kfirms)
}

#####C-firms (agent-based)####

updateCapital1<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[args$t,"k",]=(1-params[[1,"delta"]])*Cfirms[(args$t-1),"k",]
  Cfirms[args$t,"K",]=(1-params[[1,"delta"]])*Cfirms[(args$t-1),"K",]
  return(Cfirms)
}



setCprice<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args,Timer=Timer){
  rand<-sample(1:args$nF,args$nF/4)
  for(i in 1:args$nF){
  if(Timer<5){
  Cfirms[[args$t,"omega_c",i]]=Cfirms[[(args$t-1),"omega_c",i]]
  Cfirms[[args$t,"p_c",i]]=(1+Cfirms[[args$t,"omega_c",i]])*params[[1,"w"]]
  }else{
  if(is.element(i,rand)){
  if(Cfirms[[(args$t-1),"bankrupt",i]]==1 || Cfirms[[(args$t-2),"bankrupt",i]]==1||Cfirms[[(args$t-3),"bankrupt",i]]==1||Cfirms[[(args$t-4),"bankrupt",i]]==1){
  Cfirms[[args$t,"omega_c",i]]=Cfirms[(args$t-1),"omega_c",i]
  Cfirms[[args$t,"p_c",i]]=Cfirms[(args$t-1),"p_c",i]
  }else{
  Cfirms[[args$t,"omega_c",i]]=params[1,"omega"]*(2/(1+exp(params[1,"epsilon"]*((mean(Cfirms[(args$t-4):(args$t-1),"share",i])/(1/args$nF))-1))))
  Cfirms[[args$t,"p_c",i]]=((1+Cfirms[[args$t,"omega_c",i]])*params[[1,"w"]])
  }}else{
  Cfirms[[args$t,"omega_c",i]]=Cfirms[[(args$t-1),"omega_c",i]]
  Cfirms[[args$t,"p_c",i]]=Cfirms[[(args$t-1),"p_c",i]]
  }}}
  return(Cfirms)
}

estimateYield<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args,Timer=Timer){
  Cfirmsagg[[args$t,"CF"]]=sum(Cfirms[args$t,"CF",])
  Cfirmsagg[[args$t,"K"]]=sum(Cfirms[args$t,"K",])
  Cfirmsagg[[args$t,"iL"]]=sum(Cfirms[args$t,"iL",])
  for(i in 1:args$nF){
  Cfirms[[args$t,"index",i]]=params[1,"rho"]*(Cfirms[args$t,"CF",i]-Cfirms[args$t,"iL",i])*4/Cfirms[args$t,"K",i]+(1-params[1,"rho"])*(Cfirmsagg[args$t,"CF"]-Cfirmsagg[args$t,"iL"])*4/Cfirmsagg[args$t,"K"]
  Cfirms[args$t,"sentiment",i]=1/(1+exp(params[1,"eta"]*(Cfirms[[args$t,"index",i]]-params[1,"pi"])))
  sprob=runif(1)
  if(Cfirms[args$t,"sentiment",i]>sprob){
  Cfirms[args$t,"attitude",i]=1
  }else{
  Cfirms[args$t,"attitude",i]=0
  }
  if(params[1,"expdyn"]==1){
  Cfirms[args$t,"yield",i]=((Cfirms[args$t,"c_d",i]*Cfirms[(args$t),"p_c",i]-params[[1,"w"]]*Cfirms[args$t,"c_d",i]))
  if(Cfirms[args$t,"attitude",i]==1){
  Cfirms[args$t,"yield",i]=Cfirms[args$t,"yield",i]*params[1,"opt"]
  }else{
  Cfirms[args$t,"yield",i]=Cfirms[args$t,"yield",i]*params[1,"pes"]
  }}
  else{
  if(Timer>2){
  Cfirms[args$t,"yield",i]=((Cfirms[args$t,"c_d",i]*Cfirms[(args$t),"p_c",i]-params[[1,"w"]]*Cfirms[args$t,"c_d",i]))*(1+(Cfirmsagg[(args$t-1),"Y"]-Cfirmsagg[(args$t-2),"Y"])/Cfirmsagg[(args$t-2),"Y"])
  }else{
  Cfirms[args$t,"yield",i]=((Cfirms[args$t,"c_d",i]*Cfirms[(args$t),"p_c",i]-params[[1,"w"]]*Cfirms[args$t,"c_d",i]))
  }
  }
  Cfirms[args$t,"Q",i]=(Cfirms[args$t,"yield",i])/(params[1,"r_d"]+params[1,"delta"])
  if(Timer>2){
  Cfirms[args$t,"mu1",i]=params[1,"mu1"]*(max(0.001,(2/(1+exp(-(((Cfirms[(args$t),"iL",i]+Cfirms[(args$t),"rep_L",i])/Cfirms[(args$t),"CF",i])*params[1,"beta1"]-params[1,"beta2"]*((Cfirms[(args$t),"CF",i]-Cfirms[(args$t),"iL",i])*4/Cfirms[(args$t-1),"K",i])))))))
  }else{
  Cfirms[args$t,"mu1",i]=params[1,"mu1"]
  }
  }
  return(list(Cfirms=Cfirms,Cfirmsagg=Cfirmsagg))
}


Invest<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Banks=stop("need to have Banks defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Kfirms=stop("need to have Kfirms defined!"),params=params,args=args,Timer=Timer,firminits=firminits){
  Cfirms[args$t,"p_dkint",]=pmax(0,(Cfirms[args$t,"Q",]-Cfirms[args$t,"mu1",]*(Cfirms[args$t,"int",])^2)/Cfirms[args$t,"k",])
  for(i in 1:args$nF){
  if(is.na(Cfirms[[args$t,"p_dkint",i]])){
  Modelreturns<-Cfirms
  filename<-paste("data","mc",seed,sep="")
  save(Modelreturns,file = paste("Sensitivitydata/",filename, ".Rdata", sep=''))
  break
  }
  if(Cfirms[[args$t,"p_dkint",i]]<=Kfirms[[args$t,"p_k"]]){
  if(Cfirms[[args$t,"Q",i]]-Cfirms[[args$t,"k",i]]*Kfirms[[args$t,"p_k"]]>0){
  Cfirms[[args$t,"i",i]]=max(0,min(Cfirms[[args$t,"int",i]],sqrt(Cfirms[[args$t,"Q",i]]-Cfirms[[args$t,"k",i]]*Kfirms[[args$t,"p_k"]])/(sqrt(Cfirms[args$t,"mu1",i]))))
  }else{
  Cfirms[[args$t,"i",i]]=0
  }
  Cfirms[[args$t,"i",i]]=Cfirms[[args$t,"i",i]]/Kfirms[[args$t,"p_k"]]
  Cfirms[[args$t,"L",i]]=Cfirms[[(args$t-1),"L",i]]
  Cfirms[[args$t,"r_L",i]]=max(Banks[[(args$t-1),"r_0"]],Banks[[(args$t-1),"r_0"]]+Banks[(args$t-1),"mu2"]*(Cfirms[[args$t,"iL",i]]+Cfirms[[args$t,"rep_L",i]])/(Cfirms[[args$t,"CF",i]]))
  if(is.na(Cfirms[args$t,"r_L",i])){
  Cfirms[[args$t,"r_L",i]]=Banks[[(args$t-1),"r_0"]]
  }
  Cfirms[[args$t,"I",i]]=Cfirms[[args$t,"i",i]]*Kfirms[[args$t,"p_k"]]
  Cfirms[[args$t,"D_f",i]]=Cfirms[[args$t,"D_f",i]]-Cfirms[[args$t,"I",i]]
  }else{
  fun<- function(x) Kfirms[[args$t,"p_k"]]+(1-Cfirms[[args$t,"int",i]]/x)*((Cfirms[[args$t,"CF",i]]*Banks[[(args$t-1),"r_0"]]+Banks[(args$t-1),"mu2"]*params[1,"theta"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+x-Cfirms[[args$t,"int",i]]))/(Cfirms[[args$t,"CF",i]]-Banks[(args$t-1),"mu2"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+x-Cfirms[[args$t,"int",i]])))/params[1,"theta"]-(Cfirms[[args$t,"Q",i]]-Cfirms[args$t,"mu1",i]*x^2)/Cfirms[[args$t,"k",i]]
  root<-uniroot.all(fun,c(0,sum(firminits[,"k"])/25),n=sum(firminits[,"k"])/25)
  if(length(root)<1){
  root=Cfirms[[args$t,"int",i]]
  }else{
  if(length(root)>1){
  #print(c("PROBLEM! MORE THAN 1 ROOT FOUND!"))
  root<-min(root)
  }
  if(Kfirms[[args$t,"p_k"]]+(1-Cfirms[[args$t,"int",i]]/(root-0.025))*((Cfirms[[args$t,"CF",i]]*Banks[[(args$t-1),"r_0"]]+Banks[(args$t-1),"mu2"]*params[1,"theta"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root-0.025)-Cfirms[[args$t,"int",i]]))/(Cfirms[[args$t,"CF",i]]-Banks[(args$t-1),"mu2"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root-0.025)-Cfirms[[args$t,"int",i]])))/params[1,"theta"]>Kfirms[[args$t,"p_k"]]+(1-Cfirms[[args$t,"int",i]]/(root+0.025))*((Cfirms[[args$t,"CF",i]]*Banks[[(args$t-1),"r_0"]]+Banks[(args$t-1),"mu2"]*params[1,"theta"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root+0.025)-Cfirms[[args$t,"int",i]]))/(Cfirms[[args$t,"CF",i]]-Banks[(args$t-1),"mu2"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root+0.025)-Cfirms[[args$t,"int",i]])))/params[1,"theta"]){
  #if(Kfirms[[args$t,"p_k"]]+(1-Cfirms[[args$t,"int",i]]/(root-1))*((Cfirms[[args$t,"CF",i]]*Banks[[(args$t-1),"r_0"]]+Banks[(args$t-1),"mu2"]*params[1,"theta"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root-1)-Cfirms[[args$t,"int",i]]))/(Cfirms[[args$t,"CF",i]]-Banks[(args$t-1),"mu2"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root-1)-Cfirms[[args$t,"int",i]])))/params[1,"theta"]>Kfirms[[args$t,"p_k"]]+(1-Cfirms[[args$t,"int",i]]/(root+1))*((Cfirms[[args$t,"CF",i]]*Banks[[(args$t-1),"r_0"]]+Banks[(args$t-1),"mu2"]*params[1,"theta"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root+1)-Cfirms[[args$t,"int",i]]))/(Cfirms[[args$t,"CF",i]]-Banks[(args$t-1),"mu2"]*(Cfirms[[(args$t-1),"L",i]]-params[1,"theta"]*Cfirms[[(args$t-1),"L",i]]+(root+1)-Cfirms[[args$t,"int",i]])))/params[1,"theta"]){
  root<-0
  }else{
  if((Cfirms[[args$t,"Q",i]]-Cfirms[args$t,"mu1",i]*root^2)/Cfirms[[args$t,"k",i]]<0)
  root<-0
  }}
  Cfirms[[args$t,"root",i]]<-root
  Cfirms[[args$t,"i",i]]=max(0,root/Kfirms[[args$t,"p_k"]])
  Cfirms[[args$t,"I",i]]=Cfirms[[args$t,"i",i]]*Kfirms[[args$t,"p_k"]]
  Cfirms[[args$t,"L",i]]=Cfirms[[(args$t-1),"L",i]]+max(0,Cfirms[[args$t,"I",i]]-Cfirms[[args$t,"int",i]])
  Cfirms[[args$t,"D_f",i]]=Cfirms[[args$t,"D_f",i]]+Cfirms[[args$t,"L",i]]-Cfirms[[(args$t-1),"L",i]]-Cfirms[[args$t,"I",i]]
  Cfirms[[args$t,"r_L",i]]=max(Banks[[(args$t-1),"r_0"]],(Cfirms[[args$t,"CF",i]]*Banks[[(args$t-1),"r_0"]]+Banks[(args$t-1),"mu2"]*Cfirms[[args$t,"L",i]]*params[1,"theta"]-Banks[(args$t-1),"mu2"]*Cfirms[[args$t,"rep_L",i]]*params[1,"theta"])/(Cfirms[[args$t,"CF",i]]-Cfirms[[args$t,"L",i]]*Banks[(args$t-1),"mu2"]+Banks[(args$t-1),"mu2"]*Cfirms[[args$t,"rep_L",i]]))
  #Cfirms[[args$t,"r_L",i]]=Cfirms[[(args$t-1),"r_L",i]]*(Cfirms[[(args$t-1),"L",i]]-Cfirms[[args$t,"rep_L",i]])/(Cfirms[[args$t,"L",i]]-Cfirms[[args$t,"rep_L",i]])+Cfirms[[args$t,"r_L",i]]*(Cfirms[[args$t,"I",i]]-Cfirms[[args$t,"int",i]])/(Cfirms[[args$t,"L",i]]-Cfirms[[args$t,"rep_L",i]])
  if(is.na(Cfirms[args$t,"r_L",i])){
  Cfirms[[args$t,"r_L",i]]=Banks[[(args$t-1),"r_0"]] 
  }
  }
  }
  return(Cfirms)
}


distributeConsumption<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Households=stop("Need to have Households defined!"),params=params,args=args){
  for(i in 1:args$nF){
    Cfirms[[args$t,"p_rel",i]]=(Cfirms[[args$t,"p_c",i]]/mean(Cfirms[args$t,"p_c",]))^params[[1,"iota1"]]
    Cfirms[[args$t,"k_rel",i]]=(Cfirms[[(args$t-1),"k",i]]/mean(Cfirms[(args$t-1),"k",]))^params[[1,"iota2"]]
  }
  Cfirms[args$t,"p_rel",]=Cfirms[args$t,"p_rel",]/sum(Cfirms[args$t,"p_rel",])
  Cfirms[args$t,"k_rel",]=Cfirms[args$t,"k_rel",]/sum(Cfirms[args$t,"k_rel",])
  for(i in 1:args$nF){
  Cfirms[[args$t,"share",i]]=params[[1,"lambda1"]]*Cfirms[(args$t-1),"share",i]+(1-params[[1,"lambda1"]])*(Cfirms[[args$t,"p_rel",i]]+Cfirms[[args$t,"k_rel",i]])/2  }
  Cfirms[args$t,"share",]=Cfirms[args$t,"share",]/sum(Cfirms[args$t,"share",])
  for(i in 1:args$nF){
  Cfirms[args$t,"share_shock",i]=params[1,"lambda2"]*Cfirms[(args$t-1),"share_shock",i]+(1-params[1,"lambda2"])*max(0.01,rnorm(1,1,Cfirms[(args$t-1),"share",i]*params[1,"sigma"]))
  Cfirms[args$t,"share",i]=Cfirms[args$t,"share",i]*Cfirms[args$t,"share_shock",i]
  }
  #Cfirms[args$t,"share",]=Cfirms[args$t,"share",]/sum(Cfirms[args$t,"share",])
  return(Cfirms)
}

cProduction<-function(Households=stop("need to have households defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[args$t,"c_d",]=Cfirms[args$t,"share",]*Households[[args$t,"c_d"]]
  for(i in 1:args$nF){
  Cfirms[[args$t,"c",i]]=min(Cfirms[[args$t,"c_d",i]],Cfirms[[(args$t-1),"k",i]]*params[[1,"kappa"]])
  }
  Cfirms[args$t,"N_dc",]=Cfirms[args$t,"c",]
  Cfirms[args$t,"W_c",]=params[[1,"w"]]*Cfirms[args$t,"N_dc",]
  Cfirms[args$t,"u",]=Cfirms[args$t,"c",]/(Cfirms[(args$t-1),"k",]*params[[1,"kappa"]])
  Cfirms[args$t,"C",]=Cfirms[args$t,"p_c",]*Cfirms[args$t,"c",]
  Cfirms[args$t,"D_f",]=Cfirms[(args$t-1),"D_f",]+Cfirms[args$t,"C",]-Cfirms[args$t,"W_c",]
  Cfirms[args$t,"CF",]=pmax(0,Cfirms[args$t,"C",]-Cfirms[args$t,"W_c",])
  Cfirms[args$t,"Div_fc",]=pmax(0,Cfirms[(args$t-1),"gamma",]*Cfirms[args$t,"CF",])
  Cfirms[args$t,"int",]=pmax(0,Cfirms[args$t,"CF",]-Cfirms[args$t,"Div_fc",]-Cfirms[args$t,"iL",]-Cfirms[args$t,"rep_L",])
  Cfirms[args$t,"share",]=Cfirms[args$t,"c",]/sum(Cfirms[args$t,"c",])
  return(Cfirms)
}

updateCapital2<-function(Kfirms=stop("need to have Kfirms defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[args$t,"I",]=Kfirms[[args$t,"p_k"]]*Cfirms[args$t,"i",]
  Cfirms[args$t,"k",]=Cfirms[args$t,"k",]+Cfirms[args$t,"i",]
  Cfirms[args$t,"K",]=Cfirms[args$t,"K",]+Cfirms[args$t,"I",]
  Cfirms[args$t,"deltaK",]=Cfirms[args$t,"K",]-Cfirms[(args$t-1),"K",]
  return(Cfirms)
}

loanPayments<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[args$t,"iL",]=Cfirms[(args$t-1),"r_L",]*Cfirms[(args$t-1),"L",]
  Cfirms[args$t,"rep_L",]=params[[1,"theta"]]*Cfirms[(args$t-1),"L",]
  return(Cfirms)
}

profitCfirms<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirms[args$t,"Pr_fc",]=Cfirms[args$t,"C",]-Cfirms[args$t,"I",]+Cfirms[args$t,"deltaK",]-Cfirms[args$t,"W_c",]-Cfirms[args$t,"iL",]
  Cfirms[args$t,"gamma",]=pmin(1,pmax(0,params[1,"gamma"]*2/(1+exp(params[1,"tau"]*((Cfirms[args$t,"CF",]-Cfirms[args$t,"iL",])*4/Cfirms[args$t,"K",]-params[1,"pi"])))))
  return(Cfirms)
}

bankruptcyDividends<-function(Cfirms=stop("need to have Cfirms (ABM) defined!"),Kfirms=stop("need to have K-Firms defined!"),Banks=stop("need to have Banks defined!"),Aux=stop("need to have Aux Array defined!"),params=params,args=args,Timer=Timer,firminits=firminits){
  Aux[[args$t,"counter"]]=Aux[[(args$t-1),"counter"]]
  Aux[[args$t,"defaults"]]=0
  Aux[[args$t,"recap"]]=0
  Aux[args$t,"def_liqu"]=0
  Aux[args$t,"def_equ"]=0
  Cfirms[args$t,"bankrupt",]<-0
  Cfirms[args$t,"E_fc",]=Cfirms[(args$t-1),"E_fc",]+Cfirms[args$t,"Pr_fc",]
  meanD=mean(firminits[,"D_f"])
  #print(mean(Cfirms[(args$t-1),"D_f",]))
  meanlev=sum(Cfirms[args$t,"K",])/sum(Cfirms[args$t,"E_fc",])
  #print(meanlev)
  Kfirms[[args$t,"i_p"]]=Kfirms[[args$t,"i"]]
  Kfirms[[args$t,"I_p"]]=Kfirms[[args$t,"I"]]
  for(i in 1:args$nF){
  if(Cfirms[[(args$t),"D_f",i]]<(Cfirms[[(args$t),"rep_L",i]]+Cfirms[[(args$t),"iL",i]])||Cfirms[[args$t,"E_fc",i]]<0){
  if(Cfirms[[(args$t),"D_f",i]]<(Cfirms[[(args$t),"rep_L",i]]+Cfirms[[(args$t),"iL",i]])){
  Aux[args$t,"def_liqu"]=Aux[args$t,"def_liqu"]+1
  }
  if(Cfirms[[args$t,"E_fc",i]]<0){
  Aux[args$t,"def_equ"]=Aux[args$t,"def_equ"]+1
  }
  Cfirms[[args$t,"bankrupt",i]]=1
  #print(c("default of firm ",i))
  Aux[[args$t,"counter"]]=Aux[[args$t,"counter"]]+1
  Aux[[args$t,"defaults"]]=Aux[[args$t,"defaults"]]+Cfirms[[args$t,"L",i]]-Cfirms[[args$t,"D_f",i]]-params[1,"chi"]*Cfirms[[args$t,"K",i]]
  Cfirms[[args$t,"D_f",i]]=meanD
  Aux[[args$t,"recap"]]=Aux[[args$t,"recap"]]+params[1,"chi"]*Cfirms[[args$t,"K",i]]+meanD
  Cfirms[[args$t,"K",i]]=params[1,"chi"]*Cfirms[[args$t,"K",i]]
  Cfirms[[args$t,"k",i]]=params[1,"chi"]*Cfirms[[args$t,"k",i]]
  Cfirms[[args$t,"L",i]]=0
  Cfirms[[args$t,"E_fc",i]]=Cfirms[[args$t,"K",i]]-Cfirms[[args$t,"L",i]]+Cfirms[[args$t,"D_f",i]]
  Cfirms[[args$t,"iL",i]]=0
  Cfirms[[args$t,"rep_L",i]]=0
  krel=(Cfirms[(args$t),"k",]/mean(Cfirms[(args$t),"k",]))^params[[1,"iota2"]]
  krel[i]=krel[i]/sum(krel)
  Cfirms[[args$t,"share",i]]=krel[i]
  Cfirms[[args$t,"omega_c",i]]=(params[1,"omega"])*(2/(1+exp(-3*((mean(Cfirms[(args$t),"share",i])/(1/args$nF))-1))))
  Cfirms[[args$t,"p_c",i]]=(1+Cfirms[[args$t,"omega_c",i]])*params[1,"w"]
  Cfirms[[args$t,"u",i]]=mean(Cfirms[args$t,"u",])
  Cfirms[[args$t,"share_shock",i]]=1
  Cfirms[[args$t,"Div_fc",i]]=0
  Cfirms[[args$t,"r_L",i]]=Banks[[(args$t-1),"r_0"]]
  }else{
  if(Timer>6 && mean(Cfirms[(args$t-5):args$t,"share",i])<0.0001 && mean(Cfirms[(args$t-5):args$t,"bankrupt",i])==0){
  Cfirms[[args$t,"bankrupt",i]]=1
  #print(c("default of firm ",i,"(reset)"))
  Aux[[args$t,"counter"]]=Aux[[args$t,"counter"]]+1
  Aux[[args$t,"defaults"]]=Aux[[args$t,"defaults"]]+Cfirms[[args$t,"L",i]]-Cfirms[[args$t,"D_f",i]]
  Cfirms[[args$t,"D_f",i]]=meanD
  Aux[[args$t,"recap"]]=Aux[[args$t,"recap"]]+mean(Cfirms[args$t,"K",])+meanD
  Cfirms[[args$t,"K",i]]=mean(Cfirms[args$t,"K",])
  Cfirms[[args$t,"k",i]]=mean(Cfirms[args$t,"k",])
  Cfirms[[args$t,"i",i]]=Cfirms[[args$t,"i",i]]+mean(Cfirms[args$t,"k",])
  Cfirms[[args$t,"I",i]]=Cfirms[[args$t,"i",i]]*Kfirms[[args$t,"p_k"]]
  Kfirms[[args$t,"i"]]=Kfirms[[args$t,"i"]]+mean(Cfirms[args$t,"k",])
  Kfirms[[args$t,"N_dk"]]=Kfirms[[args$t,"i"]]
  Kfirms[[args$t,"W_k"]]=params[[1,"w"]]*Kfirms[[args$t,"N_dk"]]
  Kfirms[[args$t,"I"]]=Kfirms[[args$t,"i"]]*Kfirms[[args$t,"p_k"]]
  Cfirms[[args$t,"L",i]]=0
  Cfirms[[args$t,"E_fc",i]]=Cfirms[[args$t,"K",i]]-Cfirms[[args$t,"L",i]]+Cfirms[[args$t,"D_f",i]]
  Cfirms[[args$t,"iL",i]]=0
  Cfirms[[args$t,"rep_L",i]]=0
  krel=(Cfirms[(args$t),"k",]/mean(Cfirms[(args$t),"k",]))^params[[1,"iota2"]]
  krel[i]=krel[i]/sum(krel)
  Cfirms[[args$t,"share",i]]=krel[i]
  Cfirms[[args$t,"omega_c",i]]=(params[1,"omega"])*(2/(1+exp(-3*((mean(Cfirms[(args$t),"share",i])/(1/args$nF))-1))))
  Cfirms[[args$t,"p_c",i]]=(1+Cfirms[[args$t,"omega_c",i]])*params[1,"w"]
  Cfirms[[args$t,"u",i]]=mean(Cfirms[args$t,"u",])
  Cfirms[[args$t,"share_shock",i]]=1
  Cfirms[[args$t,"Div_fc",i]]=0
  Cfirms[[args$t,"r_L",i]]=Banks[[(args$t-1),"r_0"]]
  }
  else{
  #pay dividends
  Cfirms[[args$t,"D_f",i]]=Cfirms[[args$t,"D_f",i]]-Cfirms[[args$t,"iL",i]]-Cfirms[[args$t,"rep_L",i]]
  Cfirms[[args$t,"L",i]]=Cfirms[[args$t,"L",i]]-Cfirms[[args$t,"rep_L",i]]
  if(Cfirms[[args$t,"Pr_fc",i]]>0){
  Cfirms[[args$t,"Div_fc",i]]=min(Cfirms[[args$t,"D_f",i]],Cfirms[[args$t,"Div_fc",i]]+max(0,Cfirms[[args$t,"int",i]]-Cfirms[[args$t,"I",i]]))
  Cfirms[[args$t,"D_f",i]]=Cfirms[[args$t,"D_f",i]]-Cfirms[[args$t,"Div_fc",i]] 
  Cfirms[[args$t,"E_fc",i]]=Cfirms[[args$t,"E_fc",i]]-Cfirms[[args$t,"Div_fc",i]]
  }else{
  Cfirms[[args$t,"Div_fc",i]]=0
  Cfirms[[args$t,"D_f",i]]=Cfirms[[args$t,"D_f",i]]-Cfirms[[args$t,"Div_fc",i]]
  Cfirms[[args$t,"E_fc",i]]=Cfirms[[args$t,"E_fc",i]]-Cfirms[[args$t,"Div_fc",i]]
  }
  }}}
  Cfirms[args$t,"V_fc",]=Cfirms[args$t,"K",]+Cfirms[args$t,"D_f",]-Cfirms[args$t,"L",]-Cfirms[args$t,"E_fc",]
  return(list(Aux=Aux,Cfirms=Cfirms,Kfirms=Kfirms))
}

calculateCPI<-function(Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirmsagg[[args$t,"CPI"]]=sum(Cfirms[args$t,"p_c",]*(Cfirms[(args$t-1),"C",]/sum(Cfirms[(args$t-1),"C",])))
  return(Cfirmsagg)
}

Cfaggregates<-function(Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Cfirms=stop("need to have Cfirms (ABM) defined!"),params=params,args=args){
  Cfirmsagg[[args$t,"c"]]=sum(Cfirms[args$t,"c",])
  Cfirmsagg[[args$t,"C"]]=sum(Cfirms[args$t,"C",])
  Cfirmsagg[[args$t,"k"]]=sum(Cfirms[args$t,"k",])
  Cfirmsagg[[args$t,"K"]]=sum(Cfirms[args$t,"K",])
  Cfirmsagg[[args$t,"W_c"]]=sum(Cfirms[args$t,"W_c",])
  Cfirmsagg[[args$t,"Div_fc"]]=sum(Cfirms[args$t,"Div_fc",])
  Cfirmsagg[[args$t,"iL"]]=sum(Cfirms[args$t,"iL",])
  Cfirmsagg[[args$t,"rep_L"]]=sum(Cfirms[args$t,"rep_L",])
  Cfirmsagg[[args$t,"L"]]=sum(Cfirms[args$t,"L",])
  Cfirmsagg[[args$t,"D_f"]]=sum(Cfirms[args$t,"D_f",])
  Cfirmsagg[[args$t,"N_dc"]]=sum(Cfirms[args$t,"N_dc",])
  Cfirmsagg[[args$t,"E_fc"]]=sum(Cfirms[args$t,"E_fc",])
  Cfirmsagg[[args$t,"V_fc"]]=sum(Cfirms[args$t,"V_fc",])
  Cfirmsagg[[args$t,"K"]]=sum(Cfirms[args$t,"K",])
  Cfirmsagg[[args$t,"sav_fc"]]=sum(Cfirms[args$t,"sav_fc",])
  Cfirmsagg[[args$t,"Pr_fc"]]=sum(Cfirms[args$t,"Pr_fc",])
  Cfirmsagg[[args$t,"Div_fc"]]=sum(Cfirms[args$t,"Div_fc",])
  Cfirmsagg[[args$t,"I"]]=sum(Cfirms[args$t,"I",])
  Cfirmsagg[[args$t,"i"]]=sum(Cfirms[args$t,"i",])
  Cfirmsagg[[args$t,"p_c"]]=mean(Cfirms[args$t,"p_c",])
  Cfirmsagg[[args$t,"bankrupt"]]=sum(Cfirms[args$t,"bankrupt",])
  Cfirmsagg[[args$t,"Y"]]=Cfirmsagg[[args$t,"C"]]+Cfirmsagg[[args$t,"I"]]
  Cfirmsagg[[args$t,"y"]]=Cfirmsagg[[args$t,"c"]]+Cfirmsagg[[args$t,"i"]]
  Cfirmsagg[[args$t,"r_L"]]=sum(Cfirms[args$t,"r_L",]*(Cfirms[(args$t),"L",]/sum(Cfirms[(args$t),"L",])))
  Cfirmsagg[[args$t,"p"]]=sum(Cfirms[args$t,"p_c",]*(Cfirms[(args$t),"C",]/sum(Cfirms[(args$t),"C",])))
  Cfirmsagg[[args$t,"u"]]=sum(Cfirms[args$t,"u",]*(Cfirms[(args$t),"c",]/sum(Cfirms[(args$t),"c",])))
  return(Cfirmsagg)
}

###BANKS#####
bankStocks1<-function(Banks=stop("need to have Banks defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Banks[[args$t,"D_f"]]=Cfirmsagg[[args$t,"D_f"]]
  Banks[[args$t,"L"]]=Cfirmsagg[[args$t,"L"]]
  return(Banks)
}

bankStocks2<-function(Banks=stop("need to have Banks defined!"),Households=stop("Need to have Households defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),params=params,args=args){
  Banks[[args$t,"D_h"]]=Households[[args$t,"D_h"]]
  Banks[[args$t,"D"]]=Cfirmsagg[[args$t,"D_f"]]+Households[[args$t,"D_h"]]
  Banks[[args$t,"V_b"]]=Banks[[args$t,"L"]]-Banks[[args$t,"D"]]-Banks[[args$t,"E_b"]]
  return(Banks)
}


payDividendsB<-function(Banks=stop("need to have Banks defined!"),Cfirmsagg=stop("need to have Cfirms (agg) defined!"),Aux=stop("Need to have Aux defined!"),params=params,args=args){
  if(params[1,"gov"]==0){
  Banks[[args$t,"stimulus"]]=0
  }else{
  Banks[[args$t,"stimulus"]]=params[1,"phi_g"]*(params[1,"u_n"]-Cfirmsagg[args$t,"u"])
  }
  if(params[1,"bank"]==0){
  Banks[[args$t,"r_0"]]=params[1,"r_0"]
  }else{
  Banks[[args$t,"r_0"]]=max(0,params[1,"r_0"]+params[1,"phi_b"]*(Cfirmsagg[args$t,"u"]-params[1,"u_n"]))
  }
  Banks[[args$t,"E_b"]]=Banks[[(args$t-1),"E_b"]]+Banks[[args$t,"Pr_b"]]-Aux[[args$t,"defaults"]]-Banks[[args$t,"stimulus"]]
  Banks[[args$t,"mu2"]]=params[1,"mu2"]
  Banks[[args$t,"Div_b"]]=max(0,Banks[[args$t,"Pr_b"]]-Aux[[args$t,"defaults"]])
  Banks[[args$t,"E_b"]]=Banks[[args$t,"E_b"]]-Banks[[args$t,"Div_b"]]
  Banks[[args$t,"CAR"]]=Banks[[args$t,"E_b"]]/(Banks[[args$t,"L"]])
  return(list(Banks=Banks,Aux=Aux))
}