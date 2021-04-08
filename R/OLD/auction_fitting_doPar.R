library(foreach)
library(doParallel)
library(foreach)
install.packages('doParallel')

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

replications=100#nrow(feasible_space)
nrow(feasible_space)
results=data.frame()
min_n=rep(100,300)

results<-foreach (j =seq_len(replications), .combine=rbind, .packages=c("dplyr",'magrittr','MASS','waddR',.verbose=TRUE)) %dopar%  {
  #print(j)
  #print(round((j/replications),3))
  sdparam=feasible_space$possible_sd[j]
  errorGov=feasible_space$possible_error[j]
  minprofit=feasible_space$possible_minprofit[j]
  
  #1. Create monte carlo distribution of minimum values
  for (i in seq_len(300)) {
    min_j=rep(0,200)
    for (j in seq_len(1000)) {
      min_j[j]=min(rnorm(n = i,mean = 1,sd = sdparam)%>%as.vector())
      #min_j[j]=min(rt(n = i,df=df)%>%as.vector())
      
      #min_j[j]=min(rlogis(n = 1000, location = 1, scale = 0.001))
      #min_j[j]=min(rlnorm(n = i,meanlog = 0,sdlog = 0.25)%>%as.vector())
      #min_j[j]=min(runif(n = i,min=0.6,max=1.5)%>%as.vector())
    }
    min_n[i]=mean(min_j)
    #vec_min=rt(n = 4,df = 4,),mean = 1,sd = 0.05)%>%as.vector()  
    #min_n[i]=min(vec_min)
  }
  min_n[1]=2#Immportante que sea dos
  
  # 2. Simulate Bidding Distributions
  #final.bids=createBidSim(simulated.2 = edgelist.period1,sdparam = sdparam,errorGov = errorGov)
  final.bids=createBidSim_Optimal(simulated.2 = edgelist.period1,sdparam = sdparam,errorGov = errorGov,minprofit =minprofit )
  
  #Calculate Transport Costs For bidding participants and bids
  if(nrow(final.bids)>0){
    hist.sim.bids=final.bids#%>%>%mutate(weight=1/nrow(final.bids))#%>%group_by(bid)%>%count(name = count)%>% uncount(count)
    hist.period1.bids=df.period1%>%dplyr::select(MCA_MPO)#%>%group_by(bid)%>%count()%>% uncount(count)
    average.participants.sim=final.bids%>%group_by(ID)%>%count()%>%ungroup()%>%summarise(averageParticipants=mean(n))
    
    m1=hist.sim.bids$bid
    m2=hist.period1.bids$MCA_MPO
    
    dist=wasserstein_metric(x=m1, y=m2,p = 2)
    results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,minprofit=minprofit,dist.wass=dist,averageParticipants=average.participants.sim)
    #results=rbind(results,results.iter)
  }
  if(nrow(final.bids)==0){
    results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,minprofit=minprofit,dist.wass=NA,averageParticipants=NA)
    #results=rbind(results,results.iter)
  }
  results.iter
  
  }

results%>%arrange(dist.wass)
