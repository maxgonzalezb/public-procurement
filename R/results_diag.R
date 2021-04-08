
bids.sim.diag=data.frame()
for (i in seq_len(20)) {
print(i)
bids.sim.diag.iter=createBidSim_Optimal(simulated.2 = edgelist.period1,errorGov =0.01, sdparam=0.11)%>%mutate(ID=paste0(ID,i))
bids.sim.diag.iter=createBidSim(simulated.2 = simulated.2,errorGov =0.15, sdparam=0.11)%>%mutate(ID=paste0(ID,i))
bids.sim.diag=rbind(bids.sim.diag.iter,bids.sim.diag)
}

createBidSim<-function(simulated.2,sdparam,errorGov){
  
  vert.top.sim2=simulated.2%>%group_by(v.top)%>%count()
  vert.bottom.sim2=simulated.2%>%group_by(v.bottom)%>%count()
  min_n=rep(0,300)
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
  bids.sim=data.frame()
  for (v in seq_len(nrow(vert.top.sim2))) {
    
    edges.gov=simulated.2%>%filter(v.top==v)
    connected.firms=vert.bottom.sim2%>%filter(v.bottom%in%edges.gov$v.bottom)  
    
    totfirms=nrow(connected.firms)
    #commoncomp=rnorm(n = 1,mean = 0,sd = 0.1)
    commoncomp=runif(n = 1,min = -errorGov,max = errorGov)+rbinom(n = 1,prob = 0.1,size = 1)*runif(n=1,-0.8,0.8)
    #commoncomp=rt(n = 1,df=10)+1
    valuations=rnorm(n = totfirms,mean = 1,sd = sdparam)%>%as.vector()
    #valuations=rt(n = totfirms,df=df)%>%as.vector()+1
    #valuations=rlnorm(n = totfirms,mean = 0,sdlog = 0.25)%>%as.vector()
    #valuations=rlogis(n = totfirms, location = 1, scale = scaleparam)
    #valuations=runif(n = totfirms,min = 0.6,max=1.5)%>%as.vector()
    #connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse(valuation<(1/log(1+nrow(connected.firms))),yes = 1,no = 0))
    connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse((commoncomp+valuation)<=commoncomp+min_n[1+max(0,ceiling((log(totfirms)-0*rbinom(n=1,size = 1,prob = 0.5))))],yes = 1,no = 0))
    submitted.bids.unit=connected.firms.vals%>%filter(binbid==1)%>%as.data.frame()%>%mutate(ID=v,bid=valuation+commoncomp)
    bids.sim=rbind(submitted.bids.unit,bids.sim)
    
  }
  
  #3. Select which ones will bid
  final.bids=data.frame()
  for(firm in seq_len(max(bids.sim$v.bottom))) {
    firm.possible=bids.sim%>%filter(v.bottom==firm&binbid==1)  
    firm.truly=  firm.possible%>%arrange(valuation)%>%slice(1)
    final.bids=rbind(firm.truly,final.bids)
  
  }
  
  return(final.bids)
}

createBidSim_NetworkEffects<-function(simulated.2,sdparam,errorGov){
  
  vert.top.sim2=simulated.2%>%group_by(v.top)%>%count()
  vert.bottom.sim2=simulated.2%>%group_by(v.bottom)%>%count()
  min_n=rep(0,300)
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
  bids.sim=data.frame()
  for (v in seq_len(nrow(vert.top.sim2))) {
    
    edges.gov=simulated.2%>%filter(v.top==v)
    connected.firms=vert.bottom.sim2%>%filter(v.bottom%in%edges.gov$v.bottom)  
    
    totfirms=nrow(connected.firms)
    #Adjust the expected number
    totfirms.adjusted=ceiling(sum(1/(connected.firms$n)))
    
    #commoncomp=rnorm(n = 1,mean = 0,sd = 0.1)
    commoncomp=runif(n = 1,min = -errorGov,max = errorGov)+rbinom(n = 1,prob = 0.1,size = 1)*runif(n=1,-0.8,0.8)
    #commoncomp=rt(n = 1,df=10)+1
    valuations=rnorm(n = totfirms,mean = 1,sd = sdparam)%>%as.vector()
    #valuations=rt(n = totfirms,df=df)%>%as.vector()+1
    #valuations=rlnorm(n = totfirms,mean = 0,sdlog = 0.25)%>%as.vector()
    #valuations=rlogis(n = totfirms, location = 1, scale = scaleparam)
    #valuations=runif(n = totfirms,min = 0.6,max=1.5)%>%as.vector()
    #connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse(valuation<(1/log(1+nrow(connected.firms))),yes = 1,no = 0))
    connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse((commoncomp+valuation)<=commoncomp+min_n[max(0,(totfirms.adjusted))],yes = 1,no = 0))
    submitted.bids.unit=connected.firms.vals%>%filter(binbid==1)%>%as.data.frame()%>%mutate(ID=v,bid=valuation+commoncomp)
    bids.sim=rbind(submitted.bids.unit,bids.sim)
    
  }
  
  #3. Select which ones will bid
  final.bids=data.frame()
  for(firm in seq_len(max(bids.sim$v.bottom))) {
    firm.possible=bids.sim%>%filter(v.bottom==firm&binbid==1)  
    firm.truly=  firm.possible%>%arrange(valuation)%>%slice(1)
    final.bids=rbind(firm.truly,final.bids)
    
  }
  
  return(final.bids)
}

createBidSim_Optimal<-function(simulated.2,sdparam,errorGov,minprofit){
  
  vert.top.sim2=simulated.2%>%group_by(v.top)%>%count()
  vert.bottom.sim2=simulated.2%>%group_by(v.bottom)%>%count()
  
  if(FALSE){
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
  }
  
  # 2. Simulate Bidding Distributions
  bids.sim=data.frame()
  for (v in seq_len(nrow(vert.top.sim2))) {
    
    edges.gov=simulated.2%>%filter(v.top==v)
    connected.firms=vert.bottom.sim2%>%filter(v.bottom%in%edges.gov$v.bottom)  
    #connected.firms=connected.firms%>%sample_n(size = (nrow(connected.firms))) 
    totfirms=nrow(connected.firms)
    commoncomp=runif(n = 1,min = -errorGov,max = errorGov)+rbinom(n = 1,prob = 0.1,size = 1)*runif(n=1,-0.8,0.8)
    valuations=rnorm(n = totfirms,mean = 1,sd = sdparam)%>%as.vector()
    
    ##Create optimal payoffs
    connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=NA,bid=NA,profit=NA)%>%mutate(valuation=min(valuation,2))
    for (firm in seq_len(nrow(connected.firms.vals))) {
      possiblebids=seq(connected.firms.vals$valuation[firm],2.02,by=0.02)
      possiblevaluation=rep(connected.firms.vals$valuation[firm],length(possiblebids))           
      profits=(possiblebids-possiblevaluation)*(1-pnorm(possiblebids, 1, sd=sdparam)**(1+ceiling(log(totfirms))) )
      expected_max=max(profits)
      max_bid=possiblebids[which.max(profits)]
      connected.firms.vals$bid[firm]=max_bid
      connected.firms.vals$binbid[firm]=as.numeric(expected_max>minprofit)
      connected.firms.vals$profit[firm]=as.numeric(expected_max)
      }
    
    #connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse(valuation<(1/log(1+nrow(connected.firms))),yes = 1,no = 0))
    #connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse((commoncomp+valuation)<=commoncomp+min_n[1+max(0,ceiling((log(totfirms)-0*rbinom(n=1,size = 1,prob = 0.5))))],yes = 1,no = 0))
    
    submitted.bids.unit=connected.firms.vals%>%filter(binbid==1)%>%as.data.frame()%>%mutate(ID=v,bid=valuation+commoncomp)
    
    if(nrow(submitted.bids.unit)>0){
    bids.sim=rbind(submitted.bids.unit,bids.sim)
    }
  }
  
  #3. Select which ones will bid
  final.bids=data.frame()
  if(length(bids.sim$v.bottom)>0){
  for(firm in seq_len(max(bids.sim$v.bottom))) {
    firm.possible=bids.sim%>%filter(v.bottom==firm&binbid==1)  
    firm.truly=  firm.possible%>%arrange(profit)%>%slice(1)
    final.bids=rbind(firm.truly,final.bids)
    
  }
  }
  return(final.bids)
}

createBidSim_Optimal_Par<-function(simulated.2,sdparam,errorGov,minprofit){
  
  cores=detectCores()
  cl <- makeCluster(cores[1]-2) #not to overload your computer
  registerDoParallel(cl)
  
  vert.top.sim2=simulated.2%>%group_by(v.top)%>%count()
  vert.bottom.sim2=simulated.2%>%group_by(v.bottom)%>%count()
  
  #1. Create monte carlo distribution of minimum values
  # 2. Simulate Bidding Distributions
  iterations=nrow(vert.top.sim2)
  bids.sim=foreach(v =seq_len(iterations), .combine=rbind, .packages=c("dplyr",'magrittr','MASS','waddR'),.verbose=F) %dopar%  {
    
    edges.gov=simulated.2%>%filter(v.top==v)
    connected.firms=vert.bottom.sim2%>%filter(v.bottom%in%edges.gov$v.bottom)  
    connected.firms=connected.firms%>%slice_sample(n = ceiling(sqrt(nrow(connected.firms))))
    totfirms=nrow(connected.firms)
    commoncomp=runif(n = 1,min = -errorGov,max = errorGov)+rbinom(n = 1,prob = 0.1,size = 1)*runif(n=1,-0.8,0.8)
    valuations=rnorm(n = totfirms,mean = 1,sd = sdparam)%>%as.vector()
    
    ##Create optimal payoffs
    connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=NA,bid=NA,profit=NA)%>%mutate(valuation=min(valuation,2))
    for (firm in seq_len(nrow(connected.firms.vals))) {
      possiblebids=seq(connected.firms.vals$valuation[firm],2.02,by=0.02)
      possiblevaluation=rep(connected.firms.vals$valuation[firm],length(possiblebids))           
      profits=(possiblebids-possiblevaluation)*(1-pnorm(possiblebids, 1, sd=sdparam)**(1+ceiling(log(totfirms))) )
      expected_max=max(profits)
      max_bid=possiblebids[which.max(profits)]
      connected.firms.vals$bid[firm]=max_bid
      connected.firms.vals$binbid[firm]=as.numeric(expected_max>minprofit)
      connected.firms.vals$profit[firm]=as.numeric(expected_max)
    }
    
    #connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse(valuation<(1/log(1+nrow(connected.firms))),yes = 1,no = 0))
    #connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse((commoncomp+valuation)<=commoncomp+min_n[1+max(0,ceiling((log(totfirms)-0*rbinom(n=1,size = 1,prob = 0.5))))],yes = 1,no = 0))
    
    submitted.bids.unit=connected.firms.vals%>%filter(binbid==1)%>%as.data.frame()%>%mutate(ID=v,bid=valuation+commoncomp)
    
    #if(nrow(submitted.bids.unit)>0){
      #bids.sim=rbind(submitted.bids.unit,bids.sim)
    submitted.bids.unit
    #}
  }
  stopCluster(cl)
  #3. Select which ones will bid
  final.bids=data.frame()
  if(length(bids.sim$v.bottom)>0){
    for(firm in seq_len(max(bids.sim$v.bottom))) {
      firm.possible=bids.sim%>%filter(v.bottom==firm&binbid==1)  
      firm.truly=  firm.possible%>%arrange(profit)%>%slice(1)
      final.bids=rbind(firm.truly,final.bids)
      
    }
  }
  return(final.bids)
}

bids.sim.diag=createBidSim(simulated.2 = edgelist.period1,errorGov =0.13, sdparam=0.09)

compareModelsDiag<-function(simulated.bids.Model1,simulated.bids.Model2,bids.df.period2,degree.bottom.sim,Model1Name,Model2Name){
diag1=getSimDiag(simulated.bids.Model1,bids.df.period2,degree.bottom.sim,ModelName = Model1Name)  
diag2=getSimDiag(simulated.bids.Model2,bids.df.period2,degree.bottom.sim,ModelName = Model2Name)  
p1.model1=diag1[[2]]  
p2.model1=diag1[[3]]  
p3.model1=diag1[[4]]  
  
p1.model2=diag2[[2]]  
p2.model2=diag2[[3]]  
p3.model2=diag2[[4]]  

plot.row=plot_grid(p1.model1, p2.model1,p3.model1, label_size = 7,nrow = 1,ncol=3)
plot.row2=plot_grid(p1.model2, p2.model2,p3.model2, label_size = 7,nrow = 1,ncol = 3)
plot.gid=plot_grid(plot.row,plot.row2,labels = c(Model1Name,Model2Name),label_size=12,nrow = 2,ncol=1,label_x = .005)

plot_comparison=plot_grid(
  p1.model1, p2.model1,p3.model1,p1.model2,p2.model2,p3.model2,
  align = "h", axis = "b", nrow = 2,ncol=3 #rel_widths = c(1, 2)
)
table.comparison1=diag1[[6]]%>%filter(Type!='Empirical')
table.comparison2=diag2[[6]]
table.comparison=rbind(table.comparison1,table.comparison2)

return(list(plot.gid,table.comparison))
  
}

createBidSim_Mechanic<-function(edgelist.sim.2,degree.bottom.sim,lambda.df,params.df,lm.mu = lm.mu,lm.sd = lm.sd){
  tot_top=nrow(degree.top.sim)
  simulated.extra=edgelist.sim.2%>%left_join(degree.bottom.sim)  
  
  simulated.bids.Model1=data.frame()
  for (v in seq_len(tot_top)) {
    connected_firms=simulated.extra%>%filter(v.top==v)
    if(nrow(connected_firms>0)){
      auction=createAuction(degree.top.single = v,degree.bottom =connected_firms,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
      simulated.bids.Model1=rbind(auction,simulated.bids.Model1)
    }
  }  
  
  return(simulated.bids.Model1)
  
}

Model1Name='Endogenous'
Model2Name='Exogenous'

bids.sim.diag=createBidSim_NetworkEffects(simulated.2 = edgelist.period1,errorGov =0.17, sdparam=0.11)
bids.sim.diag=createBidSim_Optimal(simulated.2 = edgelist.period1,errorGov =0.01, sdparam=0.6)
bids.sim.diag=createBidSim_Optimal(simulated.2 = edgelist.period1,errorGov =0.1, sdparam=0.01,minprofit = 0.02)
bids.sim.diag=createBidSim_Optimal_Par(simulated.2 = edgelist.period1,errorGov =0.17, sdparam=0.02,minprofit = 0.02)
degree.bottom.1=degree.bottom.1%>%left_join(lookup_names.bottom.1)


#Fit one. Distribution of bids
hist.sim.bids=bids.sim.diag%>%mutate(type='Predicted')%>%dplyr::select(v.bottom,bid)
bids.simple.df.period2=bids.df.period2%>%rename(bid=MCA_MPO,v.bottom=RutProveedor)%>%mutate(type='Empirical')%>%dplyr::select(v.bottom,bid)
comparison.bids=rbind(hist.sim.bids,bids.simple.df.period2)
plot1<-ggplot(hist.sim.bids,aes(x=bid))+geom_histogram(aes(y=..density..),alpha=0.5,fill='steelblue')+geom_histogram(aes(x=bid,y=..density..),data=,bids.simple.df.period2,fill='red',alpha=0.5)+xlim(0,2)

#Fit two. Biider participation
bids.sim.participants=bids.sim.diag%>%group_by(ID)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(bid),bid.sd=sd(bid))%>%mutate(type='Predicted')
bids.period2.participants=bids.df.period2%>%group_by(ID=Codigo)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO))%>%mutate(type='Empirical')
comparison.bids.participants=rbind(bids.sim.participants,bids.period2.participants)%>%group_by(type)%>%mutate(n=n/sum(n))
p2<-ggplot(comparison.bids.participants,aes(x=participants,y=n,fill=type))+geom_bar(stat = 'identity',position = 'dodge')+xlim(0,20)

#Measure 1. Efficiency vs Number of Bidders.
bids.sim.participants=bids.sim.diag%>%group_by(ID)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(bid),bid.sd=sd(bid))%>%mutate(type='Predicted')
bids.period2.participants=bids.df.period2%>%group_by(ID=Codigo)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO))%>%mutate(type='Empirical')
comparison.bids.participants=rbind(bids.sim.participants,bids.period2.participants)
p3<-ggplot(comparison.bids.participants,aes(x=(participants),y=bid.average,color=type))+
  geom_point()+xlim(0,15)

#Measure 2. Efficiency by node degree
vert.bottom=simulated.2%>%group_by(v.top)%>%count()
degree.bids.sim=bids.sim.diag%>%group_by(n)%>%summarise(bid.average=mean(bid),bid.sd=sd(bid))%>%mutate(type='Predicted')
degree.bids.period2=bids.df.period2%>%left_join(degree.bottom.1)%>%group_by(n=d)%>%summarise(bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO))%>%mutate(type='Empirical')
comparison.bids.degrees=rbind(degree.bids.sim,degree.bids.period2)
p4<-ggplot(comparison.bids.degrees,aes(x=(n),y=bid.average,color=type))+geom_point()+xlim(0,15)



pgrid=plot_grid(plot1, p2,p3,p4, labels = c('1', '2','3','4'))
pgrid

hist(final.bids$profit)


p1
degree.bottom.1
head(bids.sim.diag)

    #stat_summary(geom = "errorbar", fun.data = bid.sd,alpha=0.7,size=1.05)+
  #stat_summary(geom = "point", fun.y = mean,color='red',size=3)+xlim(0,15)+
  #geom_smooth(data=merged.wins%>%filter(winspre>0),se=F,formula=y ~ log(x), method = 'lm',color='steelblue')+
  #geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Number of Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.4)))
pl
ggplot()

bids.sim.diag%>%head()
