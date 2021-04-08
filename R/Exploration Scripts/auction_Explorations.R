#Todo: separate fitting fromgrid exploration

#################Fit First Model
fit.participants=fitAuctionBidders(degree.top = degree.top.1,bids = df.period1)
lambda.df=fit.participants[[1]]
bics=fit.participants[[2]]%>%arrange(d)

#Fit Bids Given Participants
params.df=fitAuctionBids(degree.bottom = degree.bottom.1,df.period1)
params.df.expand=params.df%>%uncount()
lm.mu=lm(mu~poly(nParticipants,2)+poly(d,2),data =params.df )
lm.sd=lm(sd~poly(nParticipants,2),data =params.df )
aux=df.period1%>%dplyr::select(RutProveedor,MCA_MPO,Codigo)%>%left_join(degree.bottom.1)%>%group_by(Codigo)%>%mutate(nParticipants=length(Codigo))%>%filter(d<20)
aux.sd=aux%>%group_by(d,nParticipants)%>%summarise(sd=sd(MCA_MPO))

lm.mu=lm(MCA_MPO~poly(nParticipants,2)+poly((d),2),data =aux )
lm.sd=lm(sd~poly(nParticipants,2),data =aux.sd)

#Antiguo
#lm.mu=lm(mu~poly(nParticipants,2),data =params.df )
#lm.mu=lm(mu~pnParticipants,data =params.df )

###############Fit Second Model
sdparam=0.08
errorGov=0.01
scaleparam=0.05
df=50

results=data.frame()

#First Set
possible_sd=seq(0.007,0.12,by = 0.006)
possible_error=seq(1e-2,0.12,by = 0.04)
possible_minprofit=seq(0,0.025,by = 0.0025)
feasible_space=expand.grid(possible_sd,possible_minprofit,possible_error)%>%as.data.frame()
colnames(feasible_space)<-c('possible_sd','possible_minprofit','possible_error')
feasible_space=feasible_space%>%arrange(possible_sd)
head(feasible_space,n=30)
replications=3#nrow(feasible_space)
nrow(feasible_space)
results=data.frame()
min_n=rep(100,300)
est_time=nrow(feasible_space)*40/3600

#####################################2.
#Second Set
possible_sd=seq(0.01,0.3,by = 0.04)
possible_error=seq(1e-2,0.21,by = 0.04)
possible_minprofit=seq(0,0.02,by = 0.0025)
feasible_space=expand.grid(possible_sd,possible_minprofit,possible_error)%>%as.data.frame()
colnames(feasible_space)<-c('possible_sd','possible_minprofit','possible_error')
feasible_space=feasible_space%>%arrange(possible_sd)
replications=nrow(feasible_space)
est_time=nrow(feasible_space)*40/3600
results=data.frame()

for (j in seq_len(replications)) {
#print(j)
print(round((j/replications),3))
sdparam=feasible_space$possible_sd[j]
errorGov=feasible_space$possible_error[j]
minprofit=feasible_space$possible_minprofit[j]

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
meanBid.fit=mean(hist.period1.bids$MCA_MPO,na.rm = T)
meanBid.empr=mean(hist.sim.bids$bid,na.rm = T)
results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,minprofit=minprofit,dist.wass=dist,averageParticipants=average.participants.sim,meanBid.fit=meanBid.fit,meanBid.empr=meanBid.empr,nbids=nrow(final.bids))
results=rbind(results,results.iter)
print(results.iter)
}
if(nrow(final.bids)==0){
  results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,minprofit=minprofit,dist.wass=NA,averageParticipants=NA,meanBid.fit=NA,meanBid.empr=NA,nbids=0)
  results=rbind(results,results.iter)
}
}
results%>%arrange(meanBid.empr)%>%filter(minprofit<0.05)
save(results,file = 'results_OptimalNoNetworkEffects_LOG2_.Rdata')



##################################3
#Non-Optimal Model Test
possible_sd=seq(0.01,0.2,by = 0.04)
possible_error=seq(1e-2,0.21,by = 0.04)
feasible_space=expand.grid(possible_sd,possible_error)%>%as.data.frame()
colnames(feasible_space)<-c('possible_sd','possible_error')
feasible_space=feasible_space%>%arrange(possible_sd)
replications=nrow(feasible_space)
j=1
results=data.frame()
for (j in seq_len(replications)) {
  print(j)
  print(round((j/replications),3))
  sdparam=feasible_space$possible_sd[j]
  errorGov=feasible_space$possible_error[j]
  minprofit=feasible_space$possible_minprofit[j]
  

  # 2. Simulate Bidding Distributions
  #final.bids=createBidSim(simulated.2 = edgelist.period1,sdparam = sdparam,errorGov = errorGov)
  final.bids=createBidSim(simulated.2 = edgelist.period1,sdparam = sdparam,errorGov = errorGov)
  
  #Calculate Transport Costs For bidding participants and bids
  if(nrow(final.bids)>0){
    hist.sim.bids=final.bids#%>%>%mutate(weight=1/nrow(final.bids))#%>%group_by(bid)%>%count(name = count)%>% uncount(count)
    hist.period1.bids=df.period1%>%dplyr::select(MCA_MPO)#%>%group_by(bid)%>%count()%>% uncount(count)
    average.participants.sim=final.bids%>%group_by(ID)%>%count()%>%ungroup()%>%summarise(averageParticipants=mean(n))
    
    m1=hist.sim.bids$bid
    m2=hist.period1.bids$MCA_MPO
    
    dist=wasserstein_metric(x=m1, y=m2,p = 2)
    meanBid.fit=mean(hist.period1.bids$MCA_MPO,na.rm = T)
    meanBid.empr=mean(hist.sim.bids$bid,na.rm = T)
    results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,dist.wass=dist,averageParticipants=average.participants.sim,meanBid.fit=meanBid.fit,meanBid.empr=meanBid.empr,nbids=nrow(final.bids))
    results=rbind(results,results.iter)
  }
  if(nrow(final.bids)==0){
    results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,minprofit=minprofit,dist.wass=NA,averageParticipants=NA,meanBid.fit=NA,meanBid.empr=NA,nbids=0)
    results=rbind(results,results.iter)
  }
}
save(results,file = 'results_NaiveNoNetworkEffects.Rdata')

results%>%arrange(dist.wass)

##################################3
#Non-Optimal Model Test - Nwtwork Effects
possible_sd=seq(0.01,0.2,by = 0.05)
possible_error=seq(1e-2,0.21,by = 0.04)
feasible_space=expand.grid(possible_sd,possible_error)%>%as.data.frame()
colnames(feasible_space)<-c('possible_sd','possible_error')
feasible_space=feasible_space%>%arrange(possible_sd)
replications=nrow(feasible_space)
results=data.frame()
for (j in seq_len(replications)) {
  print(j)
  print(round((j/replications),3))
  sdparam=feasible_space$possible_sd[j]
  errorGov=feasible_space$possible_error[j]

  # 2. Simulate Bidding Distributions
  #final.bids=createBidSim(simulated.2 = edgelist.period1,sdparam = sdparam,errorGov = errorGov)
  final.bids=createBidSim_NetworkEffects(simulated.2 = edgelist.period1,sdparam = sdparam,errorGov = errorGov)
  
  #Calculate Transport Costs For bidding participants and bids
  if(nrow(final.bids)>0){
    hist.sim.bids=final.bids#%>%>%mutate(weight=1/nrow(final.bids))#%>%group_by(bid)%>%count(name = count)%>% uncount(count)
    hist.period1.bids=df.period1%>%dplyr::select(MCA_MPO)#%>%group_by(bid)%>%count()%>% uncount(count)
    average.participants.sim=final.bids%>%group_by(ID)%>%count()%>%ungroup()%>%summarise(averageParticipants=mean(n))
    
    m1=hist.sim.bids$bid
    m2=hist.period1.bids$MCA_MPO
    
    dist=wasserstein_metric(x=m1, y=m2,p = 2)
    meanBid.fit=mean(hist.period1.bids$MCA_MPO,na.rm = T)
    meanBid.empr=mean(hist.sim.bids$bid,na.rm = T)
    results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,dist.wass=dist,averageParticipants=average.participants.sim,meanBid.fit=meanBid.fit,meanBid.empr=meanBid.empr,nbids=nrow(final.bids))
    print(results.iter)
    
    results=rbind(results,results.iter)
  }
  if(nrow(final.bids)==0){
    results.iter=data.frame(errorGov=errorGov, sdparam=sdparam,minprofit=minprofit,dist.wass=NA,averageParticipants=NA,meanBid.fit=NA,meanBid.empr=NA,nbids=0)
    results=rbind(results,results.iter)
  }
}
save(results,file = 'results_NaiveYesNetworkEffects.Rdata')

results%>%arrange(dist.wass)










head(m)
participants.sim=final.bids%>%group_by(ID)%>%summarise(NumeroOferentes=length(ID))%>%group_by(NumeroOferentes)%>%count()%>%ungroup()%>%mutate(prop=n/sum(n))
participants.period1=((df.period1%>%group_by(Codigo)%>%summarise(NumeroOferentes=NumeroOferentes[1])))
participants.period1=participants.period1%>%ungroup()%>%group_by(NumeroOferentes)%>%count()%>%ungroup()%>%mutate(prop=n/sum(n))
participants.period1
participants.sim

mean(participants.period1$participants)
mean(participants.sim$n)

mean(final.bids$bid)
mean(df.period1$MCA_MPO)
mean(df.period2$MCA_MPO)
sd(bids.sim$bid)
sd(df.period1$MCA_MPO)

hist(df.period1$MCA_MPO,xlim = c(-1,3),breaks = 20)
hist(final.bids$bid,xlim = c(-1,3),breaks = 20)


hist(final.bids$valuation,xlim = c(-1,3))

hist(participants.period1$participants,xlim=c(0,15))
hist(final.bids$n,xlim=c(0,15))

table

uk=df.period1%>%filter(NumeroOferentes<10&MCA_MPO!=1)
uk=uk%>%mutate(month=month(FechaInicio))%>%mutate(size=ntile(MontoEstimado, 10),time=ntile(TiempoDuracionContrato, 10))
# ggplot(uk,aes(x=MCA_MPO))+geom_density()+ylim(0,30)
# ggplot(bids.sim,aes(x=bid))+geom_density()+ylim(0,30)
# ggplot(uk,aes(x=MCA_MPO))+geom_histogram(aes(y=..density..))+ylim(0,10)
# ggplot(bids.sim,aes(x=bid))+geom_histogram(aes(y=..density..))+ylim(0,30)
#  ggplot(bids.sim,aes(x=bid))+geom_histogram(aes(y=..density..))+ylim(0,30)

hist(df.period1$MCA_MPO, xlim = c(0,2),breaks = seq(0,2,by = 0.1))
hist(final.bids$bid, breaks = seq(-1,2,by = 0.1),xlim = c(0,2))
hist(bids.sim$valuation, breaks = seq(-1,2,by = 0.1),xlim = c(0,2))
#hist(bids.sim$valuation, xlim = c(0,2), breaks=10)

##Checks Real
library(stargazer)
stargazer(lm.mu,single.row = TRUE)

vert.bottom.period1=df.period1%>%select(RutProveedor,MCA_MPO)%>%left_join(degree.bottom.1)

head(final.bids)
head(vert.bottom.sim2)
vert.bottom.sim2.bid=vert.bottom.sim2%>%left_join(final.bids%>%select(-n),by = c('v.bottom'='v.bottom'))
sample.period1=vert.bottom.period1[sample(x = seq_len(nrow(vert.bottom.period1)),size = 1610),]

lambda.df%>%head()
ggplot(lambda.df,aes(x=d,y=meanLog))+theme_bw()+ylab('MeanLog')+geom_smooth()+geom_line()
