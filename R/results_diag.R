
bids.sim.diag=data.frame()
for (i in seq_len(20)) {
print(i)
bids.sim.diag.iter=createBidSim_Optimal(simulated.2 = edgelist.period1,errorGov =0.01, sdparam=0.11)%>%mutate(ID=paste0(ID,i))
bids.sim.diag.iter=createBidSim(simulated.2 = simulated.2,errorGov =0.15, sdparam=0.11)%>%mutate(ID=paste0(ID,i))
bids.sim.diag=rbind(bids.sim.diag.iter,bids.sim.diag)
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
