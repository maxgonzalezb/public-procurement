
## Generate and simulate
#todo:bring coherently
alpha.top.nbin=(fitdist(degree.top.1$d,"nbinom",method = 'mle',discrete = T))$estimate
alpha.bottom.1ln=get_parameter_ln(degree.bottom.1$d)
m_ln = dislnorm$new(degree.bottom.1$d)
m_ln$setXmin(1)
est_lm = estimate_pars(m_ln)
m_ln$setPars(est_lm)

n.top.2=nrow(degree.top.2)
n.bottom.2=nrow(degree.bottom.2)
nbottom=nrow(degree.bottom.2)

n.top.1=nrow(degree.top.1)
n.bottom.1=nrow(degree.bottom.1)
nbottom.1=nrow(degree.bottom.1)

lambda.df=fitAuctionBidders(degree.top =degree.top.1 ,bids =df.period1 )
params.df=fitdDegreesBids(degree.bottom = degree.bottom.1,bids =df.period1 )


simulated.top=NULL
simulated.bottom=NULL

res.fitted=SimNetworkNodeDistribution(alpha.top.nbin,alpha.bottom.1,ntop = n.top.1,nbottom = n.bottom.1,m_ln=m_ln)
res.simulations=SimNetworkNodeDistribution(alpha.top.nbin,alpha.bottom.1,ntop = n.top.2,nbottom = n.bottom.2,m_ln=m_ln)

fitted.1=SimEdgeCreation(res.simulations=res.fitted)
simulated.2=SimEdgeCreation(res.simulations=res.simulations)

table.compare=diagNetSim(edgelist.period2 = edgelist.period2,edgelist.period1 = edgelist.period1,simulated.2 = simulated.2,fitted.1 = fitted.1)
create_kable(table.compare,caption = 'Network statistics for fitted and simulated networks')


###Diagnose new network
sum(simulated.top)
sum(simulated.bottom)

#Obtained
mean(simulated.top)/length(simulated.bottom)
mean(simulated.bottom)/length(simulated.top)

#Real
mean(degree.top.1$d)/length(degree.bottom.1$d)
mean(degree.bottom.1$d)/length(degree.top.1$d)

#Now, create the new network

#Review Period 1
table.review.1=df.period1%>%group_by(Codigo)%>%summarise(nparticipants=length(Codigo))%>%
                                summarise(contracts=length(unique(Codigo)),nparticipants.mean=mean(nparticipants),nparticipants.sd=sd(nparticipants))
                                                 
table.review.2=df.period1%>%summarise(bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO),firms=length(unique(RutProveedor)),gov.units=length(unique(NombreOrganismo)))
table.review.period1=cbind(table.review.1,table.review.2)%>%dplyr::select(contracts,firms,gov.units,bid.average,bid.sd,nparticipants.mean,nparticipants.sd)

#Review period 2
table.review.1=df.period2%>%group_by(Codigo)%>%summarise(nparticipants=length(Codigo))%>%
  summarise(contracts=length(unique(Codigo)),nparticipants.mean=mean(nparticipants),nparticipants.sd=sd(nparticipants))

table.review.2=df.period2%>%summarise(bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO),firms=length(unique(RutProveedor)),gov.units=length(unique(NombreOrganismo)))
table.review.period2=cbind(table.review.1,table.review.2)%>%dplyr::select(contracts,firms,gov.units,bid.average,bid.sd,nparticipants.mean,nparticipants.sd)

final.table=rbind(table.review.period1,table.review.period2)%>%cbind(Period=c('Period 1','Period 2'))%>%dplyr::select(Period,everything())%>%mutate_if(is.numeric, funs((signif(., 3))))
create_kable(final.table,'Sample Descriptive Statistics')



mean.theorical=exp(alpha.bottom.1ln[2]+alpha.bottom.1ln[1]/2)
simulated.2=edgelist
#Mañana: ver los de los estadisticos que generan una distribucion bipartita
res.simulations=SimNetworkNodeDistribution(alpha.top.nbin,alpha.bottom.1,ntop = n.top.2,nbottom = n.bottom.2,m_ln=m_ln)
simulated.2=SimEdgeCreation(res.simulations=res.simulations)


OLD{
  tot_top=nrow(degree.top.sim)
  simulated.extra=simulated.2%>%left_join(degree.bottom.sim)
  simulated.bids.Model1=data.frame()
  for (v in seq_len(tot_top)) {
    connected_firms=simulated.extra%>%filter(v.top==v)
    if(nrow(connected_firms>0)){
      auction=createAuction(degree.top.single = v,degree.bottom =connected_firms,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
      simulated.bids.Model1=rbind(auction,simulated.bids.Model1)
    }
  }
}

