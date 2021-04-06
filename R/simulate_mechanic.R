
#Fit network parameters
alpha.top.nbin=(fitdist(degree.top.1$d,"nbinom",method = 'mle',discrete = T))$estimate
alpha.bottom.1ln=get_parameter_ln(degree.bottom.1$d)
m_ln = dislnorm$new(degree.bottom.1$d)
m_ln$setXmin(1)
est_lm = estimate_pars(m_ln)
m_ln$setPars(est_lm)

#Simulate Network of bids
simulated.2=simBipartiteNetwork(alpha.top.nbin = alpha.top.nbin, alpha.bottom.1 = alpha.bottom.1, ntop = n.top.2,
                                nbottom = n.bottom.2,m_ln =m_ln )

degree.bottom.sim=simulated.2%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))
degree.top.sim=simulated.2%>%group_by(v.top)%>%summarise(d=length(unique(v.bottom)))

lambda.df=fitAuctionBidders(degree.top =degree.top.1 ,bids =df.period1 )[[1]]
params.df=fitdDegreesBids(degree.bottom = degree.bottom.1,bids =df.period1 )
lm.mu=lm(mu~poly(nParticipants,2)+poly(d,2),data =params.df )
lm.sd=lm(sd~poly(nParticipants,2),data =params.df )

### Simulate Model 1
simulated.bids.Model1=createBidSim_Mechanic(edgelist.sim.2 = simulated.2,degree.bottom.sim = degree.bottom.sim,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)

### Simulate Model 2
simulated.bids.Model2=createBidSim_NetworkEffects(simulated.2 = simulated.2,errorGov =0.17, sdparam=0.11)

### Diagnostics of the simulation
bids.df.period2=df.period1%>%dplyr::select(MCA_MPO,Codigo,RutProveedor)

diag_Model1=getSimDiag(simulated.bids.Model1,bids.df.period2 = bids.df.period2,degree.bottom.sim=degree.bottom.sim,ModelName = 'Exogenous')
diag_Model2=getSimDiag(simulated.bids.Model2,bids.df.period2 = bids.df.period2,degree.bottom.sim=degree.bottom.sim,ModelName = 'Endogenous')
diag_compare=compareModelsDiag(simulated.bids.Model1 = simulated.bids.Model1,simulated.bids.Model2=simulated.bids.Model2,bids.df.period2 = bids.df.period2,
                               degree.bottom.sim=degree.bottom.sim,Model1Name = 'Exogenous',Model2Name = 'Endogenous')
diag_compare_plot=diag_compare[[1]]
diag_compare_table=diag_compare[[2]]

diag_compare_plot
create_kable(diag_compare_table,caption = 'Model Comparison Statistics')
  