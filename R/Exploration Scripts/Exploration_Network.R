##Bottom Analysis
{
  #Full analysis
  degree.organisms=degree.bottom.1$d
  m_m = displ$new(degree.organisms)
  estx = estimate_xmin(m_m)
  m_m$setXmin(1)
  est = estimate_pars(m_m)
  m_m$setPars(est)
  plot1=plot(m_m)%>%rename(observed=y)
  b=lines(m_m, col = 2)%>%rename(expected=y)
  fit.plot=full_join(plot1,b)%>%rename(degree=x)%>%pivot_longer(expected:observed)
  g2=ggplot(fit.plot,aes(x=degree))+geom_point(aes(y=value,color=name))+scale_y_log10()+scale_x_log10()+theme_minimal()+ylab('Density')+labs(color='')+theme(legend.position='top')+geom_vline(xintercept = 20)
  g2
  
  m_ln = dislnorm$new(degree.bottom.1$d)
  est_ln = estimate_xmin(m_ln)
  m_ln$setXmin(1)
  est_lm = estimate_pars(m_ln)
  m_ln$setPars(est_lm)
  
  m_ln2 = dislnorm$new(degree.bottom.2$d)
  est_ln2 = estimate_xmin(m_ln2)
  m_ln2$setXmin(1)
  est_lm2 = estimate_pars(m_ln2)
  m_ln2$setPars(est_lm2)
  plot(m_ln2)
  lines(m_ln2, col = 2, lwd = 2)
  
  plot(m_l)
  lines(m_ln, col = 2, lwd = 2)
  plot2=plot(m_ln)%>%rename(observed=y)
  b=lines(m_ln, col = 2)%>%rename(expected=y)
  fit.plot.2=full_join(plot2,b)%>%rename(degree=x)%>%pivot_longer(expected:observed)
  g2=ggplot(fit.plot.2,aes(x=degree))+geom_point(aes(y=value,color=name))+scale_y_log10()+scale_x_log10()+theme_minimal()+ylab('Density')+labs(color='')+theme(legend.position='top')+geom_vline(xintercept = 20)
  g2
  
  
  ##Power Law
  n.bottom.2=nrow(degree.bottom.2)
  n.bottom.1=nrow(degree.bottom.1)
  n=n.bottom.2
  alpha.bottom.1=get_parameter(degree.bottom.1$d)
  simulated.bottom=rpldis(n = n.bottom.2, xmin=1, alpha.bottom.1,discrete_max = 150)
  
  ##Log Normal
  n.bottom.2=nrow(degree.bottom.2)
  n=n.bottom.2
  alpha.bottom.1ln=get_parameter_ln(degree.bottom.1$d)
  simulated.bottom=dist_rand(m=m_ln, n = n.bottom.2)
  sum(simulated.bottom)
  ##Binomial-poiss
  #Poisson
  degree.sequence=degree.bottom.1$d
  fitpois=(fitdist(degree.sequence,"pois",method = 'mle',discrete = T))
  fitbin=(fitdist(degree.sequence,"nbinom",method = 'mle',discrete = T))
  fitln=(fitdist(degree.sequence,"lnorm",method = 'mle',discrete = T))
  fitln2=(fitdist(degree.bottom.2$d,"lnorm",method = 'mle',discrete = T))
  fitexp=(fitdist(degree.sequence,"exp",method = 'mle',discrete = T))
  fitgamma=(fitdist(degree.sequence,"gamma",method = 'mle',discrete = T))
  denscomp(list(fitpois, fitbin,fitln),legendtext = c("Poisson", "negative binomial",'Lognormal'), fitlty = 1)
  cdfcomp(list(fitpois, fitbin,fitln),legendtext = c("Poisson", "negative binomial",'Lognormal'), fitlty = 1)
  gof=gofstat(list(fitpois, fitbin,fitln,fitexp,fitgamma),fitnames = c("Poisson", "negative binomial",'lpgnormal','exponential','gamma'))
  
  output.table.bottom=gof$bic%>%as.data.frame()%>%mutate(Distribution=names(gof$bic))
  colnames(output.table.bottom)[1]<-c('BIC')
  rownames(output.table.bottom)<-NULL
  output.table.bottom=output.table.bottom%>%select(Distribution,BIC)
  create_kable(output.table.bottom,'BIC values for candidate distribution, bottom distribution')
  
  
  #Compare
  comp = compare_distributions(m_ln, m_m)
  ifelse(comp$test_statistic<0,"Second is better",no="First is better")
  
  #Use qqplot from lognormal
  qqplot(rlnorm(length(degree.bottom.1$d),fitln$estimate[1], fitln$estimate[2]),degree.bottom.1$d)
  abline(0,1)
  
}

#Top Analysis
{
  #Full analysis
  degree.organisms=degree.top.1$d
  m_m = displ$new(degree.organisms)
  estx = estimate_xmin(m_m)
  m_m$setXmin(1)
  m_m$setXmin(estx)
  est = estimate_pars(m_m)
  m_m$setPars(est)
  plot1=plot(m_m)%>%rename(observed=y)
  b=lines(m_m, col = 2)%>%rename(expected=y)
  fit.plot=full_join(plot1,b)%>%rename(degree=x)%>%pivot_longer(expected:observed)
  g2=ggplot(fit.plot,aes(x=degree))+geom_point(aes(y=value,color=name))+scale_y_log10()+scale_x_log10()+theme_minimal()+ylab('Density')+labs(color='')+theme(legend.position='top')+geom_vline(xintercept = 20)
  g2
  
  m_ln = dislnorm$new(degree.organisms)
  est_ln = estimate_xmin(m_ln)
  m_ln$setXmin(1)
  est_lm = estimate_pars(m_ln)
  m_ln$setPars(est_lm)
  plot(m_ln)
  lines(m_ln, col = 2, lwd = 2)
  plot2=plot(m_ln)%>%rename(observed=y)
  b=lines(m_ln, col = 2)%>%rename(expected=y)
  fit.plot.2=full_join(plot2,b)%>%rename(degree=x)%>%pivot_longer(expected:observed)
  g2=ggplot(fit.plot.2,aes(x=degree))+geom_point(aes(y=value,color=name))+scale_y_log10()+scale_x_log10()+theme_minimal()+ylab('Density')+labs(color='')+theme(legend.position='top')+geom_vline(xintercept = 20)
  g2
  
  nrow(degree.top.1)
  degree.sequence=degree.top.1$d
  #Poisson
  fitpois=(fitdist(degree.top.1$d,"pois",method = 'mle',discrete = T))
  fitbin=(fitdist(degree.top.1$d,"nbinom",method = 'mle',discrete = T))
  fitexp=(fitdist(degree.top.1$d,"exp",method = 'mle',discrete = T))
  fitgamma=(fitdist(degree.sequence,"gamma",method = 'mle',discrete = T))
  fitzerobin=(fitdist(degree.sequence,"ztnbinom",discrete = T,method = 'mme'))
  
  cdfcomp(list(fitpois, fitbin,fitgamma),legendtext = c("Poisson", "negative binomial",'gamma'), fitlty = 1)
  #gof=gofstat(list( fitbin2,fitexp2,fitpois),fitnames = c('bino2','exp2'))
  gof
  gof=gofstat(list(fitpois, fitbin,fitexp,fitgamma),fitnames = c("Poisson", "negative binomial",'exp','gamma'))
  
  output.table.top=gof$bic%>%as.data.frame()%>%mutate(Distribution=names(gof$bic))
  colnames(output.table.top)[1]<-c('BIC')
  rownames(output.table.top)<-NULL
  output.table.top=output.table.top%>%select(Distribution,BIC)
  create_kable(output.table.top,'BIC values for candidate distribution, top distribution')
  
    ##Power Law
  n.top.2=nrow(degree.top.2)
  alpha.top.1=get_parameter(degree.top.1$d)
  simulated.top=rpldis(n = n.top.2, xmin=1, alpha.top.1,discrete_max = 1000)
  
  ##Log Normal
  n.top.2=nrow(degree.top.2)
  alpha.top.1ln=get_parameter_ln(degree.top.1$d)
  simulated.top=dist_rand(m=m_ln, n = n.top.2)
  
  #Binomial
  n.top.2=nrow(degree.top.2)
  alpha.top.1nbin=fitbin$estimate
  simulated.top=rnbinom(n=n.top.2,size=alpha.top.1nbin[1],mu = alpha.top.1nbin[2] )
  
  #Exp
  n.top.2=nrow(degree.top.2)
  alpha.top.exp=fitexp$estimate
  simulated.top=round(rexp(n=n.top.2,rate = alpha.top.exp),0)
  simulated.top
  
  #Gamma
  n.top.2=nrow(degree.top.2)
  alpha.top.gamma=fitgamma$estimate
  simulated.top=round(rgamma(n=n.top.2,rate = alpha.top.gamma[2],shape=alpha.top.gamma[1]),0)
  
  table(simulated.top)
  table(degree.top.2$d)
  table(degree.top.1$d)
  sum(simulated.top)
  sum(degree.top.1$d)
  sum(degree.top.2$d)
  
  #Poisson
  alpha.top.1poss<-fitdistr(degree.top.1$d, densfun="poisson")
  
  #Compare
  comp = compare_distributions(m_ln, m_m)
  ifelse(comp$test_statistic<0,"Second is better",no="First is better")
}

#simulated.bottom=dist_rand(m=m_ln, n = n.bottom.2.theoric)
#sum(simulated.bottom)
#Match Distributions


#Explore the distribution of participants
number.participants=df.period1%>%group_by(Codigo)%>%count()
descdist(number.participants$n, boot = 1000,discrete = T)

descdist(bids.degree$participants, boot = 1000,discrete = F)

#Explore the distribution of bids
descdist(df.period1$MCA_MPO, boot = 1000)

hist(bids.degree$participants)
ggplot(params.df,aes(x=nParticipants,y=mu))+geom_point()+geom_smooth()
ggplot(params.df,aes(x=nParticipants,y=sd))+geom_point()+xlim(0,20)+geom_smooth()


ggplot(params.df%>%filter(nParticipants<10),aes(x=d,y=mu))+geom_point()+geom_smooth()+xlim(0,20)+facet_wrap(~nParticipants)
ggplot(params.df%>%filter(nParticipants<10),aes(x=d,y=sd))+geom_point()+geom_smooth()+xlim(0,20)+facet_wrap(~nParticipants)

