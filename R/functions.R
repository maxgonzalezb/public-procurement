
createBid<-function(nParticipants,degreeBidder,lm.mu,lm.sd){
newdata=data.frame(nParticipants=nParticipants,d=degreeBidder)
sigma=predict(lm.sd,newdata =newdata)   
mu=predict(lm.mu,newdata =newdata)   
draw=rnorm(n = 1,mean = mu,sd = sigma)  
return(draw)
}

createAuction<-function(degree.top.single,degree.bottom,lambda.df,params.df,lm.mu,lm.sd){
nParticipants=0
while (nParticipants==0) {
index=which(degree.top.single==lambda.df$d)
print(degree.top.single)
draw=rlnorm(n = 1,meanlog =  lambda.df$meanLog[index],sdlog =lambda.df$sdLog[index] )%>%round()
nParticipants=min(nrow(degree.bottom),draw) 
if(degree.top.single==1){nParticipants=1}
}

participants.index=sample(x = seq(1,length(degree.bottom$d)),size = nParticipants)  
participants.degrees=degree.bottom$d[participants.index]  
v.bottom=degree.bottom$v.bottom[participants.index]  
bids=c()
for (p in participants.degrees) {
bid.participant=createBid(nParticipants = (nParticipants),degreeBidder = p,lm.mu = lm.mu,lm.sd = lm.sd)

#mu=params.df$mu[which(params.df$d==p)]
#sd=params.df$sd[which(params.df$d==p)]
#bid.participant=rnorm(mean = mu,sd=sd,n = 1)  
bids=rbind(bid.participant,bids)    


}

bids=bids%>%as.vector()
bids.df=data.frame(ID=degree.top.single,v.top=degree.top.single,v.bottom=v.bottom,bid=bids)
return(bids.df%>%as.data.frame())
}

fitAuctionBidders<-function(degree.top,bids){

bids.min=bids%>%dplyr::select(Codigo,NombreOrganismo,MCA_MPO)%>%left_join(degree.top)

lambda.df=data.frame()
bics=data.frame()
max_deg=max(degree.top$d)+300
degrees.top=seq_len(max_deg)
bids.participants=bids.min%>%group_by(d,Codigo)%>%summarise(participants=length(Codigo))

for (d.top in degrees.top){
bids.degree=bids.participants%>%filter(d==d.top)
if(nrow(bids.degree)==0){
lambda.iter=lambda.df%>%filter(d==(d.top-1))%>%mutate(d=d.top)

}
if(nrow(bids.degree>0)){
fitpois=(fitdist(bids.degree$participants,"pois",method = 'mle',discrete = T))
fitbin=(fitdist(bids.degree$participants,"nbinom",method = 'mle',discrete = T))
fitln=(fitdist(bids.degree$participants,"lnorm",method = 'mle',discrete = T))  
fitgamma=(fitdist(bids.degree$participants,"gamma",method = 'mle',discrete = T))  

lambda.iter=data.frame(d=d.top,lambda=coef(fitpois),sizeBin=coef(fitbin)[1],MuBin=coef(fitbin)[2],
                       meanLog=coef(fitln)[1],sdLog=coef(fitln)[2],shapeGamma=coef(fitgamma)[1],rateGamma=coef(fitgamma)[1]  )

bics=rbind(data.frame(d=d.top,pois.bic=fitpois$bic,nbin.bic=fitbin$bic,ln.bic=fitln$bic,gamma.bic=fitgamma$bic),bics)

}
lambda.df=rbind(lambda.iter,lambda.df)


}
bics$min_colname  <- apply(bics[,-1], 1, function(x) colnames(bics[,-1])[which.min(x)])
return(list(lambda.df,bics))   
}

fitAuctionBids<-function(degree.bottom,bids){
  bids.min=bids%>%dplyr::select(Codigo,RutProveedor,MCA_MPO)%>%left_join(degree.bottom)
  params.df=data.frame()
  max_deg=max(degree.bottom$d)
  degrees.bottom=seq_len(max_deg)
  #bids.participants=bids.min%>%group_by(d,Codigo)%>%summarise(participants=length(Codigo))
 # d.bottom=46
  for (d.bottom in degrees.bottom){
    bids.iter=bids.min%>%filter(d==d.bottom)
    print(d.bottom)
    if(nrow(bids.iter)==0){
    #prev=d.bottom-1
    #params.iter=params.df%>%filter(d==(d.bottom-1))%>%mutate(d=d.bottom)
    }
    
    if(nrow(bids.iter>10)){
      bids.iter=bids.iter%>%group_by(Codigo)%>%mutate(nparticipants=length(Codigo))
     
      for (par in seq_len(20)) {
        print(par)
        bids.iter.part=bids.iter%>%filter(nparticipants==par)
        if (nrow(bids.iter.part)<10) {
          next
        }
        
        fitnormal=(fitdist(bids.iter.part$MCA_MPO,"norm",method = 'mle',discrete = F))
        mu=fitnormal$estimate[1]%>%as.numeric()
        sd=fitnormal$estimate[2]%>%as.numeric()
        params.iter=data.frame(d=d.bottom,mu=mu,sd=sd,nParticipants=par,n=nrow(bids.iter.part))
        params.df=rbind(params.iter,params.df)
        
      }
      
      
      #fitnormal=(fitdist(bids.iter$MCA_MPO,"norm",method = 'mle',discrete = F))
      #fitbin=(fitdist(bids.degree$participants,"nbinom",method = 'mle',discrete = T))
      #fitln=(fitdist(bids.degree$participants,"lnorm",method = 'mle',discrete = T))  
      #mu=fitnormal$estimate[1]%>%as.numeric()
      #sd=fitnormal$estimate[2]%>%as.numeric()
      #params.iter=data.frame(d=d.bottom,mu=mu,sd=sd)
    }
    
    
  }
  return(params.df)   
}

getSimDiag<-function(bids.sim.diag,bids.df.period2,degree.bottom.sim,ModelName){
    #Fit one. Distribution of bids
    hist.sim.bids=bids.sim.diag%>%mutate(Type='Predicted')%>%dplyr::select(v.bottom,bid,Type)
    bids.simple.df.period2=bids.df.period2%>%rename(bid=MCA_MPO,v.bottom=RutProveedor)%>%mutate(Type='Empirical')%>%dplyr::select(v.bottom,bid,Type)
    comparison.bids=rbind(hist.sim.bids,bids.simple.df.period2)
    #plot1<-ggplot(hist.sim.bids,aes(x=bid))+geom_histogram(aes(y=..density..),alpha=0.7,fill='steelblue')+geom_histogram(aes(x=bid,y=..density..),data=,bids.simple.df.period2,fill='red',alpha=0.5)+xlim(0,2)
    p1<-ggplot(comparison.bids,aes(x=bid))+geom_histogram(aes(y=..density..,fill=Type),alpha=1)+xlim(0,2)+
      theme_bw()+theme(legend.title = element_blank(),legend.position=c(.8,.75),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+ylab('Density')+xlab('Bids')
    
    #Fit two. Biider participation
    bids.sim.participants=bids.sim.diag%>%group_by(ID)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(bid),bid.sd=sd(bid))%>%mutate(type='Predicted')
    bids.period2.participants=bids.df.period2%>%group_by(ID=Codigo)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO))%>%mutate(type='Empirical')
    comparison.bids.participants=rbind(bids.sim.participants,bids.period2.participants)%>%group_by(type)%>%mutate(n=n/sum(n))
    p2<-ggplot(comparison.bids.participants,aes(x=participants,y=n,fill=type))+geom_bar(stat = 'identity',position = 'dodge')+xlim(0,20)+
      theme_bw()+theme(legend.title = element_blank(),legend.position=c(.8,.75),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+ylab('Density')+xlab('Participants')
    
    #Measure 1. Efficiency vs Number of Bidders.
    bids.sim.participants=bids.sim.diag%>%group_by(ID)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(bid),bid.sd=sd(bid))%>%mutate(type='Predicted')
    bids.period2.participants=bids.df.period2%>%group_by(ID=Codigo)%>%mutate(participants=length(ID))%>%group_by(participants)%>%summarise(n=length(ID),bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO))%>%mutate(type='Empirical')
    comparison.bids.participants=rbind(bids.sim.participants,bids.period2.participants)
    p3<-ggplot(comparison.bids.participants,aes(x=(participants),y=bid.average,color=type))+
      geom_point()+xlim(0,15)+theme_bw()+
      theme(legend.title = element_blank(),legend.position=c(.2,.25),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+
      ylab('Average Bid')+xlab('Participants')+geom_smooth(se=F,method = 'lm',alpha=0.5)
  
    #Efficiency vs Degree.
    bids.sim.participants=bids.sim.diag%>%left_join(degree.bottom.sim)%>%group_by(d)%>%summarise(n=length(ID),bid.average=mean(bid),bid.sd=sd(bid))%>%mutate(type='Predicted')
    bids.period2.participants=bids.df.period2%>%left_join(degree.bottom.2)%>%group_by(d)%>%summarise(n=length(Codigo),bid.average=mean(MCA_MPO),bid.sd=sd(MCA_MPO))%>%mutate(type='Empirical')
    comparison.bids.participants=rbind(bids.sim.participants,bids.period2.participants)
    p4<-ggplot(comparison.bids.participants,aes(x=(d),y=bid.average,color=type))+
      geom_point()+xlim(0,15)+geom_line()+
      theme_bw()+  theme(legend.title = element_blank(),legend.position=c(.8,.75),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+
      ylab('Average Bid')+xlab('Degree')
    
    ###Table of measures
    #Measure: bid average and sd, participant average sd, distance between distributions.
    bids.stats.sim=bids.sim.diag%>%summarise(mean.bid=mean(bid),sd.bid=sd(bid))
    bids.stats.2=bids.df.period2%>%summarise(mean.bid=mean(MCA_MPO),sd.bid=sd(MCA_MPO))
    c1=rbind(bids.stats.sim,bids.stats.2)
    
    participant.stats.sim=bids.sim.diag%>%group_by(ID)%>%summarise(participants=length(ID))%>%summarise(mean.participants=mean(participants),sd.participants=sd(participants))
    participant.stats.2=bids.df.period2%>%group_by(Codigo)%>%summarise(participants=length(Codigo))%>%summarise(mean.participants=mean(participants),sd.participants=sd(participants))
    c2=rbind(participant.stats.sim,participant.stats.2)
    
    m1=bids.sim.diag$bid
    m.real=bids.df.period2$MCA_MPO
    dist1=wasserstein_metric(x=m1, y=m.real,p = 2)
    dist2=wasserstein_metric(x=m.real, y=m.real,p = 2)
    dist=rbind(dist1,dist2)
    
    comparison.table=data.frame(c1,c2,dist)%>%cbind(Type=c(ModelName,'Empirical'))%>%dplyr::mutate_if(is.numeric, funs((signif(., 3))))
    
    pgrid=plot_grid(p1, p2,p3,p4, labels = c('1', '2','3','4'))
    return(list(pgrid,p1,p2,p3,p4,comparison.table))  
    
    
  }

get_parameter=function(vector){
  degree.organisms=vector
  m_m = displ$new(degree.organisms)
  #estx = estimate_xmin(m_m)
  m_m$setXmin(1)
  est = estimate_pars(m_m)  
  return(est$pars%>%as.numeric())
}

get_parameter_ln=function(vector){
  degree.organisms=vector
  m_m = dislnorm$new(degree.organisms)
  #estx = estimate_xmin(m_m)
  m_m$setXmin(1)
  est = estimate_pars(m_m)  
  return(est$pars%>%as.numeric())
}

SimNetworkNodeDistribution<-function(alpha.top.nbin,alpha.bottom.1,ntop,nbottom,m_ln){
  simulated.top=c(1000)
  simulated.bottom=c(0)
  while(abs(sum(simulated.top)-sum(simulated.bottom))>200){
    print(abs(sum(simulated.top)-sum(simulated.bottom)))
    
    simulated.top=rnbinom(n=ntop,size=alpha.top.nbin[1],mu = alpha.top.nbin[2] )
    #n.bottom.2.theoric=round(mean(simulated.top)/mean.theorical*length(simulated.top),0)
    simulated.bottom=dist_rand(m=m_ln, n = nbottom)
  }
  #return(list(top=simulated.top,bottom=simulated.bottom))
  print('First stage completed')  
  while(abs(sum(simulated.top)-sum(simulated.bottom))>10){
    print(abs(sum(simulated.top)-sum(simulated.bottom)))
    #print(length(simulated.top))
    simulated.bottom=simulated.bottom[1:(length(simulated.bottom)-5)]
    simulated.top=simulated.top[1:(length(simulated.top)-5)]
    
    extra.top=rnbinom(n=5,size=alpha.top.nbin[1],mu = alpha.top.nbin[2] )
    extra.bottom=dist_rand(m=m_ln, n = 5)
    
    simulated.top=c(simulated.top,extra.top)
    simulated.bottom=c(simulated.bottom,extra.bottom)
  }  
  return(list(top=simulated.top,bottom=simulated.bottom))
}

SimEdgeCreation<-function(res.simulations){
  simulated.top=res.simulations$top
  simulated.bottom=res.simulations$bottom  
  vertop=data.frame(vert=seq_len(length(simulated.top)),d=simulated.top)
  verbottom=data.frame(vert=seq_len(length(simulated.bottom)),d=simulated.bottom)
  vertop.update=data.frame(vert=seq_len(length(simulated.top)),d=simulated.top)
  verbottom.update=data.frame(vert=seq_len(length(simulated.bottom)),d=simulated.bottom)
  
  edgelist=data.frame() 
  for (v in seq_len(nrow(verbottom))) {
    ifelse(mod(v,1000)==0,yes=print(v),no=TRUE)
    ifelse(mod(v,1000)==0,yes=print(nrow(vertop.update)),no=TRUE)
    for (e in seq_len(as.numeric(verbottom$d[v]))) {
      
      if (nrow(vertop.update)==0) {
        break
      }
      
      vert.bottom=verbottom$vert[v]
      d.bottom=verbottom$d[v]
      
      index.selected.top=sample(seq(from = 1,to = nrow(vertop.update)),size = 1)
      selected.top=vertop.update[index.selected.top,]
      vert.top=selected.top$v
      d.top=selected.top$d
      
      verbottom.update$d[v]=verbottom.update$d[v]-1
      vertop.update$d[index.selected.top]=vertop.update$d[index.selected.top]-1
      
      # vertop.update$d[v]=(vertop.update$d[v]-1)
      vertop.update=vertop.update%>%filter(d>0)
      
      edge=data.frame(v.top=vert.top,v.bottom=vert.bottom)
      edgelist=rbind(edge,edgelist)
    }  
  }
  return(edgelist)
}

diagNetSim<-function(edgelist.period2,edgelist.period1,simulated.2,fitted.1){
  #Declare Graph
  edgelist.period1=df.period1%>%dplyr::select(RutProveedor,NombreOrganismo)%>%rename(firm=RutProveedor,gov=NombreOrganismo)%>%as.matrix()
  edgelist.period2=df.period2%>%dplyr::select(RutProveedor,NombreOrganismo)%>%rename(firm=RutProveedor,gov=NombreOrganismo)%>%as.matrix()
  graph.bids=graph_from_edgelist(edgelist.period1, directed = F)
  graph.bids.2=graph_from_edgelist(edgelist.period2, directed = F)
  
  #bipartite.mapping(graph.bids)
  #bipartite.mapping(graph.bids.2)
  V(graph.bids)$type <- bipartite_mapping(graph.bids)$type
  V(graph.bids.2)$type <- bipartite_mapping(graph.bids.2)$type
  #plot(graph.bids, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
  
  #Convert to bimatrix
  bipartite_matrix.period1 <- as_incidence_matrix(graph.bids)
  bipartite_matrix.period2 <- as_incidence_matrix(graph.bids.2)
  firm_firm.period1=(bipartite_matrix.period1) %*% t(bipartite_matrix.period1 )
  firm_firm.period2=(bipartite_matrix.period2) %*% t(bipartite_matrix.period2 )
  
  diag(firm_firm.period1) <- 0
  diag(firm_firm.period2) <- 0
  
  firm_firm.period1 <- graph_from_adjacency_matrix(firm_firm.period1, 
                                                   mode = "undirected", 
                                                   weighted = TRUE)
  firm_firm.period2 <- graph_from_adjacency_matrix(firm_firm.period2, 
                                                   mode = "undirected", 
                                                   weighted = TRUE)
  
  #Calculate different types of metrics for the network
  clust.period1=transitivity(firm_firm.period1, type = "average")
  dist.period1=average.path.length(firm_firm.period1,directed = F)
  dens.period1=graph.density(firm_firm.period1,loop=FALSE)
  
  clust.period2=transitivity(firm_firm.period2, type = "average")
  dist.period2=average.path.length(firm_firm.period2,directed = F)
  dens.period2=graph.density(firm_firm.period2,loop=FALSE)
  
  #######Simulated Comparison P2
  edgelist.sim.2=simulated.2%>%mutate(v.top=paste0('g',v.top),v.bottom=paste0('f',v.bottom))%>%as.matrix()
  graph.sim.2=graph_from_edgelist(edgelist.sim.2, directed = F)
  #bipartite.mapping(graph.sim.2)
  V(graph.sim.2)$type <- bipartite_mapping(graph.sim.2)$type
  #plot(graph.bids, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
  
  #Convert to bimatrix
  bipartite_matrix.sim2 <- as_incidence_matrix(graph.sim.2)
  firm_firm.sim2 <- t(bipartite_matrix.sim2) %*% (bipartite_matrix.sim2 )
  diag(firm_firm.sim2) <- 0
  firm_firm.sim2
  
  graph.sim.2.firms <- graph_from_adjacency_matrix(firm_firm.sim2, 
                                                   mode = "undirected", 
                                                   weighted = TRUE)
  
  
  #####Fitted 1
  fitted.1=fitted.1%>%mutate(v.top=paste0('g',v.top),v.bottom=paste0('f',v.bottom))%>%as.matrix()
  graph.fitted.1=graph_from_edgelist(fitted.1, directed = F)
  #bipartite.mapping(graph.sim.2)
  V(graph.fitted.1)$type <- bipartite_mapping(graph.fitted.1)$type
  #plot(graph.bids, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
  
  #Convert to bimatrix
  bipartite_matrix.fitted.1 <- as_incidence_matrix(graph.fitted.1)
  firm_firm.fitted.1 <- t(bipartite_matrix.fitted.1) %*% (bipartite_matrix.fitted.1 )
  diag(firm_firm.fitted.1) <- 0
  
  
  graph.fitted1.firms <- graph_from_adjacency_matrix(firm_firm.fitted.1, 
                                                   mode = "undirected", 
                                                   weighted = TRUE)
  
  
  
  
  #Calculate different types of metrics for the network
  clust.fitted1=transitivity(graph.fitted1.firms, type = "average")
  dist.fitted1=average.path.length(graph.fitted1.firms,directed = F)
  dens.fitted1=graph.density(graph.fitted1.firms,loop=FALSE)
  
  ###Comparing Compare with metrics of network 2. 
  type=c('Period 1','Fitted 1','Empirical Period 2','Predicted Period 2')
  average.distance=c(dist.period1,dist.fitted1,dist.period2,dist.sim2)
  clust.coef=c(clust.period1,clust.fitted1,clust.period2,clust.sim2)
  density.graph=c(dens.period1,dens.fitted1,dens.period2,dens.sim2)
  pred.statistics=data.frame(type,average.distance,clust.coef,density.graph)%>%dplyr::mutate_if(is.numeric, funs((signif(., 3))))
  
  return(pred.statistics)
  print(abs(clust.period2-clust.sim2)/clust.period2)
  print(abs(dist.period2-dist.sim2)/dist.period2)
  print(abs(dens.period2-dens.sim2)/dens.period2)  
  
  
  
  
  
}

create_kable<-function(df,caption){
  kable(
    df, "latex",
    booktabs = T,
    linesep = "",
    align = rep('c', 5),
    caption = caption
  ) %>% kable_styling(latex_options = c("hold_position"))
}


increaseEntryFirms<-function(n.top.2.original,n.bottom.2.original,extraProp,edgelist.original,mode){

degree.top.original.sim=edgelist.original%>%group_by(v.top)%>%count()%>%rename(d=n)
degree.bottom.original.sim=edgelist.original%>%group_by(v.bottom)%>%count()%>%rename(d=n)
n.bottom.2.new=n.bottom.2.original*(1+extraProp)
extra.nodes.bottom=ceiling(extraProp*n.bottom.2.original)
new.edgelist=data.frame()

#Random Entry
if (mode=='Random') {

#res.simulations=SimNetworkNodeDistribution(alpha.top.nbin,alpha.bottom.1,ntop = n.top.2,nbottom = extra.nodes.bottom,m_ln=m_ln)
simulated.bottom.extra=dist_rand(m=m_ln, n = extra.nodes.bottom)
for (f in seq_len(length(simulated.bottom.extra))) {
degree.extra=simulated.bottom.extra[f] 
vec_extra_top.index=sample(seq_len(nrow(degree.top.original.sim)),replace = T,size = degree.extra)
vec_extra_top=degree.top.original.sim[vec_extra_top.index,'v.top']
extra.edgelist=data.frame(v.bottom=f,v.top=vec_extra_top)
new.edgelist=rbind(extra.edgelist,new.edgelist)
}

new.edgelist=rbind(new.edgelist,edgelist.original)
}


if (mode=='High') {
  
  #res.simulations=SimNetworkNodeDistribution(alpha.top.nbin,alpha.bottom.1,ntop = n.top.2,nbottom = extra.nodes.bottom,m_ln=m_ln)
  simulated.bottom.extra<- c()
  while (length(simulated.bottom.extra)<extra.nodes.bottom) {
  simulated.bottom.extra.iter=dist_rand(m=m_ln, n = extra.nodes.bottom)
  #data.frame(d=dist_rand(m=m_ln, n = extra.nodes.bottom))%>%arrange(d)%>%mutate(q=ntile(n = 4))%>%mutate(prop=(seq_len(extra.nodes.bottom))/(extra.nodes.bottom))
  quantiles=simulated.bottom.extra.iter%>%ntile(n=4)
  simulated.bottom.extra.iter.top=c(simulated.bottom.extra.iter[quantiles>=4])
  simulated.bottom.extra=c(simulated.bottom.extra.iter.top,simulated.bottom.extra)
  }
  
  for (f in seq_len(length(simulated.bottom.extra))) {
    degree.extra=simulated.bottom.extra[f] 
    vec_extra_top.index=sample(seq_len(nrow(degree.top.original.sim)),replace = T,size = degree.extra)
    vec_extra_top=degree.top.original.sim[vec_extra_top.index,'v.top']
    extra.edgelist=data.frame(v.bottom=f,v.top=vec_extra_top)
    new.edgelist=rbind(extra.edgelist,new.edgelist)
  }
  
  
  new.edgelist=rbind(new.edgelist,edgelist.original)

  }


if (mode=='Low') {
  #res.simulations=SimNetworkNodeDistribution(alpha.top.nbin,alpha.bottom.1,ntop = n.top.2,nbottom = extra.nodes.bottom,m_ln=m_ln)
  simulated.bottom.extra<- c()
  
  while (length(simulated.bottom.extra)<extra.nodes.bottom) {
    simulated.bottom.extra.iter=dist_rand(m=m_ln, n = extra.nodes.bottom)
    #data.frame(d=dist_rand(m=m_ln, n = extra.nodes.bottom))%>%arrange(d)%>%mutate(q=ntile(n = 4))%>%mutate(prop=(seq_len(extra.nodes.bottom))/(extra.nodes.bottom))
    quantiles=simulated.bottom.extra.iter%>%ntile(n=4)
    simulated.bottom.extra.iter.top=c(simulated.bottom.extra.iter[quantiles<=1])
    simulated.bottom.extra=c(simulated.bottom.extra.iter.top,simulated.bottom.extra)
  }

  for (f in seq_len(length(simulated.bottom.extra))) {
    degree.extra=simulated.bottom.extra[f] 
    vec_extra_top.index=sample(seq_len(nrow(degree.top.original.sim)),replace = T,size = degree.extra)
    vec_extra_top=degree.top.original.sim[vec_extra_top.index,'v.top']
    extra.edgelist=data.frame(v.bottom=f,v.top=vec_extra_top)
    new.edgelist=rbind(extra.edgelist,new.edgelist)
  }
  
  
  new.edgelist=rbind(new.edgelist,edgelist.original)
  
}




return(new.edgelist)

}

simBipartiteNetwork<-function(alpha.top.nbin,alpha.bottom.1,m_ln,ntop,nbottom){
res.simulations=SimNetworkNodeDistribution(alpha.top.nbin,alpha.bottom.1,ntop = ntop,nbottom = nbottom,m_ln=m_ln)
simulated.2=SimEdgeCreation(res.simulations=res.simulations)  
return(simulated.2)  
}  

compareCounterfactuals<-function(scenario,simulated.bids.Model1,simulated.bids.Model2,cf.edgelist.Model1,cf.edgelist.Model2){
  baseline.model1.stats1=simulated.bids.Model1%>%summarise(mean.bid=mean(bid),sd.bid=sd(bid))
  baseline.model1.stats2=simulated.bids.Model1%>%group_by(ID)%>%summarise(participants=length(ID))%>%summarise(mean.participants=mean(participants),sd.participants=sd(participants))
  baseline.model1.stats=data.frame(baseline.model1.stats1,baseline.model1.stats2)%>%mutate(Simulation='Baseline',Type='Model 1')%>%pivot_longer(cols = mean.bid:sd.participants)
  
  baseline.model2.stats1=simulated.bids.Model2%>%summarise(mean.bid=mean(bid),sd.bid=sd(bid))
  baseline.model2.stats2=simulated.bids.Model2%>%group_by(ID)%>%summarise(participants=length(ID))%>%summarise(mean.participants=mean(participants),sd.participants=sd(participants))
  baseline.model2.stats=data.frame(baseline.model2.stats1,baseline.model2.stats2)%>%mutate(Simulation='Baseline',Type='Model 2')%>%pivot_longer(cols = mean.bid:sd.participants)
  
  ##Create Counterfactual Statistics
  cf.re.Model1.stats1=cf.edgelist.Model1%>%summarise(mean.bid=mean(bid),sd.bid=sd(bid))
  cf.re.Model1.stats2=cf.edgelist.Model1%>%group_by(ID)%>%summarise(participants=length(ID))%>%summarise(mean.participants=mean(participants),sd.participants=sd(participants))
  cf.re.Model1.stats=data.frame(cf.re.Model1.stats1,cf.re.Model1.stats2)%>%mutate(Simulation='Counterfactual',Type='Model 1')%>%pivot_longer(cols = mean.bid:sd.participants)
  
  cf.re.Model2.stats1=cf.edgelist.Model2%>%summarise(mean.bid=mean(bid),sd.bid=sd(bid))
  cf.re.Model2.stats2=cf.edgelist.Model2%>%group_by(ID)%>%summarise(participants=length(ID))%>%summarise(mean.participants=mean(participants),sd.participants=sd(participants))
  cf.re.Model2.stats=data.frame(cf.re.Model2.stats1,cf.re.Model2.stats2)%>%mutate(Simulation='Counterfactual',Type='Model 2')%>%pivot_longer(cols = mean.bid:sd.participants)
  
  comparison.table=rbind(baseline.model1.stats,baseline.model2.stats,cf.re.Model1.stats,cf.re.Model2.stats)
  
  ##Merge
  comparison.table.output=comparison.table%>%pivot_wider(id_cols = c(Type,name),names_from = Simulation,values_from=value)%>%
    mutate(difference=(Counterfactual-Baseline))
  comparison.table.output2=comparison.table.output%>%dplyr::select(Type,name,difference)%>%
    pivot_wider(id_cols = Type,names_from=name,values_from=difference)%>%mutate(Scenario=scenario)%>%
    dplyr::select(Scenario, everything())%>%mutate_if(is.numeric, funs((signif(., 3))))
  return(comparison.table.output2)
}

#Top Degrees

increaseDensityNetwork<-function(extraProp = 0.1,edgelist.original,mode){
  degree.bottom.sim=edgelist.original%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))
  degree.top.sim=edgelist.original%>%group_by(v.top)%>%summarise(d=length(unique(v.bottom)))
  number_extra_edges=round(nrow(edgelist.original)*(prop.extra),0)
  
  if(mode=='Random'){
    cf.randomedges.extrabottom=degree.bottom.sim$v.bottom[sample(x = seq_len(nrow(degree.bottom.sim)),replace = T,size=number_extra_edges)]
    cf.randomedges.extratop=degree.top.sim$v.top[sample(x = seq_len(nrow(degree.top.sim)),replace = T,size=number_extra_edges)]
    cf.randomedges.edgelist=data.frame(v.bottom=extra.edges.bottom,v.top=extra.edges.top)
    cf.randomedges.edgelist=rbind(cf.randomedges.edgelist,edgelist.original)
    return(cf.randomedges.edgelist)
  }
  
  if(mode=='High'||mode=='Low'){ 
  q_filter=ifelse(mode=='High',yes = 4,no = 1)
    
  edgelist.original.labels=edgelist.original%>%mutate(v.top=paste0('g',v.top),v.bottom=paste0('f',v.bottom))%>%as.matrix()
  graph.sim.original=graph_from_edgelist(el=edgelist.original.labels, directed = F)
  
  #bet <- betweenness(graph.sim.original)%>%as.vector()
  #clos <- closeness(graph.sim.original)
  eig <- eigen_centrality(graph.sim.original)$vector
  centralities=data.frame(v=names(eig),eig=eig)%>%mutate(type=ifelse(substr(v,1,1)=='g',yes = 'v.top',no = 'v.bottom'))
  centralities.bottom=centralities%>%filter(type=='v.bottom')%>%mutate(v.bottom=substr(v,2,nchar(v)))%>%dplyr::select(v.bottom,eig)%>%arrange(-eig)%>%mutate(q=ntile(eig,n = 4))%>%filter(q==q_filter)
  centralities.top=centralities%>%filter(type=='v.top')%>%mutate(v.top=substr(v,2,nchar(v)))%>%dplyr::select(v.top,eig)%>%arrange(-eig)%>%mutate(q=ntile(eig,n = 4))%>%filter(q==q_filter)
  
  cf.randomedges.extrabottom=centralities.bottom$v.bottom[sample(x = seq_len(nrow(centralities.bottom)),replace = T,size=number_extra_edges)]
  cf.randomedges.extratop=centralities.top$v.top[sample(x = seq_len(nrow(centralities.top)),replace = T,size=number_extra_edges)]
  cf.randomedges.edgelist=data.frame(v.bottom=extra.edges.bottom,v.top=extra.edges.top)
  cf.randomedges.edgelist=rbind(cf.randomedges.edgelist,edgelist.original)
  return(cf.randomedges.edgelist)
  
  
  }   



  
}


fitdDegreesBids<-function(degree.bottom,bids){
  bids.min=bids%>%dplyr::select(Codigo,RutProveedor,MCA_MPO)%>%left_join(degree.bottom)
  params.df=data.frame()
  max_deg=max(degree.bottom$d)
  degrees.bottom=seq_len(max_deg)
  #bids.participants=bids.min%>%group_by(d,Codigo)%>%summarise(participants=length(Codigo))
  
  for (d.bottom in degrees.bottom){
    bids.iter=bids.min%>%filter(d==d.bottom)
    
    if(nrow(bids.iter)==0){
      prev=d.bottom-1
      params.iter=params.df%>%filter(d==(d.bottom-1))%>%mutate(d=d.bottom)
    }
    
    if(nrow(bids.iter>0)){
      bids.iter=bids.min%>%group_by(Codigo)%>%mutate(nparticipants=length(Codigo))
      
      
      for (par in seq_len(20)) {
        bids.iter.part=bids.iter%>%filter(nparticipants==par)
        fitnormal=(fitdist(bids.iter.part$MCA_MPO,"norm",method = 'mle',discrete = F))
        mu=fitnormal$estimate[1]%>%as.numeric()
        sd=fitnormal$estimate[2]%>%as.numeric()
        params.iter=data.frame(d=d.bottom,mu=mu,sd=sd,nParticipants=par)
        params.df=rbind(params.iter,params.df)
        
      }
      
      
      #fitnormal=(fitdist(bids.iter$MCA_MPO,"norm",method = 'mle',discrete = F))
      #fitbin=(fitdist(bids.degree$participants,"nbinom",method = 'mle',discrete = T))
      #fitln=(fitdist(bids.degree$participants,"lnorm",method = 'mle',discrete = T))  
      #mu=fitnormal$estimate[1]%>%as.numeric()
      #sd=fitnormal$estimate[2]%>%as.numeric()
      #params.iter=data.frame(d=d.bottom,mu=mu,sd=sd)
    }
    
    
  }
  return(params.df)   
}


