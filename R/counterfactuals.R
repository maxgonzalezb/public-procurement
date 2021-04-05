#Counterfactuals
#1. Effect of entry
prop.extra=0.10
replicas=20
tableEntryResults.final=data.frame()
for (i in seq_len(replicas)) {
print(i)
#1.1 Random Entry of New Firms
cf.randomentry.edgelist=increaseEntryFirms(n.top.2.original = n.top.2,n.bottom.2.original = n.bottom.2,extraProp = prop.extra,edgelist.original =simulated.2,mode = 'Random' )
cf.randomentry.bdegree=cf.randomentry.edgelist%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))

## Create New Models
cf.randomentry.Model1=createBidSim_Mechanic(edgelist.sim.2 = cf.randomentry.edgelist,degree.bottom.sim = cf.randomentry.bdegree,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
cf.randomentry.Model2=createBidSim_NetworkEffects(simulated.2 = cf.randomentry.edgelist,errorGov =0.17, sdparam=0.11)

table.comparison.randomentry=compareCounterfactuals(scenario='Random Degree Entry',simulated.bids.Model1 =simulated.bids.Model1,simulated.bids.Model2 = simulated.bids.Model2,
                                          cf.edgelist.Model1 = cf.randomentry.Model1,cf.edgelist.Model2 = cf.randomentry.Model2)

#1.2 Entry of New Firms - High Degree
cf.highentry.edgelist=increaseEntryFirms(n.top.2.original = n.top.2,n.bottom.2.original = n.bottom.2,extraProp = prop.extra,edgelist.original =simulated.2,mode='High' )
cf.highentry.bdegree=cf.highentry.edgelist%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))

cf.highentry.Model1=createBidSim_Mechanic(edgelist.sim.2 = cf.highentry.edgelist,degree.bottom.sim = cf.highentry.bdegree,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
cf.highentry.Model2=createBidSim_NetworkEffects(simulated.2 = cf.highentry.edgelist,errorGov =0.17, sdparam=0.11)

table.comparison.highdegree=compareCounterfactuals(scenario='High Degree Entry',simulated.bids.Model1 =simulated.bids.Model1,simulated.bids.Model2 = simulated.bids.Model2,
                                                    cf.edgelist.Model1 = cf.highentry.Model1,cf.edgelist.Model2 = cf.highentry.Model2)
#1.3 Entry of New Firms - Low Degree
cf.lowentry.edgelist=increaseEntryFirms(n.top.2.original = n.top.2,n.bottom.2.original = n.bottom.2,extraProp = prop.extra,edgelist.original =simulated.2,mode='Low' )
cf.lowentry.bdegree=cf.lowentry.edgelist%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))

cf.lowentry.Model1=createBidSim_Mechanic(edgelist.sim.2 = cf.lowentry.edgelist,degree.bottom.sim = cf.lowentry.bdegree,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
cf.lowentry.Model2=createBidSim_NetworkEffects(simulated.2 = cf.lowentry.edgelist,errorGov =0.17, sdparam=0.11)

table.comparison.lowdegre=compareCounterfactuals(scenario='Low Degree Entry',simulated.bids.Model1 =simulated.bids.Model1,simulated.bids.Model2 = simulated.bids.Model2,
                                                   cf.edgelist.Model1 = cf.lowentry.Model1,cf.edgelist.Model2 = cf.lowentry.Model2)

tableEntry.all=rbind(table.comparison.randomentry,table.comparison.highdegree,table.comparison.lowdegre)%>%mutate_if(is.numeric, funs((signif(., 2))))%>%mutate(iter=i)
tableEntryResults.final=rbind(tableEntry.all,tableEntryResults.final)
}

tableEntryResults.final.output=tableEntryResults.final%>%group_by(Scenario,Type)%>%summarise(mean.bid=mean(mean.bid),
                                                              mean.sd.bid=mean((sd.bid)),
                                                              mean.participants=mean(mean.participants),
                                                              sd.participants=mean(sd.participants))

positions <- c("Low Degree Entry", "Random Degree Entry", "High Degree Entry")
tableEntryResults.final.output.ggplot=tableEntryResults.final.output%>%mutate(Scenario=factor(Scenario,levels = positions))%>%dplyr::select(Scenario:mean.bid,mean.participants)%>%pivot_longer(cols = mean.bid:mean.participants)

p1.entry<-ggplot(tableEntryResults.final.output.ggplot%>%filter(name=='mean.bid'),aes(x=Type,y=value,fill=Scenario))+
                                      geom_bar(color='black',stat = 'identity',position='dodge')+theme_bw()+theme(legend.title = element_blank(),
                                       legend.position=c(.750,.80),axis.text.x = element_text(size=14),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+
                                              ylab('PP Change')+xlab('')+ scale_fill_brewer(palette="OrRd",limits = positions)+ggtitle('Effect of Entry on Average Bid')
p2.entry<-ggplot(tableEntryResults.final.output.ggplot%>%filter(name=='mean.participants'),aes(x=Type,y=value,fill=Scenario))+
                                      geom_bar(color='black',stat = 'identity',position='dodge')+theme_bw()+theme(legend.title = element_blank(),
                                       legend.position=c(.23,.80),axis.text.x = element_text(size=14),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+
                                              ylab('PP Change')+xlab('')+ scale_fill_brewer(palette="OrRd",limits = positions)+ggtitle('Effect of Entry on Average Participants')
p.entry=plot_grid(p1.entry,p2.entry)
p.entry

##2.Densification of the network
prop.extra=0.10
replicas=10
tableEdgesResults.final=data.frame()
for (i in seq_len(replicas)) {
  print(i)
#2.1 Edges are created at random
cf.randomedges.edgelist=increaseDensityNetwork(extraProp = prop.extra,edgelist.original = simulated.2,mode = 'Random')
cf.randomedges.bdegree=cf.randomedges.edgelist%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))

cf.randomedges.Model1=createBidSim_Mechanic(edgelist.sim.2 = cf.randomedges.edgelist,degree.bottom.sim = cf.randomedges.bdegree,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
cf.randomedges.Model2=createBidSim_NetworkEffects(simulated.2 = cf.randomedges.edgelist,errorGov =0.17, sdparam=0.11)

table.comparison.randomedges=compareCounterfactuals(scenario='Random Edge Creation',simulated.bids.Model1 =simulated.bids.Model1,simulated.bids.Model2 = simulated.bids.Model2,
                                                 cf.edgelist.Model1 = cf.randomedges.Model1,cf.edgelist.Model2 = cf.randomedges.Model2)


#2.2 Edges are created in more central nodes
cf.highedges.edgelist=increaseDensityNetwork(extraProp = prop.extra,edgelist.original = simulated.2,mode = 'High')
cf.highedges.bdegree=cf.randomedges.edgelist%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))

cf.highedges.Model1=createBidSim_Mechanic(edgelist.sim.2 = cf.highedges.edgelist,degree.bottom.sim = cf.highedges.bdegree,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
cf.highedges.Model2=createBidSim_NetworkEffects(simulated.2 = cf.highedges.edgelist,errorGov =0.17, sdparam=0.11)

table.comparison.highedges=compareCounterfactuals(scenario='High Centrality Edge Creation',simulated.bids.Model1 =simulated.bids.Model1,simulated.bids.Model2 = simulated.bids.Model2,
                                                    cf.edgelist.Model1 = cf.highedges.Model1,cf.edgelist.Model2 = cf.highedges.Model2)


#2.2 Edges are created in less central nodes
cf.lowedges.edgelist=increaseDensityNetwork(extraProp = prop.extra,edgelist.original = simulated.2,mode = 'Low')
cf.lowedges.bdegree=cf.randomedges.edgelist%>%group_by(v.bottom)%>%summarise(d=length(unique(v.top)))

cf.lowedges.Model1=createBidSim_Mechanic(edgelist.sim.2 = cf.lowedges.edgelist,degree.bottom.sim = cf.lowedges.bdegree,lambda.df = lambda.df,params.df = params.df,lm.mu = lm.mu,lm.sd = lm.sd)
cf.lowedges.Model2=createBidSim_NetworkEffects(simulated.2 = cf.lowedges.edgelist,errorGov =0.17, sdparam=0.11)

table.comparison.lowedges=compareCounterfactuals(scenario='Low Centrality Edge Creation',simulated.bids.Model1 =simulated.bids.Model1,simulated.bids.Model2 = simulated.bids.Model2,
                                                  cf.edgelist.Model1 = cf.lowedges.Model1,cf.edgelist.Model2 = cf.lowedges.Model2)

tableEdges.all=rbind(table.comparison.randomedges,table.comparison.highedges,table.comparison.lowedges)%>%mutate_if(is.numeric, funs((signif(., 2))))%>%mutate(iter=i)
tableEdgesResults.final=rbind(tableEdges.all,tableEdgesResults.final)
}

tableEdgesResults.final.output=tableEdgesResults.final%>%group_by(Scenario,Type)%>%summarise(mean.bid=mean(mean.bid),
                                                              mean.sd.bid=mean((sd.bid)),
                                                              mean.participants=mean(mean.participants),
                                                              sd.participants=mean(sd.participants))

positions.edges <- c('Low Centrality Edge Creation', 'Random Edge Creation','High Centrality Edge Creation')
tableEdgesResults.final.output.ggplot=tableEdgesResults.final.output%>%mutate(Scenario=factor(Scenario,levels = positions.edges))%>%dplyr::select(Scenario:mean.bid,mean.participants)%>%pivot_longer(cols = mean.bid:mean.participants)

p1.edges<-ggplot(tableEdgesResults.final.output.ggplot%>%filter(name=='mean.bid'),aes(x=Type,y=value,fill=Scenario))+
  geom_bar(color='black',stat = 'identity',position='dodge')+theme_bw()+theme(legend.title = element_blank(),
                                                                              legend.position=c(.750,.80),axis.text.x = element_text(size=14),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+
  ylab('PP Change')+xlab('')+ scale_fill_brewer(palette="OrRd",limits = positions.edges)+ggtitle('Effect of Density on Average Bid')
p2.edges<-ggplot(tableEdgesResults.final.output.ggplot%>%filter(name=='mean.participants'),aes(x=Type,y=value,fill=Scenario))+
  geom_bar(color='black',stat = 'identity',position='dodge')+theme_bw()+theme(legend.title = element_blank(),
                                                                              legend.position=c(.23,.80),axis.text.x = element_text(size=14),legend.box.background = element_rect(colour = "black"),legend.background = element_blank())+
  ylab('PP Change')+xlab('')+ scale_fill_brewer(palette="OrRd",limits = positions.edges)+ggtitle('Effect of Density on Average Participants')
p.edges=plot_grid(p1.edges,p2.edges)
p.edges

