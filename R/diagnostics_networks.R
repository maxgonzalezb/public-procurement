library(igraph)

#Declare Graph
edgelist.period1=df.period1%>%dplyr::select(RutProveedor,NombreOrganismo)%>%rename(firm=RutProveedor,gov=NombreOrganismo)%>%as.matrix()
edgelist.period2=df.period2%>%dplyr::select(RutProveedor,NombreOrganismo)%>%rename(firm=RutProveedor,gov=NombreOrganismo)%>%as.matrix()
graph.bids=graph_from_edgelist(edgelist.period1, directed = F)
graph.bids.2=graph_from_edgelist(edgelist.period2, directed = F)

bipartite.mapping(graph.bids)
bipartite.mapping(graph.bids.2)
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

#Simulated Comparison
head(edgelist.sim.2)
edgelist.sim.2=simulated.2%>%mutate(v.top=paste0('g',v.top),v.bottom=paste0('f',v.bottom))%>%as.matrix()
graph.sim.2=graph_from_edgelist(edgelist.sim.2, directed = F)
bipartite.mapping(graph.sim.2)
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

#Calculate different types of metrics for the network
clust.sim2=transitivity(graph.sim.2.firms, type = "average")
dist.sim2=average.path.length(graph.sim.2.firms,directed = F)
dens.sim2=graph.density(graph.sim.2.firms,loop=FALSE)

###Comparing Compare with metrics of network 2. 
print(abs(clust.period2-clust.sim2)/clust.period2)
print(abs(dist.period2-dist.sim2)/dist.period2)
print(abs(dens.period2-dens.sim2)/dens.period2)


#Plot the networks
degree.bottom.1=df.period1%>%group_by(RutProveedor)%>%summarise(d=length(unique(NombreOrganismo)))%>%arrange(-d)%>%mutate(v.bottom=seq_len(nrow(degree.bottom.1)))
edgelist.period1=df.period1%>%dplyr::select(RutProveedor,NombreOrganismo)%>%left_join(lookup_names.top)%>%left_join(lookup_names.bottom)%>%dplyr::select(v.top,v.bottom)
edgelist.period1.g=edgelist.period1%>%mutate(v.top=paste0('g',v.top),v.bottom=paste0('v',v.bottom))
graph.bids.1=graph_from_edgelist(edgelist.period1.g%>%as.matrix(), directed = F)
V(graph.bids.1)$type <- bipartite_mapping(graph.bids.1)$type  ## Add the "type" attribute

V(graph.bids.1)$color <- ifelse(V(graph.bids)$type, "lightblue", "salmon")
V(graph.bids.1)$shape <- ifelse(V(graph.bids)$type, "circle", "square")
E(graph.bids.1)$color <- "lightgray"

write_graph(graph.bids.1,format = 'graphml',file ='C:\\repos\\public-procurement\\data\\graphperiod1.graphml')


gexf.1<-igraph.to.gexf(graph.bids.1)
print(file = 'C:\\repos\\public-procurement\\data\\graph1.gexf',gexf.1 )
write.gexf(graph.bids.1,output='C:\\repos\\public-procurement\\data\\graph1.gexf')

plot.graph<-plot(graph.bids.1, vertex.label.cex = 0.8, vertex.label.color = "black")
plot(graph.bids.1, layout=layout.bipartite, vertex.size=1, vertex.label.cex=0.01)


plot.graph<-plot(graph.bids, vertex.label.cex = 0.8, vertex.label.color = "black")
library(rgexf)

