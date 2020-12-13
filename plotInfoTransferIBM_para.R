# Function to plot results from information transfer IBM
# developed by Zach Bell and Jerod Merkle
# last update: June 2019


plotSocialIBMResults <- function(results = results, ind = ind, pop = pop, interactions = interactions, result.folder = folder){
  if(all(c("stringr","igraph","Matrix") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: igraph, stringr, and Matrix.")
  require(stringr)
  require(igraph)
  require(Matrix)
  
  # load up results files
  #load(paste0(result.folder,"/interaction_matricies.RData"))
  #load(paste0(result.folder,"/population_data.RData"))  #don't really need this if ind data is written out
  #load(paste0(result.folder,"/individual_data.RData"))
  #results <- read.csv(paste0(result.folder,"/population_stats.csv"))
  head(results)
  
  #plot general summary stats figure and save
  png(paste0(result.folder,"/summary_stats.png"), width=4, height=6, units="in", res=400)
  par(mfrow=c(3,2), mai=c(.3,.3,.03,.03), mgp=c(1,.1,0),
      tck=-0.02, cex.axis=.8)
  plot(results$t, results$pop.size, type="l",ylab="N", xlab="Time step", lwd=3,ylim=c(0,max(results$pop.size)))
  plot(results$t, results$births, type="l",ylab="Births", xlab="Time step", lwd=3,ylim=c(0,max(c(results$births,results$deaths),na.rm=TRUE)))
  plot(results$t, results$deaths, type="l",ylab="Deaths", xlab="Time step", lwd=3,ylim=c(0,max(c(results$births,results$deaths),na.rm=TRUE)))
  plot(results$t, results$med.age, type="l",ylab="Median age", xlab="Time step", lwd=3, ylim=c(0,max(results$med.age)))
  plot(results$t, results$frac.informed, type="l", 
       ylim=c(0,1.2),ylab="Proportion", xlab="Time step", lwd=3)
  lines(results$t, 1-results$frac.informed, col="grey",lwd=3)
  legend("top", c("Informed","Uninformed"), lty=1, col=c("black","grey"), 
         inset=0.02, horiz=T, cex=.8)
  while (!is.null(dev.list()))  dev.off()    #check your results folder for this figure!!!!!
  
  # build a results table of network stats and save out graph figures
  results.graph <- do.call(rbind, lapply(2:length(interactions), function(i){
    
    print(paste0(round(i/length(interactions)*100,0), "% Completed."))
    
    #create the graph for the given time step
    tmp <- interactions[[i]]
    g <- graph_from_adjacency_matrix(tmp, diag = FALSE, mode="undirected", weighted = TRUE)
    
    #manage the folders where the graphs will go
    dr <- paste0(result.folder,"/graphs")
    
    if(i == 2){
      unlink(dr, recursive=TRUE)
      dir.create(dr)
    }
    
    # save out the plot of the graph
    E(g)$width <- E(g)$weight*2
    lo <- layout_nicely(g)
    png(paste0(dr,"/graph",str_pad(i-1,3,pad="0"),".png"), width=5, height=5, units="in", res=400)
    par(mai=c(.03,.03,.03,.03))
    plot(g, layout=lo, vertex.label.cex=.4, vertex.size=7, edge.curved=.1, vertex.label=pop[[i]])
    while (!is.null(dev.list()))  dev.off()
    
    #write out dataframe with dereived metrics from the graph
    return(data.frame(t=i-1, no_ids=length(V(g)),
                      edge_density=edge_density(g), #The proportion of present edges from all possible edges in the network.
                      diameter=diameter(g, directed=F, weights=NULL),   #  length of the longest geodesic (or longest distance)
                      mean_distance=mean_distance(g),            # mean length of geodesics
                      degree=mean(degree(g, normalized=TRUE)),
                      betweenness=mean(betweenness(g, normalized=TRUE)),    #the mean number of geodesics (shortest paths) going through a vertex or an edge.
                      eigen_centrality=eigen_centrality(g, weights=E(g)$weight, scale=TRUE)$value,    # mean centrality proportional to the sum of connection centralities
                      authority=hub.score(g, weights=E(g)$weight, scale=TRUE)$value,   #hub or authority score 
                      transitivity=transitivity(g, type="global", weights=E(g)$weight)))   #probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.
    
  }))
  head(results.graph)
  if(max(results.graph$edge_density) != 0){
  # main plotting of graph results over time
  # png(paste0(result.folder,"/summary_graph_stats.png"), width=5, height=5, units="in", res=400)
  # par(mfrow=c(3,3), mai=c(.3,.3,.03,.03), mgp=c(1,.1,0),
  #    tck=-0.02, cex.axis=.8)
  for(i in 2:ncol(results.graph)){
    png(paste0(result.folder,"/graphs","/graph", colnames(results.graph)[i],".png"), res=400)
    par(mai=c(.03,.03,.03,.03))
    plot(results.graph[,1], results.graph[,i], type="l",xlab="Time step", ylab=names(results.graph)[i], lwd=3, ylim = c(0, max(results.graph[,i])))
    while (!is.null(dev.list()))  dev.off()
  }
  }
  while (!is.null(dev.list()))  dev.off()   #check your results folder for this and other figures!!!!!
  
  write.csv(results.graph, "network_stats.csv", quote = F, row.names = F)
  return("Check your results folder!!! There are 2 new summary figures as well as a folder full of network plots.")
}
