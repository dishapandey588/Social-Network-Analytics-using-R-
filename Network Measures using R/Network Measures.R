#Lab - Network Measures
#Disha Pandey

if (!require("pacman")) install.packages("pacman")
pacman::p_load(igraph,rlist, packrat, rsconnect)

#To get the working directory
getwd()
#Loading the data file
inline <- read.csv("MergerNet_Jan21_2016_forR.csv")

#Generating a combined acquisition graph
g_acq=graph_from_data_frame(inline, directed = TRUE, vertices= NULL)

## To check whether Self_loops exist, as do multiple edges
is_simple(g_acq)

E(g_acq)$weight 
g_acq_simpl<-simplify(g_acq)


#For directed graph, we check whether it is "strongly" connected
is_connected(g_acq_simpl, "strong")

# Will use the inverse of log weight for shortest path calculations
E(g_acq_simpl)$weight 
inv_weight<-1/log(E(g_acq_simpl)$weight  + 1)
num_weight<-E(g_acq_simpl)$weight
length(inv_weight)

#Subnet inverse of log weights
E(sub_net)$weight 
inv_weight_subnet <- 1/log(E(sub_net)$weight  + 1)
num_weight <- E(sub_net)$weight 
length(inv_weight_subnet)

# Remove disconnected components to create a strongly connected component. We will use this component to calculate closeness and shortest path distances.
g_acq_scc <-g_acq_simpl - vertices('814', '925', '928')
inv_weight_scc<-1/log(E(g_acq_scc)$weight  + 1)

sub_net<-induced_subgraph(g_acq_simpl, v=c('511', '541',
                                           '518', '519', '517', '325', '423', '446', '512', '523',
                                           '561', '621', '115', '482', '485', '487', '491', '492',
                                           '521', '712'))

#Creating a sub-network graph
igraph_options(vertex.size = 15)

V(g_acq_simpl)$color <- "maroon"
plot(g_acq_simpl, layout = layout_with_kk, rescale = TRUE, 
     vertex.color = V(g_acq_simpl)$color, edge.color = "lightblue")

V(sub_net)$shape <- "circle"
V(sub_net)$color <- "magenta"
V(sub_net)[c("511", "541", "518", "519")]$shape <- "rectangle"
V(sub_net)[c("511", "541", "518", "519")]$color <- "cyan" 
plot(sub_net, layout = layout_with_kk, rescale = TRUE, 
     vertex.color=V(sub_net)$color)


V(sub_net)$color <- "#cb9d06"
V(sub_net)[c("511", "541", "518", "519")]$color <- "#653780" 
close <- closeness(sub_net, normalized=T,mode="All", weights = inv_weight_subnet)
plot(sub_net, layout = layout_with_kk, rescale = TRUE, 
     vertex.color = V(sub_net)$color, edge.color = "grey", vertex.size=close*20)


