#Lab - Community Detection
#Disha Pandey

if (!require("pacman")) install.packages("pacman")
pacman::p_load(igraph,rlist, packrat, rsconnect)

#To get the working directory
getwd()

#loading the files
edges <- "Edges_sp_data_school_day_2.csv"
nodes <- "Nodes_sp_data_school_day_2.csv"

edgeframe=read.csv(edges, header = TRUE, sep = ",")
nodeframe=read.csv(nodes, header = TRUE, sep = ",")

g_primary_school=graph.data.frame(edgeframe, directed = FALSE, vertices= nodeframe)
g_primary_school <- simplify(g_primary_school)
igraph_options(vertex.size = 17)
V(g_primary_school)$label <- sub("Actor ", "", V(g_primary_school)$name)
V(g_primary_school)$shape <- "circle"
V(g_primary_school)$color <- "maroon"
V(g_primary_school)[V(g_primary_school)$classname=="Class 1B"]
V(g_primary_school)$gender

stud.class <- get.vertex.attribute(g_primary_school, "classname")
stud.gender<- get.vertex.attribute(g_primary_school, "gender")

plot(g_primary_school, layout= layout.fruchterman.reingold.grid, vertex.label = stud.class, vertex.size = 7)

#Fast Greedy Algorithm
fg_c <-  fastgreedy.community(g_primary_school, weights=E(g_primary_school)$weight)
table(membership(fg_c), stud.class, useNA = c("no"))
length(fg_c) 
sizes(fg_c) 

plot(fg_c, g_primary_school, layout = layout.fruchterman.reingold.grid, vertex.size=8, vertex.shape= "circle",edge.curved=.5, vertex.color= V(g_primary_school)$community, vertex.label= stud.class)

#Walktrap Community Algorithm
wt_c <- walktrap.community(g_primary_school, weights = E(g_primary_school)$weight, steps = 4, merges = TRUE, modularity = FALSE)

table(membership(wt_c), stud.class, useNA = c("no"))
sizes(wt_c)
plot(wt_c, g_primary_school, layout = layout.fruchterman.reingold.grid, vertex.size=8, vertex.shape= "circle", edge.curved=.5, pch = 2, vertex.color= V(g_primary_school)$community, vertex.label= stud.class)

#Spinglass Algorithm
sg_c<- spinglass.community(g_primary_school, weights = E(g_primary_school)$weight)
table(membership(sg_c), stud.class, useNA = c("no"))
sizes(sg_c)
plot(sg_c, g_primary_school, layout = layout.fruchterman.reingold.grid, vertex.size=8, vertex.shape= "circle",edge.curved=.5, pch = 2, vertex.color= V(g_primary_school)$community, vertex.label= stud.class)

#Label Propagation Algorithm
lp_c <- cluster_label_prop(g_primary_school, weights = E(g_primary_school)$weight)

table(membership(lp_c), stud.class, useNA = c("no"))
sizes(lp_c)

V(g_primary_school)$community <- lp_c$membership
color <- adjustcolor( c("navy blue", "magenta", "purple", "grey", "orange", "red", "pink", "light blue", "maroon", "yellow", "cyan"), alpha=10)
plot(lp_c ,g_primary_school, vertex.size = 8, vertex.color=color[V(g_primary_school)$community], vertex.shape = "circle",edge.color=c("dark red", "slategrey")[(E(g_primary_school)$type=="hyperlink")+1], rescale = TRUE, vertex.label= stud.class)


#Girvan-Newman method for community detection
gnm <- edge.betweenness.community(g_primary_school, directed=F)

g2<-delete.edges(g_primary_school, gnm$removed.edges[seq(length=which.max(mods)-1)])
V(g_primary_school)$color = clusters(g2)$membership
table(membership(gnm), stud.class, useNA = c("no"))

sizes(gnm)
plot(gnm, g_primary_school, layout = layout.fruchterman.reingold.grid, vertex.label= stud.class, vertex.size = 10)



#For Students in Grade 1 and 5
v_grade1<-V(g_primary_school)[V(g_primary_school)$classname=="Class 1B" | V(g_primary_school)$classname=="Class 1A"]
v_grade5<-V(g_primary_school)[V(g_primary_school)$classname=="Class 5B" | V(g_primary_school)$classname=="Class 5A"]

subgraph_g1<-induced_subgraph(g_primary_school, v_grade1)
subgraph_g5<-induced_subgraph(g_primary_school, v_grade5)


#Fast Greedy algorithm for grade 1
fc1 <-  fastgreedy.community(subgraph_g1, weights=E(subgraph_g1)$weight)

cm5 <- membership(fc1)

stud.class1 <- get.vertex.attribute(subgraph_g1, "classmate")
stud.gender1 <- get.vertex.attribute(subgraph_g1, "gender")
table(cm5, stud.class1, stud.gender1, useNA = c("no"))

#Fast Greedy algorithm for grade 5
fc5 <-  fastgreedy.community(subgraph_g5, weights=E(subgraph_g5)$weight)
cm6 <- membership(fc5)

stud.class5 <- get.vertex.attribute(subgraph_g5, "classmate")
stud.gender5 <- get.vertex.attribute(subgraph_g5, "gender")
table(cm6, stud.class5, stud.gender5, useNA = c("no"))

#
g <- graph.formula(A-B,B-C,C-D, D-E, E-A)
E(g)$sign<-c(+1,-1, -1, +1, 1)
is_connected(g, "strong")
plot(g)
count_signed_triangles(g)

#
g1 <- graph.formula(A-B,A-C,A-D, B-C, B-D, C-D )
E(g1)$sign<-c(+1,1, -1, 1, -1, 1)
is_connected(g1, "strong")
plot(g1)
count_signed_triangles(g1)


community.significance.test <- function(graph, vs, ...){
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

#Testing the community significance for fast greedy algorithm and walk trap algorithm
v_compF1 <- V(g_school)[cm==1]
v_compF2 <- V(g_school)[cm==2]
v_compF3 <- V(g_school)[cm==3]
v_compF4 <- V(g_school)[cm==4]
v_compF5 <- V(g_school)[cm==5]
v_compF6 <- V(g_school)[cm==6]
v_compF7 <- V(g_school)[cm==7]
v_compW1 <- V(g_school)[cm2==1]
v_compW2 <- V(g_school)[cm2==2]
v_compW3 <- V(g_school)[cm2==3]
v_compW4 <- V(g_school)[cm2==4]
v_compW5 <- V(g_school)[cm2==5]
v_compW6 <- V(g_school)[cm2==6]
v_compW7 <- V(g_school)[cm2==7]
v_compW8 <- V(g_school)[cm2==8]
v_compW9 <- V(g_school)[cm2==9]
community.significance.test(g_school, v_compF1)
community.significance.test(g_school, v_compF2)
community.significance.test(g_school, v_compF3)
community.significance.test(g_school, v_compF4)
community.significance.test(g_school, v_compF5)
community.significance.test(g_school, v_compF6)
community.significance.test(g_school, v_compF7)
community.significance.test(g_school, v_compW1)
community.significance.test(g_school, v_compW2)
community.significance.test(g_school, v_compW3)
community.significance.test(g_school, v_compW4)
community.significance.test(g_school, v_compW5)
community.significance.test(g_school, v_compW6)
community.significance.test(g_school, v_compW7)
community.significance.test(g_school, v_compW8)
community.significance.test(g_school, v_compW9)

