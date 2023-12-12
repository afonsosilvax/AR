library(igraph)

grafo<-read.graph("C://Users/afonso/Documents/trab_links.txt", format = c('edgelist'), directed = F)
plot(grafo)
vcount(grafo)
ecount(grafo)
edge_density(grafo)
mean(degree(grafo,mode= "in"))
table(degree(grafo,mode="in"))
is_connected(grafo)
components(grafo)
assortativity_degree(grafo,directed=F)
mean_distance(grafo,directed=F)
diameter(grafo)
count_triangles(grafo)
sum(count_triangles(grafo))
deg <- degree(grafo,mode="total")
ht <- mean(deg*deg)/(mean(deg)^2)
ht
kc <- coreness(grafo,mode="all")
plot(grafo,vertex.size=kc*10,vertex.label=kc)
kc
comp_gig<-which.max(components(grafo)$csize == 496)

plot(comp_gig)


