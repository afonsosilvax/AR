library(igraph)
library(here)
 
grafo<-read.graph(here("trab_links.txt"), format = c('edgelist'), directed = F)
plot(grafo)

# Numero de nodos
vcount(grafo)

# Numero de ligações
ecount(grafo)

# Densidade da rede
edge_density(grafo)

# Grau médio
mean(degree(grafo,mode= "in"))

# Distribuição de grau
table(degree(grafo,mode="in"))
degree_distribution(grafo)

log10(vcount(grafo))

is_connected(grafo)

# Estudo das componentes
components<-components(grafo)

components$no
min(components$csize)
max(components$csize)

# Estudo da associação do grau
assortativity_degree(grafo,directed=F)

# Distâncias mais curtas
mean_distance(grafo,directed=F)
diameter(grafo)

# Estudo dos triângulos
transitivity(grafo, type = c("global"))
count_triangles(grafo)
sum(count_triangles(grafo))

# Parâmetro de heterogeneidade
deg <- degree(grafo,mode="total")
ht <- mean(deg*deg)/(mean(deg)^2)
ht

# Decomposição de core da rede
kc <- coreness(grafo,mode="all")

max(kc)
table(kc)
plot(grafo,vertex.size=kc*10,vertex.label=kc)
kc

# Estudo da componente gigante
comp_gig<-induced.subgraph(grafo, which(components$membership == which.max(components$csize)))
plot(comp_gig)

# Numero de nodos da componente gigante
vcount(comp_gig)

# Numero de ligações da componente gigante
ecount(comp_gig)

# Densidade da componente gigante
edge_density(comp_gig)

# Grau médio da componente gigante
mean(degree(comp_gig,mode= "in"))

# Distribuição de grau da componente gigante
table(degree(grafo,mode="in"))
degree_distribution(comp_gig)

is_connected(comp_gig)

# Estudo da associação do grau da componente gigante
assortativity_degree(comp_gig,directed=F)

# Distâncias mais curtas
mean_distance(comp_gig,directed=F)
diameter(comp_gig)

log10(vcount(comp_gig))

# Estudo dos triângulos da componente gigante
transitivity(comp_gig, type = c("global"))
count_triangles(comp_gig)
sum(count_triangles(comp_gig))

# Parâmetro de heterogeneidade da componente gigante
deg <- degree(comp_gig,mode="total")
ht <- mean(deg*deg)/(mean(deg)^2)
ht

# Decomposição de core da componente gigante
core_decomp <- coreness(comp_gig)
max(core_decomp)
table(core_decomp)