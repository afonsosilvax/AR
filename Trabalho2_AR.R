library(igraph)
library(here)

pa <- function(clique_dim, rede_dim, p, lig_iter){
  grafo <- make_full_graph(clique_dim)

  for (i in (clique_dim + 1):rede_dim) {
    j <- sample(1:(i-1), 1)
    grafo <- add_vertices(grafo, 1)
    grafo <- add_edges(grafo, cbind(i, j))
    jadj <- neighbors(grafo, j, mode = "all")
    
    for (l in 1:2){
      if (runif(1) <= p) {
        adj <- sample(jadj, 1)
        
        while (adj == i || adj %in% neighbors(grafo, i, mode = "all")) {
          adj <- sample(jadj, 1)
        }
        
        grafo <- add_edges(grafo, cbind(i, adj))
      }
      
      else {
        v <- sample(1:(i-1), 1)
        
        while (v %in% neighbors(grafo, i, mode = "all")) {
          v <- sample(1:(i-1), 1)
        }
        
        grafo <- add_edges(grafo, cbind(i, v))
      }
    }
  }
  return(grafo)
}

grafos <- lapply(1:10, function(x) pa(10, 200, 0.8, 3))

h <- c()
c_clust <- c()
d_med <- c()

for (i in 1:10) {
  grafo <- grafos[[i]]
  
  # Parâmetro de heterogeneidade
  deg <- degree(grafo,mode="total")
  h <- c(h, mean(deg*deg)/(mean(deg)^2))

  # Estudo dos triângulos
  c_clust <- c(c_clust, transitivity(grafo, type = c("global")))

  # Distância média
  d_med <- c(d_med, mean_distance(grafo, directed=F))
}

plot(grafo)
boxplot(h)
boxplot(c_clust)
boxplot(d_med)
  
h
c_clust
d_med

grafos <- lapply(1:10, function(x) pa(20, 200, 0.8, 3))

h <- c()
c_clust <- c()
d_med <- c()


for (i in 1:10) {
  grafo <- grafos[[i]]
  
  # Parâmetro de heterogeneidade
  deg <- degree(grafo,mode="total")
  h <- c(h, mean(deg*deg)/(mean(deg)^2))
  
  # Estudo dos triângulos
  c_clust <- c(c_clust, transitivity(grafo, type = c("global")))
  
  # Distância média
  d_med <- c(d_med, mean_distance(grafo,directed=F))
}

plot(grafo)
boxplot(h)
boxplot(c_clust)
boxplot(d_med)
  
h
c_clust
d_med

# Rede do trabalho 1
grafo <- read.graph(here("trab_links.txt"), format = c('edgelist'), directed = F)
plot(grafo)

# Estudo da componente gigante
components<-components(grafo)
comp_gig <- induced.subgraph(grafo, which(components$membership == which.max(components$csize)))
plot(comp_gig)

# Métodos de deteção de comunidades

# Método de Louvain
louvain <- cluster_louvain(comp_gig)