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

set.seed(1)
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

set.seed(1)

plot(grafo)
boxplot(h, main = "Parâmetro de heterogeneidade")
boxplot(c_clust, main = "Coeficiente de clustering")
boxplot(d_med, main = "Distância média")
  
h
c_clust
d_med

set.seed(1)
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

set.seed(1)

plot(grafo)
boxplot(h, main = "Parâmetro de heterogeneidade")
boxplot(c_clust, main = "Coeficiente de clustering")
boxplot(d_med, main = "Distância média")
  
h
c_clust
d_med

# Rede do trabalho 1
grafo <- read.graph(here("trab_links.txt"), format = c('edgelist'), directed = F)

set.seed(1)
plot(grafo)

# Estudo da componente gigante
components<-components(grafo)
comp_gig <- induced.subgraph(grafo, which(components$membership == which.max(components$csize)))

set.seed(1)
plot(comp_gig)

## Métodos de deteção de comunidades

# Método de Louvain
set.seed(1)
louvain <- cluster_louvain(comp_gig)

# Número e dimensão das comunidades obtidas
sizes(louvain)

set.seed(1)
plot(louvain, comp_gig)


# Método de Edge Betweenness
set.seed(1)
ceb <- cluster_edge_betweenness(comp_gig)

# Número e dimensão das comunidades obtidas
sizes(ceb)

set.seed(1)
plot(ceb, comp_gig)

# Método de Fast Greedy
set.seed(1)
clm <- cluster_fast_greedy(comp_gig)

# Número e dimensão das comunidades obtidas
sizes(clm)

set.seed(1)
plot(clm, comp_gig)


# Metodo Label Propagation
set.seed(1)
clp <- cluster_label_prop(comp_gig)

# Número e dimensão das comunidades obtidas
sizes(clp)

set.seed(1)
plot(clp, comp_gig)