library(igraph)

pa <- function(clique_dim, rede_dim, p, lig_iter){
  grafo <- make_full_graph(clique_dim)

  for (i in (clique_dim + 1):rede_dim) {
    j <- sample(1:(i-1), 1)
    grafo <- add_vertices(grafo, 1)
    grafo <- add_edges(grafo, cbind(i, j))
    jadj <- neighbors(grafo, j, mode = "all")
    
    c <- 1
    
    while (c < lig_iter) {
      for (adj in jadj) {
        if (runif(1) < 1 & c < lig_iter & adj != i) {
          grafo <- add_edges(grafo, cbind(i, adj))
          c <- c + 1
        }
      }
      
      while (c < lig_iter) {
        v <- sample(1:(i-1), 1)
        
        if (v %in% neighbors(grafo, i, mode = "all")) {
          grafo <- add_edges(grafo, cbind(i, v))
          c <- c + 1
        }
      }
    }
  }
  return(grafo)
}

grafos <- lapply(1:10, function(x) pa(10, 200, 0.8, 3))

for (i in 1:10) {
  grafo <- grafos[[i]]
  
  # Parâmetro de heterogeneidade
  deg <- degree(grafo,mode="total")
  print(mean(deg*deg)/(mean(deg)^2))

  # Estudo dos triângulos
  print(transitivity(grafo, type = c("global")))

  # Distância média
  print(mean_distance(grafo,directed=F))
}

grafos <- lapply(1:10, function(x) pa(20, 200, 0.8, 3))

for (i in 1:10) {
  grafo <- grafos[[i]]
  
  # Parâmetro de heterogeneidade
  deg <- degree(grafo,mode="total")
  print(mean(deg*deg)/(mean(deg)^2))
  
  # Estudo dos triângulos
  print(transitivity(grafo, type = c("global")))
  
  # Distância média
  print(mean_distance(grafo,directed=F))
}
