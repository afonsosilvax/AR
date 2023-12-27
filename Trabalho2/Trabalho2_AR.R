library(igraph)

pa <- function(){
  grafo <- make_full_graph(10)
  p <- 0.8
  
  for (i in 11:200) {
    j <- sample(1:(i-1), 1)
    grafo <- add_vertices(grafo, 1)
    grafo <- add_edges(grafo, cbind(i, j))
    jadj <- neighbors(grafo, j, mode = "all")
    
    c <- 1
    
    while (c < 3) {
      for (adj in jadj) {
        if (runif(1) < 1 & c < 3 & adj != i) {
          grafo <- add_edges(grafo, cbind(i, adj))
          c <- c + 1
        }
      }
      
      while (c < 3) {
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

grafos <- lapply(1:10, function(x) pa())

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
