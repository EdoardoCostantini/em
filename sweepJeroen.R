# Project:   em
# Objective: Sweep operator according to my translation of Jeroen R code
# Author:    Edoardo Costantini
# Created:   2021-11-23
# Modified:  2021-11-23

G <- matrix(c(-1, NA, NA,
              2, 8, NA,
              4, 16, 32), ncol = 3, byrow = TRUE)
target <- c(1)

sweepC <- function (G, target){
  r <- ncol(G)
  for(k in target){
    # Step 1: Let D = a_kk
    division <- G[k, k]
    G[k, k] <- -1
    for(j in 1:r){
      G[j, k] <- G[j, k] / division
    }
    for(j in 1:r){
      j <- 2
      if(j != k){
        product <- G[j, k]
        for(l in j:r){
          l <- j
          if(l != k) {
            G[j, l] <- G[j, l] - G[k, l] * product
            G[l, j] <- G[j, l]
          }
        }
      }
    }
    for(j in 1:r){
      G[k, j] <- G[j, k]
    }
  }

  # Output
  return(G)
}
