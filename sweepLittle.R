# Project:   em
# Objective: Sweep operator according to Little Rubin 2002
# Author:    Edoardo Costantini
# Created:   2021-11-23
# Modified:  2021-11-26

sweepLittle <- function (G, target){
  H <- G
  for(k in target){
    # print(k)
    # Step 1
    H[k, k] <- -1/G[k, k]

    # Step 2: Replace elements in k-th row and column
    H[k, -k] <- -G[k, -k] * H[k, k]
    H[-k, k] <- -G[-k, k] * H[k, k]

    # Step 3:
    # - For every other row i != k, let B = a_ik
    # - Subtract B \times row k from row i.
    # - set a_ik = -B/D.
    for(j in 1:nrow(G)){
      for (l in 1:ncol(G)){
        if(j != k & l != k){
        # print(paste0(j, " ", l))
          H[j, l] <- G[j, l] - H[j, k] * G[k, l]
        }
      }
    }
  }

  # Output
  return(H)
}