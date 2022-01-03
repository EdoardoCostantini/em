# Project:   em
# Objective: Sweep operator according to Goodnight 1979
# Author:    Edoardo Costantini
# Created:   2021-11-23
# Modified:  2021-11-23

# Personal Sweep function
sweepGoodnight <- function (A, target){

# Based on Goodnight 1979 p. 154

  for(k in target){
    # Step 1: Let D = a_kk
    D <- A[k, k]

    # Step 2: Divide row k by D.
    A[k, ] <- A[k, ] / D

    # Step 3:
    # - For every other row i != k, let B = a_ik
    # - Subtract B \times row k from row i.
    # - set a_ik = -B/D.
    for(i in 1:nrow(A)){
      if(i != k){
        B <- A[i, k]
        A[i, ] <- A[i, ] - B * A[k, ]
        A[i, k] <- -1 * B / D
      }
    }
    # Step 4: Set a_kk = 1/D
    A[k, k] = 1/D
  }

  # Output
  return(A)
}
