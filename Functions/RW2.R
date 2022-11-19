RW2 <- function(k){
  
  rest.comp <- list()
  for(i in 3:(k-2)){
    rest.comp[[i]] <- c(i-2, i-1, i+1, i+2)
  }
  rest.comp <- unlist(rest.comp)
  
  adj = c(2, 3, 1, 3, 4, 
          rest.comp, 
          c(k-3, k-2, k, k-2, k-1)
  )
  
  num = c(2, 3, rep(4, times = c(k-4)), 3, 2)
  
  weights = c(c(2, -1, 2, 4, -1), 
              rep(c(-1, 4, 4, -1), times = c(k-4)),
              c(-1, 4, 2, -1, 2))
  
  retlist <- list()
  retlist$adj <- adj
  retlist$num <- num
  retlist$weights <- weights
  return(retlist)
  
}

