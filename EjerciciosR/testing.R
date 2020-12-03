i <- 1
repeat{
  message("Iteración número: ", i, "... Siguiente"); 
  flush.console()
  if(runif(1) > 0.95) { 
    break
  }
  i <- i + 1   
}  