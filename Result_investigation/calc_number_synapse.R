calc_number_synapse <- function(Dends){

  num_synapse <- 0
  
  for(Dend in Dends){
    for(Branch in Dend){
      if(is.matrix(Branch[["synapse"]])){
        num_synapse <- c(num_synapse,nrow(Branch[["synapse"]]))
      }
    }
  }

  return(sum(num_synapse))
}
