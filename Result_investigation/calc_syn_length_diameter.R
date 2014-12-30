calc_syn_length_diameter <- function(Dends){
  syn_length_diameter <- c()
  
  for(Dendrite in Dends){
    for(Branch in Dendrite){
      if(is.matrix(Branch[["synapse"]])){
        path_leng <- Branch[["path_leng"]]
        length <- Branch[["length"]]
        diam <- Branch[["diam"]]
        syn_length_diameter <- c(syn_length_diameter,
                                 sapply(Branch[["synapse"]][,1],function(position_rate){
                                   return((path_leng - (1 - position_rate)*length)/diam)
                                 }))
      }
    }
  }

  return(mean(syn_length_diameter))
}
