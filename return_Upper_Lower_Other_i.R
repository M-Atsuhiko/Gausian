return_Upper_Lower_Other_i <- function(TREE){
  Upper_Dend <- c()
  Lower_Dend <- c()
  Other_Dend <- c()
  
  for(Dend_i in 1:length(TREE)){
    frag <- 0
    Dend <- TREE[[Dend_i]]
    for(Branch in Dend){
      if(is.matrix(Branch[["synapse"]])){
        if(Branch[["synapse"]][1,2] == UPPER_SYNAPTIC_ZONE_INDEX){
          Upper_Dend <- c(Upper_Dend,Dend_i)
          frag <- 1
        }else{
          Lower_Dend <- c(Lower_Dend,Dend_i)          
          frag <- 2
        }
        break
      }
    }
    if(frag == 0)
      Other_Dend <- c(Other_Dend,Dend_i)
  }
  
  TREE_named_i <- list(Upper_Dend,
                       Lower_Dend,
                       Other_Dend)

  names(TREE_named_i) <- TREE_Labels
  
  return(TREE_named_i)
}
