Morpho_penalty <- function(TREE){
  Upper_Dend <- TREE[[1]]
  Lower_Dend <- TREE[[2]]

  upper_y <- max(sapply(Upper_Dend,function(Branch){
    return(Branch[["coordi"]][,2])
  }))
  lower_y <- min(sapply(Lower_Dend,function(Branch){
    return(Branch[["coordi"]][,2])
  }))

  return(-max(UPPER_SYNAPTIC_ZONE_Y - upper_y,0) + min(LOWER_SYNAPTIC_ZONE_Y - lower_y,0) + MORPHO_PENALTY_MIEW)
}
