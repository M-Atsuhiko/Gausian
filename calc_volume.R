calc_volume <- function(TREE){
  V <- sum(sapply(TREE,function(Dendrite){
    return(sum(sapply(Dendrite,function(Branch){
      return(((Branch[["diam"]]/2)^2)*pi*Branch[["length"]])
      })))
    }))
  return(V)
}
