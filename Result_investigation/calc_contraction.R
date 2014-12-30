calc_contraction <- function(TREE){

  ratio <- c()
  
  for(Dendrite in TREE){

    proximal_coordi <- Dendrite[[1]][["coordi"]][1,]#root上のDendriteの付け根
    
    for(Branch in Dendrite){
      connect <- Branch[["connect"]]
      if(is.numeric(connect) && connect == 0){#末端だったら
        path_length <- Branch[["path_leng"]]
        tip_coordi <- Branch[["coordi"]][2,]
        euclid_dist <- sqrt(sum((tip_coordi - proximal_coordi)^2))

        ratio <- c(ratio,euclid_dist/path_length)
      }
    }
  }

  return(ratio)
}

