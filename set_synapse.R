set_synapse <- function(Dendrite){
                                        #  cat("========== SET SYNAPSE ==========\n")
  for(i in 1:length(Dendrite)){
    Branch <- Dendrite[[i]]
    
    length <- Branch[["length"]]
    coordi <- Branch[["coordi"]]
    nseg <- Branch[["nseg"]]
    
    synapse_ratio_matrix <- c()

    p1 <- coordi[1,]
    p2 <- coordi[2,]

    comp_vect <- (1/nseg)*(p2 - p1)#単一コンパートメントの方向ベクトル
    
    for(n in 1:nseg){
      p1_prime <- (n - 1)*comp_vect + p1
      p2_prime <- comp_vect + p1_prime

                                        #synapse_position_ratioの計算
      synapse_coordi <- (p2_prime - p1_prime)/2 + p1_prime
      synapse_position_ratio <- sqrt(sum((synapse_coordi - p1)^2))/length
      
                                        #UPPER SYNAPSEの判定
      if((p1_prime[2] > UPPER_SYNAPTIC_ZONE_Y && p1_prime[2] < UPPER_SYNAPTIC_ZONE_BOUNDARY_Y) ||
         (p2_prime[2] > UPPER_SYNAPTIC_ZONE_Y && p2_prime[2] < UPPER_SYNAPTIC_ZONE_BOUNDARY_Y)){
        synapse_ratio_matrix <- rbind(synapse_ratio_matrix,c(synapse_position_ratio,UPPER_SYNAPTIC_ZONE_INDEX))
      }
                                        #LOWER SYNAPSEの判定
      else if((p1_prime[2] < LOWER_SYNAPTIC_ZONE_Y && p1_prime[2] > LOWER_SYNAPTIC_ZONE_BOUNDARY_Y) ||
              (p2_prime[2] < LOWER_SYNAPTIC_ZONE_Y && p2_prime[2] > LOWER_SYNAPTIC_ZONE_BOUNDARY_Y)){
        synapse_ratio_matrix <- rbind(synapse_ratio_matrix,c(synapse_position_ratio,LOWER_SYNAPTIC_ZONE_INDEX))
      }
    }
    if(is.null(synapse_ratio_matrix)){
      synapse_ratio_matrix <- -1
    }
    
    Branch[["synapse"]] <- synapse_ratio_matrix

    Dendrite[[i]] <- Branch
  }

  return(Dendrite)
}
