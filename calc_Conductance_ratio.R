calc_Conductance_ratio <- function(TREE,WITH_K,WITH_Ca){

  K_ratios <- c()
  Ca_ratios <- c()

  for(Dendrite in TREE){

    Dend_surface <- 0 #cm^2
    Dend_K_Conductance <- 0 #S
    Dend_Ca_Conductance <- 0 #S
    
    for(Branch in Dendrite){
      length <- Branch[["length"]]
      diam <- Branch[["diam"]]
      K_conductance <- Branch[["K_conductance"]]
      Ca_conductance <- Branch[["Ca_conductance"]]
      surface <- (diam*pi*length)*10^(-8) #側面の面積のみ考える cm^2単位で考えるので10^(-8)倍している
      Dend_surface <- Dend_surface + surface
      Dend_K_Conductance <- Dend_K_Conductance + surface*K_conductance
      Dend_Ca_Conductance <- Dend_Ca_Conductance + surface*Ca_conductance
    }

    K_ratios <- c(K_ratios,
                  Dend_K_Conductance/(Dend_surface*K_MAX))

    Ca_ratios <- c(Ca_ratios,
                   Dend_Ca_Conductance/(Dend_surface*Ca_MAX))
  }

  return((mean(sum(K_ratios)) + mean(sum(Ca_ratios)))/sum(WITH_K,WITH_Ca))
}
