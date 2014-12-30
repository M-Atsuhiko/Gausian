Stem_Diam <- function(Dends){
  diams <- sapply(lapply(Dends,"[[",1),"[[","diam")
  #上下どちらかのシナプティックゾーンにDendriteが2本以上到達する可能性もあるので、meanにする
  return(mean(diams))
}

#UPPER_SYNAPTIC_ZONE_INDEX      <- 1
#LOWER_SYNAPTIC_ZONE_INDEX      <- 0

#load("~/workspace/Function_Morphology/stochastic_morphology/passive_Result/SEED1_dt15_passive_Best_TREEs.xdr")
#TREE <- Best_TREEs[[length(Best_TREEs)]]

#calc_syn_length_diameter(TREE)
