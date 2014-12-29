Soma_Conductance_init <- function(WITH_K,WITH_Ca){
  Conductance <- list(runif(1,min = 0,max = MAX_PEAK)*WITH_K,
                      runif(1,min = 0,max = Ca_MAX)*WITH_Ca)

  names(Conductance) <- Soma_Conductance_Labels
  return(Conductance)
}
