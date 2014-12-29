parallel_make_TREE <- function(Individual_Data){
    set.seed(Individual_Data[["Seed"]])
    Params <- Individual_Data[["Params"]]
    
    TREE <- make_TREE(Params)
    TREE <- lapply(TREE,set_coordinate)
    TREE <- lapply(TREE,set_synapse)
    
    lapply(TREE,data_check)
    if(canSimulation(TREE)){
      Can_Sim <- TRUE
    }else{
      Can_Sim <- FALSE
    }
    Individual_Data[["TREE"]] <- TREE
    Individual_Data[["CanSim"]] <- Can_Sim
    return(Individual_Data)
}
