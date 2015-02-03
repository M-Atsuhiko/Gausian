investigate_main <- function(Type,Filename,WITH_K,WITH_Ca,Data_Dir,name,output_dir){

  if(Type == "Gausian"){
    isGausian <- TRUE
  }else{
    Type <- "Liner"
    isGausian <- FALSE
  }

  RAND_SEED <- 1:10
  DELTA_T <- seq(5,30,by=5)

  Fs <- c()
  TREE_lengths <- c()
  TREE_volumes <- c()

  Upper_Dend_volume <- c()
  Upper_Dend_length <- c()
  Lower_Dend_volume <- c()
  Lower_Dend_length <- c()

  N_Upper_Syns <- c()
  N_Lower_Syns <- c()
  Upper_Diams <- c()
  Lower_Diams <- c()

  Upper_Ca_amounts <- c()
  Lower_Ca_amounts <- c()
  Sum_Ca_amounts <- c()
  Upper_K_amounts <- c()
  Lower_K_amounts <- c()
  Sum_K_amounts <- c()

  Upper_Ca_maxs <- c()
  Lower_Ca_maxs <- c()
  Sum_Ca_maxs <- c()
  Upper_K_maxs <- c()
  Lower_K_maxs <- c()
  Sum_K_maxs <- c()

  Upper_Synapse_length_diams <- c()
  Lower_Synapse_length_diams <- c()

  Upper_Ca_distribution <- as.list(NULL)
  Lower_Ca_distribution <- as.list(NULL)
  Upper_K_distribution <- as.list(NULL)
  Lower_K_distribution <- as.list(NULL)

  #コンダクタンス分布の平均をみる
  Upper_Ca_peak <- c()
  Upper_Ca_mean <- c()
  Upper_Ca_sd <- c()
  Lower_Ca_peak <- c()
  Lower_Ca_mean <- c()
  Lower_Ca_sd <- c()

  Upper_K_peak <- c()
  Upper_K_mean <- c()
  Upper_K_sd <- c()
  Lower_K_peak <- c()
  Lower_K_mean <- c()
  Lower_K_sd <- c()

  Upper_Ca_Stem_conductance <- c()
  Upper_Ca_taper <- c()
  Lower_Ca_Stem_conductance <- c()
  Lower_Ca_taper <- c()

  Upper_K_Stem_conductance <- c()
  Upper_K_taper <- c()
  Lower_K_Stem_conductance <- c()
  Lower_K_taper <- c()

  Upper_N_bif <- c()
  Lower_N_bif <- c()

  for(dt in DELTA_T){
    cat("Delta_T:",dt,"\n")
    Datas <- as.list(NULL)
    Data_i <- 1
    for(sd in RAND_SEED){
      cat("SEED:",sd,"\n")
      input_filename <- paste(Data_Dir,"SEED",sd,"_","dt",dt,"_",Filename,sep="")
      cat("input file:",input_filename,"\n")
      load(input_filename)
      Datas[[Data_i]] <- Best_Datas[[length(Best_Datas)]]
      Data_i <- Data_i + 1
    }
    
    Fs <- cbind(Fs,
                sapply(Datas,"[[","Ratio"))
    

    TREE_lengths <- cbind(TREE_lengths,
                          sapply(lapply(Datas,"[[","TREE"),sum_length))

    TREE_volumes <- cbind(TREE_volumes,
                          sapply(lapply(Datas,"[[","TREE"),calc_volume))

    named_TREEs <- lapply(lapply(Datas,"[[","TREE"),set_Upper_or_Lower_or_Other)
    
    Upper_Dend_length <- cbind(Upper_Dend_length,
                               sapply(lapply(named_TREEs,"[[","Upper_Dend"),sum_length))

    Upper_Dend_volume <- cbind(Upper_Dend_volume,
                               sapply(lapply(named_TREEs,"[[","Upper_Dend"),calc_volume))

    Lower_Dend_length <- cbind(Lower_Dend_length,
                               sapply(lapply(named_TREEs,"[[","Lower_Dend"),sum_length))
    Lower_Dend_volume <- cbind(Lower_Dend_volume,
                               sapply(lapply(named_TREEs,"[[","Lower_Dend"),calc_volume))

    Upper_N_bif <- cbind(Upper_N_bif,
                         sapply(lapply(named_TREEs,"[[","Upper_Dend"),function(Dends){
                           return(sum(sapply(Dends[[1]],function(Branch){
                             if(length(Branch[["connect"]]) >= 2) return(1)
                             else return(0)
                           })))}))

    Lower_N_bif <- cbind(Lower_N_bif,
                         sapply(lapply(named_TREEs,"[[","Lower_Dend"),function(Dends){
                           return(sum(sapply(Dends[[1]],function(Branch){
                             if(length(Branch[["connect"]]) >= 2) return(1)
                             else return(0)
                           })))}))

    Upper_Diams <- cbind(Upper_Diams,
                         sapply(lapply(named_TREEs,"[[","Upper_Dend"),function(Dends){
                           return(mean(sapply(Dends,function(Dend){
                             return(Dend[[1]][["diam"]])
                           })))
                         }))

    Lower_Diams <- cbind(Lower_Diams,
                         sapply(lapply(named_TREEs,"[[","Lower_Dend"),function(Dends){
                           return(mean(sapply(Dends,function(Dend){
                             return(Dend[[1]][["diam"]])
                           })))
                         }))
    
    N_Upper_Syns <- cbind(N_Upper_Syns,
                          sapply(lapply(named_TREEs,"[[","Upper_Dend"),function(Dends){
                            return(sum(sapply(Dends,function(Dend){
                              return(sum(sapply(Dend,function(Branch){
                                if(is.matrix(Branch[["synapse"]])) return(nrow(Branch[["synapse"]]))
                                else return(0)
                              })))
                            })))
                          }))

    N_Lower_Syns <- cbind(N_Lower_Syns,
                          sapply(lapply(named_TREEs,"[[","Lower_Dend"),function(Dends){
                            return(sum(sapply(Dends,function(Dend){
                              return(sum(sapply(Dend,function(Branch){
                                if(is.matrix(Branch[["synapse"]])) return(nrow(Branch[["synapse"]]))
                                else return(0)
                              })))
                            })))
                          }))

    Upper_Synapse_length_diams <- cbind(Upper_Synapse_length_diams,
                                        sapply(lapply(named_TREEs,"[[","Upper_Dend"),calc_syn_length_diameter))
    
    Lower_Synapse_length_diams <- cbind(Lower_Synapse_length_diams,
                                        sapply(lapply(named_TREEs,"[[","Lower_Dend"),calc_syn_length_diameter))
    
    divided_named_TREEs <- list()

    Datas <- lapply(Datas,function(Data){
      Data[["TREE"]] <- divid_and_set_conductance(Data[["TREE"]],Data[["Params"]])
      return(Data)
    })

    divided_named_TREEs <- lapply(lapply(Datas,"[[","TREE"),set_Upper_or_Lower_or_Other)
    
    Upper_Ca_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Upper_Dend"),function(TREE){
      Conductance_amount <- calc_Conductance_amount(TREE)
      return(c(Conductance_amount[2],Conductance_amount[4]))
    }))

    Lower_Ca_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Lower_Dend"),function(TREE){
      Conductance_amount <- calc_Conductance_amount(TREE)
      return(c(Conductance_amount[2],Conductance_amount[4]))
    }))

    Upper_Ca_amounts <- cbind(Upper_Ca_amounts,
                              Upper_Ca_amount_max[,1])
    Upper_Ca_maxs <- cbind(Upper_Ca_maxs,
                           Upper_Ca_amount_max[,2])

    Lower_Ca_amounts <- cbind(Lower_Ca_amounts,
                              Lower_Ca_amount_max[,1])
    Lower_Ca_maxs <- cbind(Lower_Ca_maxs,
                           Lower_Ca_amount_max[,2])

    Upper_Distribution <- lapply(Datas,function(Data){
      Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
      if(length(Upper_i) > 1)warning("the Number of upper Dendrite is over 2")
      Upper_TREE <- Data[["TREE"]][[Upper_i]]
      Ratio <- Data[["Ratio"]]
      return(list(Ratio,
                  rbind(sapply(Upper_TREE,"[[","path_leng"),
                        sapply(Upper_TREE,"[[","length"),
                        sapply(Upper_TREE,"[[","Ca_conductance"),
                        sapply(Upper_TREE,function(Branch){
                          if(is.matrix(Branch[["synapse"]])) return(1)
                          else return(0)}))))
    })
    
    Upper_Ca_distribution[[length(Upper_Ca_distribution) + 1]] <- Upper_Distribution

    Lower_Distribution <- lapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      if(length(Lower_i) > 1)warning("the Number of upper Dendrite is over 2")
      Lower_TREE <- Data[["TREE"]][[Lower_i]]
      Ratio <- Data[["Ratio"]]
      return(list(Ratio,
                  rbind(sapply(Lower_TREE,"[[","path_leng"),#Lowerは負の値にする
                        sapply(Lower_TREE,"[[","length"),
                        sapply(Lower_TREE,"[[","Ca_conductance"),
                        sapply(Lower_TREE,function(Branch){
                          if(is.matrix(Branch[["synapse"]])) return(1)
                          else return(0)}))))
    })

    Lower_Ca_distribution[[length(Lower_Ca_distribution) + 1]] <- Lower_Distribution


    Upper_Gaus_Ca_Datas <- sapply(Datas,function(Data){
      Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
      Upper_Param <- Data[["Params"]][[Upper_i]]
      if(is.null(Upper_Param[["Ca_peak"]]))
        return(c(0,0,0))
      else
        return(c(Upper_Param[["Ca_peak"]],
                 Upper_Param[["Ca_Gaus_mean"]],
                 Upper_Param[["Ca_Gaus_sd"]]))})

    Lower_Gaus_Ca_Datas <- sapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      Lower_Param <- Data[["Params"]][[Lower_i]]
      if(is.null(Lower_Param[["Ca_peak"]]))
        return(c(0,0,0))
      else
        return(c(Lower_Param[["Ca_peak"]],
                 Lower_Param[["Ca_Gaus_mean"]],
                 Lower_Param[["Ca_Gaus_sd"]]))})

    Upper_Gaus_K_Datas <- sapply(Datas,function(Data){
      Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
      Upper_Param <- Data[["Params"]][[Upper_i]]
      if(is.null(Upper_Param[["K_peak"]]))
        return(c(0,0,0))
      else
        return(c(Upper_Param[["K_peak"]],
                 Upper_Param[["K_Gaus_mean"]],
                 Upper_Param[["K_Gaus_sd"]]))})

    Lower_Gaus_K_Datas <- sapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      Lower_Param <- Data[["Params"]][[Lower_i]]
      if(is.null(Lower_Param[["K_peak"]]))
        return(c(0,0,0))
      else
        return(c(Lower_Param[["K_peak"]],
                 Lower_Param[["K_Gaus_mean"]],
                 Lower_Param[["K_Gaus_sd"]]))})

    Upper_Ca_peak <- cbind(Upper_Ca_peak,
                           Upper_Gaus_Ca_Datas[1,])
    Upper_Ca_mean <- cbind(Upper_Ca_mean,
                           Upper_Gaus_Ca_Datas[2,])
    Upper_Ca_sd <- cbind(Upper_Ca_sd,
                           Upper_Gaus_Ca_Datas[3,])

    Lower_Ca_peak <- cbind(Lower_Ca_peak,
                           Lower_Gaus_Ca_Datas[1,])
    Lower_Ca_mean <- cbind(Lower_Ca_mean,
                           Lower_Gaus_Ca_Datas[2,])
    Lower_Ca_sd <- cbind(Lower_Ca_sd,
                           Lower_Gaus_Ca_Datas[3,])

    Upper_K_peak <- cbind(Upper_K_peak,
                           Upper_Gaus_K_Datas[1,])
    Upper_K_mean <- cbind(Upper_K_mean,
                           Upper_Gaus_K_Datas[2,])
    Upper_K_sd <- cbind(Upper_K_sd,
                           Upper_Gaus_K_Datas[3,])

    Lower_K_peak <- cbind(Lower_K_peak,
                           Lower_Gaus_K_Datas[1,])
    Lower_K_mean <- cbind(Lower_K_mean,
                           Lower_Gaus_K_Datas[2,])
    Lower_K_sd <- cbind(Lower_K_sd,
                           Lower_Gaus_K_Datas[3,])
    
    Upper_Liner_Ca_Datas <- sapply(Datas,function(Data){
      Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
      Upper_Param <- Data[["Params"]][[Upper_i]]
      if(is.null(Upper_Param[["Ca_Stem_conductance"]]))
        return(c(0,0))
      else
        return(c(Upper_Param[["Ca_Stem_conductance"]],
                 Upper_Param[["Ca_taper"]]))})

    Lower_Liner_Ca_Datas <- sapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      Lower_Param <- Data[["Params"]][[Lower_i]]
      if(is.null(Lower_Param[["Ca_Stem_conductance"]]))
        return(c(0,0))
      else
        return(c(Lower_Param[["Ca_Stem_conductance"]],
                 Lower_Param[["Ca_taper"]]))})

    Upper_Liner_K_Datas <- sapply(Datas,function(Data){
      Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
      Upper_Param <- Data[["Params"]][[Upper_i]]
      if(is.null(Upper_Param[["K_Stem_conductance"]]))
        return(c(0,0))
      else
        return(c(Upper_Param[["K_Stem_conductance"]],
                 Upper_Param[["K_taper"]]))})

    Lower_Liner_K_Datas <- sapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      Lower_Param <- Data[["Params"]][[Lower_i]]
      if(is.null(Lower_Param[["K_Stem_conductance"]]))
        return(c(0,0))
      else
        return(c(Lower_Param[["K_Stem_conductance"]],
                 Lower_Param[["K_taper"]]))})

    Upper_Ca_Stem_conductance <- cbind(Upper_Ca_Stem_conductance,
                                   Upper_Liner_Ca_Datas[1,])
    Upper_Ca_taper <- cbind(Upper_Ca_taper,
                                   Upper_Liner_Ca_Datas[2,])

    Lower_Ca_Stem_conductance <- cbind(Lower_Ca_Stem_conductance,
                                   Lower_Liner_Ca_Datas[1,])
    Lower_Ca_taper <- cbind(Lower_Ca_taper,
                                   Lower_Liner_Ca_Datas[2,])

    Upper_K_Stem_conductance <- cbind(Upper_K_Stem_conductance,
                                   Upper_Liner_K_Datas[1,])
    Upper_K_taper <- cbind(Upper_K_taper,
                                   Upper_Liner_K_Datas[2,])

    Lower_K_Stem_conductance <- cbind(Lower_K_Stem_conductance,
                                   Lower_Liner_K_Datas[1,])
    Lower_K_taper <- cbind(Lower_K_taper,
                                   Lower_Liner_K_Datas[2,])
    

    Upper_K_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Upper_Dend"),function(TREE){
      Conductance_amount <- calc_Conductance_amount(TREE)
      return(c(Conductance_amount[1],Conductance_amount[3]))
    }))

    Lower_K_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Lower_Dend"),function(TREE){
      Conductance_amount <- calc_Conductance_amount(TREE)
      return(c(Conductance_amount[1],Conductance_amount[3]))
    }))

    Upper_K_amounts <- cbind(Upper_K_amounts,
                             Upper_K_amount_max[,1])

    Upper_K_maxs <- cbind(Upper_K_maxs,
                          Upper_K_amount_max[,2])

    Lower_K_amounts <- cbind(Lower_K_amounts,
                             Lower_K_amount_max[,1])
    Lower_K_maxs <- cbind(Lower_K_maxs,
                          Lower_K_amount_max[,2])

    Upper_Distribution <- lapply(Datas,function(Data){
      Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
      if(length(Upper_i) > 1)warning("the Number of upper Dendrite is over 2")
      Upper_TREE <- Data[["TREE"]][[Upper_i]]
      Ratio <- Data[["Ratio"]]
      return(list(Ratio,
                  rbind(sapply(Upper_TREE,"[[","path_leng"),
                        sapply(Upper_TREE,"[[","length"),
                        sapply(Upper_TREE,"[[","K_conductance"),
                        sapply(Upper_TREE,function(Branch){
                          if(is.matrix(Branch[["synapse"]])) return(1)
                          else return(0)}))))
    })
    
    Upper_K_distribution[[length(Upper_K_distribution) + 1]] <- Upper_Distribution

    Lower_Distribution <- lapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      if(length(Lower_i) > 1)warning("the Number of upper Dendrite is over 2")
      Lower_TREE <- Data[["TREE"]][[Lower_i]]
      Ratio <- Data[["Ratio"]]
      return(list(Ratio,
                  rbind(sapply(Lower_TREE,"[[","path_leng"),
                        sapply(Lower_TREE,"[[","length"),
                        sapply(Lower_TREE,"[[","K_conductance"),
                        sapply(Lower_TREE,function(Branch){
                          if(is.matrix(Branch[["synapse"]])) return(1)
                          else return(0)}))))
    })

    Lower_K_distribution[[length(Lower_K_distribution) + 1]] <- Lower_Distribution

  }


  

  Label_SEED <- paste(RAND_SEED)
  Label_Delta_T <- paste(DELTA_T)

  colnames(Upper_Ca_peak) <- Label_Delta_T
  rownames(Upper_Ca_peak) <- Label_SEED
  colnames(Upper_Ca_mean) <- Label_Delta_T
  rownames(Upper_Ca_mean) <- Label_SEED
  colnames(Upper_Ca_sd) <- Label_Delta_T
  rownames(Upper_Ca_sd) <- Label_SEED

  colnames(Lower_Ca_peak) <- Label_Delta_T
  rownames(Lower_Ca_peak) <- Label_SEED
  colnames(Lower_Ca_mean) <- Label_Delta_T
  rownames(Lower_Ca_mean) <- Label_SEED
  colnames(Lower_Ca_sd) <- Label_Delta_T
  rownames(Lower_Ca_sd) <- Label_SEED

  colnames(Upper_K_peak) <- Label_Delta_T
  rownames(Upper_K_peak) <- Label_SEED
  colnames(Upper_K_mean) <- Label_Delta_T
  rownames(Upper_K_mean) <- Label_SEED
  colnames(Upper_K_sd) <- Label_Delta_T
  rownames(Upper_K_sd) <- Label_SEED

  colnames(Lower_K_peak) <- Label_Delta_T
  rownames(Lower_K_peak) <- Label_SEED
  colnames(Lower_K_mean) <- Label_Delta_T
  rownames(Lower_K_mean) <- Label_SEED
  colnames(Lower_K_sd) <- Label_Delta_T
  rownames(Lower_K_sd) <- Label_SEED

  colnames(Upper_Ca_Stem_conductance) <- Label_Delta_T
  rownames(Upper_Ca_Stem_conductance) <- Label_SEED
  colnames(Upper_Ca_taper) <- Label_Delta_T
  rownames(Upper_Ca_taper) <- Label_SEED

  colnames(Lower_Ca_Stem_conductance) <- Label_Delta_T
  rownames(Lower_Ca_Stem_conductance) <- Label_SEED
  colnames(Lower_Ca_taper) <- Label_Delta_T
  rownames(Lower_Ca_taper) <- Label_SEED
  
  colnames(Upper_K_Stem_conductance) <- Label_Delta_T
  rownames(Upper_K_Stem_conductance) <- Label_SEED
  colnames(Upper_K_taper) <- Label_Delta_T
  rownames(Upper_K_taper) <- Label_SEED

  colnames(Lower_K_Stem_conductance) <- Label_Delta_T
  rownames(Lower_K_Stem_conductance) <- Label_SEED
  colnames(Lower_K_taper) <- Label_Delta_T
  rownames(Lower_K_taper) <- Label_SEED


  colnames(Upper_N_bif) <- Label_Delta_T
  rownames(Upper_N_bif) <- Label_SEED
  colnames(Lower_N_bif) <- Label_Delta_T
  rownames(Lower_N_bif) <- Label_SEED

  prefix <- paste(output_dir,name,"_",extra_prefix,"_",sep="")

  rowname <- expression(paste("Optimized ",Delta,"t [ms]"))

  line_type <- "solid"

##########################
  colname <- "F"
  Filename <- paste(prefix,"Fs.eps",sep="")
  color <- c("red")
  legend <- c()
  showMax <- TRUE
  plot_func(list(Fs),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"Fs.xdr",sep="")
  rownames(Fs) <- Label_SEED
  colnames(Fs) <- Label_Delta_T
  save(Fs,file=Filename)
##########################

  colname <- expression(paste("TREE Length [",mu,"m]"))
  Filename <- paste(prefix,"TREE_lengths.eps",sep="")
  color <- c("red")
  legend <- c()
  showMax <- FALSE
  plot_func(list(TREE_lengths),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"TREE_lengths.xdr",sep="")
  rownames(TREE_lengths) <- Label_SEED
  colnames(TREE_lengths) <- Label_Delta_T
  save(TREE_lengths,file=Filename)
##########################

  colname <- expression(paste("TREE Volume [",mu,m^3,"]"))
  Filename <- paste(prefix,"TREE_volume.eps",sep="")
  color <- c("red")
  legend <- c()
  showMax <- FALSE
  plot_func(list(TREE_volumes),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"TREE_volumes.xdr",sep="")
  rownames(TREE_volumes) <- Label_SEED
  colnames(TREE_volumes) <- Label_Delta_T
  save(TREE_volumes,file=Filename)

##########################
  colname <- expression(paste("Dendrite Length [",mu,"m]"))
  Filename <- paste(prefix,"Dendrite_lengths.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Red Dendrite","Blue Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))
  showMax <- FALSE
  plot_func(list(Upper_Dend_length,Lower_Dend_length),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"Dendrite_lengths.xdr",sep="")
  rownames(Upper_Dend_length) <- Label_SEED
  colnames(Upper_Dend_length) <- Label_Delta_T
  rownames(Lower_Dend_length) <- Label_SEED
  colnames(Lower_Dend_length) <- Label_Delta_T
  Dendrite_lengths <- list(Upper_Dend_length,Lower_Dend_length)
  names(Dendrite_lengths) <- c("Upper_Dend_length","Lower_Dend_length")
  save(TREE_lengths,file=Filename)

##########################
  colname <- expression(paste("Dendrite Volume [",mu,m^3,"]"))
  Filename <- paste(prefix,"Dendrite_volumes.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Red Dendrite","Blue Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))
  showMax <- FALSE
  plot_func(list(Upper_Dend_volume,Lower_Dend_volume),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"Dendrite_volumes.xdr",sep="")
  rownames(Upper_Dend_volume) <- Label_SEED
  colnames(Upper_Dend_volume) <- Label_Delta_T
  rownames(Lower_Dend_volume) <- Label_SEED
  colnames(Lower_Dend_volume) <- Label_Delta_T
  Dendrite_volumes <- list(Upper_Dend_volume,Lower_Dend_volume)
  names(Dendrite_volumes) <- c("Upper_Dend_volume","Lower_Dend_volume")
  save(TREE_volumes,file=Filename)

##########################

  colname <- expression(paste("Stem diam [",mu,"m]"))
  Filename <- paste(prefix,"Stem_diam.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Red Dendrite","Blue Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))
  rownames(Upper_Diams) <- Label_SEED
  colnames(Upper_Diams) <- Label_Delta_T
  rownames(Lower_Diams) <- Label_SEED
  colnames(Lower_Diams) <- Label_Delta_T
  Diams <- list(Upper_Diams,Lower_Diams)
  names(Diams) <- c("Upper_Diam","Lower_Diam")
  showMax <- FALSE
  plot_func(Diams,color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"Stem_diams.xdr",sep="")
  save(Diams,file=Filename)
##########################

  colname <- expression(paste("Synaptic length/diameter"))
  Filename <- paste(prefix,"Synapse_length_diam.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Red Dendrite","Blue Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))
  rownames(Upper_Synapse_length_diams) <- Label_SEED
  colnames(Upper_Synapse_length_diams) <- Label_Delta_T
  rownames(Lower_Synapse_length_diams) <- Label_SEED
  colnames(Lower_Synapse_length_diams) <- Label_Delta_T
  Synapse_length_diams <- list(Upper_Synapse_length_diams,Lower_Synapse_length_diams)
  names(Synapse_length_diams) <- c("Upper_Synapse_length_diams","Lower_Synapse_length_diams")
  showMax <- FALSE
  plot_func(Synapse_length_diams,color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"Synapse_length_diams.xdr",sep="")
  save(Synapse_length_diams,file=Filename)
##########################

  colname <-paste("Number of Synapses")
  Filename <- paste(prefix,"Number_synapse.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Red Synapse","Blue Synapse"),
                  c("red","blue"),
                  c(line_type,line_type))
  rownames(N_Upper_Syns) <- Label_SEED
  colnames(N_Upper_Syns) <- Label_Delta_T
  rownames(N_Lower_Syns) <- Label_SEED
  colnames(N_Lower_Syns) <- Label_Delta_T
  Synapses <- list(N_Upper_Syns,N_Lower_Syns)
  names(Synapses) <- c("N_Upper_Syns","N_Lower_Syns")
  showMax <- FALSE
  plot_func(Synapses,color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            line_type,
            showMax)
  Filename <- paste(prefix,"Synapses.xdr",sep="")
  save(Synapses,file=Filename)
##########################

  rownames(Upper_Ca_amounts) <- Label_SEED
  colnames(Upper_Ca_amounts) <- Label_Delta_T
  rownames(Lower_Ca_amounts) <- Label_SEED
  colnames(Lower_Ca_amounts) <- Label_Delta_T

  rownames(Upper_Ca_maxs) <- Label_SEED
  colnames(Upper_Ca_maxs) <- Label_Delta_T
  rownames(Lower_Ca_maxs) <- Label_SEED
  colnames(Lower_Ca_maxs) <- Label_Delta_T

  colname <-paste("Ca Conductance ratio")
  Filename <- paste(prefix,"Ca_conductance_ratio.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Upper Dendrite","Lower Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))

  Upper_Ca_ratios <- Upper_Ca_amounts/Upper_Ca_maxs
  Lower_Ca_ratios <- Lower_Ca_amounts/Lower_Ca_maxs

  Ca_Ratios <- list(Upper_Ca_ratios,Lower_Ca_ratios)
  names(Ca_Ratios) <- c("Upper_Ca_ratios","Lower_Ca_ratios")
  showMax <- FALSE
  if(WITH_Ca){
    plot_func(Ca_Ratios,color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
    Filename <- paste(prefix,"Ca_ratio.xdr",sep="")
    save(Ca_Ratios,file=Filename)
  }


##########################

  colname <-paste("Ca Conductance amount")
  Filename <- paste(prefix,"Ca_conductance_amount.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Upper Dendrite","Lower Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))
  Ca_amounts <- list(Upper_Ca_amounts,Upper_Ca_maxs,Lower_Ca_amounts,Lower_Ca_maxs)
  names(Ca_amounts) <- c("Upper_Ca_amounts","Upper_Ca_maxs","Lower_Ca_amounts","Lower_Ca_maxs")
  showMax <- FALSE
  if(WITH_Ca){
    plot_func(list(Ca_amounts[[1]],Ca_amounts[[3]]),color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
    Filename <- paste(prefix,"Ca_amount.xdr",sep="")
    save(Ca_amounts,file=Filename)
  }

##########################

  colname <-paste("Ca Conductance amount")
  Filename <- paste(prefix,"TREE_Ca_conductance_amount.eps",sep="")
  color <- c("red")
  legend <- c()
  TREE_Ca_amounts <- Ca_amounts[["Upper_Ca_amounts"]] + Ca_amounts[["Lower_Ca_amounts"]]
  showMax <- FALSE
  if(WITH_Ca){
    plot_func(list(TREE_Ca_amounts),color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
  }

##########################

  colname <-paste("Ca Conductance_ratio")
  Filename <- paste(prefix,"TREE_Ca_conductance_ratio.eps",sep="")
  color <- c("red")
  legend <- c()
  TREE_Ca_maxs <- Ca_amounts[["Upper_Ca_maxs"]] + Ca_amounts[["Lower_Ca_maxs"]]
  TREE_Ca_ratios <- TREE_Ca_amounts/TREE_Ca_maxs
  showMax <- FALSE
  if(WITH_Ca){
    plot_func(list(TREE_Ca_ratios),color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
  }

##########################
  filename_prefix <- paste(prefix,"Ca_distribution_",sep="")
  mainName <- "CaT distribution"
  colname <- expression(paste("[S/c",m^2,"]",sep=""))
  rowname <- expression(paste("Dendrite length [",mu,"m]",sep=""))
  mapply(function(Upper_Conductance,Lower_Conductance,delta_t){
    main <- paste(Type," ",delta_t,"ms ",mainName,sep="")
    Filename <- paste(filename_prefix,"dt",delta_t,".eps",sep="")
    if(WITH_Ca){
      plot_Conductance_distribution(Upper_Conductance,
                                    Lower_Conductance,
                                    main,
                                    colname,
                                    rowname,
                                    Filename,
                                    isGausian,
                                    Ca_MAX)
    }},
         Upper_Ca_distribution,Lower_Ca_distribution,DELTA_T)

##########################

  names(Upper_K_amounts) <- "Upper_K_amounts"
  names(Lower_K_amounts) <- "Lower_K_amounts"
  rownames(Upper_K_amounts) <- Label_SEED
  colnames(Upper_K_amounts) <- Label_Delta_T
  rownames(Lower_K_amounts) <- Label_SEED
  colnames(Lower_K_amounts) <- Label_Delta_T

  names(Upper_K_maxs) <- "Upper_K_maxs"
  names(Lower_K_maxs) <- "Lower_K_maxs"
  rownames(Upper_K_maxs) <- Label_SEED
  colnames(Upper_K_maxs) <- Label_Delta_T
  rownames(Lower_K_maxs) <- Label_SEED
  colnames(Lower_K_maxs) <- Label_Delta_T

  colname <-paste("K Conductance ratio")
  Filename <- paste(prefix,"K_conductance_ratio.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Upper Dendrite","Lower Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))
  Upper_K_ratios <- Upper_K_amounts/Upper_K_maxs
  Lower_K_ratios <- Lower_K_amounts/Lower_K_maxs
  K_Ratios <- list(Upper_K_ratios,Lower_K_ratios)
  names(K_Ratios) <- c("Upper_K_ratios","Lower_K_ratios")
  showMax <- FALSE
  if(WITH_K){
    plot_func(K_Ratios,color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
    Filename <- paste(prefix,"K_ratio.xdr",sep="")
    save(K_Ratios,file=Filename)
  }
##########################

  colname <-paste("K Conductance amount")
  Filename <- paste(prefix,"K_conductance_amount.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Upper Dendrite","Lower Dendrite"),
                  c("red","blue"),
                  c(line_type,line_type))
  K_amounts <- list(Upper_K_amounts,Upper_K_maxs,Lower_K_amounts,Lower_K_maxs)
  names(K_amounts) <- c("Upper_K_amounts","Upper_K_maxs","Lower_K_amounts","Lower_K_maxs")
  showMax <- FALSE
  if(WITH_K){
    plot_func(list(K_amounts[[1]],K_amounts[[3]]),color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
    Filename <- paste(prefix,"K_amount.xdr",sep="")
    save(K_amounts,file=Filename)
  }


##########################

  colname <-paste("K Conductance amount")
  Filename <- paste(prefix,"TREE_K_conductance_amount.eps",sep="")
  color <- c("red")
  legend <- c()
  TREE_K_amounts <- K_amounts[["Upper_K_amounts"]] + K_amounts[["Lower_K_amounts"]]
  showMax <- FALSE
  if(WITH_K){
    plot_func(list(TREE_K_amounts),color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
  }

##########################

  colname <-paste("K Conductance_ratio")
  Filename <- paste(prefix,"TREE_K_conductance_ratio.eps",sep="")
  color <- c("red")
  legend <- c()
  TREE_K_maxs <- K_amounts[["Upper_K_maxs"]] + K_amounts[["Lower_K_maxs"]]
  TREE_K_ratios <- TREE_K_amounts/TREE_K_maxs
  showMax <- FALSE
  if(WITH_K){
    plot_func(list(TREE_K_ratios),color,DELTA_T,Filename,
              colname,
              rowname,
              legend,
              line_type,
              showMax)
  }

##########################
  filename_prefix <- paste(prefix,"K_distribution_",sep="")
  mainName <- "Ka distribution"
  colname <- expression(paste("[S/c",m^2,"]",sep=""))
  rowname <- expression(paste("Dendrite length [",mu,"m]",sep=""))
  mapply(function(Upper_Conductance,Lower_Conductance,delta_t){
    main <- paste(Type," ",delta_t,"ms ",mainName,sep="")
    Filename <- paste(filename_prefix,"dt",delta_t,".eps",sep="")
    if(WITH_K){
      plot_Conductance_distribution(Upper_Conductance,
                                    Lower_Conductance,
                                    main,
                                    colname,
                                    rowname,
                                    Filename,
                                    isGausian,
                                    K_MAX)
    }},
         Upper_K_distribution,Lower_K_distribution,DELTA_T)


############################
  if(!(WITH_K) && !(WITH_Ca)){
    filename_prefix <- paste(prefix,"passive_syn_position_",sep="")
    mainName <- "Synapse position"
    rowname <- expression(paste("Dendrite length [",mu,"m]",sep=""))
    mapply(function(Upper_syn_data,Lower_syn_data,delta_t){
      main <- paste("Passive ",delta_t,"ms ",mainName,sep="")
      Filename <- paste(filename_prefix,"dt",delta_t,".eps",sep="")
      plot_Conductance_synapse_position(Upper_syn_data,Lower_syn_data,
                                        main,
                                        rowname,
                                        K_MAX,
                                        Filename)
    },
           Upper_K_distribution,Lower_K_distribution,DELTA_T)
  }



                                        #データフレームを作成する
  All_dt <- c()
  All_sd <- c()

  All_Fs <- c()
  All_TREE_lengths <- c()
  All_TREE_volumes <- c()

  All_Upper_Dend_volume <- c()
  All_Upper_Dend_length <- c()
  All_Lower_Dend_volume <- c()
  All_Lower_Dend_length <- c()

  All_N_Upper_Syns <- c()
  All_N_Lower_Syns <- c()
  All_Upper_Diams <- c()
  All_Lower_Diams <- c()

  All_Upper_Synapse_length_diam <- c()
  All_Lower_Synapse_length_diam <- c()

  All_Sum_Ca_amounts <- c()
  All_Sum_Ca_ratios <- c()
  All_Upper_Ca_amounts <- c()
  All_Lower_Ca_amounts <- c()
  All_Upper_Ca_ratios <- c()
  All_Lower_Ca_ratios <- c()

  All_Sum_K_amounts <- c()
  All_Sum_K_ratios <- c()
  All_Upper_K_amounts <- c()
  All_Lower_K_amounts <- c()
  All_Upper_K_ratios <- c()
  All_Lower_K_ratios <- c()

  All_Upper_Ca_maxs <- c()
  All_Lower_Ca_maxs <- c()
  All_Sum_Ca_maxs <- c()

  All_Upper_K_maxs <- c()
  All_Lower_K_maxs <- c()
  All_Sum_K_maxs <- c()

  All_Upper_Ca_peak <- c()
  All_Upper_Ca_mean <- c()
  All_Upper_Ca_sd <- c()
  
  All_Lower_Ca_peak <- c()
  All_Lower_Ca_mean <- c()
  All_Lower_Ca_sd <- c()

  All_Upper_K_peak <- c()
  All_Upper_K_mean <- c()
  All_Upper_K_sd <- c()
  
  All_Lower_K_peak <- c()
  All_Lower_K_mean <- c()
  All_Lower_K_sd <- c()

  All_Upper_Ca_Stem_conductance <- c()
  All_Upper_Ca_taper <- c()
  
  All_Lower_Ca_Stem_conductance <- c()
  All_Lower_Ca_taper <- c()

  All_Upper_K_Stem_conductance <- c()
  All_Upper_K_taper <- c()
  
  All_Lower_K_Stem_conductance <- c()
  All_Lower_K_taper <- c()

  All_Upper_N_bif <- c()
  All_Lower_N_bif <- c()

  for(dt in DELTA_T){
    for(sd in RAND_SEED){
      All_dt <- c(All_dt,dt)
      All_sd <- c(All_sd,sd)
      
      All_Fs <- c(All_Fs,
                  Fs[paste(sd),paste(dt)])

      All_TREE_lengths <- c(All_TREE_lengths,
                            TREE_lengths[paste(sd),paste(dt)])
      
      All_TREE_volumes <- c(All_TREE_volumes,
                            TREE_volumes[paste(sd),paste(dt)])

      All_Upper_Dend_volume <- c(All_Upper_Dend_volume,
                                 Upper_Dend_volume[paste(sd),paste(dt)])
      All_Upper_Dend_length <- c(All_Upper_Dend_length,
                                 Upper_Dend_length[paste(sd),paste(dt)])
      
      All_Lower_Dend_volume <- c(All_Lower_Dend_volume,
                                 Lower_Dend_volume[paste(sd),paste(dt)])
      All_Lower_Dend_length <- c(All_Lower_Dend_length,
                                 Lower_Dend_length[paste(sd),paste(dt)])

      All_N_Upper_Syns <- c(All_N_Upper_Syns,
                            N_Upper_Syns[paste(sd),paste(dt)])
      All_N_Lower_Syns <- c(All_N_Lower_Syns,
                            N_Lower_Syns[paste(sd),paste(dt)])

      All_Upper_Diams <- c(All_Upper_Diams,
                           Upper_Diams[paste(sd),paste(dt)])
      All_Upper_Synapse_length_diam <- c(All_Upper_Synapse_length_diam,
                                         Upper_Synapse_length_diams[paste(sd),paste(dt)])
      
      All_Lower_Diams <- c(All_Lower_Diams,
                           Lower_Diams[paste(sd),paste(dt)])
      All_Lower_Synapse_length_diam <- c(All_Lower_Synapse_length_diam,
                                         Lower_Synapse_length_diams[paste(sd),paste(dt)])

      
      All_Sum_Ca_amounts <- c(All_Sum_Ca_amounts,
                              TREE_Ca_amounts[paste(sd),paste(dt)])

      All_Sum_Ca_maxs <- c(All_Sum_Ca_maxs,
                           TREE_Ca_maxs[paste(sd),paste(dt)])
      
      All_Sum_Ca_ratios <- c(All_Sum_Ca_ratios,
                             TREE_Ca_ratios[paste(sd),paste(dt)])


      All_Upper_Ca_amounts <- c(All_Upper_Ca_amounts,
                                Upper_Ca_amounts[paste(sd),paste(dt)])
      All_Upper_Ca_ratios <- c(All_Upper_Ca_ratios,
                               Upper_Ca_ratios[paste(sd),paste(dt)])
      All_Upper_Ca_maxs <- c(All_Upper_Ca_maxs,
                             Upper_Ca_maxs[paste(sd),paste(dt)])

      All_Lower_Ca_amounts <- c(All_Lower_Ca_amounts,
                                Lower_Ca_amounts[paste(sd),paste(dt)])
      All_Lower_Ca_ratios <- c(All_Lower_Ca_ratios,
                               Lower_Ca_ratios[paste(sd),paste(dt)])
      All_Lower_Ca_maxs <- c(All_Lower_Ca_maxs,
                             Lower_Ca_maxs[paste(sd),paste(dt)])

      All_Sum_K_amounts <- c(All_Sum_K_amounts,
                             TREE_K_amounts[paste(sd),paste(dt)])

      All_Sum_K_maxs <- c(All_Sum_K_maxs,
                          TREE_K_maxs[paste(sd),paste(dt)])
      
      All_Sum_K_ratios <- c(All_Sum_K_ratios,
                            TREE_K_ratios[paste(sd),paste(dt)])
      
      All_Upper_K_amounts <- c(All_Upper_K_amounts,
                               Upper_K_amounts[paste(sd),paste(dt)])
      All_Upper_K_ratios <- c(All_Upper_K_ratios,
                              Upper_K_ratios[paste(sd),paste(dt)])
      All_Upper_K_maxs <- c(All_Upper_K_maxs,
                            Upper_K_maxs[paste(sd),paste(dt)])

      All_Lower_K_amounts <- c(All_Lower_K_amounts,
                               Lower_K_amounts[paste(sd),paste(dt)])
      All_Lower_K_ratios <- c(All_Lower_K_ratios,
                              Lower_K_ratios[paste(sd),paste(dt)])
      All_Lower_K_maxs <- c(All_Lower_K_maxs,
                            Lower_K_maxs[paste(sd),paste(dt)])

      All_Upper_Ca_peak <- c(All_Upper_Ca_peak,
                             Upper_Ca_peak[paste(sd),paste(dt)])
      All_Upper_Ca_mean <- c(All_Upper_Ca_mean,
                             Upper_Ca_mean[paste(sd),paste(dt)])
      All_Upper_Ca_sd <- c(All_Upper_Ca_sd,
                             Upper_Ca_sd[paste(sd),paste(dt)])

      All_Lower_Ca_peak <- c(All_Lower_Ca_peak,
                             Lower_Ca_peak[paste(sd),paste(dt)])
      All_Lower_Ca_mean <- c(All_Lower_Ca_mean,
                             Lower_Ca_mean[paste(sd),paste(dt)])
      All_Lower_Ca_sd <- c(All_Lower_Ca_sd,
                             Lower_Ca_sd[paste(sd),paste(dt)])

      All_Upper_K_peak <- c(All_Upper_K_peak,
                             Upper_K_peak[paste(sd),paste(dt)])
      All_Upper_K_mean <- c(All_Upper_K_mean,
                             Upper_K_mean[paste(sd),paste(dt)])
      All_Upper_K_sd <- c(All_Upper_K_sd,
                             Upper_K_sd[paste(sd),paste(dt)])

      All_Lower_K_peak <- c(All_Lower_K_peak,
                             Lower_K_peak[paste(sd),paste(dt)])
      All_Lower_K_mean <- c(All_Lower_K_mean,
                             Lower_K_mean[paste(sd),paste(dt)])
      All_Lower_K_sd <- c(All_Lower_K_sd,
                             Lower_K_sd[paste(sd),paste(dt)])

      All_Upper_Ca_Stem_conductance <- c(All_Upper_Ca_Stem_conductance,
                                         Upper_Ca_Stem_conductance[paste(sd),paste(dt)])
      All_Upper_Ca_taper <- c(All_Upper_Ca_taper,
                              Upper_Ca_taper[paste(sd),paste(dt)])

      All_Lower_Ca_Stem_conductance <- c(All_Lower_Ca_Stem_conductance,
                                         Lower_Ca_Stem_conductance[paste(sd),paste(dt)])
      All_Lower_Ca_taper <- c(All_Lower_Ca_taper,
                              Lower_Ca_taper[paste(sd),paste(dt)])

      All_Upper_K_Stem_conductance <- c(All_Upper_K_Stem_conductance,
                                         Upper_K_Stem_conductance[paste(sd),paste(dt)])
      All_Upper_K_taper <- c(All_Upper_K_taper,
                              Upper_K_taper[paste(sd),paste(dt)])

      All_Lower_K_Stem_conductance <- c(All_Lower_K_Stem_conductance,
                                         Lower_K_Stem_conductance[paste(sd),paste(dt)])
      All_Lower_K_taper <- c(All_Lower_K_taper,
                              Lower_K_taper[paste(sd),paste(dt)])

      All_Upper_N_bif <- c(All_Upper_N_bif,
                           Upper_N_bif[paste(sd),paste(dt)])
      All_Lower_N_bif <- c(All_Lower_N_bif,
                           Lower_N_bif[paste(sd),paste(dt)])

    }
  }

  ALL_DATA_FRAME <- data.frame(DT=All_dt,
                               SEED=All_sd,
                               
                               F=All_Fs,
                               
                               TREE_length=All_TREE_lengths,
                               TREE_volume=All_TREE_volumes,
                               
                               Upper_Dend_volume=All_Upper_Dend_volume,
                               Lower_Dend_volume=All_Lower_Dend_volume,
                               Upper_Dend_length=All_Upper_Dend_length,
                               Lower_Dend_length=All_Lower_Dend_length,

                               N_Upper_Syn=All_N_Upper_Syns,
                               N_Lower_Syn=All_N_Lower_Syns,
                               Upper_Diam=All_Upper_Diams,
                               Lower_Diam=All_Lower_Diams,

                               Upper_Synaptic_length_diam=All_Upper_Synapse_length_diam,
                               Lower_Synaptic_length_diam=All_Lower_Synapse_length_diam,
                               
                               TREE_Ca_amount=All_Sum_Ca_amounts,
                               TREE_Ca_max=All_Sum_Ca_maxs,
                               TREE_Ca_ratio=All_Sum_Ca_ratios,
                               
                               Upper_Ca_amount=All_Upper_Ca_amounts,
                               Upper_Ca_max=All_Upper_Ca_maxs,
                               Upper_Ca_ratio=All_Upper_Ca_ratios,
                               
                               Lower_Ca_amount=All_Lower_Ca_amounts,
                               Lower_Ca_max=All_Lower_Ca_maxs,
                               Lower_Ca_ratio=All_Lower_Ca_ratios,
                               
                               TREE_K_amount=All_Sum_K_amounts,
                               TREE_K_ratio=All_Sum_K_ratios,
                               TREE_K_max=All_Sum_K_maxs,
                               
                               Upper_K_amount=All_Upper_K_amounts,
                               Upper_K_max=All_Upper_K_maxs,
                               Upper_K_ratio=All_Upper_K_ratios,
                               
                               Lower_K_amount=All_Lower_K_amounts,
                               Lower_K_max=All_Lower_K_maxs,
                               Lower_K_ratio=All_Lower_K_ratios,

                               Upper_Gaus_Ca_peak=All_Upper_Ca_peak,
                               Upper_Gaus_Ca_mean=All_Upper_Ca_mean,
                               Upper_Gaus_Ca_sd=All_Upper_Ca_sd,

                               Lower_Gaus_Ca_peak=All_Lower_Ca_peak,
                               Lower_Gaus_Ca_mean=All_Lower_Ca_mean,
                               Lower_Gaus_Ca_sd=All_Lower_Ca_sd,

                               Upper_Gaus_K_peak=All_Upper_K_peak,
                               Upper_Gaus_K_mean=All_Upper_K_mean,
                               Upper_Gaus_K_sd=All_Upper_K_sd,

                               Lower_Gaus_K_peak=All_Lower_K_peak,
                               Lower_Gaus_K_mean=All_Lower_K_mean,
                               Lower_Gaus_K_sd=All_Lower_K_sd,

                               Upper_Liner_Ca_Stem_conductance=All_Upper_Ca_Stem_conductance,
                               Upper_Liner_Ca_taper=All_Upper_Ca_taper,
                               
                               Lower_Liner_Ca_Stem_conductance=All_Lower_Ca_Stem_conductance,
                               Lower_Liner_Ca_taper=All_Lower_Ca_taper,
                               
                               Upper_Liner_K_Stem_conductance=All_Upper_K_Stem_conductance,
                               Upper_Liner_K_taper=All_Upper_K_taper,
                               
                               Lower_Liner_K_Stem_conductance=All_Lower_K_Stem_conductance,
                               Lower_Liner_K_taper=All_Lower_K_taper,

                               N_Upper_bif=All_Upper_N_bif,
                               N_Lower_bif=All_Lower_N_bif
                               )
  Filename <- paste(prefix,"All_Data_FRAME.xdr",sep="")
  save(ALL_DATA_FRAME,file=Filename)
  cat("saved:",Filename,"\n")
}
