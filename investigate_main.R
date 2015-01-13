Type <- "Gausian"

source(paste(Type,"_Dendritic_function_parameter.R",sep=""))
source("plot_func.R")
source("./calc_syn_length_diameter.R")
source("return_Upper_Lower_Other_i.R")
source("plot_Conductance_distribution.R")

par(lwd=3,
    cex=1.4,
    mex=1.2)

output_dir <- paste("./",Type,"_Result/",sep="")

WITH_K <- FALSE
WITH_Ca <- TRUE

if(Type == "Gausian"){
  isGausian <- TRUE
}else{
  isGausian <- FALSE
}


RAND_SEED <- 1:10
DELTA_T <- seq(5,30,by=5)
Function_ratio <- 75
Conductance_ratio <- 10
Morphology_ratio <- 100 - (Function_ratio + Conductance_ratio)
extra_prefix <- paste("Rerative_Gaus_",Function_ratio,"_",Conductance_ratio,sep="")

if(WITH_K*WITH_Ca){
  name <- "k_ca"
}else if(WITH_K){
  name <- "k"
}else if(WITH_Ca){
  name <- "ca"
}else name <- "passive"

cat("inciude conductance:",name,"\n")

Data_Dir <- paste("./",name,"_Result/",sep="")

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

for(dt in DELTA_T){
  cat("Delta_T:",dt,"\n")
  Datas <- as.list(NULL)
  Data_i <- 1
  for(sd in RAND_SEED){
    cat("SEED:",sd,"\n")
    input_filename <- paste(Data_Dir,"SEED",sd,"_","dt",dt,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_Datas.xdr",sep="")
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

  for(Data_i in 1:length(Datas)){
    divided_named_TREEs[[Data_i]] <- set_Upper_or_Lower_or_Other(
      divid_and_set_conductance(Datas[[Data_i]][["TREE"]],Datas[[Data_i]][["Params"]]))
  }
  
  Upper_Ca_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Upper_Dend"),function(TREE){
    Conductance_amont <- calc_Conductance_amount(TREE)
    return(c(Conductance_amont[2],Conductance_amont[4]))
  }))

  Lower_Ca_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Lower_Dend"),function(TREE){
    Conductance_amont <- calc_Conductance_amount(TREE)
    return(c(Conductance_amont[2],Conductance_amont[4]))
  }))

  Upper_Ca_amounts <- cbind(Upper_Ca_amounts,
                            Upper_Ca_amount_max[,1])
  Upper_Ca_maxs <- cbind(Upper_Ca_maxs,
                         Upper_Ca_amount_max[,2])

  Lower_Ca_amounts <- cbind(Lower_Ca_amounts,
                            Lower_Ca_amount_max[,1])
  Lower_Ca_maxs <- cbind(Lower_Ca_maxs,
                         Lower_Ca_amount_max[,2])

  Upper_Distribution <- sapply(Datas,function(Data){
    Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
    Upper_TREE <- Data[["TREE"]][[Upper_i]]
    Upper_Params <- Data[["Params"]][[Upper_i]]
    Ratio <- Data[["Ratio"]]
    if(isGausian){
      return(c(Upper_Params[["Ca_peak"]],
               Upper_Params[["Ca_Gaus_mean"]],
               Upper_Params[["Ca_Gaus_sd"]],
               Upper_TREE[[length(Upper_TREE)]][["path_leng"]],
               Ratio))
    }else{
      return(c(Upper_Params[["Ca_Stem_conductance"]],
               Upper_Params[["Ca_taper"]],
               Upper_Params[["Length_MIEW"]],
               Upper_TREE[[length(Upper_TREE)]][["path_leng"]],
               Ratio))
    }
  })
  
  Upper_Ca_distribution[[length(Upper_Ca_distribution) + 1]] <- Upper_Distribution
  
  Lower_Ca_distribution[[length(Lower_Ca_distribution) + 1]] <-
    sapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      Lower_TREE <- Data[["TREE"]][[Lower_i]]
      Lower_Params <- Data[["Params"]][[Lower_i]]
      Ratio <- Data[["Ratio"]]        
      if(isGausian){
        return(c(Lower_Params[["Ca_peak"]],
                 Lower_Params[["Ca_Gaus_mean"]],
                 Lower_Params[["Ca_Gaus_sd"]],
                 Lower_TREE[[length(Lower_TREE)]][["path_leng"]],
                 Ratio))
      }else{
        return(c(Lower_Params[["Ca_Stem_conductance"]],
                 Lower_Params[["Ca_taper"]],
                 Lower_Params[["Length_MIEW"]],
                 Lower_TREE[[length(Lower_TREE)]][["path_leng"]],
                 Ratio))
      }
    })
  
  Upper_K_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Upper_Dend"),function(TREE){
    Conductance_amont <- calc_Conductance_amount(TREE)
    return(c(Conductance_amont[1],Conductance_amont[3]))
  }))

  Lower_K_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Lower_Dend"),function(TREE){
    Conductance_amont <- calc_Conductance_amount(TREE)
    return(c(Conductance_amont[1],Conductance_amont[3]))
  }))

  Upper_K_amounts <- cbind(Upper_K_amounts,
                           Upper_K_amount_max[,1])
  Upper_K_maxs <- cbind(Upper_K_maxs,
                        Upper_K_amount_max[,2])

  Lower_K_amounts <- cbind(Lower_K_amounts,
                           Lower_K_amount_max[,1])
  Lower_K_maxs <- cbind(Lower_K_maxs,
                        Lower_K_amount_max[,2])

  Upper_Distribution <- sapply(Datas,function(Data){
    Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
    Upper_TREE <- Data[["TREE"]][[Upper_i]]
    Upper_Params <- Data[["Params"]][[Upper_i]]
    Ratio <- Data[["Ratio"]]
    if(isGausian){
      return(c(Upper_Params[["K_peak"]],
               Upper_Params[["K_Gaus_mean"]],
               Upper_Params[["K_Gaus_sd"]],
               Upper_TREE[[length(Upper_TREE)]][["path_leng"]],
               Ratio))
    }else{
      return(c(Upper_Params[["K_Stem_conductance"]],
               Upper_Params[["K_taper"]],
               Upper_Params[["Length_MIEW"]],
               Upper_TREE[[length(Upper_TREE)]][["path_leng"]],
               Ratio))
    }
  })
  
  Upper_K_distribution[[length(Upper_K_distribution) + 1]] <- Upper_Distribution
  
  
  Lower_K_distribution[[length(Lower_K_distribution) + 1]] <-
    sapply(Datas,function(Data){
      Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
      Lower_TREE <- Data[["TREE"]][[Lower_i]]
      Lower_Params <- Data[["Params"]][[Lower_i]]
      Ratio <- Data[["Ratio"]]        
      if(isGausian){
        return(c(Lower_Params[["K_peak"]],
                 Lower_Params[["K_Gaus_mean"]],
                 Lower_Params[["K_Gaus_sd"]],
                 Lower_TREE[[length(Lower_TREE)]][["path_leng"]],
                 Ratio))
      }else{
        return(c(Lower_Params[["K_Stem_conductance"]],
                 Lower_Params[["K_taper"]],
                 Lower_Params[["Length_MIEW"]],
                 Lower_TREE[[length(Lower_TREE)]][["path_leng"]],
                 Ratio))
      }
    })
}

prefix <- paste(output_dir,name,"_",extra_prefix,"_",sep="")

rowname <- expression(paste("Optimized ",Delta,"t [ms]"))

line_type <- "solid"

Label_SEED <- paste(RAND_SEED)
Label_Delta_T <- paste(DELTA_T)

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

Ca_Ratios <- list(Upper_Ca_amounts/Upper_Ca_maxs,Lower_Ca_amounts/Lower_Ca_maxs)
names(Ca_Ratios) <- c("Upper_Ca_ratios","Lower_Ca_ratios")
showMax <- FALSE
if(WITH_Ca){
  plot_func(Ca_Ratios,color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
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
            showMax)
  Filename <- paste(prefix,"Ca_amount.xdr",sep="")
  save(Ca_amounts,file=Filename)
}

##########################

colname <-paste("Ca Conductance amount")
Filename <- paste(prefix,"TREE_Ca_conductance_amount.eps",sep="")
color <- c("black")
legend <- c()
TREE_Ca_amounts <- Ca_amounts[["Upper_Ca_amounts"]] + Ca_amounts[["Lower_Ca_amounts"]]
showMax <- FALSE
if(WITH_Ca){
  plot_func(list(TREE_Ca_amounts),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
}

##########################

colname <-paste("Ca Conductance_ratio")
Filename <- paste(prefix,"TREE_Ca_conductance_ratio.eps",sep="")
color <- c("black")
legend <- c()
TREE_Ca_maxs <- Ca_amounts[["Upper_Ca_maxs"]] + Ca_amounts[["Lower_Ca_maxs"]]
TREE_Ca_ratios <- TREE_Ca_amounts/TREE_Ca_maxs
showMax <- FALSE
if(WITH_Ca){
  plot_func(list(TREE_Ca_ratios),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
}

##########################
filename_prefix <- paste(prefix,"Ca_distribution_",sep="")
mainName <- "Ca distribution"
colname <- expression(paste("[S/c",m^2,"]",sep=""))
rowname <- expression(paste("Dendrite length [",mu,"m]",sep=""))
mapply(function(Upper_Conductance,Lower_Conductance,delta_t){
  main <- paste(delta_t,"ms ",mainName,sep="")
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
K_Ratios <- list(Upper_K_amounts/Upper_K_maxs,Lower_K_amounts/Lower_K_maxs)
names(K_Ratios) <- c("Upper_K_ratios","Lower_K_ratios")
showMax <- FALSE
if(WITH_K){
  plot_func(K_Ratios,color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
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
            showMax)
  Filename <- paste(prefix,"K_amount.xdr",sep="")
  save(K_amounts,file=Filename)
}


##########################

colname <-paste("K Conductance amount")
Filename <- paste(prefix,"TREE_K_conductance_amount.eps",sep="")
color <- c("black")
legend <- c()
TREE_K_amounts <- K_amounts[["Upper_K_amounts"]] + K_amounts[["Lower_K_amounts"]]
showMax <- FALSE
if(WITH_K){
  plot_func(list(TREE_K_amounts),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
}

##########################

colname <-paste("K Conductance_ratio")
Filename <- paste(prefix,"TREE_K_conductance_ratio.eps",sep="")
color <- c("black")
legend <- c()
TREE_K_maxs <- K_amounts[["Upper_K_maxs"]] + K_amounts[["Lower_K_maxs"]]
TREE_K_ratios <- TREE_K_amounts/TREE_K_maxs
showMax <- FALSE
if(WITH_K){
  plot_func(list(TREE_K_ratios),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
}

##########################
filename_prefix <- paste(prefix,"K_distribution_",sep="")
mainName <- "K distribution"
colname <- expression(paste("[S/c",m^2,"]",sep=""))
rowname <- expression(paste("Dendrite length [",mu,"m]",sep=""))
mapply(function(Upper_Conductance,Lower_Conductance,delta_t){
  
  main <- paste(delta_t,"ms ",mainName,sep="")
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

All_Sum_Ca_amounts <- c()
All_Sum_Ca_ratios <- c()
All_Upper_Ca_amounts <- c()
All_Lower_Ca_amounts <- c()

All_Sum_K_amounts <- c()
All_Sum_K_ratios <- c()
All_Upper_K_amounts <- c()
All_Lower_K_amounts <- c()


All_Upper_Ca_maxs <- c()
All_Lower_Ca_maxs <- c()
All_Sum_Ca_maxs <- c()

All_Upper_K_maxs <- c()
All_Lower_K_maxs <- c()
All_Sum_K_maxs <- c()

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
    All_Lower_Diams <- c(All_Lower_Diams,
                         Lower_Diams[paste(sd),paste(dt)])
    
    All_Sum_Ca_amounts <- c(All_Sum_Ca_amounts,
                            TREE_Ca_amounts[paste(sd),paste(dt)])

    All_Sum_Ca_maxs <- c(All_Sum_Ca_maxs,
                         TREE_Ca_maxs[paste(sd),paste(dt)])
     
    All_Sum_Ca_ratios <- c(All_Sum_Ca_ratios,
                           TREE_Ca_ratios[paste(sd),paste(dt)])


    All_Upper_Ca_amounts <- c(All_Upper_Ca_amounts,
                              Upper_Ca_amounts[paste(sd),paste(dt)])
    All_Upper_Ca_maxs <- c(All_Upper_Ca_maxs,
                           Upper_Ca_maxs[paste(sd),paste(dt)])

    All_Lower_Ca_amounts <- c(All_Lower_Ca_amounts,
                              Lower_Ca_amounts[paste(sd),paste(dt)])
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
    All_Upper_K_maxs <- c(All_Upper_K_maxs,
                          Upper_K_maxs[paste(sd),paste(dt)])

    All_Lower_K_amounts <- c(All_Lower_K_amounts,
                             Lower_K_amounts[paste(sd),paste(dt)])
    All_Lower_K_maxs <- c(All_Lower_K_maxs,
                          Lower_K_maxs[paste(sd),paste(dt)])
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
                             
                             TREE_Ca_amount=All_Sum_Ca_amounts,
                             TREE_Ca_max=All_Sum_Ca_maxs,
                             TREE_Ca_ratio=All_Sum_Ca_ratios,
                             Upper_Ca_amount=All_Upper_Ca_amounts,
                             Upper_Ca_max=All_Upper_Ca_maxs,
                             Lower_Ca_amount=All_Lower_Ca_amounts,
                             Lower_Ca_max=All_Lower_Ca_maxs,                             
                             TREE_K_amount=All_Sum_K_amounts,
                             TREE_K_ratio=All_Sum_K_ratios,
                             TREE_K_max=All_Sum_K_maxs,
                             Upper_K_amount=All_Upper_K_amounts,
                             Upper_K_max=All_Upper_K_maxs,
                             Lower_K_amount=All_Lower_K_amounts,
                             Lower_K_max=All_Lower_K_maxs
                             )

print(ALL_DATA_FRAME)
Filename <- paste(prefix,"All_Data_FRAME.xdr",sep="")
save(ALL_DATA_FRAME,file=Filename)
