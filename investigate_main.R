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

RAND_SEED <- 1:10
DELTA_T <- seq(5,30,by=5)
Function_ratio <- 75
Conductance_ratio <- 0
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
N_Upper_Syns <- c()
N_Lower_Syns <- c()
Upper_Diams <- c()
Lower_Diams <- c()

Upper_Ca_amounts <- c()
Lower_Ca_amounts <- c()
Upper_K_amounts <- c()
Lower_K_amounts <- c()

Upper_Ca_maxs <- c()
Lower_Ca_maxs <- c()
Upper_K_maxs <- c()
Lower_K_maxs <- c()

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
  
  
  if(WITH_Ca || WITH_K){

    divided_named_TREEs <- list()

    for(Data_i in 1:length(Datas)){
      divided_named_TREEs[[Data_i]] <- set_Upper_or_Lower_or_Other(
        divid_and_set_conductance(Datas[[Data_i]][["TREE"]],Datas[[Data_i]][["Params"]]))
    }
    
    if(WITH_Ca){
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
        if(Type == "Gausian"){
          return(c(Upper_Params[["Ca_peak"]],
                   Upper_Params[["Ca_Gaus_mean"]],
                   Upper_Params[["Ca_Gaus_sd"]],
                   Upper_TREE[[length(Upper_TREE)]][["path_leng"]]))
        }else{
          return(c(Upper_Params[["Ca_Stem_conductance"]],
                   Upper_Params[["Ca_taper"]],
                   Upper_TREE[[length(Upper_TREE)]][["path_leng"]]))
        }
      })
      
      Upper_Ca_distribution[[length(Upper_Ca_distribution) + 1]] <- Upper_Distribution
      
      
      Lower_Ca_distribution[[length(Lower_Ca_distribution) + 1]] <-
        sapply(Datas,function(Data){
          Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
          Lower_TREE <- Data[["TREE"]][[Lower_i]]
          Lower_Params <- Data[["Params"]][[Lower_i]]
                    if(Type == "Gausian"){
                      return(c(Lower_Params[["Ca_peak"]],
                               Lower_Params[["Ca_Gaus_mean"]],
                               Lower_Params[["Ca_Gaus_sd"]],
                               Lower_TREE[[length(Lower_TREE)]][["path_leng"]]))
                    }else{
                      return(c(Lower_Params[["Ca_Stem_conductance"]],
                               Lower_Params[["Ca_taper"]],
                               Lower_TREE[[length(Lower_TREE)]][["path_leng"]]))
                    }
        })
    }

    if(WITH_K){
      Upper_K_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Upper_Dend"),function(TREE){
        Conductance_amont <- calc_Conductance_amount(TREE)
        return(c(Conductance_amont[2],Conductance_amont[4]))
      }))

      Lower_K_amount_max <- t(sapply(lapply(divided_named_TREEs,"[[","Lower_Dend"),function(TREE){
        Conductance_amont <- calc_Conductance_amount(TREE)
        return(c(Conductance_amont[2],Conductance_amont[4]))
      }))

      Upper_K_amounts <- cbind(Upper_K_amounts,
                                     Upper_K_amount_max[,1])
      Upper_K_maxs <- cbind(Upper__K_amounts,
                                  Upper_K_amount_max[,2])

      Lower_K_amounts <- cbind(Lower_K_amounts,
                                     Lower_K_amount_max[,1])
      Lower_K_maxs <- cbind(Lower_K_amounts,
                                 Lower_K_amount_max[,2])
      
      Upper_K_distribution[[length(Upper_K_distribution) + 1]] <-
        sapply(Datas,function(Data){
          Upper_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Upper_Dend"]]
          Upper_TREE <- Data[["TREE"]][[Upper_i]]
          Upper_Params <- Data[["Params"]][[Upper_i]]
                    if(Type == "Gausian"){
                      return(c(Upper_Params[["K_peak"]],
                               Upper_Params[["K_Gaus_mean"]],
                               Upper_Params[["K_Gaus_sd"]],
                               Upper_TREE[[length(Upper_TREE)]][["path_leng"]]))
                    }else{
                      return(c(Upper_Params[["K_Stem_conductance"]],
                               Upper_Params[["K_taper"]],
                               Upper_TREE[[length(Upper_TREE)]][["path_leng"]]))
                    }
        })

      Lower_K_distribution[[length(Lower_K_distribution) + 1]] <-
        sapply(Datas,function(Data){
          Lower_i <- return_Upper_Lower_Other_i(Data[["TREE"]])[["Lower_Dend"]]
          Lower_TREE <- Data[["TREE"]][[Lower_i]]
          Lower_Params <- Data[["Params"]][[Lower_i]]
                    if(Type == "Gausian"){
                      return(c(Lower_Params[["K_peak"]],
                               Lower_Params[["K_Gaus_mean"]],
                               Lower_Params[["K_Gaus_sd"]],
                               Lower_TREE[[length(Lower_TREE)]][["path_leng"]]))
                    }else{
                      return(c(Lower_Params[["K_Stem_conductance"]],
                               Lower_Params[["K_taper"]],
                               Lower_TREE[[length(Lower_TREE)]][["path_leng"]]))
                    }
        })
    }
  }
}


prefix <- paste(output_dir,name,"_",extra_prefix,"_",sep="")

rowname <- expression(paste("Optimized ",Delta,"t [ms]"))

line_type <- "solid"

Label_SEED <- paste("SEED:",RAND_SEED,sep="")
Label_Delta_T <- paste("dt:",DELTA_T,sep="")

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

if(WITH_Ca){
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
  plot_func(Ca_Ratios,color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
  Filename <- paste(prefix,"Ca_ratio.xdr",sep="")
  save(Ca_Ratios,file=Filename)
  
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
  plot_func(list(Ca_amounts[[1]],Ca_amounts[[3]]),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
  Filename <- paste(prefix,"Ca_amount.xdr",sep="")
  save(Ca_amounts,file=Filename)

##########################

  filename_prefix <- paste(prefix,"Ca_distribution_",sep="")
  mainName <- "Ca distribution"
  colname <- expression(paste("[S/c",m^2,"]",sep=""))
  rowname <- expression(paste("Dendrite length [",mu,"m]",sep=""))
  mapply(function(Upper_Conductance,Lower_Conductance,delta_t){
    main <- paste(delta_t,"ms ",mainName,sep="")
    Filename <- paste(filename_prefix,"dt",delta_t,".eps",sep="")
    plot_Conductance_distribution(Upper_Conductance,
                                  Lower_Conductance,
                                  main,
                                  colname,
                                  rowname,
                                  Filename,
                                  TRUE)},
         Upper_Ca_distribution,Lower_Ca_distribution,DELTA_T)
}

##########################

if(WITH_K){
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
  plot_func(K_Ratios,color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
  Filename <- paste(prefix,"K_ratio.xdr",sep="")
  save(K_Ratios,file=Filename)
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
  plot_func(list(K_amounts[[1]],K_amounts[[3]]),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
  Filename <- paste(prefix,"K_amount.xdr",sep="")
  save(K_amounts,file=Filename)
}
