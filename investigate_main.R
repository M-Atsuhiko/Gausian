Type <- "Tsuishi"

source(paste(Type,"_Dendritic_function_parameter.R",sep=""))
source("plot_func.R")
source("./calc_syn_length_diameter.R")

par(lwd=3,
    cex=1.4,
    mex=1.2)

output_dir <- paste("./",Type,"_Result/",sep="")

WITH_K <- FALSE
WITH_Ca <- TRUE

RAND_SEED <- 1:5
DELTA_T <- seq(5,30,by=5)
Function_ratio <- 75
Conductance_ratio <- 0
Morphology_ratio <- 100 - (Function_ratio + Conductance_ratio)
extra_prefix <- paste("Rerative_liner_",Function_ratio,"_",Conductance_ratio,sep="")

if(WITH_K*WITH_Ca){
  name <- "k_ca"
}else if(WITH_K){
  name <- "k"
}else if(WITH_Ca){
  name <- "ca"
}else name <- "passive"

cat("inciude conductance:",name,"\n")

Data_Dir <- paste("./",name,"_Result/",sep="")

Datas <- as.list(NULL)

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

Upper_Synapse_length_diam <- c()
Lower_Synapse_length_diam <- c()

for(dt in DELTA_T){
  cat("Delta_T:",dt,"\n")
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

  Upper_Synapse_length_diam <- cbind(Upper_Synapse_length_diam,
                                     sapply(lapply(named_TREEs,"[[","Upper_Dend"),calc_syn_length_diameter))
  
  Lower_Synapse_length_diam <- cbind(Lower_Synapse_length_diam,
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
rownames(Upper_Synapse_length_diam) <- Label_SEED
colnames(Upper_Synapse_length_diam) <- Label_Delta_T
rownames(Lower_Synapse_length_diam) <- Label_SEED
colnames(Lower_Synapse_length_diam) <- Label_Delta_T
Synapse_length_diam <- list(Upper_Synapse_length_diam,Lower_Synapse_length_diam)
showMax <- FALSE
plot_func(Synapse_length_diam,color,DELTA_T,Filename,
          colname,
          rowname,
          legend,
          showMax)
Filename <- paste(prefix,"Synapse_length_diam.xdr",sep="")
save(Synapse_length_diam,file=Filename)
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
  
  colname <-paste("Ca Conductance ratio")
  Filename <- paste(prefix,"Ca_conductance_ratio.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Upper Dendrite","Lower Dendrite"),
                c("red","blue"),
                c(line_type,line_type))
  
  Ca_Ratios <- list(Upper_Ca_amounts/Upper_Ca_maxs,Lower_Ca_amounts/Lower_Ca_maxs)
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
  showMax <- FALSE
  plot_func(list(Ca_amounts[[1]],Ca_amounts[[3]]),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
  Filename <- paste(prefix,"Ca_amount.xdr",sep="")
  save(Ca_amounts,file=Filename)
}

##########################

if(WITH_K){
  rownames(Upper_K_amounts) <- Label_SEED
  colnames(Upper_K_amounts) <- Label_Delta_T
  rownames(Lower_K_amounts) <- Label_SEED
  colnames(Lower_K_amounts) <- Label_Delta_T
  
  colname <-paste("K Conductance ratio")
  Filename <- paste(prefix,"K_conductance_ratio.eps",sep="")
  color <- c("red","blue")
  legend <- cbind(c("Upper Dendrite","Lower Dendrite"),
                c("red","blue"),
                c(line_type,line_type))
  K_Ratios <- list(Upper_K_amounts/Upper_K_maxs,Lower_K_amounts/Lower_K_maxs)
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
  showMax <- FALSE
  plot_func(list(K_amounts[[1]],K_amounts[[3]]),color,DELTA_T,Filename,
            colname,
            rowname,
            legend,
            showMax)
  Filename <- paste(prefix,"K_amount.xdr",sep="")
  save(K_amounts,file=Filename)
}
