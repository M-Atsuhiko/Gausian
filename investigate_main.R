Type <- "Gausian"

source(paste(Type,"_Dendritic_function_parameter.R",sep=""))
source("plot_func.R")

par(lwd=3,
    cex=1.4,
    mex=1.2)

output_dir <- paste("./",Type,"_Result/",sep="")

WITH_K <- FALSE
WITH_Ca <- TRUE

RAND_SEED <- 1:10
DELTA_T <- 30#seq(5,30,by=5)
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

Datas <- as.list(NULL)

Fs <- c()
TREE_lengths <- c()
TREE_volumes <- c()
N_Upper_Syns <- c()
N_Lower_Syns <- c()
Upper_Diams <- c()
Lower_Diams <- c()
Ca_ratios <- c()
K_ratios <- c()

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
  
  if(WITH_Ca || WITH_K){

    divided_TREEs <- list()

    for(Data_i in 1:length(Datas)){
      divided_TREEs[[Data_i]] <- divid_and_set_conductance(Datas[[Data_i]][["TREE"]],Datas[[Data_i]][["Params"]])
    }
    
    if(WITH_Ca)
      Ca_ratios <- cbind(Ca_ratios,
                          sapply(divided_TREEs,function(TREE){
                            Conductance_amont <- calc_Conductance_amount(TREE)
                            return(Conductance_amont[2]/Conductance_amont[4])
                          }))
    if(WITH_K)
      K_ratios <- cbind(K_ratios,
                         sapply(divided_TREEs,function(TREE){
                           Conductance_amont <- calc_Conductance_amount(TREE)
                           return(Conductance_amont[1]/Conductance_amont[3])
                         }))
  }
}


prefix <- paste(output_dir,name,"_",extra_prefix,"_",sep="")

rowname <- expression(paste("Optimized ",Delta,"t [ms]"))

line_type <- "solid"

##########################
colname <- "F"
Filename <- paste(prefix,"Fs.eps",sep="")
color <- c("red")
legend <- c()
plot_func(list(Fs),color,DELTA_T,Filename,
          colname,
          rowname,
          legend)
Filename <- paste(prefix,"Fs.xdr",sep="")
save(Fs,file=Filename)

##########################

colname <- expression(paste("TREE Length [",mu,"m]"))
Filename <- paste(prefix,"TREE_lengths.eps",sep="")
color <- c("red")
legend <- c()
plot_func(list(TREE_lengths),color,DELTA_T,Filename,
          colname,
          rowname,
          legend)
Filename <- paste(prefix,"TREE_lengths.xdr",sep="")
save(TREE_lengths,file=Filename)

##########################

colname <- expression(paste("TREE Volume [",mu,m^3,"]"))
Filename <- paste(prefix,"TREE_volume.eps",sep="")
color <- c("red")
legend <- c()
plot_func(list(TREE_volumes),color,DELTA_T,Filename,
          colname,
          rowname,
          legend)
Filename <- paste(prefix,"TREE_volumes.xdr",sep="")
save(TREE_volumes,file=Filename)

##########################

colname <- expression(paste("Stem diam [",mu,"m]"))
Filename <- paste(prefix,"Stem_diam.eps",sep="")
color <- c("red","blue")
legend <- cbind(c("Red Branch","Blue Branch"),
                c("red","blue"),
                c(line_type,line_type))
Diams <- list(Upper_Diams,Lower_Diams)
plot_func(Diams,color,DELTA_T,Filename,
          colname,
          rowname,
          legend)
Filename <- paste(prefix,"Stem_diams.xdr",sep="")
save(Diams,file=Filename)

##########################

colname <-paste("Number of Synapses")
Filename <- paste(prefix,"Number_synapse.eps",sep="")
color <- c("red","blue")
legend <- cbind(c("Red Synapse","Blue Synapse"),
                c("red","blue"),
                c(line_type,line_type))
Synapses <- list(N_Upper_Syns,N_Lower_Syns)
plot_func(Synapses,color,DELTA_T,Filename,
          colname,
          rowname,
          legend)
Filename <- paste(prefix,"Synapses.xdr",sep="")
save(Synapses,file=Filename)

##########################

if(WITH_Ca){
  colname <-paste("Ca Conductance ratio")
  Filename <- paste(prefix,"Ca_conductance_ratio.eps",sep="")
  color <- "red"
  legend <- c()
  plot_func(list(Ca_ratios),color,DELTA_T,Filename,
            colname,
            rowname,
            legend)
  Filename <- paste(prefix,"Ca_tatio.xdr",sep="")
  save(Ca_ratios,file=Filename)
}

##########################

if(WITH_K){
    colname <-paste("K Conductance ratio")
    Filename <- paste(prefix,"K_conductance_ratio.eps",sep="")
    color <- "red"
    legend <- c()
    plot_func(list(K_ratios),color,DELTA_T,Filename,
              colname,
              rowname,
              legend)
    Filename <- paste(prefix,"K_tatio.xdr",sep="")
    save(K_ratios,file=Filename)
}
