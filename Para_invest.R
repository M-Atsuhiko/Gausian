source("Dendritic_function_parameter.R")

plot_dgamma <- function(alfa,beta,length,point,Bif_or_not){
  peak <- dgamma(beta*(alfa - 1),shape=alfa,scale=beta)
  curve((dgamma(x,shape=alfa,scale=beta)/peak)*MAX_PEAK,0,length,
        xlim=c(0,500),
        ylim=c(0,MAX_PEAK))
  points(point,(dgamma(point,shape=alfa,scale=beta)/peak)*MAX_PEAK,
         col=Bif_or_not)
  par(new=TRUE)
}

Dir <- "./Result_investigation/"

WITH_K <- FALSE
WITH_Ca <- TRUE
RAND_SEED <- 1
DELTA_T <- 30
Function_ratio <- 75
Conductance_ratio <- 25
Morphology_ratio <- 100 - Function_ratio
extra_prefix <- "ordinal"

if(WITH_K*WITH_Ca){
  name <- "k_ca"
}else if(WITH_K){
  name <- "k"
}else if(WITH_Ca){
  name <- "ca"
}else name <- "passive"

cat("Delta_T:",DELTA_T,"\n")
cat("SEED:",RAND_SEED,"\n")
cat(name,"\n")

#Data_Dir <- paste("./",name,"_Result/",sep="")
Data_Dir <- "./test/"
input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",DELTA_T,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Last_Generation.xdr",sep="")
#load(input_filename)
#input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",DELTA_T,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_Datas.xdr",sep="")
load(input_filename)

Upper_Param_mx <- c()
Lower_Param_mx <- c()
other_Param_mx <- c()

good_ratios <- c()
alfa_beta <- c()
par(new=FALSE)
frame()

## Best_Data <- Best_Datas[[length(Best_Datas)]]
## Params <- Best_Data[["Params"]]

## for(Param in Params){
##   peak <- Param[["Ca_peak"]]
##   Gaus_Max <- dnorm(Gaus_mean,mean=Gaus_mean,sd=Gaus_sd)
##   print(peak)
##   Gaus_mean <- Param[["Ca_Gaus_mean"]]
##   Gaus_sd <- Param[["Ca_Gaus_sd"]]
##   curve(dnorm(x,mean=Gaus_mean,sd=Gaus_sd)*peak/Gaus_Max,0,1,
##         ylim=c(0,0.0025))
##   par(new=TRUE)
## }
## par(new=FALSE)


for(Individual_Data in Last_Generation){

  ratio <- Individual_Data[["Ratio"]]
  if(ratio < (BAD_RATIO)^(-1)) next

  good_ratios <- c(good_ratios,ratio)
  ## cat("Rank:",Individual_Data[["Rank"]],"\n")
  ## cat("Ratio:",ratio,"\n")
  ## cat("Parent:",Individual_Data[["Parent"]],"\n")
  
  Upper_i <- c()
  Lower_i <- c()
  other_i <- c()
  
  TREE <- Individual_Data[["TREE"]]

  for(Dend_i in 1:length(TREE)){
    Dendrite <- TREE[[Dend_i]]
    synapse_frag <- 0
    for(Branch in Dendrite){
      if(is.matrix(Branch[["synapse"]])){
        if(Branch[["synapse"]][1,2] == UPPER_SYNAPTIC_ZONE_INDEX) Upper_i <- c(Upper_i,Dend_i)
        else Lower_i <- c(Lower_i,Dend_i)
        synapse_frag <- 1
        break
      }
    }
    if(synapse_frag == 0) other_i <- c(other_i,Dend_i)
  }

  Params <- Individual_Data[["Params"]]
  
  for(Param_i in Upper_i){
    Param <- Params[[Param_i]]
    Param_vect <- c()
    for(p in Param){
      Param_vect <- c(Param_vect,p)
    }
    Upper_Param_mx <- rbind(Upper_Param_mx,Param_vect)
  }
  for(Param_i in Lower_i){
    Param <- Params[[Param_i]]
    Param_vect <- c()
    for(p in Param){
      Param_vect <- c(Param_vect,p)
    }

    ## TREE_length <- length(TREE[[Param_i]])
    ## path_leng <- TREE[[Param_i]][[TREE_length]][["path_leng"]]
    ## point <- sapply(TREE[[Param_i]],"[[","path_leng")
    ## Bif_or_not <- sapply(sapply(TREE[[Param_i]],"[[","connect"),function(connect){
    ##   if(length(connect) == 2) return("red")
    ##   else return("black")
    ## })
    ## plot_dgamma(Param[["Bif_alfa"]],Param[["Bif_beta"]],path_leng,point,Bif_or_not)
    ## alfa_beta <- rbind(alfa_beta,c(Param[["Bif_alfa"]],Param[["Bif_beta"]]))
    Lower_Param_mx <- rbind(Lower_Param_mx,Param_vect)
  }

  for(Param_i in other_i){
    Param <- Params[[Param_i]]
    Param_vect <- c()
    for(p in Param){
      Param_vect <- c(Param_vect,p)
    }
    other_Param_mx <- rbind(other_Param_mx,Param_vect)
  }
}

cat("N_upper:",nrow(Upper_Param_mx),"\n")
cat("N_lower:",nrow(Lower_Param_mx),"\n")

colnames(Upper_Param_mx) <- Param_Labels
colnames(Lower_Param_mx) <- Param_Labels
colnames(other_Param_mx) <- Param_Labels

M <- 4
par(mfrow=c(M,floor(length(Param_Labels)/M)),
    oma=c(0,0,6,0))
for(name in Param_Labels){
  hist(Lower_Param_mx[,name],main=name)
}
mtext("ca Lower Params Hists",side=3,outer=TRUE,cex=3)
#dev.copy2eps(file="other_params.eps")
