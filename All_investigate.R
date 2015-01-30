source("investigate_main.R")
Type <- "Gausian"
output_dir <- paste("./",Type,"_Result/",sep="")

source(paste(Type,"_Dendritic_function_parameter.R",sep=""))
source("plot_func.R")
source("./calc_syn_length_diameter.R")
source("return_Upper_Lower_Other_i.R")
source("plot_Conductance_distribution.R")
source("plot_Conductance_synapse_position.R")
library(colorspace)

par(lwd=3,
    cex=1.4,
    mex=1.2)

Conductance_ratios <- c(5,0)
WITH_Ks <- c(FALSE,TRUE)
WITH_Cas <- c(FALSE,TRUE)


Function_ratio <- 75

for(conductance_ratio in Conductance_ratios){
  Morphology_ratio <- 100 - (Function_ratio + conductance_ratio)
  for(WITH_K in WITH_Ks){
    for(WITH_Ca in WITH_Cas){
      
      if(Type == "Gausian"){
        Conductance_distribution <- "Gaus_"
      }else if(Type == "Tsuishi"){
        Conductance_distribution <- "liner_"
      }
      
      if(WITH_K*WITH_Ca){
        name <- "k_ca"
      }else if(WITH_K){
        name <- "k"
      }else if(WITH_Ca){
        name <- "ca"
      }else{
        name <- "passive"
        if(Type == "Tsuishi" || conductance_ratio == 5) next
        else Conductance_distribution <- ""
      }

      extra_prefix <- paste("Rerative_",Conductance_distribution,Function_ratio,"_",conductance_ratio,sep="")
      
      cat("inciude conductance:",name,"\n")
    
      Data_Dir <- paste("./",name,"_Result/",sep="")
    
      Filename <- paste(paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_Datas.xdr",sep="")
      investigate_main(Type,Filename,WITH_K,WITH_Ca,Data_Dir,output_dir)
    }
  }
}


