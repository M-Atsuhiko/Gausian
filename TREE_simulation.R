source("Gausian_Dendritic_function_parameter.R")
source("display_conductance_on_morphology.R")
source("TREE_simulation_function.R")
source("./display_morphology.R")
source("./display_synapse.R")
source("./display_synaptic_zone.R")
source("TREE_2simulations_K.R")
source("TREE_2simulations_Ca.R")
source("other_dt_test.R")

Dir <- "../R_functions/"
source(paste(Dir,"calc_syn_length_diameter.R",sep=""))
source(paste(Dir,"calc_number_synapse.R",sep=""))
source(paste(Dir,"calc_contraction.R",sep=""))
source(paste(Dir,"Stem_diam.R",sep=""))

WITH_K <- TRUE
WITH_Ca <- FALSE
RAND_SEED <- 3
DELTA_T <- 10
Function_ratio <- 75
Conductance_ratio <- 5
Morphology_ratio <- 100 - (Function_ratio + Conductance_ratio*(WITH_K || WITH_Ca))
extra_prefix <- paste("Rerative_Gaus_",Function_ratio,"_",Conductance_ratio,sep="")
#extra_prefix <- paste("volume_",Function_ratio,"_",Conductance_ratio,sep="")
#extra_prefix <- paste("volume_",Function_ratio,"_",Conductance_ratio,sep="")
#extra_prefix <- "times_Ldet"

if(WITH_K*WITH_Ca){
  name <- "k_ca"
}else if(WITH_K){
  name <- "k"
}else if(WITH_Ca){
  name <- "ca"
}else name <- "passive"

## if(WITH_Ca && !(WITH_K)){
##   SIM_TIME                     <- 100 #シミュレーションの長さ
## }else{
##   SIM_TIME                     <- 50 #シミュレーションの長さ
## }

SIM_TIME <- 50

cat("Delta_T:",DELTA_T,"\n")
cat("SEED:",RAND_SEED,"\n")
cat("inciude conductance:",name,"\n")

#Data_Dir <- paste("~/Datas/Tsuishi/",name,"_Result/",sep="")
#Data_Dir <- "./test/"
Data_Dir <- paste("./",name,"_Result/",sep="")

#load(paste(Data_Dir,"SEED",RAND_SEED,"_dt",DELTA_T,"_",name,"NA_LAST_TREEs.xdr",sep=""))
#load(paste(Data_Dir,"SEED",RAND_SEED,"_dt",DELTA_T,"_",name,"NA_Best_TREEs.xdr",sep=""))
input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",DELTA_T,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_Datas.xdr",sep="")
#input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",DELTA_T,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_TREEs.xdr",sep="")
cat("input file:",input_filename,"\n")
load(input_filename)

GENERATION <- length(Best_Datas)
#GENERATION <- length(Best_TREEs)
                                        #GENERATION <- c(1,seq(0,650,by=50)[-1])
for(i in GENERATION){
  cat("GENERATION:",i,"\n")
  cat("dt:",DELTA_T,"\n")

#  TREE <- Best_TREEs[[i]]
  TREE <- Best_Datas[[i]][["TREE"]]
  Params <- Best_Datas[[i]][["Params"]]

#  filename <- "Display"
  filename <- paste("~/Desktop/",name,"_EPSP.eps",sep="")

  named_TREE <- set_Upper_or_Lower_or_Other(TREE)
  
  divided_TREE <- divid_and_set_conductance(TREE,Params)

  K_Ca_Conductace <- calc_Conductance_amount(divided_TREE)

  N_Upper_synapse <- calc_number_synapse(named_TREE[["Upper_Dend"]])
  N_Lower_synapse <- calc_number_synapse(named_TREE[["Lower_Dend"]])
  
  cat("lower num syn: ",N_Lower_synapse,"upper num syn:",N_Upper_synapse,"\n")
  cat("lower stem diam:",Stem_Diam(named_TREE[["Lower_Dend"]]),"\n")
  cat("upper stem diam:",Stem_Diam(named_TREE[["Upper_Dend"]]),"\n")
  cat("length-diam",calc_syn_length_diameter(named_TREE[["Upper_Dend"]]),calc_syn_length_diameter(named_TREE[["Lower_Dend"]]),"\n")
  cat("sum length:",sum_length(divided_TREE),"Min length:",L0,"\n")
  cat("sum volume:",calc_volume(divided_TREE),"Min volume:",V0,"\n")
  cat("Amount of K,Ca:",K_Ca_Conductace[1],",",K_Ca_Conductace[2],"(S)\n")
  cat("Max Amount of K,Ca:",K_Ca_Conductace[3],",",K_Ca_Conductace[4],"(S)\n")
  cat("Parcent:",round(100*K_Ca_Conductace[1]/K_Ca_Conductace[3],digits=3),",",
      round(100*K_Ca_Conductace[2]/K_Ca_Conductace[4],digits=3),"(%)\n")

  ## if(WITH_Ca){
  ##   Peak_Max <- -100
  ##   Conductance_curves <- c()
  ##   for(Param in Params){
  ##     Ca_peak <- Param[["Ca_peak"]]
  ##     Gaus_mean <- Param[["Ca_Gaus_mean"]]
  ##     Gaus_sd <- Param[["Ca_Gaus_sd"]]

  ##     Gaus_peak <- dnorm(Gaus_mean,mean=Gaus_mean,sd=Gaus_sd)
  ##     Conductance_curves <- rbind(Conductance_curves,c(Ca_peak,Gaus_mean,Gaus_sd,Gaus_peak))

  ##     Peak_Max <- max(Peak_Max,Ca_peak)
  ##   }

  ##   frame()
  ##   par(mfrow=c(1,2),
  ##       lwd=2)
  ##   for(plot_i in 1:nrow(Conductance_curves)){
  ##     if(plot_i == 1) color <- "red"
  ##     else if(plot_i == 2) color <- "blue"
  ##     row <- Conductance_curves[plot_i,]
  ##     curve((dnorm(x,mean=row[2],sd=row[3])/row[4])*row[1],0,1,
  ##           ylim=c(0,Peak_Max),
  ##           ylab="",
  ##           col=color)
  ##     par(new=TRUE)
  ##   }
  ##   mtext("conductance distribution [S/cm^2]",
  ##         side=2,
  ##         line=2.5)

  ##   par(new=FALSE)
  ## }

  if(N_Upper_synapse > 0 && N_Lower_synapse > 0){
#    print(TREE_simulation_function(divided_TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)[1:2])
#    if(WITH_K)
      TREE_2simulations_K(divided_TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)
#    if(WITH_Ca)
#      TREE_2simulations_Ca(divided_TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)
    ## else{
#    other_dt_test(divided_TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)
#    }
  }else{
    cat("This neuron can't simulation.\n")
  }

  if(WITH_K && WITH_Ca){
    display_conductance_on_morphology(divided_TREE,"Ca_conductance")
  }else if(WITH_Ca){
    display_conductance_on_morphology(divided_TREE,"Ca_conductance")
  }else if(WITH_K){
    display_conductance_on_morphology(divided_TREE,"K_conductance")
  }else {
    display_morphology(TREE)
  }
                                        #display_synaptic_zone()

  ## webGL_output <- "~/Datas/webGL/TREE_sim.html"
  ## writeWebGL(width=750,height=750,
  ##            dir="~/Datas/webGL",
  ##            filename=webGL_output,
  ##            snapshot=TRUE)

#  cat("webGL output:",webGL_output,"\n")
#  readline("next?")
}
#rgl.shapshot("~/workspace/Syuron/Images_Result/",name,"_gaus_TREE_sample_dt",DELTA_T,"_C",Conductance_ratio,".png")
#rgl.snapshot(file=paste("~/Desktop/",name,"_gaus_TREE_sample_dt",DELTA_T,"_C",Conductance_ratio,".png",sep=""))
#dev.copy2eps(file=paste("~/Desktop/",name,"_gaus_somaV_dt",DELTA_T,"_C",Conductance_ratio,".eps",sep=""))
