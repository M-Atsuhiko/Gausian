Dir <- "./Result_investigation/"

source(paste(Dir,"calc_syn_length_diameter.R",sep=""))
source(paste(Dir,"calc_number_synapse.R",sep=""))
source(paste(Dir,"calc_contraction.R",sep=""))
source(paste(Dir,"Stem_diam.R",sep=""))

source("./Dendritic_function_parameter.R")
source("./TREE_simulation.R")

#Data_Directory <- "./modify_esti_Result/"
Data_Directory <- "./passive_Result/"
#Output_Dir <- paste(Dir,"esti_modify_graph/",sep="")
Output_Dir <- paste(Dir,"origi_graph/",sep="")

SEED <- 1:5
DeltaT <- seq(5,30,by=5)

Estimate_mean_sd <- c()
F_mean_sd <- c()

TREE_size_mean_sd <- c()

Num_left_syn <- c()
Num_right_syn <- c()

Len_Diam_left <- c()
Len_Diam_right <- c()

Stem_Diam_upper <- c()
Stem_Diam_lower <- c()

for(dt in DeltaT){
  TREEs <- as.list(NULL)
  for(se in SEED){
    if(dt == 5 && se != 3) next
    if(dt == 10 && (se == 1 || se == 5)) next
    cat("dt = ",dt," SEED = ",se,"\n")
    load(paste(Data_Directory,"SEED",se,"_dt",dt,"_passive_Best_TREEs.xdr",sep=""))
    TREE <- Best_TREEs[[length(Best_TREEs)]]
    TREEs[[length(TREEs) + 1]] <- TREE
    if(dt == 5 && se == 3) TREEs[[length(TREEs) + 1]] <- TREE
  }

  Syn_num <- lapply(TREEs,calc_number_synapse)
  Num_left_syn <- rbind(Num_left_syn,c(mean(sapply(Syn_num,'[',1)),sd(sapply(Syn_num,'[',1))))
  Num_right_syn <- rbind(Num_right_syn,c(mean(sapply(Syn_num,'[',2)),sd(sapply(Syn_num,'[',2))))

  Syn_len_diam <- lapply(TREEs,calc_syn_length_diameter)
  Len_Diam_left <- rbind(Len_Diam_left,c(mean(sapply(Syn_len_diam,'[',1)),sd(sapply(Syn_len_diam,'[',1))))
  Len_Diam_right <- rbind(Len_Diam_right,c(mean(sapply(Syn_len_diam,'[',2)),sd(sapply(Syn_len_diam,'[',2))))

#  Estimates <- sapply(TREEs,TREE_simulation,dt)
#  Estimate_mean_sd <- rbind(Estimate_mean_sd,c(mean(Estimates),sd(Estimates)))
  Fs <- sapply(TREEs,TREE_simulation,dt)
  F_mean_sd <- rbind(F_mean_sd,c(mean(Fs),sd(Fs)))


  size <- sapply(TREEs,calc_volume)
  TREE_size_mean_sd <- rbind(TREE_size_mean_sd,c(mean(size),sd(size)))

  diam_upper <- c()
  diam_lower <- c()
  for(TREE in TREEs){
    diam_upper <- c(diam_upper,Stem_Diam(TREE,UPPER_SYNAPTIC_ZONE_INDEX))
    diam_lower <- c(diam_lower,Stem_Diam(TREE,LOWER_SYNAPTIC_ZONE_INDEX))
  }
  
  Stem_Diam_upper <- rbind(Stem_Diam_upper,c(mean(diam_upper),sd(diam_upper)))
  Stem_Diam_lower <- rbind(Stem_Diam_lower,c(mean(diam_lower),sd(diam_lower)))

  print(F_mean_sd)
  print(TREE_size_mean_sd)
  print(Stem_Diam_upper)
  
}

#### Num of synapse
postscript(paste(Output_Dir,"Num_of_synapses.eps",sep=""),horizontal=FALSE)
par(lwd=4,
    ps=17)
plot(rbind(c(min(DeltaT),min(Num_left_syn[,1] - Num_left_syn[,2],Num_right_syn[,1]  - Num_right_syn[,2])),
           c(max(DeltaT),max(Num_left_syn[,1] + Num_left_syn[,2],Num_right_syn[,1] + Num_right_syn[,2]))),
     type="n",
     xlab=expression(paste("Optimized ",delta,"t")),
     ylab="Number of synapses")

print(mean(Num_right_syn[,1]/Num_left_syn[,1]))

lines(cbind(DeltaT,Num_left_syn[,1]),col="blue")
arrows(DeltaT,Num_left_syn[,1],DeltaT,Num_left_syn[,1] + Num_left_syn[,2],angle=90,length=0.1,col="blue",
       lwd=1)
arrows(DeltaT,Num_left_syn[,1],DeltaT,Num_left_syn[,1] - Num_left_syn[,2],angle=90,length=0.1,col="blue",
       lwd=1)

lines(cbind(DeltaT,Num_right_syn[,1]),col="red")
arrows(DeltaT,Num_right_syn[,1],DeltaT,Num_right_syn[,1] + Num_right_syn[,2],angle=90,length=0.1,col="red",
       lwd=1)
arrows(DeltaT,Num_right_syn[,1],DeltaT,Num_right_syn[,1] - Num_right_syn[,2],angle=90,length=0.1,col="red",
       lwd=1)
legend("topright",c("Red Synapse","Blue Synapse"),col=c("red","blue"),lwd=4)
dev.off()

### length vs diam
postscript(paste(Output_Dir,"length_diam.eps",sep=""),horizontal=FALSE)
par(lwd=4,
    ps=17)
plot(rbind(c(min(DeltaT),min(Len_Diam_left[,1] - Len_Diam_left[,2],Len_Diam_right[,1]  - Len_Diam_right[,2])),
           c(max(DeltaT),max(Len_Diam_left[,1] + Len_Diam_left[,2],Len_Diam_right[,1] + Len_Diam_right[,2]))),
     type="n",
     xlab=expression(paste("Optimized ",delta,"t")),
     ylab="Length to synapse/diameter")

lines(cbind(DeltaT,Len_Diam_left[,1]),col="blue")
arrows(DeltaT,Len_Diam_left[,1],DeltaT,Len_Diam_left[,1] + Len_Diam_left[,2],angle=90,length=0.1,col="blue",
       lwd=1)
arrows(DeltaT,Len_Diam_left[,1],DeltaT,Len_Diam_left[,1] - Len_Diam_left[,2],angle=90,length=0.1,col="blue",
       lwd=1)

lines(cbind(DeltaT,Len_Diam_right[,1]),col="red")
arrows(DeltaT,Len_Diam_right[,1],DeltaT,Len_Diam_right[,1] + Len_Diam_right[,2],angle=90,length=0.1,col="red",
       lwd=1)
arrows(DeltaT,Len_Diam_right[,1],DeltaT,Len_Diam_right[,1] - Len_Diam_right[,2],angle=90,length=0.1,col="red",
       lwd=1)
dev.off()

### F ###
postscript(paste(Output_Dir,"F.eps",sep=""),horizontal=FALSE)
par(lwd=4,
    ps=17)
plot(rbind(c(min(DeltaT),min(F_mean_sd[,1] - F_mean_sd[,2])),
           c(max(DeltaT),max(F_mean_sd[,1] + F_mean_sd[,2]))),
     type="n",
     xlab=expression(paste("Optimized ",delta,"t")),
     ylab="F")

lines(cbind(DeltaT,F_mean_sd[,1]),col="black")
arrows(DeltaT,F_mean_sd[,1],DeltaT,F_mean_sd[,1] + F_mean_sd[,2],angle=90,length=0.1,col="black",
       lwd=1)
arrows(DeltaT,F_mean_sd[,1],DeltaT,F_mean_sd[,1] - F_mean_sd[,2],angle=90,length=0.1,col="black",
       lwd=1)
dev.off()
### Estimate ###
## postscript(paste(Output_Dir,"Estimate.eps",sep=""),horizontal=FALSE)
## par(lwd=4,
##     ps=17)
## plot(rbind(c(min(DeltaT),min(Estimate_mean_sd[,1] - Estimate_mean_sd[,2])),
##            c(max(DeltaT),max(Estimate_mean_sd[,1] + Estimate_mean_sd[,2]))),
##      type="n",
##      xlab=expression(paste("Optimized ",delta,"t")),
##      ylab="Estimate")

## lines(cbind(DeltaT,Estimate_mean_sd[,1]),col="black")
## arrows(DeltaT,Estimate_mean_sd[,1],DeltaT,Estimate_mean_sd[,1] + Estimate_mean_sd[,2],angle=90,length=0.1,col="black",
##        lwd=1)
## arrows(DeltaT,Estimate_mean_sd[,1],DeltaT,Estimate_mean_sd[,1] - Estimate_mean_sd[,2],angle=90,length=0.1,col="black",
##        lwd=1)
## dev.off()

### TREE size ###
postscript(paste(Output_Dir,"Size.eps",sep=""),horizontal=FALSE)
par(lwd=4,
    ps=17)
plot(rbind(c(min(DeltaT),min(TREE_size_mean_sd[,1] - TREE_size_mean_sd[,2])),
           c(max(DeltaT),max(TREE_size_mean_sd[,1] + TREE_size_mean_sd[,2]))),
     type="n",
     xlab=expression(paste("Optimized ",delta,"t")),
     ylab="Neuron Size")

lines(cbind(DeltaT,TREE_size_mean_sd[,1]),col="black")
arrows(DeltaT,TREE_size_mean_sd[,1],DeltaT,TREE_size_mean_sd[,1] + TREE_size_mean_sd[,2],angle=90,length=0.1,col="black",
       lwd=1)
arrows(DeltaT,TREE_size_mean_sd[,1],DeltaT,TREE_size_mean_sd[,1] - TREE_size_mean_sd[,2],angle=90,length=0.1,col="black",
       lwd=1)
dev.off()

### diam ###
postscript(paste(Output_Dir,"Diam.eps",sep=""),horizontal=FALSE)
par(lwd=4,
    ps=15)
plot(rbind(c(min(DeltaT),min(Stem_Diam_lower[,1] - Stem_Diam_lower[,2],Stem_Diam_upper[,1]  - Stem_Diam_upper[,2])),
           c(max(DeltaT),max(Stem_Diam_lower[,1] + Stem_Diam_lower[,2],Stem_Diam_upper[,1] + Stem_Diam_upper[,2]))),
     type="n",
     xlab=expression(paste("Optimized ",delta,"t")),
     ylab=expression(paste("Stem diameter ",mu,"m")))

lines(cbind(DeltaT,Stem_Diam_lower[,1]),col="blue")
arrows(DeltaT,Stem_Diam_lower[,1],DeltaT,Stem_Diam_lower[,1] + Stem_Diam_lower[,2],angle=90,length=0.1,col="blue",
       lwd=1)
arrows(DeltaT,Stem_Diam_lower[,1],DeltaT,Stem_Diam_lower[,1] - Stem_Diam_lower[,2],angle=90,length=0.1,col="blue",
       lwd=1)

lines(cbind(DeltaT,Stem_Diam_upper[,1]),col="red")
arrows(DeltaT,Stem_Diam_upper[,1],DeltaT,Stem_Diam_upper[,1] + Stem_Diam_upper[,2],angle=90,length=0.1,col="red",
       lwd=1)
arrows(DeltaT,Stem_Diam_upper[,1],DeltaT,Stem_Diam_upper[,1] - Stem_Diam_upper[,2],angle=90,length=0.1,col="red",
       lwd=1)
legend("topright",c("Red","Blue"),col=c("red","blue"),lwd=4)
dev.off()
