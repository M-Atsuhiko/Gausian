property <- "passive"
SEED <- c(2,3,4)
extra <- "times_Ldet"

colors <- 1:10
par(lwd = 3)

input_Dir <- paste("./",property,"_Result/",sep="")
Dts <- seq(5,30,5)

length <- c()
Bif_alfa <- c()
Bif_beta <- c()
Trm_alfa <- c()
Trm_beta <- c()

for(seed in SEED){
  for(dt in Dts){
    load(paste(input_Dir,"SEED",seed,"_dt",dt,"_",property,"_FR75_",extra,"_Best_Datas.xdr",sep=""))
    Params <- Best_Datas[[length(Best_Datas)]][["Params"]]
    TREE <- Best_Datas[[length(Best_Datas)]][["TREE"]]
    for(Dend_N in 1:length(TREE)){
      for(Branch in TREE[[Dend_N]]){
        if(is.matrix(Branch[["synapse"]])){
          if(Branch[["synapse"]][1,2] == 0){#Lower
            length <- c(length,Params[[Dend_N]][["Length_MIEW"]])
            Bif_alfa<- c(Bif_alfa,Params[[Dend_N]][["Bif_alfa"]])
            Bif_beta<- c(Bif_beta,Params[[Dend_N]][["Bif_beta"]])
            Trm_alfa<- c(Trm_alfa,Params[[Dend_N]][["Trm_alfa"]])
            Trm_beta<- c(Trm_beta,Params[[Dend_N]][["Trm_beta"]])
          }
        }
      }
    }
  }
}

cat("finish\n")

plot.new()

MAX_PEAK <- 0.8
for(i in 1:length(Bif_alfa)){
  print(i)

  curve(pgamma(x,shape=Trm_alfa[i],scale=Trm_beta[i]),
        xlim=c(0,170),
        ylim=c(0,1),
        col=colors[i])
  
  points(seq(0,170,by=length[i]),pgamma(seq(0,170,by=length[i]),shape=Trm_alfa[i],scale=Trm_beta[i]),
        col=colors[i])
  
#  peak <- dgamma(Bif_beta[i]*(Bif_alfa[i] - 1),shape = Bif_alfa[i], scale = Bif_beta[i])
#  curve((dgamma(x,shape=Bif_alfa[i],scale=Bif_beta[i])/peak)*MAX_PEAK,
#        xlim=c(0,2000),
#        ylim=c(0,1),
#        col=colors[i])
  
#  points(seq(0,170,by=length[i]),(dgamma(seq(0,170,by=length[i]),shape=Bif_alfa[i],scale=Bif_beta[i])/peak)*MAX_PEAK,
#         col=colors[i])
  par(new=TRUE)
}
par(new=FALSE)
