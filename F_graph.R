RAND_SEED <- 1
DELTA_T <- seq(5,25,by=5)
name <- "passive"
Function_ratio <- 75
Conductance_ratio <- 0
extra_prefix <- paste("Tsuishi_liner_",Function_ratio,"_",Conductance_ratio,sep="")
Data_Dir <- paste("./",name,"_Result/",sep="")

MAX_GENERATION <- 500
GENERATION <- 1:MAX_GENERATION

Fs <- c()
for(dt in DELTA_T){
  input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",dt,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_Datas.xdr",sep="")
  print(input_filename)
  load(input_filename)

  Fs <- rbind(Fs,sapply(Best_Datas,"[[","Ratio"))
  rm(Best_Datas)
}

plot(rbind(c(0,0),c(MAX_GENERATION,max(Fs))),
     type="n")

for(i in 1:nrow(Fs)){
  lines(cbind(GENERATION,Fs[i,]),
        col=i)
}
