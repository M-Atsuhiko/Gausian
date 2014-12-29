source("Dendritic_function_parameter.R")
source("display_conductance_on_morphology.R")

Dir <- "./WebGL/"

propertys <- c("k","ca","k_ca")
passive_SEED <- c(2:7,9,10)
k_SEED <- 2:5
ca_SEED <- c(2,4:6)
k_ca_SEED <- c(2,4:6)

Delta_T<- seq(5,30,by=5)

for(prop in propertys){
  if(prop == "passive") SEED <- passive_SEED
  else if(prop == "k") SEED <- k_SEED
  else if(prop == "ca") SEED <- ca_SEED
  else SEED <- k_ca_SEED
  for(seed in SEED){
    for(dt in Delta_T){
      cat(paste(prop,"_SEED",seed,"_dt_",dt,sep=""),"\n")
      load(paste("./",prop,"_Result/SEED",seed,"_dt",dt,"_",prop,"NA_Best_TREEs.xdr",sep=""))
      TREE <- Best_TREEs[[length(Best_TREEs)]]
      conductance <- ""
      if(prop == "passive")
        display_morphology(TREE)
      else if(prop == "k"){
        display_conductance_on_morphology(TREE,"K_conductance")
        conductance <- "_K"
      }
      else if(prop == "ca"){
        display_conductance_on_morphology(TREE,"Ca_conductance")
        conductance <- "_Ca"
      }
      else{
        display_conductance_on_morphology(TREE,"K_conductance")
        rgl.texts(300,100,0,text=paste(prop,"_SEED",seed,"_dt_",dt,sep=""))
        conductance <- "_K"
      }
      
      rgl.texts(300,100,0,text=paste(prop,"_SEED",seed,"_dt_",dt,sep=""))
      writeWebGL(filename=file.path(Dir,paste(prop,"_SEED",seed,"_dt_",dt,conductance,".html",sep="")),snapshot=FALSE,width=1000,height=1000)

      if(prop == "k_ca"){
        display_conductance_on_morphology(TREE,"Ca_conductance")
        conductance <- "_Ca"
        rgl.texts(300,100,0,text=paste(prop,"_SEED",seed,"_dt_",dt,sep=""))
        writeWebGL(filename=file.path(Dir,paste(prop,"_SEED",seed,"_dt_",dt,".html",sep="")),snapshot=FALSE,width=1000,height=1000)
      }
    }
  }
}



