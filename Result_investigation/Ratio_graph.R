data_dir <- "../Result/"

property <- c("passive","k","ca","k_ca")
Delta_T<- seq(5,30,by=5)
Data_Matrix <- c()

for(name in property){
  load(paste(data_dir,name,"_result_save_Ratio.xdr",sep=""))
  data <- save_Ratio[,2:ncol(save_Ratio)]
  data <- apply(data,1,"mean")
  Data_Matrix <- cbind(Data_Matrix,data)
}

postscript("All_Ratio.eps",horizontal=FALSE)
plot(cbind(c(min(Delta_T),max(Delta_T)),c(min(Data_Matrix),max(Data_Matrix))),
     type="n",
     xlab=expression(paste("Optimized ",Delta,"t [ms]")),
     ylab="F",
     ps=17)

color <- c("black","red","green","black")
mark <- c(4,11,22,1)
for(i in 1:4){
  lines(Delta_T,Data_Matrix[,i],col=color[i])
  points(Delta_T,Data_Matrix[,i],col=color[i],pch=mark[i])
}

legend("topright",legend=property,col=color,pch=mark,lty=c(1,1,1,1))
dev.off()
