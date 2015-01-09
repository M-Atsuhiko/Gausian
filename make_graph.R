make_F_graph <- function(property,lineColor,point_mk,output_file){
  par(lwd=3,
      pch=5,
      cex=1.4,
      mar=c(4,4,3,6))

  Data_Dir <- "./Result/"

  All_Datas <- as.list(NULL)

  Ratio_Max <- -100
  Ratio_Min <- 100

  for(prop in property){
    filename <- paste(Data_Dir,prop,"_FR75_result_times_Ldet_save_Ratio.xdr",sep="")
    load(filename)
    save_Ratio <- save_Ratio[,-1]
    All_Datas[[length(All_Datas) + 1]] <- save_Ratio
    Ratio_Max <- max(save_Ratio,Ratio_Max)
    Ratio_Min <- min(save_Ratio,Ratio_Min)
  }

  Dts <- seq(5,30,5)

  Datas <- lapply(All_Datas,function(Ratios){
    Mean <- apply(Ratios,1,mean)
    sd <- apply(Ratios,1,sd)
    max <- apply(Ratios,1,max)
    return(cbind(Mean,sd,max))
  })
  print(Datas)
  plot(rbind(c(Dts[1],Ratio_Max),
             c(Dts[length(Dts)],Ratio_Min)),
       type="n",
       main="Function ratio",
       xlab=expression(paste("Optimized ",delta,"t")),
       ylab="F")

  par(lty="solid")
  for(i in 1:length(property)){#mean
    lines(cbind(Dts,Datas[[i]][,1]),
          col=lineColor[i])
    points(cbind(Dts,Datas[[i]][,1]),
           col=lineColor[i],
           pch=point_mk[i])
    for(j in 1:2)#後茶房
      arrows(Dts,Datas[[i]][,1],Dts,Datas[[i]][,1] + ((-1)^(j))*Datas[[i]][,2],
             angle=90,
             length = 0.1,
             lwd=0.7,
             col=lineColor[i])
  }

  par(lty="dashed")
  for(i in 1:length(property)){#Max
    lines(cbind(Dts,Datas[[i]][,3]),
          col=lineColor[i])
  }
  
  par(lty="solid",xpd=TRUE)
  legend(par()$usr[2],par()$usr[4],legend=property,
         col=lineColor,
         lty=rep("solid",length(property)),
         pch=point_mk)

  output_file <- paste(Output_Dir,"all_Function.eps",sep="")
  cat("output_file:",output_file,"\n")
  dev.copy2eps(file=output_file)
}



property <- c("passive","k","ca","k_ca")
lineColor <- c("blue","red","green","black")
point_mk <- c(4,11,22,1)
Output_Dir <- "./madeGraphs/"

output_file <- paste(Output_Dir,"all_Function.eps")

make_F_graph(property,
             lineColor,
             point_mk,
             output_file)
