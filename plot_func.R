plot_func <- function(Datas,Colors,row,filename,colname,rowname,legend){
  plot.new()

  mean_sd_max_Datas <- lapply(Datas,function(Data){
    return(rbind(apply(Data,2,mean),
                 apply(Data,2,sd),
                 apply(Data,2,max)))
  })

  Max_Data <- max(sapply(mean_sd_max_Datas,function(Data){
    return(max(Data[1,] + Data[2,]))
  }))

  Min_Data <- min(sapply(mean_sd_max_Datas,function(Data){
    return(min(Data[1,] - Data[2,]))
  }))

  plot(rbind(c(row[1],Min_Data),
             c(row[length(row)],Max_Data)),
       type="n",
       xlab=rowname,
       ylab=colname)

  mapply(function(Data,color){
    means <- Data[1,]
    sds <- Data[2,]
    maxs <- Data[3,]
    arrows(row,means,
           row,means - sds,
           angle=90,length=0.1,lwd=2)

    arrows(row,means,
           row,means + sds,
           angle=90,length=0.1,lwd=2)

    if(ncol(Data) == 1){
      points(row,means,
             col=color,
             cex=2,
             pch=16)
    }else{
      lines(cbind(row,means),
            col=color,
            lty=line_type)

      lines(cbind(row,maxs),
            col=color,
            lty="dashed")
    }
  },mean_sd_max_Datas,
         Colors)

  if(length(legend) > 0){
    legend("topright",
           legend=legend[,1],
           col=legend[,2],
           lty=legend[,3]
           )
  }

  dev.copy2eps(file=filename)
}
