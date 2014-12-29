plot_func <- function(Datas,Colors,row,filename,xlab,ylab,legend){
  plot.new()

  color_i <- 1

  Max_Data <- max(sapply(Datas,max))
  Min_Data <- min(sapply(Datas,min))
  
  plot(rbind(c(row[1],Min_Data),
             c(row[length(row)],Max_Data)),
       type="n",
       xlab=rowname,
       ylab=colname)

  for(Data in Datas){
    
    means <- apply(Data,2,mean)
    sds <- apply(Data,2,sd)
    maxs <- apply(Data,2,max)

    lines(cbind(row,means)
          ,col=Colors[color_i])

    lines(cbind(row,maxs),
          col=Colors[color_i],
          lty="dashed")
    

    arrows(row,means,
           row,means - sds,
           angle=90,length=0.1,lwd=2)

    arrows(row,means,
           row,means + sds,
           angle=90,length=0.1,lwd=2)
    
    color_i <- color_i + 1
  }

  if(length(legend) > 0){
    legend("topright",
           legend=legend[,1],
           col=legend[,2],
           lty=legend[,3]
           )
  }
  
  dev.copy2eps(file=filename)
}
