plot_Conductance_distribution <- function(Upper_Data,
                                          Lower_Data,
                                          mainName,
                                          colname,
                                          rowname,
                                          filename,
                                          isGausian,
                                          Conductance_Max){
  frame()

  par(oma=c(0,0,0,5),
      xpd=NA)
  if(isGausian){

    Max_peak <- max(Upper_Data[1,],Lower_Data[1,])

    Upper_Max_Length <- max(Upper_Data[4,])
    Lower_Max_Length <- max(Lower_Data[4,])

    plot(rbind(c(-Lower_Max_Length,0),
               c(Upper_Max_Length,Max_peak)),
         type="n",
         main=mainName,
         xlab=rowname,
         ylab=colname)
    
    lines(rbind(c(-190,Max_peak),
                c(-170,Max_peak)),
          col="blue",
          lty="dashed")

    lines(rbind(c(-170,0),
                c(-170,Max_peak)),
          col="blue",
          lty="dashed")

    lines(rbind(c(-190,0),
                c(-190,Max_peak)),
          col="blue",
          lty="dashed")

    lines(rbind(c(-190,0),
                c(-170,0)),
          col="blue",
          lty="dashed")

    lines(rbind(c(190,Max_peak),
                c(170,Max_peak)),
          col="red",
          lty="dashed")

        lines(rbind(c(170,0),
                c(170,Max_peak)),
          col="red",
          lty="dashed")

    lines(rbind(c(190,0),
                c(190,Max_peak)),
          col="red",
          lty="dashed")

    lines(rbind(c(190,0),
                c(170,0)),
          col="red",
          lty="dashed")
    
    lines(rbind(c(0,0),
                c(0,Max_peak)),
          col="black",
          lty="dashed")

    par(new=TRUE)
    
    legend_data <- c()
    
    for(col_i in 1:ncol(Upper_Data)){
      if(col_i < 9){
        linetype <- "solid"
      }else{
        linetype <- "dotdash"
      }
      upper_col <- Upper_Data[,col_i]
      peak <- upper_col[1]
      Gaus_mean <- upper_col[2]
      Gaus_sd <- upper_col[3]
      path_leng <- upper_col[4]
      Ratio <- upper_col[5]

      legend_data <- rbind(legend_data,
                           c(col_i,linetype,round(Ratio,digits=3)))
      
      Gaus_peak <- dnorm(Gaus_mean,mean=Gaus_mean,sd=Gaus_sd)
      curve((dnorm(x/path_leng,mean=Gaus_mean,sd=Gaus_sd)/Gaus_peak)*peak,
            0,path_leng,
            xlab="",
            ylab="",
            lty=linetype,
            xlim=c(-Lower_Max_Length,Upper_Max_Length),
            ylim=c(0,Max_peak),
            col=col_i)
      par(new=TRUE)

      lower_col <- Lower_Data[,col_i]
      peak <- lower_col[1]
      Gaus_mean <- lower_col[2]
      Gaus_sd <- lower_col[3]
      path_leng <- lower_col[4]
      Gaus_peak <- dnorm(Gaus_mean,mean=Gaus_mean,sd=Gaus_sd)
      curve((dnorm(-x/path_leng,mean=Gaus_mean,sd=Gaus_sd)/Gaus_peak)*peak,
            0,-path_leng,
            xlab="",
            ylab="",
            lty=linetype,
            xlim=c(-Lower_Max_Length,Upper_Max_Length),
            ylim=c(0,Max_peak),
            col=col_i)
      par(new=TRUE)
    }
  }else{ #線形分布の場合
    Max_peak <- max(apply(Upper_Data,2,function(dataCol){
      Stem_amount <- dataCol[1]
      taper <- dataCol[2]
      N_branch <- dataCol[4]/dataCol[3]
      return(min(max(Stem_amount*(taper^(N_branch - 1)),Stem_amount),
                 Conductance_Max))
    }),
                    apply(Lower_Data,2,function(dataCol){
                      Stem_amount <- dataCol[1]
                      taper <- dataCol[2]
                      N_branch <- dataCol[4]/dataCol[3]
                      return(min(max(Stem_amount*(taper^(N_branch - 1)),Stem_amount),
                                 Conductance_Max))
                    }))

    Upper_Max_Length <- max(Upper_Data[4,])
    Lower_Max_Length <- max(Lower_Data[4,])

        plot(rbind(c(-Lower_Max_Length,0),
               c(Upper_Max_Length,Max_peak)),
         type="n",
         main=mainName,
         xlab=rowname,
         ylab=colname)
    
    lines(rbind(c(-190,Max_peak),
                c(-170,Max_peak)),
          col="blue",
          lty="dashed")

    lines(rbind(c(-170,0),
                c(-170,Max_peak)),
          col="blue",
          lty="dashed")

    lines(rbind(c(-190,0),
                c(-190,Max_peak)),
          col="blue",
          lty="dashed")

    lines(rbind(c(-190,0),
                c(-170,0)),
          col="blue",
          lty="dashed")

    lines(rbind(c(190,Max_peak),
                c(170,Max_peak)),
          col="red",
          lty="dashed")

        lines(rbind(c(170,0),
                c(170,Max_peak)),
          col="red",
          lty="dashed")

    lines(rbind(c(190,0),
                c(190,Max_peak)),
          col="red",
          lty="dashed")

    lines(rbind(c(190,0),
                c(170,0)),
          col="red",
          lty="dashed")
    
    lines(rbind(c(0,0),
                c(0,Max_peak)),
          col="black",
          lty="dashed")

    par(new=TRUE)

    legend_data <- c()

    for(col_i in 1:ncol(Upper_Data)){
      if(col_i < 9){
        linetype <- "solid"
      }else{
        linetype <- "dotdash"
      }
      
      Stem_amount <- Upper_Data[,col_i][1]
      taper <- Upper_Data[,col_i][2]
      length <- Upper_Data[,col_i][3]
      path_length <- Upper_Data[,col_i][4]
      Ratio <- Upper_Data[,col_i][5]
      
      legend_data <- rbind(legend_data,
                           c(col_i,linetype,round(Ratio,digits=3)))
      
      N_branch <- path_length/length
      Lengths_Conductances <- rbind(c(0,0),
                                    t(sapply(1:N_branch,function(branch_i){
                                      return(c(branch_i*length,Stem_amount*(taper^(branch_i - 1))))})))
      for(row_i in nrow(Lengths_Conductances):2){
        Conductance_amount <- min(Conductance_Max,Lengths_Conductances[row_i,2])
        lines(rbind(c(Lengths_Conductances[row_i,1],Conductance_amount),
                    c(Lengths_Conductances[row_i - 1,1],Conductance_amount)),
              col=col_i,
              lty=linetype)
      }
          
      Stem_amount <- Lower_Data[,col_i][1]
      taper <- Lower_Data[,col_i][2]
      length <- Lower_Data[,col_i][3]
      path_length <- Lower_Data[,col_i][4]
      N_branch <- as.integer(path_length/length)
      Lengths_Conductances <- rbind(c(0,0),
                                    t(sapply(1:N_branch,function(branch_i){
                                      return(c(-branch_i*length,Stem_amount*(taper^(branch_i - 1))))})))
      for(row_i in nrow(Lengths_Conductances):2){
        Conductance_amount <- min(Conductance_Max,Lengths_Conductances[row_i,2])
        lines(rbind(c(Lengths_Conductances[row_i,1],Conductance_amount),
                    c(Lengths_Conductances[row_i - 1,1],Conductance_amount)),
              col=col_i,
              lty=linetype)
      }
    }
  }

  legend(par()$usr[2],par()$usr[4],
         legend=mapply(function(name,F){
           return(paste("No ",name,", F:",F,sep=""))},
           legend_data[,1],legend_data[,3]),
         col=legend_data[,1],
         lty=legend_data[,2])

  dev.copy2eps(file=filename)
  cat("Output ->",filename,"\n")
  par(new=FALSE,
      oma=c(0,0,0,0))
}
