Conductance_stepplot <- function(mat,color,linetype){
  line_data <- apply(mat,2,function(col){
    Conductance_amount <- col[3]
    lines(rbind(c(col[1],Conductance_amount),
                c(col[1] - col[2],Conductance_amount)),
          col=color,
          lty=linetype)
    return(c(col[1] - col[2]/2,Conductance_amount))
  })

  lines(t(unique(line_data,MARGIN=2)),
        col=color,
        lty="dashed",
        lwd=1)
}

plot_Step_Conductance <- function(Upper_Data,
                                  Lower_Data,
                                  mainName,
                                  colname,
                                  rowname,
                                  filename,
                                  isGausian,
                                  Conductance_Max){
  frame()

  #一つ目の図
  N_Data <- length(Upper_Data)
  legend_names <- paste("Sample",1:N_Data,sep="")
  Linetypes <- rep("solid",N_Data)
  Colors <- rainbow_hcl(N_Data,c=100)

  Max_Upper_length <- max(sapply(Upper_Data,function(Data){
    return(max(Data[[2]][1,]))
  }))

  Max_Lower_length <- max(sapply(Lower_Data,function(Data){
    return(max(Data[[2]][1,]))
  }))

  plot(rbind(c(Max_Upper_length,0),
             c(-Max_Lower_length,Conductance_Max)),
       type="n",
       ylab=expression(paste("Conducrance amount [S/c",m^2,"]",,sep="")),
       xlab=expression(paste("Dendrite position [",mu,"m]",sep="")))


  lines(rbind(c(0,0),
              c(0,Conductance_Max)),
        lty="dashed",
        lwd=1)

  mapply(function(up_data,lw_data,color,linetype){
    Ratio <- up_data[[1]]

    up_mtx <- up_data[[2]]
    lw_mtx <- lw_data[[2]]

    lw_mtx[1,] <- lw_mtx[1,]*-1
    lw_mtx[2,] <- lw_mtx[2,]*-1

    Conductance_stepplot(up_mtx,color,linetype)
    Conductance_stepplot(lw_mtx,color,linetype)
  },Upper_Data,Lower_Data,Colors,Linetypes)

  dev.copy2eps(file=filename)
  cat("Output ->",filename,"\n")
}
