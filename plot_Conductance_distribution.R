Conductance_lineplot <- function(mat,
                                 index,
                                 Conductance_Max){
  apply(mat,2,function(col){
    path_leng <- col[1]
    length <- col[2]
    conductance <- col[3]
    synapse <- col[4]

    conductance_ratio <- conductance/Conductance_Max

    lines(rbind(c(path_leng,index),
                c((path_leng - length),index)),
          col=rgb(conductance_ratio,0,(1 - conductance_ratio)),
          lty="solid",
          lend="square",
          lwd=5)
    
    if(synapse == 1){
      points((path_leng - length/2),index - 0.3,
             pch=21,
             cex=0.5,
             col="darkolivegreen")

    }
  })
}

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

plot_Conductance_distribution <- function(Upper_Data,
                                          Lower_Data,
                                          mainName,
                                          colname,
                                          rowname,
                                          filename,
                                          isGausian,
                                          Conductance_Max){

  
  frame()
  former_par <- par(mfcol=c(2,1),
                    mar=c(0,5,0,8),
                    oma=c(4,0,4,0),
                    xpd=NA)

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
       xlab="",
       xaxt="n")


  title(mainName,outer=TRUE)

  lines(rbind(c(0,0),
              c(0,Conductance_Max)),
        lty="dashed",
        lwd=1)

  polygon(c(UPPER_SYNAPTIC_ZONE_Y,UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH,
            UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH,UPPER_SYNAPTIC_ZONE_Y),
          c(0,0,Conductance_Max,Conductance_Max),
          density=c(6,1),
          lwd=1,
          border="red",
          col="red")

  polygon(c(LOWER_SYNAPTIC_ZONE_Y,LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH,
            LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH,LOWER_SYNAPTIC_ZONE_Y),
          c(0,0,Conductance_Max,Conductance_Max),
          density=c(6,1),
          lwd=1,
          border="blue",
          col="blue")

  mapply(function(up_data,lw_data,color,linetype){
    Ratio <- up_data[[1]]

    up_mtx <- up_data[[2]]
    lw_mtx <- lw_data[[2]]

    lw_mtx[1,] <- lw_mtx[1,]*-1
    lw_mtx[2,] <- lw_mtx[2,]*-1

    Conductance_stepplot(up_mtx,color,linetype)
    Conductance_stepplot(lw_mtx,color,linetype)
  },Upper_Data,Lower_Data,Colors,Linetypes)


  for(ratio in seq(0,1,by=0.01)){
    lines(rbind(c(par()$usr[1] + 15,ratio*Conductance_Max),
                c(par()$usr[1] +  5,ratio*Conductance_Max)),
          col=rgb(ratio,0,(1 - ratio)),
          lwd=3)
  }

  legend(par()$usr[2],par()$usr[4],
         legend=legend_names,
         col=Colors,
         lty=Linetypes)

  #二つ目の図

  y_min <- 0.5
  
  plot(rbind(c(Max_Upper_length,N_Data),
             c(-Max_Lower_length,y_min)),
       type="n",
       xlab=expression(paste("Dendrite position [",mu,"m]",sep="")),
       ylab="",
       yaxt="n")

  lines(rbind(c(0,y_min),
              c(0,N_Data)),
        lty="dashed",
        lwd=1)

  polygon(c(UPPER_SYNAPTIC_ZONE_Y,UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH,
            UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH,UPPER_SYNAPTIC_ZONE_Y),
          c(y_min,y_min,N_Data,N_Data),
          density=c(6,1),
          lwd=1,
          border="red",
          col="red")

  polygon(c(LOWER_SYNAPTIC_ZONE_Y,LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH,
            LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH,LOWER_SYNAPTIC_ZONE_Y),
          c(y_min,y_min,N_Data,N_Data),
          density=c(6,1),
          lwd=1,
          border="blue",
          col="blue")
  mapply(function(up_data,lw_data,index,color){
    Ratio <- up_data[[1]]

    up_mtx <- up_data[[2]]
    lw_mtx <- lw_data[[2]]

    lw_mtx[1,] <- lw_mtx[1,]*-1
    lw_mtx[2,] <- lw_mtx[2,]*-1

    Conductance_lineplot(up_mtx,index,Conductance_Max)
    Conductance_lineplot(lw_mtx,index,Conductance_Max)

  },Upper_Data,Lower_Data,length(Upper_Data):1,Colors)

  legend_names <- mapply(function(name,F){
    return(paste(name," F:",round(F,3),sep=""))
  },legend_names,sapply(Upper_Data,"[[",1))

  axis(side=4,
       at=1:N_Data,
       labels=rev(legend_names),#leged_nameを逆に出していることに注意
       las=1)

  dev.copy2eps(file=filename)
  cat("Output ->",filename,"\n")
  par(former_par)
}
