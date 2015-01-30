Conductance_lineplot <- function(mat,
                                 index,
                                 Conductance_Max){
  apply(mat,2,function(col){
    path_leng <- col[1]
    length <- col[2]
    conductance <- col[3]
    synapse <- col[4]

    conductance_ratio <- conductance/Conductance_Max

    if(path_leng > 0) synapse_color <- "violetred"
    else synapse_color <- "skyblue"

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
             col=synapse_color)

    }
  })
}

plot_Conductance_synapse_position <- function(Upper_Data,
                                              Lower_Data,
                                              main,
                                              rowname,
                                              Conductance_Max,
                                              Filename){

  frame()
  old_par <- par(mfrow=c(2,1),
                 mar=c(3,3,3,8),
                 xpd=NA)

  N_Data <- length(Upper_Data)
  Colors <- rainbow(N_Data)
  legend_names <- paste("Sample",1:N_Data,sep="")
  
  Max_Upper_length <- max(sapply(Upper_Data,function(Data){
    return(max(Data[[2]][1,]))
  }))

  Max_Lower_length <- max(sapply(Lower_Data,function(Data){
    return(max(Data[[2]][1,]))
  }))

  y_min <- 0.5
  
  plot(rbind(c(Max_Upper_length,N_Data),
             c(-Max_Lower_length,y_min)),
       type="n",
       main=main,
       xlab=expression(paste("Dendrite position [",mu,"m]",sep="")),
       ylab="",
       yaxt="n")

  lines(rbind(c(0,y_min),#soma位置
              c(0,N_Data)),
        lty="dashed",
        lwd=1)


  soma_radius <- 12.5

  polygon(c(UPPER_SYNAPTIC_ZONE_Y - soma_radius,UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH, #Upper Synaptic zoneの図示
            UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH,UPPER_SYNAPTIC_ZONE_Y - soma_radius),
          c(y_min,y_min,N_Data,N_Data),
          density=c(6,1),
          lwd=1,
          border="red",
          col="red")

  polygon(c(LOWER_SYNAPTIC_ZONE_Y + soma_radius,LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH, #Lower Synaptic zoneの図示
            LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH,LOWER_SYNAPTIC_ZONE_Y + soma_radius),
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

  dev.copy2eps(file=Filename)
  cat("Output ->",Filename,"\n")
  par(old_par)
}
