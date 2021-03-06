TREE_simulation_function <- function(TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params){
  ### TREEの状態からシミュレーションを行う関数 ###

  make_simul_parameter(SIMUL_PARAMETER_FILE,celsius,cai,cao,ek)
    Id_Ind <- 0
  
  Morpho_file <- paste(Dir_Test,"test_mor.hoc",sep="")
  Synapse_file <- paste(Dir_Test,"test_syn.hoc",sep="")

  Output_upper_lower <- paste(Dir_Test,"upper_lower_test.data",sep="")
  Output_lower_upper<- paste(Dir_Test,"lower_upper_test.data" ,sep="")
  Output_upper_test <- paste(Dir_Test,"upper_test.data",sep="")
  Output_lower_test <- paste(Dir_Test,"lower_test.data",sep="")

  if(file.exists(Output_upper_lower)) file.remove(Output_upper_lower)
  if(file.exists(Output_lower_upper)) file.remove(Output_lower_upper)
  if(file.exists(Output_upper_test)) file.remove(Output_upper_test)
  if(file.exists(Output_lower_test)) file.remove(Output_lower_test)

  ## Data_file <- paste(Dir_Test,"test_data.hoc",sep="")
  ## Do_file <- paste(Dir_Test,"test_do.hoc",sep="")
  
  ## make_NEURON_Hoc(TREE,
  ##                 Data_file,
  ##                 Do_file,
  ##                 Output_upper_lower,
  ##                 Output_lower_upper,
  ##                 Output_upper_test,
  ##                 Output_lower_test)
  
  ## system(paste("nrniv",Do_file,"1> /dev/null 2> /dev/null"))

  make_NEURON_morpho_conductance_data(TREE,Morpho_file)
  make_NEURON_synapse_data(TREE,Synapse_file)

  system(paste(paste("./simulation.sh",sep=""),
               Morpho_file,
               Synapse_file,
               Output_upper_lower,
               Output_lower_upper,
               Output_upper_test,
               Output_lower_test,
               FIRST_ACTIVATE_TIME,
               DELTA_T,
               SIM_TIME,
               V_INIT,
               Id_Ind,
               SIMUL_PARAMETER_FILE,
               Dir_SimHoc,
               sep=" "))

  All_Length <- sum_length(TREE)

  Simul_Estimate <- return_result(Output_upper_lower,
                                  Output_lower_upper,
                                  Output_upper_test,
                                  Output_lower_test,
                                  V_INIT,
                                  TRUE)


  ul_data <- read.table(Output_upper_lower)
  lu_data <- read.table(Output_lower_upper)
  u_test_data <- read.table(Output_upper_test)
  l_test_data <- read.table(Output_lower_test)

#  print(abs(V_INIT - max(lu_data[,2]))/abs(V_INIT - max(ul_data[,2])))
  
  ymin <- min(ul_data[,2],lu_data[,2],u_test_data[,2],u_test_data[,2])
  xmin <- min(ul_data[,1])

  ymax <- max(ul_data[,2],lu_data[,2],u_test_data[,2],u_test_data[,2])
  xmax <- max(ul_data[,1])
  
  if(filename != "Not_display"){
    par(lwd=4,
        ps=20,
        cex=1,
        oma=c(0,1,0,0))
    
    plot(rbind(c(xmin,ymin),c(xmax,ymax)),
         type="n",
         xlab="time [ms]",
         ylab="soma membrame potential [mV]")

    lines(lu_data,col="blue")
    lines(ul_data,col="red")

#    lines(l_test_data,col="blue")
#    lines(u_test_data,col="red")

    ## legend("bottomright",
    ##        legend=c(expression(Blue %->% Red),expression(Red %->% Blue)),
    ##        col=c("blue","red"),
    ##        lwd=c(4,4),
    ##        cex=0.9,
    ##        bty="n",
    ##        y.intersp=1.5)
    
#=====================================================================
    #修論, 図 input_order_detection.epsを作るための変更
    #input file: ./passive_Result/SEED2_dt15_passive_FR75_Rerative_75_0_Best_Datas.xdr
    
    ## lu_max_data <- lu_data[which(lu_data[,2] == max(lu_data[,2])),]

    lines(rbind(c(5 + DELTA_T,V_INIT),
                c(5 + DELTA_T,ymax)),
          lty="dashed",
          lwd=1)

    lines(rbind(c(5,V_INIT),
                c(5,ymax)),
          lty="dashed",
          lwd=1)
    
    lines(rbind(c(0,V_INIT),
                c(SIM_TIME,V_INIT)),
          lty="dashed",
          lwd=1)

    ## arrows(5,-64,20,-64,
    ##        code=3,
    ##        length=0.1,
    ##        lwd=2)

    ## text(12.5,-63.8,
    ##      labels=expression(paste(Delta,"t",sep="")))

    ## arrows(lu_max_data[[1]],V_INIT,lu_max_data[[1]],lu_max_data[[2]],
    ##        col="blue",
    ##        lend="square",
    ##        lwd=2,
    ##        code=3,
    ##        length=0.2,
    ##       lty="dashed")
    
    ## points(rbind(lu_max_data,
    ##              c(lu_max_data[[1]],V_INIT)),
    ##        col="blue",
    ##        cex=0)

    ## text(26,-67,
    ##      labels=expression(R[B%->%R]))

    ## ul_max_data <- ul_data[which(ul_data[,2] == max(ul_data[,2])),]

    ## arrows(ul_max_data[[1]],V_INIT,ul_max_data[[1]],ul_max_data[[2]],
    ##        col="red",
    ##        lend="square",
    ##        lwd=2,
    ##        code=3,
    ##        length=0.2,
    ##        lty="dashed")

    ## points(rbind(ul_max_data,
    ##              c(ul_max_data[[1]],V_INIT)),
    ##        col="red",
    ##        cex=0)

    ## text(38,-68,
    ##      labels=expression(R[R%->%B]))

    ## legend(32.5,-69,
    ##        legend=c(expression(Blue %->% Red),expression(Red %->% Blue)),
    ##        col=c("blue","red"),
    ##        lwd=c(4,4),
    ##        cex=0.9,
    ##        bty="n",
    ##        y.intersp=1.5)

    ## dev.copy2eps(file="~/workspace/Syuron/Images/input_order_detection.eps")
#=====================================================================
    


    ## par(lwd=4,
    ##     ps=20,
    ##     mfcol=c(1,2),
    ##     oma=c(3,3,3,0),
    ##     cex=1)
    ## plot(rbind(c(xmin,ymin),c(xmax,ymax)),
    ##      type="n",
    ##      xlab="",
    ##      ylab="")

    ## lines(lu_data,col="red")
    ## lines(l_test_data,col="blue")

    ## plot(rbind(c(xmin,ymin),c(xmax,ymax)),
    ##      type="n",
    ##      xlab="",
    ##      ylab="")

    ## lines(ul_data,col="blue")
    ## lines(u_test_data,col="red")
    
    ## mtext("time [ms]",
    ##       outer=TRUE,
    ##       side=1,
    ##       cex=1)
    ## mtext("soma membrame potential [mV]",
    ##       outer=TRUE,
    ##       side=2,
    ##       cex=1)

#    if(filename != "Display") dev.copy2eps(file=filename)
  }
#  file.remove(Output_upper_lower) #作成したテスト実行結果は削除しておいた方がいい

  return(c(Simul_Estimate,All_Length))
}
