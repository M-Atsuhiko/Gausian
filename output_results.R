output_results <- function(Generation_Estimates,Generation_Performance,Best_Datas,Last_Generation){
  
### Generation_Performance のグラフ作成 ###
  postscript(PERFORMANCE_GRAPH,horizontal=FALSE)
  plot(rbind(c(1,0),c(MAX_GENERATION,N_INDIVIDUAL)),type="n")
  Sim_Penalty_Border <- Generation_Performance[,1] + Generation_Performance[,2]#シミュレーションで評価された個体の数とEPSPペナルティとなった個体の数の境界
  polygon(rbind(c(1,N_INDIVIDUAL),
                cbind(1:MAX_GENERATION,Sim_Penalty_Border),
                c(MAX_GENERATION,N_INDIVIDUAL)),
          col="green",
          border=F)#SIM_INDの領域
  polygon(rbind(c(1,0),
                cbind(1:MAX_GENERATION,Generation_Performance[,1]),
                c(MAX_GENERATION,0)),
          col="red",
          border=F)#MOR_PENALTY_INDの領域
  Mor_Sim_Penalty_border <- Generation_Performance[,1]#形態でペナルティになった個体の個数とシミュレーションでEPSPペナルティになった個体の個数
  polygon(rbind(cbind(1:MAX_GENERATION,Sim_Penalty_Border),
                cbind(rev(1:MAX_GENERATION),rev(Generation_Performance[,1]))),
          col="yellow",
          border=F)#SIM_PENALTY_INDの領域
  dev.off()

### Estimationのグラフ作成 ###
  postscript(ESTIMATE_GRAPH,horizontal=FALSE)

  minus_sd <- Generation_Estimates[,2] - Generation_Estimates[,3]
  plus_sd <- Generation_Estimates[,2] + Generation_Estimates[,3]

  xlimit <- c(0,MAX_GENERATION)
  ylimit <- c(min(Generation_Estimates[,1],minus_sd),max(Generation_Estimates[,1],plus_sd))

  plot(Generation_Estimates[,1],
       ylim=ylimit,xlim=xlimit,
       type="l",col="red",lwd=2,
       xlab="GENERATION",
       ylab="ESTIMATION")

  lines(cbind(1:MAX_GENERATION,Generation_Estimates[,2]),col="blue",lwd=2)
  ## 標準偏差をのせる
  lines(cbind(1:MAX_GENERATION,Generation_Estimates[,2] - Generation_Estimates[,3]),col="black",lwd=2)
  lines(cbind(1:MAX_GENERATION,Generation_Estimates[,2] + Generation_Estimates[,3]),col="black",lwd=2)
  dev.off()

  save(Best_Datas,file=BEST_Datas_FILE)
  save(Generation_Estimates,file=GENERATION_ESTIMATE_FILE)
  save(Last_Generation,file=LAST_GENERATION_FILE)
  
  cat(" result output graph -> ",ESTIMATE_GRAPH,"\n")
  cat(" result performance graph -> ",PERFORMANCE_GRAPH,"\n")
  cat(" result Best TREEs   -> ",BEST_Datas_FILE,"\n")
  cat(" result max mean sd  -> ",GENERATION_ESTIMATE_FILE,"\n")
  cat(" result last generation parameter -> ",LAST_GENERATION_FILE,"\n")
}
