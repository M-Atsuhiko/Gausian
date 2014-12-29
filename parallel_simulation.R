parallel_simulation <- function(Sim_Datas){
  if(length(Sim_Datas) == 0) return(NULL)
  CoreID <- Sim_Datas[[1]][["CoreID"]]
  CoreMainHoc <- paste(Prefix_CoreMainHoc,CoreID,".hoc",sep="")

  #実行Hocファイルの作成
  make_Simulation_File(Sim_Datas,OUTPUT_SIM_SOMA,CoreMainHoc)

  system(paste("nrniv ",CoreMainHoc,sep=""))

  Sim_Datas <- lapply(Sim_Datas,function(Individual_Data){
    TREE <- Individual_Data[["TREE"]]
    ul_file <- Individual_Data[["Output_upper_lower_file_name"]]
    lu_file <- Individual_Data[["Output_lower_upper_file_name"]]
    l_file <- Individual_Data[["Output_lower_test_file_name"]]
    u_file <- Individual_Data[["Output_upper_test_file_name"]]
    Estimate_Value <- estimate(ul_file,lu_file,u_file,l_file,V_INIT,calc_volume(TREE))
    Individual_Data[["Estimate"]] <- Estimate_Value
    return(Individual_Data)
    })
  #EPSE エラーの場合がある mainプログラムでランダムな数を出力する必要があるか
  return(Sim_Datas)
}

