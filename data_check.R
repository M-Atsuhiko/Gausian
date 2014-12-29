data_check <- function(Dendrite){
  ### --- 作成したデータ形式が正しいかをチェックする ---###
  #チェック項目
  #
  #Branch_NoとDendriteリストの並び順が一致しているか
  #synapse_position_ratioが1以下の値か

  N_Branch <- length(Dendrite)
  
  for(i in 1:N_Branch){
    Branch <- Dendrite[[i]]

    No <- Branch[["No"]]
    synapse_mat <- Branch[["synapse"]]

    if(No != i)cat("ERROR: In Branch[",i,"]\n    Branch No (",No,")is not ordered\n")
    if(is.matrix(synapse_mat)){
      for(j in nrow(synapse_mat)){
        synapse_position_ratio <- synapse_mat[j,1]
        if(synapse_position_ratio > 1 || synapse_position_ratio <= 0)
          cat("ERROR: In Branch[",i,"]\n    synapse positon ratio is over 1 or under 0\n")
      }
    }
  }
}
