fresh_line <- function(output){
  writeLines("",output,sep="\n")
}

objref_synapse <- function(synapse_name,N_variable_name,output){
  writeLines(paste("objref ",synapse_name,"[",N_variable_name,"]",sep=""),output,sep="\n")
}

new_synapse <- function(syn_output_matrix,synapse_name,output){
  for(i in 1:nrow(syn_output_matrix)){
    TREE_i <- syn_output_matrix[i,1]
    Branch_No<- syn_output_matrix[i,2]
    position_rate <- syn_output_matrix[i,3]
    position_rate <- round(position_rate,digits=Round_DIGITS)
    writeLines(paste("dend",TREE_i,"[",Branch_No - 1,"] ",synapse_name,"[",i - 1,"] = new ",SYN_TYPE_NAME,"(",position_rate,")",sep=""),output,sep="\n")
  }
}

define_N_syn <- function(variable_name,N,output){
  writeLines(paste(variable_name," = ",N,sep=""),output,sep="\n")
}

make_NEURON_synapse_data <- function(TREE,filename){
#  cat("===== MAKING HOC SYNAPSE FILE =====\n")
  #hocファイルは添字が0始まりなので注意
  
  output <- file(filename,"w")

  N_DEND <- length(TREE)

  upper_syn_output_matrix <- c()
  lower_syn_output_matrix <- c()
  #   upper_syn_output_matrixとlower_syn_output_matrixは出力するシナプスの情報を保持する
  #   (TREE_index, Branch_No, synapse_position_rate)の三つ組みからなる
  for(TREE_i in 1:N_DEND){
    for(Branch in TREE[[TREE_i]]){
      synapse_mat <- Branch[["synapse"]]
      if(is.matrix(synapse_mat)){
        No <- Branch[["No"]]
        for(i in 1:nrow(synapse_mat)){
          synapse_position_ratio <- synapse_mat[i,1]
          syn_zone <- synapse_mat[i,2]
          if(syn_zone == UPPER_SYNAPTIC_ZONE_INDEX) upper_syn_output_matrix <- rbind(upper_syn_output_matrix,c(TREE_i,No,synapse_position_ratio))
          else lower_syn_output_matrix <- rbind(lower_syn_output_matrix,c(TREE_i,No,synapse_position_ratio))
        }
      }
    }
  }

  if(!(is.null(upper_syn_output_matrix))){
    N_upper_syn <- nrow(upper_syn_output_matrix)
    define_N_syn(paste("N_",UPPER_SYNAPSE,sep=""),N_upper_syn,output)
    objref_synapse(UPPER_SYNAPSE,paste("N_",UPPER_SYNAPSE,sep=""),output)
    fresh_line(output)
    new_synapse(upper_syn_output_matrix,UPPER_SYNAPSE,output)
    fresh_line(output)
  }else{
    define_N_syn(paste("N_",UPPER_SYNAPSE,sep=""),0,output)
  }
  if(!(is.null(lower_syn_output_matrix))){
    N_lower_syn <- nrow(lower_syn_output_matrix)
    define_N_syn(paste("N_",LOWER_SYNAPSE,sep=""),N_lower_syn,output)
    objref_synapse(LOWER_SYNAPSE,paste("N_",LOWER_SYNAPSE,sep=""),output)
    fresh_line(output)
    new_synapse(lower_syn_output_matrix,LOWER_SYNAPSE,output)
    fresh_line(output)
  }else{
    define_N_syn(paste("N_",LOWER_SYNAPSE,sep=""),0,output)
  }
  
  close(output)
#  cat("output file -> ",OUTPUT_SYNAPSE_FILE,"\n")
}

