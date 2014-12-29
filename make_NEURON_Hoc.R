make_NEURON_Hoc <- function(TREE,
                            Data_file,
                            Do_file,
                            Output_upper_lower,
                            Output_lower_upper,
                            Output_upper_test,
                            Output_lower_test){
  
  #Dendriteの全ての通し番号を表すlist
  IDs_Dend <- as.list(NULL)
  #somaの形態情報を持った変数も作る

  Upper_Syn <- c()
  Lower_Syn <- c()

  All_0xyz <- c()
  All_1xyz <- c()
  All_1diam <- c()
  All_0diam <- c()
  All_K_cond <- c()
  All_Ca_cond <- c()
  All_nseg <- c()

  #NEURONは添字が0始まりなのに注意
  ALL_N <- 0

  soma_connect <- c()
  soma_K_conductance <- c()
  soma_Ca_conductance <- c()
  
  for(Dend_i in 1:length(TREE)){
    IDs <- c()
    for(Branch in TREE[[Dend_i]]){
      ID <- Branch[["No"]]
      coordi <- Branch[["coordi"]]
      parent <- Branch[["parent"]]

      All_nseg <- c(All_nseg,Branch[["nseg"]])
      
      All_0xyz <- rbind(All_0xyz,coordi[1,])
      All_1xyz <- rbind(All_1xyz,coordi[2,])

      All_1diam <- c(All_1diam,Branch[["diam"]])
      All_K_cond <- c(All_K_cond,Branch[["K_conductance"]])
      All_Ca_cond <- c(All_Ca_cond,Branch[["Ca_conductance"]])

      if(ID == 1){
        soma_connect <- c(soma_connect,ALL_N)
        soma_K_conductance <- Branch[["K_conductance"]]
        soma_Ca_conductance <- Branch[["Ca_conductance"]]
        All_0diam <- c(All_0diam,Branch[["diam"]])
      }else{
        All_0diam <- c(All_0diam,TREE[[Dend_i]][[parent]][["diam"]])#コンパートメントの0側と1側で太さを変えているが、dividするとnseg=1なので意味があまりない
      }

      synapse <- Branch[["synapse"]]
      
      IDs <- c(IDs,ALL_N)
      if(ID != length(IDs)){
        stop(paste("Dend:",Dend_i,"ID:",ID," is not in order\n"))
      }

      if(is.matrix(synapse)){
        for(i in 1:nrow(synapse)){
          if(synapse[i,2] == UPPER_SYNAPTIC_ZONE_INDEX)
            Upper_Syn <- rbind(Upper_Syn,c(ALL_N,synapse[i,1]))
          else
            Lower_Syn <- rbind(Lower_Syn,c(ALL_N,synapse[i,1]))
        }
      }
      ALL_N <- ALL_N + 1      
    }
    IDs_Dend[[Dend_i]] <- IDs
  }

  All_connect <- as.list(NULL)
  
  for(Dend_i in 1:length(TREE)){
    for(Branch in TREE[[Dend_i]]){
      ID <- Branch[["No"]]
      connect <- Branch[["connect"]]
      connect <- connect[connect > 0]
      for(con in connect){
        All_connect <- rbind(All_connect,
                             c(IDs_Dend[[Dend_i]][ID],IDs_Dend[[Dend_i]][con]))
      }
    }
  }

  #実行ファイルの作成
  output <- file(Do_file,"w")
  
  input_file <- "input_file"
  output_file_u <- "output_file_u"
  output_file_l <- "output_file_l"
  output_file_ul <- "output_file_ul"
  output_file_lu <- "output_file_lu"

  strdefs(c(input_file,
            output_file_u,
            output_file_l,
            output_file_ul,
            output_file_lu),output)

  fresh_line(output)
  
  equal(input_file,paste("\"",Data_file,"\"",sep=""),output)
  equal(output_file_u,paste("\"",Output_upper_test,"\"",sep=""),output)
  equal(output_file_l,paste("\"",Output_lower_test,"\"",sep=""),output)
  equal(output_file_ul,paste("\"",Output_upper_lower,"\"",sep=""),output)
  equal(output_file_lu,paste("\"",Output_lower_upper,"\"",sep=""),output)

  fresh_line(output)
  
  load_file(SIM_TEMPLATE,output)
  close(output)

  #データファイルの作成
  output <- file(Data_file,"w")
  comment(paste("//",Data_file),output)
  comment("N_dend:",output)
  write_value(ALL_N,output)#N_Dend
  comment("N_upper_syn:",output)
  write_value(nrow(Upper_Syn),output)#N_upper_syn
  comment("N_lower_syn:",output)
  write_value(nrow(Lower_Syn),output)#N_lower_syn
  comment("N_soma_connect:",output)
  write_value(length(soma_connect),output)#N_soma_Connect
  comment("N_connect:",output)
  write_value(nrow(All_connect),output)#N_Connect
  
  fresh_line(output)

  comment("soma xyz_diam:",output)
  write_value(c(-SOMA_DIAM/4,0,0,SOMA_DIAM),output) #soma x0,y0,z0
  write_value(c(SOMA_DIAM/4,0,0,SOMA_DIAM),output) #s0ma x1,y1,z1
  comment("soma_diam:",output)
  write_value(SOMA_DIAM,output) #soma diam
  comment("soma cat conductance:",output)
  write_value(soma_Ca_conductance,output) #soma_cat
  comment("soma k conductance:",output)
  write_value(soma_K_conductance,output) #soma_k

  fresh_line(output)

  comment("dend:",output)
  for(i in 1:ALL_N){
    write_value(c(All_0xyz[i,],All_0diam[i]),output) #dend xyz0 diam0
    write_value(c(All_1xyz[i,],All_1diam[i]),output) #dend xyz1 diam1
    write_value(All_nseg[i],output) #dend nseg
    write_value(All_Ca_cond[i],output) #dend Ca
    write_value(All_K_cond[i],output) #dend K
    fresh_line(output)
  }

  fresh_line(output)

  comment("soma_connect:",output)
  write_value(soma_connect,output) #soma connect
  fresh_line(output)

  comment("connects:",output)
  for(i in 1:nrow(All_connect)){
    write_value(c(All_connect[i,2],All_connect[i,1]),output)
  }

  fresh_line(output)

  comment("Upper syn:",output)  #Upper_Syn #
  for(i in 1:nrow(Upper_Syn)){
    write_value(Upper_Syn[i,],output)
  }

  comment("Lower syn:",output)  #Lower_Syn #
  for(i in 1:nrow(Lower_Syn)){
    write_value(Lower_Syn[i,],output)
  }

  close(output)
}
