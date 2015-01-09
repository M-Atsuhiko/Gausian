pt3dadd <- function(x,y,z,diam,output){
  writeLines(paste("    pt3dadd(",x,", ",y,", ",z,", ",diam,")",sep=""),output,sep="\n")
}

create <- function(part,output){
  writeLines(paste("create ",part,sep=""),output,sep="\n")
}

insert_conductance <- function(current_name,output){
  writeLines(paste("    insert ",current_name,sep=""),output,sep="\n")
}

start_forall <- function(output){
  writeLines(paste("forall{",sep=""),output,sep="\n")
}

start_edit <- function(part,output){
  writeLines(paste(part,"{\n","    pt3dclear()",sep=""),output,sep="\n")
}

end_edit <- function(output){
  writeLines(paste("}",sep=""),output,sep="\n")
}

set_nseg <- function(nseg,output){
  set_property("nseg",nseg,output)
}

make_connect <- function(connected_part,connect_part,parent_position,output){
  #connect_partをconnected_partの後ろにくっつける
  writeLines(paste("connect ",connect_part,"(0), ",connected_part,"(",parent_position,") ",sep=""),output,sep="\n")
}

customize_soma <- function(soma_k_conductance,soma_ca_conductance,output){
  start_edit(SOMA,output)
  pt3dadd(-(SOMA_LENGTH/2),0.0,0.0,SOMA_DIAM,output)
  pt3dadd(SOMA_LENGTH/2,0.0,0.0,SOMA_DIAM,output)
  set_property("diam",SOMA_DIAM,output)
  if(WITH_K) set_property(G_K,
                          round(soma_k_conductance,digits=Round_DIGITS)
                          ,output)
  if(WITH_Ca) set_property(G_Ca,
                           round(soma_ca_conductance,digits=Round_DIGITS)
                           ,output)
  end_edit(output)
}

set_property <- function(name,value,output){
    writeLines(paste("    ",name," = ",value,sep=""),output,sep="\n")
}

customize_dend <- function(Dendrite,i,output){
  for(Branch in Dendrite){
    No <- Branch[["No"]]
    my_diam <- Branch[["diam"]]
    nseg <- Branch[["nseg"]]
    coordi <- Branch[["coordi"]]
    parent_i <- Branch[["parent"]]

    if(parent_i == -1) parent_diam = my_diam
    else
      parent_diam <- Dendrite[[parent_i]][["diam"]]

    coordi <- round(coordi,digits=Round_DIGITS)
    my_diam <- round(my_diam,digits=Round_DIGITS) 
    parent_diam <- round(parent_diam,digits=Round_DIGITS) 
    
    start_edit(paste(DEND,i,"[",No - 1,"]",sep=""),output)
    fresh_line(output)
    set_nseg(nseg,output)

    #コンダクタンスの設定
    if(WITH_K) set_property(G_K,
                            round(Branch[["K_conductance"]],digits=Round_DIGITS)
                            ,output)
    if(WITH_Ca) set_property(G_Ca,
                             round(Branch[["Ca_conductance"]],digits=Round_DIGITS)
                             ,output)
    fresh_line(output)

    pt3dadd(coordi[1,1],coordi[1,2],coordi[1,3],parent_diam,output)
    pt3dadd(coordi[2,1],coordi[2,2],coordi[2,3],my_diam,output)
    
    end_edit(output)
  }
}

define_N_dend <- function(variable_name,N,output){
  writeLines(paste(variable_name," = ",N,sep=""),output,sep="\n")
}

connect_dend <- function(Dendrite,i,output){
  for(Branch in Dendrite){
    No <- Branch[["No"]]
    parent <- Branch[["parent"]]
    
    if(parent == -1){
      make_connect(SOMA,paste(DEND,i,"[",No - 1,"]",sep=""),0.5,output)
    }else{#後ろにつないでいく
      make_connect(paste(DEND,i,"[",parent - 1,"]",sep=""),paste(DEND,i,"[",No - 1,"]",sep=""),1,output)
    }
  }
}

finitialize <- function(output){
  writeLines(paste("finitialize(",V_INIT,")",sep=""),output,sep="\n")
}

fcurrent <- function(output){
  writeLines("fcurrent()",output,sep="\n")
}

Modify_soma_e_pass <- function(v_rest,inserted_I,output){
  writeLines(paste(SOMA,".",E_PAS," = ",V_REST," + (",sep=""),output,sep="")
  for(I in inserted_I){
    if(I == "0"){
      writeLines(I,output,sep="")
    }
    else{
      writeLines(paste(" + ",SOMA,".",I,sep=""),output,sep="")
    }
  }
  writeLines(paste(")/",SOMA,".",G_PAS,sep=""),output,sep="\n")
}

Modify_dend_e_pass <- function(i,v_rest,inserted_I,output){
  writeLines(paste("for i = 0, N_dend",i," - 1{",sep=""),output,sep="\n")
  writeLines(paste("    ",DEND,i,"[i].",E_PAS," = ",V_REST," + (",sep=""),output,sep="")
  for(I in inserted_I){
    if(I == "0"){
      writeLines(I,output,sep="")
    }
    else{
      writeLines(paste(" + ",DEND,i,"[i].",I,sep=""),output,sep="")
    }
  }
  writeLines(paste(")/",DEND,i,"[i].",G_PAS,sep=""),output,sep="\n")
  writeLines("}",output,sep="\n")
}

make_NEURON_morpho_conductance_data <- function(TREE,filename){
#  cat("===== MAKING HOC MORPHO FILE =====\n")
  #hocファイルは添字が0始まりなので注意
  
  output <- file(filename,"w")

  N_DEND <- length(TREE)

  create(SOMA,output)
  fresh_line(output)
  
  for(i in 1:N_DEND){
    N_Branch <- length(TREE[[i]])
    define_N_dend(paste("N_",DEND,i,sep=""),N_Branch,output)
    create(paste(DEND,i,"[N_dend",i,"]",sep=""),output)
    fresh_line(output)
  }

  start_forall(output)
  insert_conductance(PAS,output)

  inserted_I <- "0"
  
  if(WITH_K){
    insert_conductance(Ka,output)
    inserted_I <- c(inserted_I,"ik")
  }
  if(WITH_Ca){
    insert_conductance(CaT,output)
#    insert_conductance("cadifus",output)
    
    inserted_I <- c(inserted_I,"ica")
  }
  
  set_property("Ra",Ra,output)
  set_property("cm",cm,output)
  set_property(G_PAS,g_pas,output)
  set_property(E_PAS,e_pas,output)
  end_edit(output)
  fresh_line(output)

##   #somaのk caコンダクタンスは各stemのコンダクタンスの平均値とする
##   soma_k_conductance <- 0
##   soma_ca_conductance <- 0
##   for(Dend in TREE){
## #    cat("Ca: ",Dend[[1]][["Ca_conductance"]]," K: ",Dend[[1]][["K_conductance"]],"\n")
##     soma_k_conductance <- soma_k_conductance + Dend[[1]][["K_conductance"]]
##     soma_ca_conductance <- soma_ca_conductance + Dend[[1]][["Ca_conductance"]]
##   }

  ## soma_k_conductance <- soma_k_conductance/length(TREE)
  ## soma_ca_conductance <- soma_ca_conductance/length(TREE)

  soma_k_conductance <- 0
  soma_ca_conductance <- 0
#  cat("soma: ",soma_ca_conductance,"\n")
  
  customize_soma(soma_k_conductance,soma_ca_conductance,output)
  fresh_line(output)
  
  for(i in 1:N_DEND){
    customize_dend(TREE[[i]],i,output)
    fresh_line(output)
  }

  #結合情報を出力する
  for(i in 1:N_DEND){
    connect_dend(TREE[[i]],i,output)
    fresh_line(output)
  }

  #初期化を行い、各コンパートメントのica,ikの値を用いてe_pasをいじる
  
  if(WITH_K){
    writeLines(paste("ek = ",ek,sep=""),output,sep="\n")
  }

  if(WITH_Ca){
    writeLines(paste("cai = ",cai,sep=""),output,sep="\n")
    writeLines(paste("cao = ",cao,sep=""),output,sep="\n")
  }
  
  finitialize(output)
  fcurrent(output)

  fresh_line(output)

  Modify_soma_e_pass(V_REST,inserted_I,output)
  fresh_line(output)
  
  for(i in 1:N_DEND){
    Modify_dend_e_pass(i,V_REST,inserted_I,output)
  }
  
  close(output)
#  cat("output file -> ",OUTPUT_MORPHO_FILE,"\n")
}


