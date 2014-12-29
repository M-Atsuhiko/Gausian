load_file <- function(load_filename,output){
  writeLines(paste("load_file(\"",load_filename,"\")",sep=""),output,sep="\n")
}

set_Vector <- function(Vector_name,vect,output){
  objref(Vector_name,output)
  equal(Vector_name,paste("new Vector()",sep=""),output)
  writeLines(paste(Vector_name,".append(",paste(vect,collapse=","),")",sep=""),output,sep="\n")
}

make_fp <- function(output){
  objref(paste("fp[N_IND]",sep=""),output)
  writeLines("for i = 0,N_IND - 1{",output,sep="\n")
  writeLines("  fp[i] = new File()",output,sep="\n")
  writeLines("}",output,sep="\n")
}

set_str <- function(name,string,output){
  writeLines(paste("strdef ",name,sep=""),output,sep="\n")
  equal(name,string,output)
}

Do_initiarize <- function(ID,output){
  writeLines(paste("ID",ID,"_",DEND,"_epas_init()",sep=""),output,sep="\n")
}

make_Simulation_File <- function(Individual_Data,CoreMainHoc){
  output <- file(CoreMainHoc,"w")
  writeLines(paste("//",date(),sep=""),output,sep="\n")

  IDs <- sapply(Sim_Datas,"[[","ID_IND")
  
  Dend_Names <- paste("ID",IDs,"_",DEND,sep="")
  Upper_syn_Names <- paste("ID",IDs,"_",UPPER_SYNAPSE,sep="")
  Lower_syn_Names <- paste("ID",IDs,"_",LOWER_SYNAPSE,sep="")
  Soma_Names <- paste(SOMA,"[",((1:length(IDs)) - 1),"]",sep="")

  equal("N_IND",length(IDs),output)
  #あとで扱いやすいようにsomaだけ配列にしておく
  create(paste(SOMA,"[N_IND]",sep=""),output)

  fresh_line(output)

  load_file(SIM_CVODE_FILE,output)
  load_file(SIMUL_PARAMETER_FILE,output)
  load_file(SIM_STIMULATOR_FILE,output)
  
  fresh_line(output)
  
  #使用する形態データ、シナプスの情報をHocにする
  for(Sim_i in 1:length(Sim_Datas)){
    Individual_Data <- Sim_Datas[[Sim_i]]
    Id_Ind<- Individual_Data[["ID_IND"]]
    Morpho_file <- Individual_Data[["Morpho_file_name"]]
    Synapse_file <- Individual_Data[["Synapse_file_name"]]
    Stim_file <- Individual_Data[["Stim_file_name"]]
    TREE <- Individual_Data[["TREE"]]

    Dend_name <- Dend_Names[Sim_i]
    Upper_syn_name <- Upper_syn_Names[Sim_i]
    Lower_syn_name <- Lower_syn_Names[Sim_i]
    Soma_name <- Soma_Names[Sim_i]

    make_NEURON_morpho_conductance_data(TREE,Soma_name,Dend_name,Morpho_file)
    make_NEURON_synapse_data(TREE,Dend_name,Upper_syn_name,Lower_syn_name,Id_Ind,Synapse_file,Stim_file)

    load_file(Morpho_file,output) #このファイルをloadすると、createまで行われる
    load_file(Synapse_file,output)#このファイルをloadすると、createまで行われる
  }
  fresh_line(output)

  start_forall(output)
  insert_conductance(PAS,output)
  
  if(WITH_K){
    insert_conductance(Ka,output)
  }
  if(WITH_Ca){
    insert_conductance(CaT,output)
  }
  
  set_property("Ra",Ra,output)
  set_property("cm",cm,output)
  set_property(G_PAS,g_pas,output)
  set_property(E_PAS,e_pas,output)
  end_edit(output)
  fresh_line(output)

    #形態情報とシナプス情報の初期化
  for(name in Dend_Names){#形態情報、結合情報の初期化
    writeLines(paste(name,"_morpho_conductance_init()",sep=""),output,sep="\n")
  }
  fresh_line(output)
  for(name_i in 1:length(IDs)){#シナプスの付加
    writeLines(paste(Upper_syn_Names[name_i],"_",Lower_syn_Names[name_i],"_property_init()",sep=""),output,sep="\n")
  }
  fresh_line(output)
  sapply(Sim_Datas,function(One_Data){#stimulatorの作成
    load_file(One_Data[["Stim_file_name"]],output)
  })
  fresh_line(output)
  
  if(WITH_K){
    writeLines(paste("ek = ",ek,sep=""),output,sep="\n")
  }

  if(WITH_Ca){
    writeLines(paste("cai = ",cai,sep=""),output,sep="\n")
    writeLines(paste("cao = ",cao,sep=""),output,sep="\n")
  }

  fresh_line(output)

  finitialize(output)
  fcurrent(output)

  fresh_line(output)

  sapply(Sim_Datas,function(One_Data){
    Do_initiarize(One_Data[["ID_IND"]],output)#Dendriteの平衡電位の初期化
  })
  
  fresh_line(output)

  set_str("File_prefix",paste("\"",Output_Dir,"\"",sep=""),output)
  set_Vector("ID_Vector",IDs,output)
  fresh_line(output)
  make_fp(output)
  fresh_line(output)
  set_Vector("Upper_activate_time",ACTIVATE_TIME_MAT[,1],output)
  set_Vector("Lower_activate_time",ACTIVATE_TIME_MAT[,2],output)

  fresh_line(output)
  #outputファイルポインタの設定
  load_file(SIM_TEMPLATE,output)
  close(output)
}
