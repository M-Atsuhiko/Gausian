make_simul_parameter <- function(filename,celsius,cai,cao,ek){
  output <- file(filename,"w")

  #この部分はsim_template2がうまく行かなかったら変える
  writeLines("forall{",output,sep="\n")
  insert_conductance(PAS,output)
  insert_conductance(Ka,output)
  insert_conductance(CaT,output)
  set_property("Ra",Ra,output)
  set_property("cm",cm,output)
  set_property(G_PAS,g_pas,output)
  set_property(E_PAS,e_pas,output)
  writeLines("}",output,sep="\n")
  fresh_line(output)
  #ここまで

  equal("celsius",celsius,output)
  equal("tau1",tau1,output)
  equal("tau2",tau2,output)
  equal("g_syn",G_SYN,output)
  equal("V_INIT",V_INIT,output)
  equal("tstop",SIM_TIME,output)

  fresh_line(output)
  
  equal("MIN_EPSP",MIN_EPSP,output)
  epsp_error_message_valiable <- "EPSP_ERROR_MESSAGE"
  strdefs(epsp_error_message_valiable,output)
  equal(epsp_error_message_valiable,paste("\"",EPSP_ERROR_MESSAGE,"\"",sep=""),output)

  fresh_line(output)

  Upper_syn_activate_time <- "UPPER_SYN_ACTIVATE_TIME"
  Lower_syn_activate_time <- "LOWER_SYN_ACTIVATE_TIME"

  #シナプス活性化時間の設定 upper_test -> lower_test -> upper_lower -> lower_upperの順に試行する ここの設定すごく重要
  UPPER_SYN_ACTIVATE_TIME        <- c(FIRST_ACTIVATE_TIME,
                                      NOT_ACTIVATE_TIME,
                                      FIRST_ACTIVATE_TIME,
                                      FIRST_ACTIVATE_TIME + DELTA_T) 
  LOWER_SYN_ACTIVATE_TIME        <- c(NOT_ACTIVATE_TIME,
                                      FIRST_ACTIVATE_TIME,
                                      FIRST_ACTIVATE_TIME + DELTA_T,
                                      FIRST_ACTIVATE_TIME)

  objrefs(c(Upper_syn_activate_time,Lower_syn_activate_time),output)
  Vectors(c(Upper_syn_activate_time,Lower_syn_activate_time),output)

  fresh_line(output)
  Vector_append(Upper_syn_activate_time,UPPER_SYN_ACTIVATE_TIME,output)
  Vector_append(Lower_syn_activate_time,LOWER_SYN_ACTIVATE_TIME,output)
  
  close(output)
}
