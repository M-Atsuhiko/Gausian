make_NEURON_Hocs <- function(Id_Ind,TREE,Morpho_file,Synapse_file,Soma_name,Dend_name,Upper_syn_name,Lower_syn_name){
  make_NEURON_morpho_conductance_data(TREE,Soma_name,Dend_name,Morpho_file)
  make_NEURON_synapse_data(TREE,Dend_name,Upper_syn_name,Lower_syn_name,Id_Ind,Synapse_file)
}
