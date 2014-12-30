load("/Users/atsuhiko/workspace/Function_Morphology/stochastic_morphology/passive_Result/test/SEED1_dt15_passive_GA_CORSSOVER0.5_LAST_GENERATION.xdr")

param_name <- "Bif_beta"

Datas <- lapply(Last_Generation,function(TREE){
  sapply(TREE,function(Dendrite){
    return(Dendrite[[param_name]])
    })
  })

Datas_vect <- c()

#単一のベクトルにするのも良くない。いくつかのDendriteの組み合わせが重要になるから
for(data in Datas)
  Datas_vect <- c(Datas_vect,data)

hist(Datas_vect)

