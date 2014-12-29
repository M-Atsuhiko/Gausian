#木の全てのDendriteの長さの合計を返す
sum_length <- function(TREE){
  return(sum(sapply(TREE,function(Dend){
    return(sum(sapply(Dend,"[[","length")))
  })))
}
