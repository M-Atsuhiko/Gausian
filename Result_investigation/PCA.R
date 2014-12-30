correlation_factor <- function(X,Y){
  return(sum((X - mean(X))*(Y - mean(Y)))/sqrt(sum((X - mean(X))^2)*sum((Y - mean(Y))^2)))
}

SEED <- c(2:7,9:10)
dt <- 15

Gene_data <- as.list(NULL)
Tree_data <- as.list(NULL)

for(seed in SEED){
  load(paste("../passive_Result/SEED",seed,"_dt",dt,"_passiveNA_LAST_TREEs.xdr",sep=""))
  load(paste("../passive_Result/SEED",seed,"_dt",dt,"_passiveNA_LAST_GENERATION.xdr",sep=""))
  Gene_data <- c(Gene_data,Last_Generation)
  Tree_data <- c(Tree_data,TREEs)
}

#load("./test_Last_Gene.xdr") #Data name -> "Last_Generation"
#load("./test_Last_Trees.xdr") #Data name -> "TREEs"
#source("./display_dendrite.R")
#library("rgl")
#rgl.clear()

UPPER_SYNAPTIC_ZONE_INDEX      <- 1
LOWER_SYNAPTIC_ZONE_INDEX      <- 0

Data_Matrix <- c()
All_Dend <- as.list(NULL)
Tree_Dendrite_index <- as.list(NULL)
Max_data_num <- length(Gene_data)
Property_num <- length(Gene_data[[1]][[1]]) - 4
synapse_lower_upper <- c()

Dend_index <- 1 #全Dendriteの通し番号
for(i in 1:Max_data_num){
  Dends_index <- c()
  Params <- Gene_data[[i]]
  Tree <- Tree_data[[i]]
  if(length(Params) != length(Tree)){
    cat("Error: Data length is irregular!\n")
  }
  for(Param in Params){
    Data_Matrix <- rbind(Data_Matrix,unlist(Param[1:Property_num]))
  }
  for(Dend in Tree){
    for(Branch in Dend){
      synapse_lower_upper <- rbind(synapse_lower_upper,c(0,0))
      if(is.matrix(Branch[["synapse"]])){
        if(Branch[["synapse"]][1,2] == LOWER_SYNAPTIC_ZONE_INDEX){
          synapse_lower_upper[Dend_index,1] <- 1
        }else{
          synapse_lower_upper[Dend_index,2] <- 1
        }
        break
      }
    }
    All_Dend <- c(All_Dend,Dend)
    Dends_index <- c(Dends_index,Dend_index)
    Dend_index <- Dend_index + 1
  }
  Tree_Dendrite_index[[length(Tree_Dendrite_index) + 1]] <- Dends_index
}

lower_dends <- which(synapse_lower_upper[,1] == 1)
upper_dends <- which(synapse_lower_upper[,2] == 1)

result <- princomp(Data_Matrix)

PCA_result <- Data_Matrix %*% (unclass(loadings(result)))

print((unclass(loadings(result)))[,1:2])

plot(PCA_result,type="p")
points_data <- PCA_result[which(synapse_lower_upper[,1] == 1),]
points(points_data[,1],points_data[,2],col="blue")
points_data <- PCA_result[which(synapse_lower_upper[,2] == 1),]
points(points_data[,1],points_data[,2],col="red")

segments_data <- c()

for(i in 1:Max_data_num){
  lower_dend_index <- c()
  upper_dend_index <- c()
  Dendrite_index <- Tree_Dendrite_index[[i]]
  lower_dend_index <- Dendrite_index[is.element(Dendrite_index,lower_dends)]
  upper_dend_index <- Dendrite_index[is.element(Dendrite_index,upper_dends)]
  if(length(lower_dend_index)*length(upper_dend_index) > 0){
   segments(PCA_result[lower_dend_index[1],1],PCA_result[lower_dend_index[1],2],PCA_result[upper_dend_index[1],1],PCA_result[upper_dend_index[1],2])
  }
}

cat("All Trees num:",Max_data_num,"\n")
cat("All Dends num:",Dend_index,"\n")

## for(i in 1:Max_data_num){
##   Params <- Gene_data[[i]]
##   dend_index <- c()
##   for(Dend in Params){
##     Data_Matrix <- rbind(Data_Matrix,unlist(Dend[1:Property_num]))
##     dend_index <- c(dend_index,nrow(Data_Matrix))
##   }
##   Dendrite_index[[i]] <- dend_index
## }

## Dends <- as.list(NULL)
## Tree_Dends <- as.list(NULL)
## synapse_lower_upper <- cbind(rep(-1,nrow(Data_Matrix)),rep(-1,nrow(Data_Matrix)))
## i <- 1

## for(TREE in Tree_data){
##   upper_dendrite <- c()
##   lower_dendrite <- c()
##   dend_index <- c()
##   for(Dend in TREE){
##     Dends[[length(Dends) + 1]] <- Dend
##     for(Branch in Dend){
##       if(is.matrix(Branch[["synapse"]])){
##         if(Branch[["synapse"]][1,2] == UPPER_SYNAPTIC_ZONE_INDEX){
##           synapse_lower_upper[i,2] <- 1
## #          cat("upper:",Data_Matrix[i,"Stem_elevation_MIEW"],"\n")
##         }
##         else{
##           synapse_lower_upper[i,1] <- 1
## #          cat("lower:",Data_Matrix[i,"Stem_elevation_MIEW"],"\n")
##         }
##         break
##       }
##     }
##     dend_index <- c(dend_index,i)
##     i <- i + 1
##   }
##   Tree_Dends[[length(Tree_Dends) + 1]] <- dend_index
## }

## cat("All dends",i,"\n")
## cat("lower dend",sum(synapse_lower_upper[,1] == 1),"\n")
## cat("upper dend",sum(synapse_lower_upper[,2] == 1),"\n")

## ## for(index in 1:nrow(synapse_lower_upper)){
## ##   if(synapse_lower_upper[index,1] == 1)
## ##     display_dendrite(Dends[[index]])
## ## }

## #全てのデータを一度に解析すると、シナプティックゾーンに到達していないデータが、結果に強く影響している可能性がある
## #シナプスのあるdendのみを解析するべきかもしれない

## result <- princomp(Data_Matrix)
## #print(summary(result))

## par(mfcol = c(2,2))
## result_PCA <- Data_Matrix %*% (unclass(loadings(result))[,1:3])
## #print((unclass(loadings(result)))[,1:2])
## plot(result_PCA,col="black")


## plot_data <- which(synapse_lower_upper[,1] == 1)
## points(result_PCA[plot_data,1],result_PCA[plot_data,2],col="blue")
## #rgl.points(result_PCA[plot_data,],col="blue")

## plot_data <- which(synapse_lower_upper[,2] == 1)
## points(result_PCA[plot_data,1],result_PCA[plot_data,2],col="red")
## #rgl.points(result_PCA[plot_data,],col="red")
## #rgl.bbox()

## ### 上下のDendriteに分けて主成分分析してみる
## lower_dend_index <- which(synapse_lower_upper[,1] == 1)
## upper_dend_index <- which(synapse_lower_upper[,2] == 1)

## lower_Data <- Data_Matrix[lower_dend_index,]
## upper_Data <- Data_Matrix[upper_dend_index,]

## lower_result <- princomp(lower_Data)
## upper_result <- princomp(upper_Data)

## #print(summary(lower_result))
## #print(summary(upper_result))

## lower_result_PCA <- lower_Data %*% (unclass(loadings(lower_result)))
## upper_result_PCA <- upper_Data %*% (unclass(loadings(upper_result)))

## plot(lower_result_PCA,col="blue")
## plot(upper_result_PCA,col="red")

## if(0){
## #plot(lower_result_PCA,col="white")

## lower_right_index <- which(lower_result_PCA[,1] > -5)
## lower_left_index <- which(lower_result_PCA[,1] < -5)

## lower_right_dends_index <- lower_dend_index[lower_right_index]
## lower_left_dends_index <- lower_dend_index[lower_left_index]

## #points_data <- Data_Matrix[lower_right_dends_index,] %*% (unclass(loadings(lower_result)))[,1:2]
## #points(points_data[,1],points_data[,2],col="yellow")

## #rgl.points(lower_result_PCA,col="blue")
## #rgl.bbox()

## upper_plot_data <- as.list(NULL)

## for(i in 1:length(Tree_Dends)){
##   #上下のシナプティックゾーンにDendriteを持つもののみ取り出す
##   if(sum(is.element(Tree_Dends[[i]],lower_dend_index)) * sum(is.element(Tree_Dends[[i]],upper_dend_index))){
##     Tree_lower_dends <- Tree_Dends[[i]][is.element(Tree_Dends[[i]],lower_dend_index)]
##     Tree_upper_dends <- Tree_Dends[[i]][is.element(Tree_Dends[[i]],upper_dend_index)]
##     for(lower_index in Tree_lower_dends){
##       if(is.element(lower_index,lower_right_dends_index)){
##         lower_color <- "orange"
##       }else if(is.element(lower_index,lower_left_dends_index)){
##         lower_color <- "green"
##       }else lower_color <- "purple"
##       points_data <- Data_Matrix[lower_index,] %*% (unclass(loadings(lower_result)))
##       text(points_data[,1],points_data[,2],labels=paste(i),col=lower_color)
##     }
##     for(upper_index in Tree_upper_dends){
##       # lower_dendが複数の場合、最後の枝の色になってしまう
##       upper_plot_data[[length(upper_plot_data) + 1]] <- list(upper_index,lower_color,i)
##     }
##   }
## }

## plot(upper_result_PCA,col="white")
## for(i in 1:length(upper_plot_data)){
##   upper_index <- upper_plot_data[[i]][[1]]
##   color <- upper_plot_data[[i]][[2]]
##   tree_i <- upper_plot_data[[i]][[3]]
##   points_data <- Data_Matrix[upper_index,] %*% (unclass(loadings(upper_result)))
##   text(points_data[,1],points_data[,2],labels=tree_i,col=color)
## }
## #これ以上の解析はシミュレーションを伴う
## }
