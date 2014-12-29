set_coordinate <- function(Dendrite){
  
  y_coordinate <- rbind(c(0,0,0),c(0,AXIS_LENG,0))
  z_coordinate <- rbind(c(0,0,0),c(0,0,AXIS_LENG))
  
  Stem_length <- Dendrite[[1]][["length"]]
                                        #  Stem_coordinate <- rbind(c(0,0,0),c(Stem_length,0,0))
  Stem_coordinate <- rbind(c(-Stem_length,0,0),c(0,0,0))

  parent_data <- list(list(Stem_coordinate,y_coordinate,z_coordinate,1))

  while(length(parent_data) > 0){
    new_data <- as.list(NULL)
    for(i in 1:length(parent_data)){
      parent_coordinate <- parent_data[[i]][[1]]
      y_coordinate <- parent_data[[i]][[2]]
      z_coordinate <- parent_data[[i]][[3]]
      dend_i <- parent_data[[i]][[4]]

      length <- Dendrite[[dend_i]][["length"]]
      elevation <- Dendrite[[dend_i]][["elevation"]]
      rotation <- Dendrite[[dend_i]][["rotation"]]
      daughter_i <- Dendrite[[dend_i]][["connect"]]

      daughter_i <- daughter_i[daughter_i != 0]

      ele_rad <- elevation*pi/180
      rot_rad <- rotation*pi/180
      
      y_length <- sqrt(sum((y_coordinate[2,] - y_coordinate[1,])^2))
      z_length <- sqrt(sum((z_coordinate[2,] - z_coordinate[1,])^2))

                                        #k: 親の枝との長さの比
      k <- length/sqrt(sum((parent_coordinate[1,] - parent_coordinate[2,])^2))
      
      Branch_trm <- k*(parent_coordinate[2,] - parent_coordinate[1,]) + parent_coordinate[2,]

      Branch_coordinate <- rbind(parent_coordinate[2,],
                                 (Branch_trm - parent_coordinate[2,])*cos(ele_rad)
                                 + (y_coordinate[2,] - y_coordinate[1,])*length*sin(ele_rad)/y_length
                                 + parent_coordinate[2,]) #Branch_elevation

      y_coordinate[2,] <- (y_coordinate[2,] - y_coordinate[1,])*cos(ele_rad) + (Branch_coordinate[1,] - Branch_trm)*y_length*sin(ele_rad)/length + Branch_coordinate[1,] #y_elevation

      Branch_ele <- Branch_coordinate[2,]#elevationを動かしたところで状態を一時保存する

      Branch_coordinate[2,] <- (Branch_coordinate[2,] - Branch_coordinate[1,])*cos(rot_rad) + (z_coordinate[1,] - z_coordinate[2,])*length*sin(rot_rad)/z_length + Branch_coordinate[1,] #Branch_rotation

      z_coordinate[2,] <- (z_coordinate[2,] - z_coordinate[1,])*cos(rot_rad) + (Branch_ele - z_coordinate[1,])*z_length*sin(rot_rad)/length + z_coordinate[1,] #z_rotation

      pararel_move <- Branch_coordinate[2,] - Branch_coordinate[1,]
      y_coordinate <- rbind(y_coordinate[1,] + pararel_move,y_coordinate[2,] + pararel_move)#y'平行移動
      z_coordinate <- rbind(z_coordinate[1,] + pararel_move,z_coordinate[2,] + pararel_move)#z'平行移動

      Dendrite[[dend_i]][["coordi"]] <- Branch_coordinate #結果の保存

      for(d_i in daughter_i){
        new_data[[length(new_data) + 1]] <- list(Branch_coordinate,y_coordinate,z_coordinate,d_i)
      }
    }
    parent_data <- new_data
  }
  
#  Dendrite <- recur_calc_coordinate(Stem_coordinate,1,Dendrite,y_coordinate,z_coordinate)
                                        #最後に、dendrite全体をsomaの表面に移動させる

  k <- SOMA_DIAM/(2*Stem_length)

  soma_surface <- Dendrite[[1]][["coordi"]][2,]*k

  for(i in 1:length(Dendrite)){
    Dendrite[[i]][["coordi"]] <- rbind(Dendrite[[i]][["coordi"]][1,] + soma_surface,Dendrite[[i]][["coordi"]][2,] + soma_surface)
    Dendrite[[i]][["coordi"]] <- round(Dendrite[[i]][["coordi"]],Morpho_Round_DIGITS)
  }
  
  return(Dendrite)
}
