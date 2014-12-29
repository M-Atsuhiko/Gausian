recur_calc_coordinate <- function(parent_coordinate,i,Dendrite){

  length <- Dendrite[[i]][["length"]]
  elevation <- Dendrite[[i]][["elevation"]]
  rotation <- Dendrite[[i]][["rotation"]]
  daughter_i <- Dendrite[[i]][["connect"]]
  daughter_i <- daughter_i[daughter_i != 0]  

  ele_rad <- elevation*pi/180
  rot_rad <- rotation*pi/180

  Branch_coordinate <- rbind(parent_coordinate,(parent_coordinate + c(length,0,0))) #新しい枝は最初はx軸の向きに伸びているとする

  Branch_coordinate[2,] <- c(length*cos(ele_rad),length*sin(ele_rad),0) # elevation
  Branch_coordinate[2,] <- c(Branch_coordinate[2,1]*cos(rot_rad),
                             Branch_coordinate[2,2],
                             -Branch_coordinate[2,1]*sin(rot_rad)) #rotation
  
  Branch_coordinate[2,] <- Branch_coordinate[2,] + Branch_coordinate[1,] #平行移動

  Dendrite[[i]][["coordinate"]] <- Branch_coordinate

  for(i in daughter_i) Dendrite <- recur_calc_coordinate(Branch_coordinate,i,Dendrite,y_coordinate,z_coordinate)
  return(Dendrite)
}

set_coordinate <- function(Dendrite){
  #回転の際の軸は三次元空間のxyzの向きで固定する elevationの回転とrotationの回転が干渉しないようにする
  
  Stem_length <- Dendrite[[1]][["length"]]
  Stem_coordinate <- rbind(c(0,0,0),c(Stem_length,0,0))
  
  Dendrite <- recur_calc_coordinate(Stem_coordinate,1,Dendrite)
  
  #最後に、dendrite全体をsomaの表面に移動させる

  k <- SOMA_DIAM/(2*Stem_length)

  soma_surface <- Dendrite[[1]][["coordi"]][2,]*k

  for(i in 1:length(Dendrite))
    Dendrite[[i]][["coordi"]] <- rbind(Dendrite[[i]][["coordi"]][1,] + soma_surface,Dendrite[[i]][["coordi"]][2,] + soma_surface)

  return(Dendrite)
}
