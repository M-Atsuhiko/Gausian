display_mor <- function(Dendrite,Color){
#  rgl.spheres(c(0,0,0),radius = SOMA_DIAM/2,color = "green",sides = 10)#somaの描画
  #somaとdendriteの間に隙間があるように見えるが、これはRGLの球の描画が荒いせい？

  for(i in 1:length(Dendrite)){
    Branch_coordinate <- Dendrite[[i]][["coordi"]]
    diam <- Dendrite[[i]][["diam"]]
    Dendrite_cyl <- cylinder3d(Branch_coordinate,closed = -2,radius = diam,sides = 100)
    shade3d(Dendrite_cyl,color = Color)
  }
}
