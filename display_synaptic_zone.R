source("Simulation_Parameters.R")

display_synaptic_zone <- function(){
  library("rgl")
#  rgl.bg(color="white")
  rgl.spheres(c(0,0,0),radius = SOMA_DIAM/2,sides = 10,color="green")#,texture = SOMA_TEXTURE)

  shade3d( translate3d( scale3d(cube3d(col=UPPER_SYNAPTIC_ZONE_COLOR,alpha = ALP),SYNAPTIC_SHOW_SQUARE,SYNAPTIC_ZONE_DEPTH/2,SYNAPTIC_SHOW_SQUARE), 0, UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH/2, 0) )
  shade3d( translate3d( scale3d(cube3d(col=LOWER_SYNAPTIC_ZONE_COLOR,alpha = ALP),SYNAPTIC_SHOW_SQUARE,SYNAPTIC_ZONE_DEPTH/2,SYNAPTIC_SHOW_SQUARE), 0, LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH/2, 0) )

}

display_synaptic_zone()
