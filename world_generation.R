generate_system <- function(){
  system_planets <- planet_tilesetter(create_planet())
  planet_buffer <- list()
  for (i in 1:8){
   planet_buffer[i] <- system_planets[which(system_planets$x >50*(i-1) & 
                                            system_planets$y >50*(i-1) &
                                            system_planets$x <50*i & 
                                            system_planets$y <50*i),]
  }
  
  
  plot(wut$x,wut$y)
  
  
  system_planets[[1]] <- planet_tilesetter(create_planet())
  
  
  
  for (i in 1:3){
    system_planets[[i+1]] <- temp_level()
  }
  system_planets
}



