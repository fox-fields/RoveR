#### Planet Generator ##########################################################
# RoveR
# Planet Generation Functions ("planet_generator.R") 
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields
#
# Funnctions that generate planets.
# Contents:
# [+] Create Landscape: create_landscape(map_buffer, prob, bias, smoothing, 
#                                       size, bedrock)
# [+] Create Climate: create_climate(map_buffer, prob, smoothing) 
# [+] Create River: create_river()
# [+] River System: river_system()
# [+] Create Planet: create_planet()
# [+] Planet_tilesetter: planet_tilesetter(planet_map)

#### Create Landscape ##########################################################
# Generate a simulated landscape.
#
# + map_buffer = input map (raster)
# + prob = bias during percolation mapping (numeric)
# + bias = bias during filling of map (numeric)
# + smoothing = extent of smoothing (numeric)
# + size = size of the generated map (numeric)
# + bedrock = generate a new map? (boolean)
# = returns; updated map (raster)

create_landscape <- function(map_buffer, 
                      prob = 0.50,
                      bias = 1.5, 
                      smoothing = 3,
                      size = 50,
                      bedrock = FALSE) {
  if (bedrock == TRUE) {
    map_buffer<-rasterFromXYZ(expand.grid(x = 1 : size, y = 1 : size, z = 1))
  }
  map_buffer@data@values[which(map_buffer@data@values >= 1)] <-
  sample(c(rep(0, round((1 - prob) * 100)), rep(1, round(prob * 100))),
         length(map_buffer@data@values[which(map_buffer@data@values >= 1)]),
         replace = TRUE)
  map_buffer<-clump(map_buffer, directions = 4)
  map_buffer@data@values[which(map_buffer@data@values >= 1)] <- 
    abs(round(sin(map_buffer@data@values[which(map_buffer@data@values >= 1 )]))) + 1
  map_buffer@data@values[which(is.na(map_buffer@data@values))] <- bias
  map_buffer<-focal(map_buffer, w = matrix(1, nrow = smoothing, ncol = smoothing), 
                    fun=mean)
  map_buffer@data@values<-round(map_buffer@data@values - 1)
  return(map_buffer)
}

#### Create Climate ############################################################
# Generate a simulated climate gradient.
#
# + map_buffer = input map (raster)
# + prob = bias during percolation mapping (numeric)
# + smoothing = extent of smoothing (numeric)
# = returns; updated map (raster)

create_climate <- function(map_buffer, prob = 0.59, smoothing = 3) {
  map_buffer@data@values[which(map_buffer@data@values >= 1)] <-
    sample(c(rep(0, round((1 - prob) * 100)), rep(1, round(prob * 100))),
           length(map_buffer@data@values[which(map_buffer@data@values >= 1)]),
           replace = TRUE)
  map_buffer<-clump(map_buffer, directions = 4)
  map_buffer@data@values[which(is.na(map_buffer@data@values))] <- 550
  map_buffer<-focal(map_buffer, w = matrix(1, 
                                           nrow = smoothing,
                                           ncol = smoothing),
                                           fun=mean)
  map_buffer@data@values<-(map_buffer@data@values/10) - 50
  return(map_buffer)
}


#### Create River ##############################################################
# Generate a simulated river. TODO: river generation could be improved. Perhaps
# a branching random walk that is weighed by elevation? 
#
# = returns; a smoothed random walk to generate a river (data.frame)

create_river<-function(){
  data.frame(
    x = abs(round(smooth.spline(cumsum(rnorm(n = 50000)))$y)) + sample(1:400, 1),
    y = abs(round(smooth.spline(cumsum(rnorm(n = 50000)))$y)) + sample(1:400, 1),
    z = 1)
  }

#### Create River System #######################################################
# Generate a simulated river system.
#
# = returns; a series of smoothed random walk river system (data.frame)
river_system <- function (){
  river_buffer <- create_river()
  for (i in 1:sample(c(1, 2), 1)){
    river_buffer<-rbind(river_buffer, create_river())
  }
  river_buffer<-unique(river_buffer)
  river_buffer<-rbind(river_buffer, data.frame(x = c(1, 500), 
                                               y = c(1, 500),
                                               z = 1))
  if (sample(c(1, 0), 1) == 1){
    names(river_buffer) <- c("y", "x", "z")
  }
  return(river_buffer[which(river_buffer$x <= 500 &
                            river_buffer$x >= 0 &
                            river_buffer$y <= 500 &
                            river_buffer$y >= 0
                            ),])
}

#### Create Planet #############################################################
# Generate a simulated planet. TODO: planet generation could be improved. For now,
# parameters are not varied, so the planets are uniform. As it stands, there are
# multiple layers generated for 'elevation' these dictate what is generated at 
# each. A lattiduinal themal gradient is applied across the map, and this is multipled by
# elevation such that a there is both an elevation and latitudinal themal gradient. Water 
# acts as a themal buffer, so water is generally cooler or warmer. 
#
# = returns; a series of smoothed random walk river system (data.frame)

create_planet <- function(){

  map_buffer <- create_landscape(1, prob = 0.58, bias = 1.5, smoothing = 3,
                                 size = 400, bedrock = TRUE)
  rivers_buffer <- rasterFromXYZ(river_system())
  rivers_buffer@data@values[which(is.na(rivers_buffer@data@values))] <- 0
  map_buffer <- extend(map_buffer,rivers_buffer)
  map_buffer <- map_buffer - rivers_buffer
  map_buffer@data@values[which(map_buffer@data@values < 0)] <- 0
  map_buffer_2 <- create_landscape(map_buffer, 0.58, bias = 1.5, smoothing = 5)
  map_buffer_3 <- create_landscape(map_buffer, 0.58)
  map_buffer_4 <- create_landscape(map_buffer, 0.58)
  map_buffer_5 <- create_landscape(map_buffer, 0.58, bias = 0.8)
  elevation_buffer <- map_buffer + map_buffer_2 + map_buffer_3 + map_buffer_4 +
    map_buffer_5
  
  return(elevation_buffer)

}

#### Planet Tilesetter #########################################################
# Assigns a tileset for the planet. For now this is simply based on the elevation
# raster. Creates a very earth like planet. Water, trees and rocks block. TODO: 
# This is poorly written and needs to be fixed. 
#
# = returns; a series of smoothed random walk river system (data.frame)

planet_tilesetter <- function(planet_map){
  map_buffer <- as.data.frame(planet_map, xy = TRUE, na.rm = TRUE)
  map_buffer <- map_buffer[which(map_buffer$layer >= 0),]
  
  map_buffer$char <- map_buffer$layer
  map_buffer$char[which(map_buffer$char == 0)] <-"≈"
  map_buffer$char[which(map_buffer$char == 1)] <-"≈"
  map_buffer$char[which(map_buffer$char == 2)] <-"."
  map_buffer$char[which(map_buffer$char == 3)] <-sample(c("〃",".","“",".",".","."),
                                                      length(map_buffer$char[which(map_buffer$char == 3)]),
                                                      replace = TRUE)
  map_buffer$char[which(map_buffer$char == 4)] <-sample(c("♠","♣","¥"),
                                                      length(map_buffer$char[which(map_buffer$char == 4)]),
                                                      replace=TRUE)
  map_buffer$char[which(map_buffer$char == 5)] <-sample(c("⩓","⩕","⩚","⨇","∧","∆"),
                                                      length(map_buffer$char[which(map_buffer$char== 5)]),
                                                      replace=TRUE)
  
  
   map_buffer$size <- map_buffer$layer
   map_buffer$size[which(map_buffer$size == 0)] <- 1.5
   map_buffer$size[which(map_buffer$size == 1)] <- 1.5
   map_buffer$size[which(map_buffer$size == 2)] <- 0.7
   map_buffer$size[which(map_buffer$size == 3)] <- 0.7
   map_buffer$size[which(map_buffer$size == 4)] <- 1.5
   map_buffer$size[which(map_buffer$size == 5)] <- 1.5
  
   map_buffer$color <- scales::viridis_pal()(7)[map_buffer$layer + 2]

   map_buffer$blocks <- TRUE
   map_buffer$blocks[which(map_buffer$layer < 4 & map_buffer$layer > 1)] <- FALSE
  
  map_buffer <- data.frame(x = map_buffer$x,
                           y = map_buffer$y,
                           char = map_buffer$char,
                           color = map_buffer$color,
                           size = map_buffer$size,
                           blocks = map_buffer$blocks,
                           name = paste('Wall', c(1:length(map_buffer$x)), sep="-"),
                           move = as.character('passive_call'),
                           attack = as.character('passive_call'),
                           behaviour = as.character('passive_call'),
                           integrity = 20,
                           energy = 0,
                           shield = 0,
                           power = 0,
                           data = 0,
                           max_integrity = 20,
                           max_energy = 0,
                           death = as.character('kill_entity'),
                           stall = FALSE,
                           short = FALSE,
                           target_name = NA,
                           class = 'prop',
                           description = "",
                           stringsAsFactors = FALSE
                           
                           
                           
             )
  
  system_planets <- map_buffer
  
  return(system_planets)
}

#### Edge Doors #########################################################
# TODO:Document

edge_doors <- function(system_planets){
  
  system_planets$color[which(system_planets$x == 50 |
                             system_planets$x == 100 |
                             system_planets$x == 150 |
                             system_planets$x == 200 |
                             system_planets$x == 250 |
                             system_planets$x == 300 |
                             system_planets$x == 350 
  )] <- "grey"
  system_planets$char[which(system_planets$x == 50 |
                            system_planets$x == 100 |
                            system_planets$x == 150 |
                            system_planets$x == 200 |
                            system_planets$x == 250 |
                            system_planets$x == 300 |
                            system_planets$x == 350
  )] <- "→"
  system_planets$death[which(system_planets$x == 50 |
                             system_planets$x == 100 |
                             system_planets$x == 150 |
                             system_planets$x == 200 |
                             system_planets$x == 250 |
                             system_planets$x == 300 |
                             system_planets$x == 350
  )] <- "map_right"
  system_planets$blocks[which(system_planets$x == 50 |
                              system_planets$x == 100 |
                              system_planets$x == 150 |
                              system_planets$x == 200 |
                              system_planets$x == 250 |
                              system_planets$x == 300 |
                              system_planets$x == 350
  )] <- TRUE
  system_planets$size[which(system_planets$x == 50 |
                              system_planets$x == 100 |
                              system_planets$x == 150 |
                              system_planets$x == 200 |
                              system_planets$x == 250 |
                              system_planets$x == 300 |
                              system_planets$x == 350
  )] <- 1
  system_planets$integrity[which(system_planets$x == 50 |
                                   system_planets$x == 100 |
                                   system_planets$x == 150 |
                                   system_planets$x == 200 |
                                   system_planets$x == 250 |
                                   system_planets$x == 300 |
                                   system_planets$x == 350
  )] <- 1
  system_planets$class[which(system_planets$x == 50 |
                                   system_planets$x == 100 |
                                   system_planets$x == 150 |
                                   system_planets$x == 200 |
                                   system_planets$x == 250 |
                                   system_planets$x == 300 |
                                   system_planets$x == 350
  )] <- 'door'
  


  system_planets$color[which(system_planets$y == 50 |
                             system_planets$y == 100 |
                             system_planets$y == 150 |
                             system_planets$y == 200 |
                             system_planets$y == 250 |
                             system_planets$y == 300 |
                             system_planets$y == 350 
  )] <- "grey"
  system_planets$char[which(system_planets$y == 50 |
                            system_planets$y == 100 |
                            system_planets$y == 150 |
                            system_planets$y == 200 |
                            system_planets$y == 250 |
                            system_planets$y == 300 |
                            system_planets$y == 350
  )] <- "↑"
  system_planets$death[which(system_planets$y == 50 |
                               system_planets$y == 100 |
                               system_planets$y == 150 |
                               system_planets$y == 200 |
                               system_planets$y == 250 |
                               system_planets$y == 300 |
                               system_planets$y == 350
  )] <- "map_up"
  system_planets$blocks[which(system_planets$y == 50 |
                                system_planets$y == 100 |
                                system_planets$y == 150 |
                                system_planets$y == 200 |
                                system_planets$y == 250 |
                                system_planets$y == 300 |
                                system_planets$y == 350
  )] <- TRUE
  system_planets$size[which(system_planets$y == 50 |
                              system_planets$y == 100 |
                              system_planets$y == 150 |
                              system_planets$y == 200 |
                              system_planets$y == 250 |
                              system_planets$y == 300 |
                              system_planets$y == 350
  )] <- 1
  system_planets$integrity[which(system_planets$y == 50 |
                                   system_planets$y == 100 |
                                   system_planets$y == 150 |
                                   system_planets$y == 200 |
                                   system_planets$y == 250 |
                                   system_planets$y == 300 |
                                   system_planets$y == 350
  )] <- 1
  system_planets$class[which(system_planets$y == 50 |
                                   system_planets$y == 100 |
                                   system_planets$y == 150 |
                                   system_planets$y == 200 |
                                   system_planets$y == 250 |
                                   system_planets$y == 300 |
                                   system_planets$y == 350
  )] <- 'door'
  
  
  
  system_planets$color[which(system_planets$y == 51 |
                               system_planets$y == 101 |
                               system_planets$y == 151 |
                               system_planets$y == 201 |
                               system_planets$y == 251 |
                               system_planets$y == 301 |
                               system_planets$y == 351 
  )] <- "grey"
  system_planets$char[which(system_planets$y == 51 |
                              system_planets$y == 101 |
                              system_planets$y == 151 |
                              system_planets$y == 201 |
                              system_planets$y == 251 |
                              system_planets$y == 301 |
                              system_planets$y == 351
  )] <- "↓"
  system_planets$death[which(system_planets$y == 51 |
                               system_planets$y == 101 |
                               system_planets$y == 151 |
                               system_planets$y == 201 |
                               system_planets$y == 251 |
                               system_planets$y == 301 |
                               system_planets$y == 351
  )] <- "map_down"
  system_planets$blocks[which(system_planets$y == 51 |
                                system_planets$y == 101 |
                                system_planets$y == 151 |
                                system_planets$y == 201 |
                                system_planets$y == 251 |
                                system_planets$y == 301 |
                                system_planets$y == 351
  )] <- TRUE
  system_planets$size[which(system_planets$y == 51 |
                              system_planets$y == 101 |
                              system_planets$y == 151 |
                              system_planets$y == 201 |
                              system_planets$y == 251 |
                              system_planets$y == 301 |
                              system_planets$y == 351
  )] <- 1
  system_planets$integrity[which(system_planets$y == 51 |
                                   system_planets$y == 101 |
                                   system_planets$y == 151 |
                                   system_planets$y == 201 |
                                   system_planets$y == 251 |
                                   system_planets$y == 301 |
                                   system_planets$y == 351
  )] <- 1
  system_planets$class[which(system_planets$y == 51 |
                                   system_planets$y == 101 |
                                   system_planets$y == 151 |
                                   system_planets$y == 201 |
                                   system_planets$y == 251 |
                                   system_planets$y == 301 |
                                   system_planets$y == 351
  )] <- 'door'
  
  
  system_planets$color[which(system_planets$x == 51 |
                               system_planets$x == 101 |
                               system_planets$x == 151 |
                               system_planets$x == 201 |
                               system_planets$x == 251 |
                               system_planets$x == 301 |
                               system_planets$x == 351 
  )] <- "grey"
  system_planets$char[which(system_planets$x == 51 |
                              system_planets$x == 101 |
                              system_planets$x == 151 |
                              system_planets$x == 201 |
                              system_planets$x == 251 |
                              system_planets$x == 301 |
                              system_planets$x == 351
  )] <- "←" 
  system_planets$death[which(system_planets$x == 51 |
                               system_planets$x == 101 |
                               system_planets$x == 151 |
                               system_planets$x == 201 |
                               system_planets$x == 251 |
                               system_planets$x == 301 |
                               system_planets$x == 351
  )] <- "map_left"
  system_planets$blocks[which(system_planets$x == 51 |
                                system_planets$x == 101 |
                                system_planets$x == 151 |
                                system_planets$x == 201 |
                                system_planets$x == 251 |
                                system_planets$x == 301 |
                                system_planets$x == 351
  )] <- TRUE
  system_planets$size[which(system_planets$x == 51 |
                              system_planets$x == 101 |
                              system_planets$x == 151 |
                              system_planets$x == 201 |
                              system_planets$x == 251 |
                              system_planets$x == 301 |
                              system_planets$x == 351
  )] <- 1
  system_planets$integrity[which(system_planets$x == 51 |
                                   system_planets$x == 101 |
                                   system_planets$x == 151 |
                                   system_planets$x == 201 |
                                   system_planets$x == 251 |
                                   system_planets$x == 301 |
                                   system_planets$x == 351
  )] <- 1
  system_planets$class[which(system_planets$x == 51 |
                                   system_planets$x == 101 |
                                   system_planets$x == 151 |
                                   system_planets$x == 201 |
                                   system_planets$x == 251 |
                                   system_planets$x == 301 |
                                   system_planets$x == 351
  )] <- 'door'
  
  
  
  system_planets$color[which(system_planets$x == 3 |
                             system_planets$x == 399 
  )] <- "grey"
  system_planets$char[which(system_planets$x == 3 |
                              system_planets$x == 399
                            
  )] <- "⊞"
  system_planets$integrity[which(system_planets$x == 3 |
                                   system_planets$x == 399
                                 
  )] <- 9999
  system_planets$size[which(system_planets$x == 3 |
                                   system_planets$x == 399
                                 
  )] <- 1.3
  system_planets$blocks[which(system_planets$x == 3 |
                                   system_planets$x == 399
                                 
  )] <- TRUE
  
  
  
  
  
  system_planets$color[which(system_planets$y == 3 |
                               system_planets$y == 399
  )] <- "grey"
  system_planets$char[which(system_planets$y == 3 |
                              system_planets$y== 399 
                            
  )] <- "⊞"
  system_planets$integrity[which(system_planets$y == 3 |
                                   system_planets$y == 399
                                 
  )] <- 9999
  system_planets$size[which(system_planets$y == 3 |
                              system_planets$y == 399
                            
  )] <- 1.3
  system_planets$blocks[which(system_planets$y == 3 |
                                system_planets$y == 399
                              
  )] <- TRUE
  
  system_planets <- system_planets[-which(system_planets$char == "."),]
  return(system_planets)
}





















