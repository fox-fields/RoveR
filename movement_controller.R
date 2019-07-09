#### Movement Controller #######################################################
# RoveR
# Movement Functions ("movement_contoller.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields
#
# Functions that control movement of entities
# Contents:
# [+] Move Entity: move_entity(dx, dy, self, blocks, archit, objs)
# [+] Move Towards: move_towards(self, target, blocks, archit, objs)
# [+] Distance To: distance_to(self, target, archit, objs)
# [+] Pathfind: pathfind(self, target, blocks, archit, objs)

#### [+] Move Entity ###########################################################
# Move entities dx and dy.

# + dx = how much to change x position (numeric)
# + dy = how much to change y position (numeric)
# + self = the entity to move (string)
# + blocks = T/F does the object obey blocks? (boolean)
# + archit = architecture list (reactive list)
# + objs = objects list (reactive list)
# = returns nothing; moves entity

move_entity <- function(dx, dy, self, blocks, archit, objs) {
  if (blocks == TRUE) {
    obs <- data.frame(x = sapply(objs, "[[", "x"),
                      y = sapply(objs, "[[", "y"),
                      blocks = sapply(objs, "[[", "blocks")
                      )
    cells <- data.frame(x = archit[['cells']]$x,
                        y = archit[['cells']]$y,
                        blocks =  archit[['cells']]$blocks
                        )
    obs <- rbind(obs[which(obs$blocks == TRUE),], 
                 cells[which(cells$blocks == TRUE),]
                 )
    if (any(obs$x == objs[[self]]$x + dx &
            obs$y == objs[[self]]$y + dy)) {
      objs[[self]]$x <- objs[[self]]$x
      objs[[self]]$y <- objs[[self]]$y
    }
    else{
      objs[[self]]$x <- objs[[self]]$x + dx
      objs[[self]]$y <- objs[[self]]$y + dy
    }
  }
  if (blocks == FALSE) {
    objs[[self]]$x <- objs[[self]]$x + dx
    objs[[self]]$y <- objs[[self]]$y + dy
  }
  else{
  }
}

#### [+] Move Towards ##########################################################
# Moves entity in a direct beeline to the target. Calculates vector from object
# to target and the distance, normalizes the distance to one tile along the map
# grid.
#
# + self = the entity to move (string)
# + target = the entity to target (string)
# + blocks = T/F does the object obey blocks? (logic)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns nothing; moves entity toward target

move_towards <- function(self, target, blocks, archit, objs) {
  dx <-  objs[[target]]$x - objs[[self]]$x
  dy <-  objs[[target]]$y - objs[[self]]$y
  distance <- sqrt(dx ^ 2 + dy ^ 2)
  dx <- as.integer(round(dx / distance, digits = 0))
  dy <- as.integer(round(dy / distance, digits = 0))
  move_entity(dx, dy, self, blocks, archit, objs)
}

#### [+] Distance To ##########################################################
# Calculate the distance between the self and target entities. 
#
# + self = the entity to move (string)
# + target = the entity to target (string)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns nothing; moves entity toward target

distance_to<-function(self, target, archit, objs){
  dx = objs[[target]]$x - objs[[self]]$x
  dy = objs[[target]]$y- objs[[self]]$y
  return(sqrt(dx^2 + dy^2))
}


#### [+] Pathfind ##############################################################
# Pathfind entity to target while avoiding obstacles. Blocks is set to True.
#
# + self = the entity to move (string)
# + target = the entity to target (string)
# + archit = obstacle list for blocks check (reactive list) 
# + objs = objects list (reactive list)
# = returns nothing; moves entity toward target in pathfind

pathfind <- function(self, target, blocks, archit, objs) {
  if (distance_to(self, target, archit, objs) >= 0 & 
      distance_to(self, target, archit, objs) <= 8) {
    obs <- data.frame(x = sapply(objs, "[[", "x"),
                      y = sapply(objs, "[[", "y"),
                      blocks = sapply(objs, "[[", "blocks"),
                      z = 1
    )
    cells <- data.frame(x = archit[['cells']]$x,
                        y = archit[['cells']]$y,
                        blocks =  archit[['cells']]$blocks,
                        z = 1
    )
    obs <- rbind(obs[which(obs$blocks == TRUE),], 
                 cells[which(cells$blocks == TRUE),]
    )
    obs <- obs[which(obs$x <= 8+objs[['player']]$x &
               obs$x >= objs[['player']]$x-8 &
               obs$y <= 8+objs[['player']]$y &
               obs$y >= objs[['player']]$y-8),]
    
  
    coords <- xtabs(z ~ x + y, data = obs) # create a matrix
    coords <- (coords - 1) * (-1) # make obstacles to 0
    coords <- coords / coords # convert 0 to NA
    attr(coords, "class") <- NULL
    attr(coords, "call") <- NULL # convert xtabs class to a matrix
    
    coords[as.character(objs[[self]]$x),
           as.character(objs[[self]]$y)] <- 2 # assign self as 2
    coords[as.character(objs[[target]]$x),
           as.character(objs[[target]]$y)] <- 3 # assign target as 3
    
    density_map <- raster(coords)
    

    self_position <-
      SpatialPoints(xyFromCell(density_map, 
                               Which(density_map == 2, cells = TRUE)))
    target_position   <-
      SpatialPoints(xyFromCell(density_map, 
                               Which(density_map == 3, cells = TRUE)))
    
    transition_map <- transition(density_map, transitionFunction = mean, directions = 4)
    shortest_map <-
      shortestPath(transition_map, self_position, target_position, output = "SpatialLines")

    steps <- sign(diff(coordinates(shortest_map)[[1]][[1]]))
    (t(-steps) + c(2, 3))[t(steps != 0)]
    
    ## Sometimes steps throws a numeric(0), this is a hacky workaround using
    ## a matix if statment.
    if (class(steps) == "matrix") {
      path_buffer <- shortest_map@lines[[1]]@Lines[[1]]@coords
      path_buffer <- path_buffer[2, ] - path_buffer[1, ]
      
      ## Convert to 0 or 1 while keeping sign
      if (path_buffer['x'] > 0) {
        path_buffer['x'] = (path_buffer['x'] / path_buffer['x'])
      }
      if (path_buffer['x'] < 0) {
        path_buffer['x'] = -(path_buffer['x'] / path_buffer['x'])
      }
      if (path_buffer['y'] > 0) {
        path_buffer['y'] = (path_buffer['y'] / path_buffer['y'])
      }
      if (path_buffer['y'] < 0) {
        path_buffer['y'] = -(path_buffer['y'] / path_buffer['y'])
      }
      
      move_entity(-path_buffer['y'], path_buffer['x'], self, blocks, archit, objs)
    }

  }
  else{
    if (distance_to(self, target, archit, objs) == 0) {
      ## do nothing if already at target
    }
  }
}


