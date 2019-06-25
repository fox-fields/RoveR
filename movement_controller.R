#### Movement Controller #######################################################
# RoveR
# Movement Functions ("movement_contoller.R")
# June 2019 (RoveR version 2: "Apollo Lunar Rover")
# FoxFields
#
# General functions that control movement of entities
# Contents:
# [+] Move Entity: move_entity(dx, dy, self, blocks, archit, objs)

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
                      blocks = sapply(objs, "[[", "blocks"))
    obs <- obs[which(obs$blocks==TRUE)]
    cells <- data.frame(x = archit[['cells']]$x,
                        y = archit[['cells']]$y)
    obs <-rbind(obs,cells)
    
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
  else{}
}

