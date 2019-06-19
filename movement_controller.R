#### Movement Controller #######################################################
# RoveR
# Movement Functions ("movement_contoller.R")
# July 2019
# FoxFields
#
# General functions that control movement of entities
#
# Contents:
# [-] Move Entity: move_entity(dx, dy, self, blocks=TRUE,  objs)

#### [-] Move Entity ###########################################################
# Move entities dx and dy.

# + dx = how much to change x position (numeric)
# + dy = how much to change y position (numeric)
# + self = the entity to move (string)
# + blocks = T/F does the object obey blocks? (not implemented)
# + objs = objects list (reactive list)
# = returns nothing; moves entity

move_entity <- function(dx, dy, self, blocks, objs) {
  if (blocks == TRUE) {
      objs[[self]]$x <- objs[[self]]$x + dx
      objs[[self]]$y <- objs[[self]]$y + dy
    }
  if (blocks == FALSE) {
    objs[[self]]$x <- objs[[self]]$x + dx
    objs[[self]]$y <- objs[[self]]$y + dy
  }
  else{
  }
}

