#### Movement Controller #######################################################
# RoveR
# Movement Functions ("movement_contoller.R")
# July 2019 (RoveR version 0.5: "Lunokhod 3")
# FoxFields
#
# Functions that control movement of entities
# Contents:
# [+] Move Player: move_player(dx, dy, self, blocks, archit)
# [+] Move Entity: move_entity(dx, dy, self, blocks, archit)
# [+] Move Towards: move_towards(self, target, blocks, archit)

#### [+] Move Player ###########################################################
#' Move player
#' 
#' `move_player()` Move player dx and dy. Attacks with melee if there is an 
#' object in the way.
#'
#' @param dx The amount to change x position (numeric).
#' @param dy The amount to change y position (numeric).
#' @param self The entity to move (string).
#' @param blocks Does the object obey blocks? (boolean).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [move_entity()] moves entities dx and dy.

move_player <- function(dx, dy, self, blocks, archit) {
  if (blocks == TRUE) {
    entities <-  archit[['entities']]
    entity <- entities[name == self]
    entities <- entities[blocks == TRUE & name != self]
    if (any(entities$x == entity$x + dx & entities$y == entity$y + dy)) {
      target <- entities[x == entity$x + dx & y == entity$y + dy]$name
      target<-target[1]
      attack <- get(as.character(entity$attack))
      attack(self, target, archit)
      }
    else{
      entity$x <- entity$x + dx
      entity$y <- entity$y + dy
    }
  }
  else{
    entities <-  archit[['entities']]
    entity <- entities[name == self]
    entity$x <- entity$x + dx
    entity$y <- entity$y + dy
  }
  archit[['entities']][name == self] <- entity
  archit[['state']] <- 'entity_turn'
}

#### [+] Move Entity ###########################################################
#' Move entity
#' 
#' `move_entity()` Move entity dx and dy. Attacks with melee if there is an 
#' object in the way.
#'
#' @param dx The amount to change x position (numeric).
#' @param dy The amount to change y position (numeric).
#' @param self The entity to move (string).
#' @param blocks Does the object obey blocks? (boolean).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [move_entity()] moves entities dx and dy.

move_entity <- function(dx, dy, self, target, blocks, archit) {
  if (blocks == TRUE) {
    entities <-  archit[['entities']]
    entity <- entities[name == self]
    entities <- entities[blocks == TRUE & name != self]
    if (any(entities$x == entity$x + dx & entities$y == entity$y + dy)) {
      blocker <- entities[x == entity$x + dx & y == entity$y + dy]$name
      if (blocker == target){
        attack <- get(as.character(entity$attack))
        attack(self, target, archit)
      }
    }
    else{
      entity$x <- entity$x + dx
      entity$y <- entity$y + dy
    }
  }
  else{
    entities <-  archit[['entities']]
    entity <- entities[name == self]
    entity$x <- entity$x + dx
    entity$y <- entity$y + dy
  }
  archit[['entities']][name == self] <- entity
}

#### [+] Move Towards ##########################################################
#' Move entity towards a target
#' 
#' `move_towards()` moves an entity in a direct beeline to the target. 
#' Calculates vector from object to target and the distance, normalizes the 
#' distance to one tile along the map grid.
#'
#' @param self The entity to move (string).
#' @param target The entity to target (string).
#' @param blocks Does the object obey blocks? (boolean).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [move_entity()] moves entities dx and dy.

move_towards <- function(self, target, blocks, archit) {
  entities <-  archit[['entities']]
  entity <- entities[name == self]
  target <- entities[name == target]
  dx <-  target$x - entity$x
  dy <-  target$y - entity$y
  distance <- sqrt(dx ^ 2 + dy ^ 2)
  dx <- round(dx / distance, digits = 0)
  dy <- round(dy / distance, digits = 0)
  move_entity(dx, dy, self, target$name, blocks, archit)
}
