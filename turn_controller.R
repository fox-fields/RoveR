#### Turn Controller #######################################################
# RoveR
# Turn Handling Functions ("turn_contoller.R")
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields
#
# Functions that control the events of player and enemy turns. 
# Contents:
# [+] Handle Turns: handle_turns(key, archit)
# [+] Entity Turn: entity_turn(self, target, archit)

#### [+] Handle Turns #######################################################
#' Handle turns
#' 
#' `handle_turns()` executes the sequence of events during each turn of the game 
#' loop. Players are given the option to move, perform an action or open the 
#' menu. In each case, the key control is set accordingly. Moving or peforming 
#' an aciton ends the player's turn. Each non-player entity then performs their 
#' turns. At the end of the turn, short, stall and shield values are reset for
#' all entities. 
#'
#' @param key The keydown keyboard input (numeric).
#' @param archit A data table of the game architecture (data table).
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [movement_keys()] controls keyborad input during player movement.
#' * [action_keys()] controls keyboard input during player actions.
#' * [menu_keys()] controls keyboad input duing menu use.
#' * [entity_turn()] performs a turn for a given non-player entity.

handle_turns<-function(key, archit){
  if (archit[['state']] == 'player_movement'){
      movement_keys(key, archit)
  }
  if (archit[['state']] == 'player_action'|
      archit[['state']] == 1 |
      archit[['state']] == 2 |
      archit[['state']] == 3 ){ 
      action_keys(key, archit)
  }
  if (archit[['state']] == 'player_menu'){
     menu_keys(key, archit)
  }
  if (archit[['state']] == 'drop_menu'){
     drop_keys(key, archit)
  }
  if (archit[['state']] == 'entity_turn'){
    entities <- archit[["entities"]]
    entities <- entities[name != 'player' & class == 'actor']
    mapply(entity_turn, 
           self = entities$name,
           target = entities$target_name,
           MoreArgs = list(archit = archit)
    )
    archit[['state']] <- 'player_movement'
  }
  archit[["entities"]]$stall <- FALSE
  archit[["entities"]]$short <- FALSE
  archit[["entities"]]$shield <- 0
} 

#### [+] Entity Turn #######################################################
#' Perform an entity's turn
#' 
#' `entity_turn()` executes a non-player entity's turn.
#'
#' @param self An entity's name (string.)
#' @param taget The name of an entity's target (string).
#' @param archit A data table of the game architecture (data table).  
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [handle_turns()] executes the sequence of events during each turn.

entity_turn <- function(self, target, archit){
  entities <- archit[["entities"]] 
  entity_move <- get(as.character(entities[name == self]$behaviour))
  entity_move(self, target, blocks = TRUE, archit)
}

