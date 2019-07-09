#### Turn Controller #######################################################
# RoveR
# Turn Handling Functions ("turn_contoller.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields
#
# Functions that control the turn of entities
# Contents:
# [+] Handle Turn: handle_turn(dx, dy, self, blocks, archit, objs)
# [+] Enemy Turn: enemy_turn(self, target, archit, objs)

#### [+] Handle Turn ###########################################################
# Each turn begins with the player's action. The player may move. At the end of 
# the players turn, the enemy takes a turn and the game state updates. 
#
# + key = keyboard input (key)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing. 

handle_turn<-function(key, state, archit, objs){

  ### Player turn
  if (state[['turn']] == 'player_turn'){
      movement_keys(key, archit, objs)
      melee_keys(key, archit,objs)
      state[['turn']] <- 'enemy_turn'
    }
  ### enemy turns
  if (state[['turn']] == 'enemy_turn'){
    mapply(enemy_turn, 
           sapply(objs, "[[", "self"), 
           MoreArgs = list(target = 'player',
                           archit=archit,
                           objs=objs)
                           )
    state[['turn']] <- 'player_turn'
  } 
}

#### [+] Enemy Turn ###########################################################
# Takes a signle turn for a given enemy by calling its behaviour function.  
#
# + self = the entity taking the turn (string)
# + target = the target entity (defaults to 'player'; string)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing. 

enemy_turn <- function(self, target, archit, objs){
  objs[[as.character(self)]][['behaviour']](self, target, archit, objs)
}

