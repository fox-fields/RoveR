#### Turn Controller #######################################################
# RoveR
# Turn Handling Functions ("turn_contoller.R")
# July 2019 (RoveR version 0.3: "Lunokhod 2")
# FoxFields
#
# Functions that control the turn of entities
# Contents:
# [+] Handle Turn: handle_turn(dx, dy, self, blocks, archit, objs)


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
      # Player turn bits
      movement_keys(key, archit, objs)
    }
  ### enemy turns
  if (state[['turn']] == 'enemy_turn'){
    # Enemy turn bits
    state[['turn']] <- 'player_turn'
  } 
}