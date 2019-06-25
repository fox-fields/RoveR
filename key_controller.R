#### Key Controller ##############################################################
# RoveR
# Key Manager Functions ("key_controller.R")
# June 2019 (RoveR version 2: "Apollo Lunar Rover")
# FoxFields
#
# General functions that control keyboard input.
# Contents:
# [+] Movement Keys: movement_keys(key, archit, objs)

#### [+] Movement Keys #########################################################
# Keys that handle player movement.

# + key = keyboard input (key)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing. 

movement_keys<-function(key, archit, objs){
    if (key == 68){ #68 (keydown) or 100 (keypress)
      objs[['player']][['move']](dx = 1, dy = 0, 'player', TRUE, archit, objs)
    }            
    if (key == 65){ #65 (keydown) or 97 (keypress)
      objs[['player']][['move']](dx =- 1, dy = 0, 'player', TRUE, archit, objs)
    }           
    if (key == 87){ #87 (keydown) or 119 (keypress)
      objs[['player']][['move']](dx = 0, dy = 1, 'player', TRUE, archit, objs)
    }     
    if (key == 83){ #83 (keydown) or 115 (keypress)
      objs[['player']][['move']](dx = 0, dy =- 1, 'player', TRUE, archit, objs)
    }
 }
