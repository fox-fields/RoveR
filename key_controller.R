#### Key Controller ############################################################
# RoveR
# Key Manager Functions ("key_controller.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields
#
# Functions that control keyboard input.
# Contents:
# [+] Movement Keys: movement_keys(key, archit, objs)
# [+] Melee Keys: melee_keys(key, archit, objs)

#### [+] Movement Keys #########################################################
# Keys that handle player movement.
#
# + key = keyboard input (key)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing. 

movement_keys <- function(key, archit, objs){
    if (key == 68){ #68 (keydown)
      objs[['player']][['move']](dx = 1, dy = 0, 'player', TRUE, archit, objs)
    }            
    if (key == 65){ #65 (keydown)
      objs[['player']][['move']](dx = -1, dy = 0, 'player', TRUE, archit, objs)
    }           
    if (key == 87){ #87 (keydown)
      objs[['player']][['move']](dx = 0, dy = 1, 'player', TRUE, archit, objs)
    }     
    if (key == 83){ #83 (keydown)
      objs[['player']][['move']](dx = 0, dy = -1, 'player', TRUE, archit, objs)
    }
    if (key == 69){ #69 (keydown)
      objs[['player']][['move']](dx = 1, dy = 1, 'player', TRUE, archit, objs)
    }            
    if (key == 81){ #81 (keydown)
      objs[['player']][['move']](dx =- 1, dy = 1, 'player', TRUE, archit, objs)
    }           
    if (key == 90){ #90 (keydown)
      objs[['player']][['move']](dx =- 1, dy =- 1, 'player', TRUE, archit, objs)
    }     
    if (key == 67){ #67 (keydown)
      objs[['player']][['move']](dx = 1, dy =- 1, 'player', TRUE, archit, objs)
    }
}

#### [+] Melee Keys #########################################################
# Keys that handle player melee attacks. 
#
# + key = keyboard input (key)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing.

melee_keys <- function(key, archit, objs){
  if (key == 68){ #68 (keydown)
    objs[['player']][['attack']](dx = 1, dy = 0, 'player', TRUE, archit, objs)
  }            
  if (key == 65){ #65 (keydown)
    objs[['player']][['attack']](dx = -1, dy = 0, 'player', TRUE, archit, objs)
  }           
  if (key == 87){ #87 (keydown)
    objs[['player']][['attack']](dx = 0, dy = 1, 'player', TRUE, archit, objs)
  }     
  if (key == 83){ #83 (keydown)
    objs[['player']][['attack']](dx = 0, dy = -1, 'player', TRUE, archit, objs)
  }
  if (key == 69){ #69 (keydown)
    objs[['player']][['attack']](dx = 1, dy = 1, 'player', TRUE, archit, objs)
  }            
  if (key == 81){ #81 (keydown)
    objs[['player']][['attack']](dx =- 1, dy = 1, 'player', TRUE, archit, objs)
  }           
  if (key == 90){ #90 (keydown)
    objs[['player']][['attack']](dx =- 1, dy =- 1, 'player', TRUE, archit, objs)
  }     
  if (key == 67){ #67 (keydown)
    objs[['player']][['attack']](dx = 1, dy =- 1, 'player', TRUE, archit, objs)
  }
}
