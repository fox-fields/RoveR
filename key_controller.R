#### Key Controller ############################################################
# RoveR
# Key Manager Functions ("key_controller.R")
# July 2019 (RoveR version 0.5: "Lunokhod 3")
# FoxFields
#
# Functions that control keyboard input.
# Contents:
# [+] Movement Keys: movement_keys(key, archit)
# [+] Action Keys: action_keys(key, archit)
# [+] Menu Keys: menu_keys(key, archit)
# [+] Drop Keys: drop_keys(key, archit)

#### [+] Movement Keys #########################################################
#' Movement keyboard input
#' 
#' `movement_keys()` moves the player by accepting keydown keyboard input.
#'
#' @param key A keydown input code (numeric).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [action_keys()] controls key input during player actions. 
#' * [menu_keys()] controls key input during menu use.

movement_keys <- function(key, archit){
    entities <- archit[['entities']]
    player_move <- get(as.character(entities[name == 'player']$move))
    if (key == 68){ #68 (keydown = east)
      player_move(dx = 1, dy = 0, 'player', TRUE, archit)
    }            
    else if (key == 65){ #65 (keydown = west)
      player_move(dx = -1, dy = 0, 'player', TRUE, archit)
    }           
    else if (key == 87){ #87 (keydown = north)
      player_move(dx = 0, dy = 1, 'player', TRUE, archit)
    }     
    else if (key == 83){ #83 (keydown = south)
      player_move(dx = 0, dy = -1, 'player', TRUE, archit)
    }
    else if (key == 69){ #69 (keydown = north east)
      player_move(dx = 1, dy = 1, 'player', TRUE, archit)
    }            
    else if (key == 81){ #81 (keydown = north west)
      player_move(dx = -1, dy = 1, 'player', TRUE, archit)
    }           
    else if (key == 90){ #90 (keydown = south west)
      player_move(dx = -1, dy = -1, 'player', TRUE, archit)
    }     
    else if (key == 67){ #67 (keydown = south east)
      player_move(dx = 1, dy = -1, 'player', TRUE, archit)
    }
    else if (key == 49 | key == 50 | key == 51){ #49, 50, 51 (keydown = 1, 2, 3)
      archit[['state']] <- 'player_action'
    }
    else if (key == 56){ #187 (keydown = +)
      archit[['state']] <- 'player_menu'
    }
    else if (key == 55){ #189 (keydown = -)
      archit[['state']] <- 'drop_menu'
    }
}

#### [+] Action Keys ###########################################################
#' Action keyboard input
#' 
#' `action_keys()` handles player actions by accepting keydown keyboard input.
#'
#' @param key A keydown input code (numeric).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [movement_keys()] controls key input during player movement. 
#' * [menu_keys()] controls key input during menu use.

action_keys<-function(key, archit){
  if (key == 49){ #49 (keydown = 1)
    archit[['state']] <- 1
  }
  if (key == 50){ #50 (keydown = 2)
    archit[['state']] <- 2
  }
  if (key == 51){ #51 (keydown = 3)
    archit[['state']] <- 3
  }
  action <- archit[['action']][archit[['state']],]
  action_targeting <- get(as.character(action$action_targeting))
  action_targeting(archit[['state']], key, 'player', archit)
}

#### [+] Menu Keys #########################################################
#' Menu keyboard input
#' 
#' `menu_keys()` controls menu events by accepting keydown keyboard input.
#'
#' @param key A keydown input code (numeric).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [action_keys()] controls key input during player actions. 
#' * [movement_keys()] controls key input during player movement.

menu_keys <- function(key, archit){
  if (key >=65 & key <=77){ # 65-77 (keydown = a, b, c, etc.)

    inventory <- archit[["inventory"]]
    item <- inventory[key - 64]
    if (!is.na(item$name)){
    use_item <- get(as.character(item$behaviour))
    use_item(item$max_integrity, "player", archit)
    archit[["inventory"]] <- archit[["inventory"]][!(name == item$name)]
    archit[['state']] <- 'entity_turn'
    }
    else{}
  }
  else if (key == 55){ # 192 (keydown = `)
    archit[['state']] <- 'drop_menu'
  }
  else if (key == 57){ # 192 (keydown = `)
    archit[['state']] <- 'player_movement'
  }
}

#### [+] Drop Keys #########################################################
#' Drop item keyboard input
#' 
#' `drop_keys()` controls menu events by accepting keydown keyboard input.
#'
#' @param key A keydown input code (numeric).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [action_keys()] controls key input during player actions. 
#' * [movement_keys()] controls key input during player movement.

drop_keys <- function(key, archit){
  if (key >=65 & key <=77){ # 65-77 (keydown = a, b, c, etc.)
    inventory <- archit[["inventory"]]
    entities <- archit[["entities"]]
    player <- entities[name == "player"]
    item <- inventory[key - 64]
    if (!is.na(item$name)){
      item$x <- player$x
      item$y <- player$y
      item$integrity <- 1
      archit[["entities"]] <- rbindlist(list(archit[["entities"]],item))
      archit[["inventory"]] <- archit[["inventory"]][!(name == item$name)]
      archit[['state']] <- 'entity_turn'
    }
    else{}
  }
  else if (key == 56){ # 192 (keydown = `)
    archit[['state']] <- 'player_menu'
  }
  else if (key == 57){ # 192 (keydown = `)
    archit[['state']] <- 'player_movement'
  }
}

