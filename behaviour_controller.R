#### Behaviour Controller ######################################################
# RoveR
# Entity Behaviour Functions ("behaviour_contoller.R")
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields
#
# Functions that control entity behaviour.
# Contents:
# [+] Passive Call: passive_call()
# [+] Kill Entity: kill_entity(self, archit)
# [+] Give Item: give_item(self, archit)

#### [+] Passive Call ##########################################################
#' A passive function
#' 
#' `passive_call()` accepts arguments, but does nothing.
#'
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [expand_entities()] expands entities at the end of the game loop.

passive_call <- function(...){
}

#### [+] Kill Entity ###########################################################
#' Kill entity
#' 
#' `kill_entity()` kills an entity when the entity's integrity drops below 
#' zero. Gives the dead entity with passive call functions.
#'
#' @param self The entity that is to be killed (string)
#' @param archit A data table of the game architecture (data table).
#' @return Returns nothing.
#' @examples 
#' @seealso
#' * [give_item()] transfers an item from the field to the player's inventory.

kill_entity<-function(self, archit){
  entities <-  archit[['entities']]
  entity <- entities[name == self]
  entity$char <- sample(c("⑆","⑇","⑈ ","⑉"),1)
  entity$color <-  "grey"
  entity$blocks <- FALSE
  entity$class <- "prop"
  entity$move <- as.character('passive_call')
  entity$attack <- as.character('passive_call')
  entity$death <- as.character('passive_call')
  entity$size <- 1
  entity$description <- "It's some debris. Squick."
  archit[['entities']][name == self] <- entity
}

#### [+] Give Item #############################################################
#' Give player an item
#' 
#' `give_item()` provides an inventory item to the player when the item entity
#' is removed from the map.
#'
#' @param self The entity that is to be killed (string)
#' @param archit A data table of the game architecture (data table).
#' @return Returns nothing.
#' @examples 
#' @seealso
#' * [kill_entity()] kills an entity at zero integrity.

give_item<-function(self, archit){
  entities <-  archit[['entities']]
  archit[['inventory']] <- rbindlist(list(archit[['inventory']],entities[name == self]))
  entities <- entities[!(name == self)]
  archit[['entities']] <- entities
}


#### [+] Go down a level #######################################################

change_level<-function(self, archit){
  level_temp <- temp_level()
  level_temp <- level_temp[which(level_temp$x <50 & level_temp$y <50),]
  acceptable_tiles <- expand.grid(x=min(level_temp$x):max(level_temp$x), 
                                  y=min(level_temp$y):max(level_temp$y))
  level_tiles <- data.frame(x = level_temp$x,
                            y = level_temp$y)
  acceptable_tiles <- setdiff(acceptable_tiles,level_tiles)
  entities_temp <- read.csv('entities_example.csv',
                            header = TRUE,
                            stringsAsFactors = FALSE)
  acceptable_tiles <- sample_n(acceptable_tiles, length(entities_temp$x), replace =TRUE)
  entities_temp$x <- acceptable_tiles$x
  entities_temp$y <- acceptable_tiles$y
  
  archit[['entities']] <-  as.data.table(rbind(entities_temp,
                                               level_temp))
  archit[['hidden']] <- list()
  archit[['state']] <- 'player_movement'

}



map_right<-function(self, archit){
  entities <-  rbindlist(list(archit[['entities']],archit[['hidden']]))
  player <- entities[name == 'player']

  world <- archit[['world']]
  level_temp <- world[which(world$x > player$x + 1 &
                            world$x < player$x + 52 &
                            world$y > min(entities$y)-1 &
                            world$y < max(entities$y)+1
                            ),]
  
  archit[['entities']] <-  as.data.table(rbind(level_temp,
                                               player))
  
  archit[['hidden']] <- list()
  archit[['state']] <- 'player_movement'
  archit[['entities']][name == 'player']$x <- as.numeric(player$x) + 3
  
  
}


map_left<-function(self, archit){
  entities <-  rbindlist(list(archit[['entities']],archit[['hidden']]))
  player <- entities[name == 'player']
  
  world <- archit[['world']]
  level_temp <- world[which(world$x > player$x - 52 &
                              world$x < player$x - 1 &
                              world$y > min(entities$y)-1 &
                              world$y < max(entities$y)+1
  ),]
  
  archit[['entities']] <-  as.data.table(rbind(level_temp,
                                               player))
  
  archit[['hidden']] <- list()
  archit[['state']] <- 'player_movement'
  archit[['entities']][name == 'player']$x <- as.numeric(player$x) - 3
  
  
}


map_down<-function(self, archit){
  entities <-  rbindlist(list(archit[['entities']],archit[['hidden']]))
  player <- entities[name == 'player']
  
  world <- archit[['world']]
  level_temp <- world[which(world$y > player$y - 52 &
                              world$y < player$y - 1 &
                              world$x > min(entities$x)-1 &
                              world$x < max(entities$x)+1
  ),]
  
  
  archit[['entities']] <-  as.data.table(rbind(level_temp,
                                               player))
  
  archit[['hidden']] <- list()
  archit[['state']] <- 'player_movement'
  archit[['entities']][name == 'player']$y <- as.numeric(player$y) - 3
  
  
}


map_up <-function(self, archit){
  entities <-  rbindlist(list(archit[['entities']],archit[['hidden']]))
  player <- entities[name == 'player']
  
  world <- archit[['world']]
  level_temp <- world[which(world$y > player$y + 1 &
                              world$y < player$y + 52 &
                              world$x > min(entities$x)-1 &
                              world$x < max(entities$x)+1
  ),]
  archit[['entities']] <-  as.data.table(rbind(level_temp,
                                               player))
  
  archit[['hidden']] <- list()
  archit[['state']] <- 'player_movement'
  archit[['entities']][name == 'player']$y <- as.numeric(player$y) + 3
  
  
}



