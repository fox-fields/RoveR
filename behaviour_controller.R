#### Behaviour Controller ######################################################
# RoveR
# Entity Behaviour Functions ("behaviour_contoller.R")
## July 2019 (RoveR version 0.5: "Lunokhod 3")
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
