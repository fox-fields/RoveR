#### Combat Controller #########################################################
# RoveR
# Combat Functions ("combat_contoller.R")
# July 2019 (RoveR version 0.5: "Lunokhod 3")
# FoxFields
#
# Functions that control combat between entities
# Contents:
# [+] Attack Target: attack_target(self, target, archit)
# [+] Take Damage: take_damage(self, damage, archit)

#### [+] Attack Target ########################################################
#' Attack target
#' 
#' `attack_target()` perfoms the attacks of one entity agaisnt another entity.
#'
#' @param self The entity that is attacking (string).
#' @param target The entity that is being attacked (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_damage()] deals damage directly to a target.

attack_target<-function(self, target, archit){
  entities <-  archit[['entities']]
  entity <- entities[name == self]
  target <- entities[name == target]
  damage <- entity$power - target$shield
  if (damage > 0){
    take_damage(target$name, damage, archit)
    log_text(paste("The", self,"hits", target$name, "for", damage,"damage.",
                   sep =" "), archit)
  }
}

#### [+] Take Damage ###########################################################
#' Apply Damage
#' 
#' `take_damage()` deals damage to an entity. If integrity is at or below zero, 
#' evaluates the death function for the entity.
#'
#' @param self The entity that is attacking (string).
#' @param damage The damage to be dealt to the entity (numeric).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [attack_target()] deals damage directly to a target.

take_damage<-function(self, damage, archit){
  if (damage > 0){
    entities <-  archit[['entities']]
    entity <- entities[name == self]
    entity$integrity <- entity$integrity - damage
    archit[['entities']][name == self] <- entity
  }
  if (entity$integrity <= 0){
    death <- get(as.character(entity$death))
    death(self, archit)
    log_text(paste(self, "collapses into debris.",
                   sep =" "), archit)
  }
}

