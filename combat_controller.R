#### Combat Controller #########################################################
# RoveR
# Combat Functions ("combat_contoller.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields
#
# Functions that control combat between entities
# Contents:
# [+] Impose Combat: impose_combat(attack, integrity, energy, shield, power,
#                                 data) 
# [+] Attack Target: attack_target(self, target, archit, objs)
# [+] Take Damage: take_damage(self, damage, archit, objs)
# [+] Player Attack: player_attack(dx, dy, self, blocks, archit, objs)

#### [+] Impose Combat #########################################################
# Impose combat data on an entity.  
#
# + attack = the attack function (function)
# + integrity = integrity of the entity (numeric)
# + energy = energy avaliable to the entity (numeric)
# + shield = shielding of the entitiy (numeric)
# + power = power avaliable to the entity (numeric)
# + data = data avaliable to the entity (numeic)
# = returns; nothing. 

impose_combat <- function(self, integrity, energy, shield, power, data) {
  reactiveValues(
    self = self,
    max_integrity = integrity,
    integrity = integrity,
    max_energy = energy,
    energy = energy,
    shield = shield,
    power = power, 
    data = data,
    stall = FALSE,
    short = FALSE,
    death = NULL
  )
}

#### [+] Attack Target #########################################################
# Perfoms the attacks of one entity agaisnt another entity.  
#
# + self = the entity that is attacking (string)
# + taget = the entity that is taking damage (string)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing. 

attack_target<-function(self, target, archit, objs){
  if (!is.null(objs[[target]][['fighter']])){
    damage <- objs[[self]][['fighter']]$power -
      objs[[target]][['fighter']]$shield
    if (damage > 0){
      take_damage(target, damage, archit, objs)
      log_text(paste("The", self,"hits", target, "for", damage,"damage.",
                     sep =" "), archit)
    }
  }
}

#### [+] Take Damage ###########################################################
# Deals damage to an entity. If integrity is at or below zero, evaluates the 
# death function for the entity. 
#
# + self = the entity to take damage (string)
# + damage = the damage to be removed from the integrity of the entity (numeric)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing. 

take_damage<-function(self, damage, archit, objs){
  if (damage > 0){
    objs[[self]][['fighter']]$integrity <- 
      objs[[self]][['fighter']]$integrity - damage
  }
  if (objs[[self]][['fighter']]$integrity <= 0){
    objs[[self]][['death']](self, objs)
  }
}

#### [+] Player Attack #########################################################
# Perfoms the attacks of the player agaisnt another entity.  
#
# + dx = the displacement in the x axis (numeric)
# + dy = the displacement in the y axis (numeric)
# + self = the entity that is attacking (string)
# + blocks = does the player block or not (boolean)
# + archit = architecture list (reactive list)
# + objs = object list (reactive list)
# = returns; nothing. 

player_attack<-function(dx, dy, self, blocks, archit, objs){
  obs <- data.frame(self = sapply(objs, "[[", "self"),
                    x = sapply(objs, "[[", "x"),
                    y = sapply(objs, "[[", "y"),
                    blocks = sapply(objs, "[[", "blocks")
  )
  obs <- obs[which(obs$blocks == TRUE),]
  if (any(obs$x==objs[[self]]$x+dx & obs$y==objs[[self]]$y+dy)){
    target <- obs[which(obs$x==objs[[self]]$x+dx & obs$y==objs[[self]]$y+dy),]

    target_buffer <- as.character(target$self)
    attack_target(self, target_buffer, archit, objs)
  }
  else{
    
  }
}
