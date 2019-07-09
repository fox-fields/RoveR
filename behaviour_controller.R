#### Behaviour Controller ######################################################
# RoveR
# Entity Behaviour Functions ("behaviour_contoller.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields
#
# Functions that control entity behaviour.
# Contents:
# [+] Taxis Attack: taxis_attack(self, target, archit, objs)
# [+] Passive Call: passive_call()
# [+] Kill Entity: kill_entity(self, objs)

#### [+] Taxis Attack ##########################################################
# A simple taxis and attack behaviour. The self entity moves towards target entity 
# directly until it hits an obstalce or until contact. Thereafter the entity 
# attacks if in contact.
#
# + self = the entity that is moving in response to a target (string)
# + target = the entity that is targetted by self (string; defaults to 'player') 
# + range = the range overwhich self can detect target (string)
# + archit = architecture list (reactive list)
# + objs = objects list (reactive list)
# = returns; nothing. 

taxis_attack <- function(self, target, archit, objs){
  distance_buffer <- distance_to(self, target, archit, objs)
  if (distance_buffer >= 2 & distance_buffer < 8){
    objs[[self]][['move']](self, target, TRUE, archit, objs)
  }
  else{
    if (distance_buffer < 2){
      objs[[self]][['attack']](self, target, archit, objs)
    }
  }
}

#### [+] Passive Call ##########################################################
# A passive call that accepts arguments, but does nothing.
#
# = returns; nothing. 

passive_call <- function(...){
  
}

#### [+] Kill Entity ###########################################################
# Kills an entity when the entity's integrity drops below zero. Transforms the 
# dead entity with passive call functions.
#
# + self = the entity that is moving in response to a target (string)
# + objs = objects list (reactive list)
# = returns; nothing.

kill_entity<-function(self, objs){
  objs[[self]]$char = "⑈"
  objs[[self]]$color =  "grey"
  objs[[self]]$blocks = FALSE
  objs[[self]]$move = passive_call
  objs[[self]]$attack = passive_call
  objs[[self]]$behaviour = passive_call
  objs[[self]]$fighter = NULL
  objs[[self]]$death  = NA
}



