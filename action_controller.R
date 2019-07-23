#### Action Controller #########################################################
# RoveR
# Action Functions ("action_contoller.R")
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields 
#
# Functions that contol entity actions.
#
# Contents:
# [+] Damage:damage_action(d_value, target, archit)
# [+] Repair: repair_action(r_value, self, archit)
# [+] Shield: shield_action(s_value, self, archit)
# [+] Hack: hack_action(h_value, self, target, archit)
# [+] Stall: stall_action(target, archit)
# [+] Short: short_action(target, archit)
# [+] Teleport: teleport_action(self, target, archit)
# [+] Generate: generate_action(e_value, self, archit)
# [+] Take Action: take_action(action_number, self, targets, archit)
# [+] Burst targetting: burst_targeting(action_number, key, self, archit) 

#### [+] Damage ################################################################
#' Damage action
#' 
#' `damage_action()` deals damage to a target entity. Damage is reduced by the 
#' shield value of the the target entitiy. 
#' 
#' @param d_value The amount of damage directed at the target entity (numeric).
#' @param target The entity that is being damaged (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.
#' * [take_damage()] deals damage directly to a target.

damage_action<-function(d_value, target, archit){
          entities <-  archit[['entities']]
          target <- entities[name == target]
          damage <- d_value - target$shield
        if (damage > 0){
          take_damage(target$name, damage, archit)
        }
}

#### [+] Repair ################################################################
#' Repair action
#' 
#' `repair_action()` restores integrity to an entity. Repair does not revive entities. Repair 
# cannot restore more than an entity's max integrity.  
#' 
#' @param r_value The amount of integrity to repair (numeric).
#' @param self The entity to repair (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.

repair_action<-function(r_value, self, archit){
  entities <-  archit[['entities']]
  entity <- entities[name == self]
    if ((entity$integrity + r_value) < entity$max_integrity){
      entity$integrity <- entity$integrity + r_value
    }
    else{
      entity$integrity <- entity$max_integrity
    }
   archit[['entities']][name == self] <- entity
}

#### [+] Shield ################################################################
#' Shielding action
#' 
#' `shield_action()` adds shielding to an object. Each shield value blocks one 
#' damage. Shield lasts until the end of next turn. 
#' 
#' @param s_value The amount of shielding to add (numeric).
#' @param self The entity to repair (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.

shield_action <- function(s_value, self, archit) {
  entities <-  archit[['entities']]
  entity <- entities[name == self]
  entity$shield <- entity$shield + s_value
  archit[['entities']][name == self] <- entity
}

#### [+] Hack ##################################################################
#' Hacking action
#' 
#' `Hack_action()` takes data packages from the target entity. If the target 
#' lackssufficient data packages, hack takes all the data packages from the 
#' target.
#' 
#' @param h_value The amount of data to steal (numeric).
#' @param target The entity to hack (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.

hack_action<-function(h_value, self, target, archit){
  entities <-  archit[['entities']]
  entity <- entities[name == self]
  target <- entities[name == target]
  if (target$data >= h_value){
      target$data <- target$data - h_value
      entity$data <- entity$data + h_value
    }
    else{
      entity$data <- entity$data + target$data
      target$data <- 0
    }
  archit[['entities']][name == self] <- entity
  archit[['entities']][name == target$name] <- target
}

#### [+] Stall #################################################################
#' Stall action
#' 
#' `stall_action()` prevents entities from moving for one turn. Affected 
#' entities may still use actions. If another aciton causes and entity to move, 
#' stall does not affect that action.
#' 
#' @param target The entity to stall (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.

stall_action<-function(target, archit){
  entities <-  archit[['entities']]
  entity <- entities[name == target]
  entity$stall <- TRUE
  archit[['entities']][name == target] <- entity
}

#### [+] Short #################################################################
#' Short action
#' 
#' `short_action()` allows entities to move but prevents any other action. 
#' 
#' @param target The entity to short (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.
#'

short_action<-function(target, archit){
  entities <-  archit[['entities']]
  entity <- entities[name == target]
  entity$short <- TRUE
  archit[['entities']][name == target] <- entity
}

#### [+] Teleport ##############################################################
#' Teleport action
#' 
#' `teleport_action()` teleport changes the location of the target and self 
#' entities on the map. This action can be used by stalled entity. 
#' 
#' @param self The entity to teleport (string).
#' @param target The entity that is targeted for teleporting (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.
#'

teleport_action<-function(self, target, archit){

   entities <-  archit[['entities']]
   entity <- entities[name == self]
   target <- entities[name == target[1]]

   buff_x <- entity$x
   buff_y <- entity$y
   buff2_x <- target$x
   buff2_y <- target$y
    
   target$x <- buff_x
   target$y <- buff_y
   entity$x <- buff2_x
   entity$y <- buff2_y 
   
   archit[['entities']][name == self] <- entity
   archit[['entities']][name == target$name] <- target
   
}

#### [+] Generate ##############################################################
#' Generate action
#' 
#' `generate_action()` generates energy for an entity. 
#' 
#' @param e_value The amount of energy to generate (numeric).
#' @param self The entity to regenerate (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [take_action()] enacts actions by looking up action variables in seqeuence.
#'

generate_action<-function(e_value, self, archit){
  entities <-  archit[['entities']]
  entity <- entities[name == self]
  entity$energy <- entity$energy + e_value
  archit[['entities']][name == self] <- entity
}

#### [+] Take Action ###########################################################
#' Perform action
#' 
#' `take_action()` performs an action.
#' 
#' @param action_number The numeric value of the action (numeric).
#' @param self The entity that is acting (string).
#' @param targets The targets of the action (string)
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [repair_action()] restores integrity to an entity.
#' * [damage_action()] deals damage to a target entity. 
#' * [shield_action()] adds shielding to an object.
#' * [hack_action()] takes data packages from the target entity.
#' * [stall_action()] prevents entities from moving for one turn.
#' * [short_action()] allows entities to move but prevents any other action. 
#' * [teleport_action()] teleport changes the location of the target and self.
#' * [generate_action()] generates energy for an entity. 

take_action<-function(action_number, self, targets, archit){
  action <-  archit[['action']][action_number,]
  entities <-  archit[['entities']]
  entity <- entities[name == self]
  targets<-targets[targets!='player']
  if (entity$energy > action$cost & entity$short == FALSE) {
    entity$energy <- entity$energy - action$cost
    archit[['entities']][name == self] <- entity
    if (action$repair > 0){
      repair_action(action$repair, 
                    self,
                    archit)
    }
    if (action$shield > 0){
      shield_action(action$shield, 
                    self,
                    archit)
    }
    if (action$generate > 0 ){
      generate_action(action$generate,
                      self,
                      archit)
    } 
    
    if (length(targets) >= 1){
    if (action$teleport > 0 ){
      teleport_action(self, 
                      targets,
                      archit)
    }
    for (i in 1:length(targets)){
      if (action$hack > 0 ){
        hack_action(action$hack,
                    self,
                    targets[i],
                    archit)
      }
      if (action$short > 0 ){
        short_action(targets[i],
                    archit)
      }
      if (action$stall > 0 ){
        stall_action(targets[i],
                    archit)
      }  
      if (action$damage > 0){
        damage_action(action$damage, 
                      targets[i],
                      archit)
      }
    }
    }
  }
}

#### [+] Burst Targeting #######################################################
#' Burst Targeting
#' 
#' `burst_targeting()` targeting for an action targets all entities within a 
#' radius of squares measued outward from the acting entity. The acting entity 
#' is not negatively affected by burst.
#' 
#' @param action_number The numeric value of the action (numeric).
#' @param key The keyboard input (numeric).
#' @param self The acting entity (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [hit_targeting()] restores integrity to an entity.
#' * [pierce_targeting()] deals damage to a target entity. 

burst_targeting <- function(action_number, key, self, archit){
  action <- archit[['action']][action_number,]
  entities <- archit[['entities']]
  entity <- entities[name == self]
  range_b <- action$range
  targets <- entities[sqrt((entity$x - x)^2 + (entity$y - y)^2) < range_b &
                        blocks == TRUE &
                        name != self &
                        class != 'door']
  if (!is.null(targets)){
  targets$char <- "◼"
  targets$color <- "#287C8E50"
  targets$blocks <- FALSE
  targets$class <- 'overlay'
  targets$size <- 1.7
  
  archit[['entities']]<-rbindlist(list(archit[['entities']], targets))
  
  if (key == 13){ # 13 (keydown = enter)
    archit[['entities']] <- archit[['entities']][class != 'overlay']
      if (length(targets >= 1)){
        take_action(action_number, self, targets$name, archit)
      }
    archit[['state']] <- 'entity_turn'
  }
  }
  if (key == 57){ # 192 (keydown = `)
    archit[['entities']] <- archit[['entities']][class != 'overlay']
    archit[['state']] <- 'player_movement'
  }
}


#### [+] Pierce Targeting #######################################################
#' Pierce Targeting
#' 
#' `pierce_targeting()` targeting that affects every entity in a beeline from
#' the player to the target.
#' 
#' @param action_number The numeric value of the action (numeric).
#' @param key The keyboard input (numeric).
#' @param self The acting entity (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [burst_targeting()] restores integrity to an entity.
#' * [hit_targeting()] deals damage to a target entity. 

pierce_targeting <- function(action_number, key, self, archit){
  hover_buffer <- event_data("plotly_hover")
  player <- archit[['entities']][name == 'player']
  if (is.null(hover_buffer)){hover_buffer <- player}
  targets <- select_path(hover_buffer,player)
  targets <- archit[['entities']][paste(x,y) %in% paste(targets$x,targets$y)]
  targets <- targets[blocks == TRUE & class != 'door']
  if (key == 13){ # 13 (keydown = enter)
    if (length(targets >= 1)){
      take_action(action_number, self, targets$name, archit)
    }
    archit[['state']] <- 'entity_turn'
  }
  if (key == 57){ # 192 (keydown = `)
    archit[['state']] <- 'player_movement'
  }
}


#### [+] Hit Targeting #######################################################
#' Hit Targeting
#' 
#' `hit_targeting()` targeting that affects the first entity in a beeline from
#'  player to the target.
#' 
#' @param action_number The numeric value of the action (numeric).
#' @param key The keyboard input (numeric).
#' @param self The acting entity (string).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [burst_targeting()] restores integrity to an entity.
#' * [pierce_targeting()] deals damage to a target entity. 

hit_targeting <- function(action_number, key, self, archit){
  hover_buffer <- event_data("plotly_hover")
  player <- archit[['entities']][name == 'player']
  if (is.null(hover_buffer)){hover_buffer <- player}
  targets <- select_path(hover_buffer,player)
  targets <- archit[['entities']][paste(x,y) %in% paste(targets$x,targets$y)]

  if (key == 13){ # 13 (keydown = enter)
    if (length(targets >= 1)){
      targets <- targets[blocks == TRUE & name != 'player' & class != 'door']
      targets$d <- sqrt((targets$x - player$x)^2 + (targets$y - player$y)^2)
      targets <- targets[targets$d == min(targets$d)]
      take_action(action_number, self, targets$name, archit)
    }
    archit[['state']] <- 'entity_turn'
  }
  if (key == 57){ # 192 (keydown = `)
    archit[['state']] <- 'player_movement'
  }
}

