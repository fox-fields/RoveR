#### Architecture controller ###################################################
# RoveR
# Architecture Functions ("architecture_controller.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields
#
# Functions that handles the game architecture.
# Contents:
# [+] Create Entity: create_entity(self, x, y, char, color, blocks, sight, move, 
#                               attack, fighter, ai, death, objs)
# [+] Preload Entities: peload_entities(entities_file, objs)

#### [+] Create Entity #########################################################
# Creates an object as a reactive values list. 
#  
# + self = name of object (string)
# + x = starting x position of object (numeric)
# + y = starting y position of object (numeric)
# + char = character representing object (string)
# + color = color representing the object (string)
# + blocks = does it prevent other objects from passing through it? (boolean)
# + sight = does it prevent the passage of light (not implemented)
# + move = holds the move function for the object (function)
# + attack = holds the attack function for entity (not implemented)
# + fighter = holds the fighter information (not implemented)
# + death = death function (not implemented)
# + objs = object list (reactive list)
# = returns; nothing. 

create_entity <- function(self,
                          x,
                          y,
                          char,
                          color,
                          blocks,
                          move,
                          attack,
                          behaviour,
                          integrity,
                          energy,
                          shield,
                          power,
                          data,
                          death,
                          objs,
                          size) {
  obj_buffer <- reactiveValues(self = self,
                               x = x,
                               y = y,
                               char = char,
                               color = color,
                               blocks = blocks,
                               move = move,
                               attack = attack,
                               behaviour = behaviour,
                               fighter = impose_combat(self = self,
                                                       integrity = integrity,
                                                       energy = energy,
                                                       shield = shield,
                                                       power = power,
                                                       data = data),
                                death = death,
                                size = size
                              )
  objs[[self]] <- obj_buffer
}

#### [+] Preload Entities ######################################################
# Preload entities from a data csv.
#
# + entities_file = path to csv file to load (string)
# + objs = object list (reactive list)
# = returns; nothing. 

preload_entities<-function(entities_file, objs, acceptabe_tiles){
  entities <- read.csv(entities_file, header = TRUE,encoding="UTF-8")

  for (i in 1:length(entities$self)){
    acceptable_tile <- sample_n(acceptabe_tiles,1)
    entities_buffer <- entities[i,]
    create_entity(
      self = as.character(entities_buffer$self),
      #x = as.numeric(entities_buffer$x),
      #y = as.numeric(entities_buffer$y),
      x = acceptable_tile$x,
      y = acceptable_tile$y,
      char = as.character(entities_buffer$char),
      color = as.character(entities_buffer$color),
      blocks = entities_buffer$blocks,
      move = get(as.character(entities_buffer$move)),
      attack = get(as.character(entities_buffer$attack)),
      behaviour = get(as.character(entities_buffer$behaviour)),
      integrity = entities_buffer$integrity,
      energy = entities_buffer$energy,
      shield = entities_buffer$shield,
      power = entities_buffer$power,
      data = entities_buffer$data,
      death = get(as.character(entities_buffer$death)),
      size = entities_buffer$size,
      objs=objs
    )
  }
}
