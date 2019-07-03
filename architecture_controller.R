#### Architecture controller ###################################################
# RoveR
# Architecture Functions ("architecture_controller.R")
# July 2019 (RoveR version 0.3: "Lunokhod 2")
# FoxFields
#
# Functions that handle the game architecture.
# Contents:
# [+] Create Entity: create_entity(self, x, y, char, color, blocks, sight, move, 
#                               attack, fighter, ai, death, objs)
# [+] Preload Entities: peload_entities(entities_file, objs)

#### [+] Create Entity ###########################################################
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

create_entity <- function(self, x, y, char, color, blocks, move, attack,
                          fighter, ai, death, objs, size) {
  obj_buffer <- reactiveValues(self = self,
                                  x = x,
                                  y = y,
                                  char = char,
                                  color = color,
                                  blocks = blocks,
                                  move = move,
                                  attack = attack,
                                  fighter = fighter,
                                  ai = ai,
                                  death = death,
                                  size = size
                              )

  reactive(if (!is.null(obj_buffer[[self]][['fighter']])) {
    obj_buffer[[self]][['fighter']]$self <- self
  })

  reactive(if (!is.null(obj_buffer[[self]]$ai)) {
    obj_buffer[[self]][['ai']]$self <- self
  })

  objs[[self]] <- obj_buffer
  
}

#### [+] Preload Entities ######################################################
# Preload entities from a data csv.
#
# + entities_file = path to csv file to load (string)
# + objs = object list (reactive list)
# = returns; nothing. 

preload_entities<-function(entities_file, objs){
  entities <- read.csv(entities_file, header = TRUE)
  for (i in 1:length(entities$self)){
    entities_buffer <- entities[i,]
    create_entity(
      self = as.character(entities_buffer$self),
      x = as.numeric(entities_buffer$x),
      y = as.numeric(entities_buffer$y),
      char = as.character(entities_buffer$char),
      color = as.character(entities_buffer$color),
      blocks = entities_buffer$blocks,
      move = get(as.character(entities_buffer$move)),
      attack = entities_buffer$attack,
      fighter = entities_buffer$fighter,
      ai = entities_buffer$ai,
      death = entities_buffer$death,
      size = entities_buffer$size,
      objs=objs
    )
  }
}

