#### Entities Controller #######################################################
# RoveR
# Movement Functions ("entities_controller.R")
# June 2019
# FoxFields
#
# General functions that control movement of entities
# Contents:
# + create_entity: create_entity(self, x, y, char, color, blocks, sight, move, 
#                               attack, fighter, ai, death, objs)

#### + Create Entity ###########################################################
# Creates an object as a reactive values list. 
#
# self = name of object (string)
# x = starting x position of object (numeric)
# y = starting y position of object (numeric)
# char = character representing object (string)
# color = color representing the object (string)
# blocks = does it prevent other objects from passing through it? (boolean)
# sight = does it prevent the passage of light (not implemented)
# move = holds the move function for the object (function)
# attack = holds the attacke function for entity (not implemented)
# fighter = holds the fighter information (not implemented)
# death = death function (not implemented)

create_entity <- function(self, x, y, char, color, blocks, move, attack,
                          fighter, ai,
                          death, objs) {
  obj_buffer <- reactiveValues(
    self = self,
    x = x,
    y = y,
    char = char,
    color = color,
    blocks = blocks,
    move = move,
    attack = attack,
    fighter = fighter,
    ai = ai,
    death = death
  )
  
  reactive(if (!is.null(obj_buffer[[self]][['fighter']])) {
    obj_buffer[[self]][['fighter']]$self <- self
  })
  
  reactive(if (!is.null(obj_buffer[[self]]$ai)) {
    obj_buffer[[self]][['ai']]$self <- self
  })
  
  objs[[self]] <- obj_buffer
}
