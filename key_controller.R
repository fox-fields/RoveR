#### Key Controller ##############################################################
# RoveR
# Key Manager Functions ("key_controller.R")
# June 2019
# FoxFields
#
# General functions that control keyboard input.
#
# Contents:
# [+] Movement Keys: movement_keys(key, gstates, obsta, objs)

#### [+] Movement Keys #########################################################
# Keys that handle player movement.

# + key = keyboard input
# + objs = object list (reactive list)
# = returns; nothing. 

movement_keys<-function(key, objs){
    # Movement keys
    if (key == 100){
      objs[['player']][['move']](dx=1,dy=0,'player', TRUE, objs)
    }            
    if (key == 97){
      objs[['player']][['move']](dx=-1,dy=0,'player', TRUE, objs)
    }           
    if (key == 119){
      objs[['player']][['move']](dx=0,dy=1,'player', TRUE, objs)
    }     
    if (key == 115){
      objs[['player']][['move']](dx=0,dy=-1,'player', TRUE, objs)
    }
}
