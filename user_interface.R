#### User Interface ############################################################
# RoveR
# User Interface Functions ("user_interface.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields
#
# Functions that display user interface
# Contents:
# [+] Movement Keys: movement_keys(key, archit, objs)
# [+] Melee Keys: melee_keys(key, archit, objs)

#### [+] Panel Stats ###########################################################
# Displays summary statistics about the player.
#
# + objs = object list (reactive list)
# = returns; summary statistics about the player (string)

panel_stats <- function(objs){
paste("\t[Integrity:", 
      objs[['player']][['fighter']]$integrity, 
      "/",
      objs[['player']][['fighter']]$max_integrity,
      "]",
      
      "\t[Energy:",
      objs[['player']][['fighter']]$energy,
      "/",
      objs[['player']][['fighter']]$max_energy,
      "]",
      
      "\t[Power:",
      objs[['player']][['fighter']]$power,
      "]",
      
      "\t[Shielding:",
      objs[['player']][['fighter']]$shield,
      "]",
      
      "\t[Data:",
      objs[['player']][['fighter']]$data,
      "]",
      sep = " "
     )
}

#### [+] Log Text ##############################################################
# Keeps a log of events.
#
# + string = new text to add to the log (string)
# + archit = architecture list (reactive list)
# = returns; nothing.


log_text <- function(string, archit){
  archit[['log']]$log_text1 <-  archit[['log']]$log_text2
  archit[['log']]$log_text2 <-  archit[['log']]$log_text3
  archit[['log']]$log_text3 <-  archit[['log']]$log_text4
  archit[['log']]$log_text4 <-  archit[['log']]$log_text5
  archit[['log']]$log_text5 <-  string
}

#### [+] Panel Log #############################################################
# Displays a log of events.
#
# + archit = architecture list (reactive list)
# = returns; a string of log events to display.

panel_log <- function(archit){
 paste(
  archit[['log']]$log_text5,
  archit[['log']]$log_text4,
  archit[['log']]$log_text3,
  #archit[['log']]$log_text2,
  #archit[['log']]$log_text1, 
  sep="<br/>"
  )
  
}
