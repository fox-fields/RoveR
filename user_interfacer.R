#### User Interfacer ############################################################
# RoveR
# User Interface Functions ("user_interfacer.R")
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields
#
# Functions that display user interface
# Contents:
# [+] Panel Stats: panel_stats(archit)
# [+] Log Text: log_text(string, archit)
# [+] Panel Log: panel_log(archit)
# [+] Panel Inventory: panel_inventory(archit)

#### [+] Panel Stats ###########################################################
# Displays summary statistics about the player.
#
# + archit = architecture list (reactive list)
# = returns; summary statistics about the player (string)

panel_stats <- function(archit){
  tiles <- archit[['entities']]
  player <- tiles[which(tiles$name == 'player'),]
  if (archit[['state']] == 'title_screen'){
    paste("")
  }
  else{
  paste(
      "[Integrity:", 
      player$integrity, 
      "/",
      player$max_integrity,
      "]",
      
      "\t[Energy:",
      player$energy,
      "/",
      player$max_energy,
      "] ",
      
      "----[Power:",
      player$power,
      "]",
      
      "\t[Shielding:",
      player$shield,
      "]",
      
      "\t[Data:",
      player$data,
      "]",
      sep = " "
     )
}
  
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
  if (archit[['state']] == 'title_screen'){
    paste("")
  }
  else{
 paste(
  archit[['log']]$log_text5,
  archit[['log']]$log_text4,
  archit[['log']]$log_text3,
  sep="<br/>"
  )
  }
}


#### [+] Panel Inventory #######################################################
# Displays a inventory of items
#
# + archit = architecture list (reactive list)
# = returns; a string of log events to display.

panel_inventory <- function(archit){
  paste(
    archit[['inventory']],
    sep="<br/>"
  )
  
}


#### [+] Panel Key ###########################################################
# Displays summary statistics about the player.
#
# + archit = architecture list (reactive list)
# = returns; summary statistics about the player (string)

panel_key <- function(archit){
  if (archit[['state']] == 'title_screen'){
    paste("")
  }
  else{
 paste( "Controls:",
        "[WASD - Movement]",
        "[Mouse - Aim Targeting]",
        "[1 - Module 1]",
        "[2 - Module 2]",
        "[3 - Module 3]",
        "[Enter - Attack with Targeting]",
        "[8 - Inventory]",
        "[9 - Exit Inventory/Targeting]",
        sep = " "
  )
}
}


