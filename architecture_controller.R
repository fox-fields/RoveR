#### Architecture controller ###################################################
# RoveR
# Architecture Functions ("architecture_controller.R")
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields
#
# Functions that handle the game architecture.
# Contents:
# [+] Subset Entities: subset_entities(archit)
# [+] Expand Entities: expand_entities(archit)

#### [+] Subset Entities #######################################################
#' Subset entities
#' 
#' `subset_entities()` subsets entities at the start of the game loop to include 
#' only entities that are visable in the active plot window. Entities that are 
#' not included in the subset are stored in the 'hidden' game architecture.
#'
#' @param half_width The half width of tiles to subset, centered on the player 
#'   (numeric).
#' @param half_height The half height of tiles to subset, centered on the player
#'   (numeric).
#' @param archit A data table of the game architecture (data table). 
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [expand_entities()] expands entities at the end of the game loop.

subset_entities <- function(half_width, half_height, archit = archit){
  tiles <- archit[['entities']]
  player <- tiles[name == 'player']
  archit[['hidden']] <- tiles[!(x <= (half_width + player$x) &
                                x >= (player$x - half_width) &
                                y <= (half_height + player$y) &
                                y >= (player$y - half_height))]
  archit[['entities']] <- tiles[x <= (half_width + player$x) &
                                x >= (player$x - half_width) &
                                y <= (half_height + player$y) &
                                y >= (player$y - half_height)]
}

#### [+] Expand Entities #######################################################
#' Expand entities
#' 
#' `expand_entities()` expands entities at the end of the game loop by collating
#' entities that are visable in the active plot window and hidden entities.
#' Hidden entities are stored in the 'hidden' game architecture. 
#'
#' @param archit  A data table of the game architecture (data table).
#' @return Returns nothing. 
#' @examples 
#' @seealso
#' * [subset_entities()] subsets entities at the start of the game loop.

expand_entities <- function(archit){
  archit[['entities']] <- rbindlist(list(archit[['entities']],
                                         archit[['hidden']]
                                         )
                                    )
}