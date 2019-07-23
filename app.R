##### RoveR Application ########################################################
# RoveR 
# Application ("app.R")
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields
#

#### Source Files ##############################################################

# Controllers
source("action_controller.R") # need to fix targetting on bursts
source("architecture_controller.R")
source("behaviour_controller.R")
source("combat_controller.R")
source("key_controller.R")
source("movement_controller.R")
source("turn_controller.R")

#replace
source("target_drafting.R")
#TODO: Place inanother source file. Tidy.

# Generators
source("planet_generator.R")
source("station_generator.R")
#TODO: Document and tidy. Improve generation.

# User Interfacers
source("user_interfacer.R")
source("tile_plotter.R")
#TODO: Document and tidy.

#Configuration
source("configuration.R") 
#TODO: Document and tidy. Should Javascript and html scripts be in .R?

#### Packages ##################################################################
require(shiny)
require(shinyWidgets)
require(data.table)
require(raster)
require(gdistance)
require(dplyr)
require(plotly)
require(igraph)
require(profvis)
require(compiler)
enableJIT(3) 

#### Shiny UI ##################################################################
ui = fillPage(
  tags$script(js_keys()),
  setBackgroundColor("#353535"),
  span(textOutput('stat_panel'),style="color:#35B779"),
  span(textOutput('key_panel'),style="color:#FDE725"),
  plotlyOutput('plot', height = 750, width = 1400),
  span(htmlOutput('log'),style="color:white"),
  tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
  tags$head(tags$style(HTML(html_notification()))),
  tags$audio(src = "slowmotion.wav", type = "audio/wav", 
            autoplay=NA, controls = NA, style="display:none;")
)

#### Shiny server ############################################################## 
server = function(input, output, session) {

#### Initialization ############################################################
  archit <- reactiveValues(entities = list(),
                            actions = list(),
                          inventory = list(),
                             hidden = list(),
                                log = list(),
                              state = list(),
                              world = list(),
                            station = list()
                           )

#### Temporary game loading ####################################################
  
  ### v delete this. Sigh.

  level_temp <- edge_doors(planet_tilesetter(create_planet()))
  archit[['world']] <- level_temp
  level_temp <- level_temp[which(level_temp$x <51 & level_temp$y <51),]
  acceptable_tiles <- expand.grid(x=min(level_temp$x):max(level_temp$x), 
                                  y=min(level_temp$y):max(level_temp$y))
  level_tiles <- data.frame(x = level_temp$x,
                            y = level_temp$y)
  acceptable_tiles <- setdiff(acceptable_tiles,level_tiles)
  entities_temp <- read.csv('entities_example.csv',
           header = TRUE,
           stringsAsFactors = FALSE)
  acceptable_tiles <- sample_n(acceptable_tiles, length(entities_temp$x), replace =TRUE)
  entities_temp$x <- acceptable_tiles$x
  entities_temp$y <- acceptable_tiles$y
  
  ### ^ delete this. Sigh.

  archit[['state']] <- 'player_movement'
  archit[['log']] <- list(log_text5 = "...", 
                          log_text4 = "...",
                          log_text3 = "...")
  archit[["entities"]] <- as.data.table(rbind(entities_temp,
                                              level_temp))
   archit[["inventory"]] <- as.data.table(read.csv('items_example.csv', 
                                                    header=TRUE,
                                                    stringsAsFactors=FALSE))
  archit[["action"]] <- as.data.table(read.csv('actions_example.csv', 
                                                header=TRUE,
                                                stringsAsFactors=FALSE))

  ####  Plot screen ###############################################################
  output$plot = renderPlotly({
    plot_buffer <- plot_ly() %>%  config(displayModeBar = F) 
    if (archit[['state']] == 'player_movement' |
        archit[['state']] == 'entity_turn' |
        archit[['state']] == 1 ){ # move state 1 down to player actions 
      plot_buffer <- plot_tiles(plot_buffer, archit)
      plot_buffer <- plot_grid(plot_buffer, archit)
    }
    else if (archit[['state']] == 'player_menu' |
             archit[['state']] == 'drop_menu'){
      plot_buffer <- plot_inventory(plot_buffer, archit)
      plot_buffer <- menu_grid(plot_buffer, archit)
    }
    else if (archit[['state']] == 'player_action'|
             archit[['state']] == 2 |
             archit[['state']] == 3 ){
      plot_buffer <- plot_tiles2(plot_buffer, archit)
      plot_buffer <- plot_grid(plot_buffer, archit)
      plot_buffer <- plot_overlay(plot_buffer, archit)
    }
 })

#### Temporary user interface #################################################
  output$stat_panel = renderText({
    panel_stats(archit)
  })
  output$key_panel = renderText({
    panel_key(archit)
  })
  output$log = renderUI({
    HTML(panel_log(archit))
  })

#### Game loop #################################################################
  observeEvent(input$key, {
    subset_entities(half_width = 8, half_height = 5, archit = archit)
    handle_turns(input$key, archit)
    expand_entities(archit)
  })
}

#### Launch application ########################################################
app <- shinyApp(ui, server)
