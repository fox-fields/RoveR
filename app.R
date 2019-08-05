##### RoveR Application ########################################################
# RoveR 
# Application ("app.R")
# July 2019 (RoveR version 0.7: "Sojourner")
# FoxFields
# 

#### Source Files ##############################################################

# Controllers
source("action_controller.R") # need to fix targetting on bursts
source("architecture_controller.R") #
source("behaviour_controller.R") #
source("combat_controller.R")
source("key_controller.R") #
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
ui = fluidPage(

  tags$script(js_keys()),
  setBackgroundColor("#282828"),
  h1( 
    plotlyOutput('plot', height = 750, width = 1150),
  class = 'crt'),
  fluidRow(class='crt',
  column(4,textOutput('stat_panel'), style="color:#fadb2f; font-size: 20px;"),
  #column(3,textOutput('key_panel'), style="color:#fadb2f; font-size: 20px;"),
  column(6,
   fluidRow(
   column(1),
   column(10,htmlOutput('log'),style="color:#fadb2f; font-size: 20px;")))
  ),
  tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
  tags$audio(src = "slowmotion.wav", type = "audio/wav", 
            autoplay=NA, controls = NA, style="display:none;"),
  tags$style("

    .crt::before {
      content: ' ';
      display: block;
      position: absolute;
      top: 0;
      left: 0;
      bottom: 0;
      right: 0;
      background: linear-gradient(rgba(18, 16, 16, 0) 50%, rgba(0, 0, 0, 0.25) 50%), linear-gradient(90deg, rgba(255, 0, 0, 0.06), rgba(0, 255, 0, 0.02), rgba(0, 0, 255, 0.06));
      z-index: 2;
      background-size: 100% 3px, 1px 100%;
      pointer-events: none;
    }
.crt {
    text-shadow: 1px 1px 5px #fadb2f, 0px 0px 55px #fadb2f, 0px 0px 15px #fadb2f;
}
    }

"
  )
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
                            station = list(),
                            text = list(),
                            menu_status = list()
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

  archit[['state']] <- 'title_screen'
  archit[['menu_status']] <- 'title'
  archit[['log']] <- list(log_text5 = "...", 
                          log_text4 = "...",
                          log_text3 = "...",
                          log_text2 = "...",
                          log_text1 = "...")
  archit[["entities"]] <- as.data.table(rbind(level_temp,entities_temp))
   archit[["inventory"]] <- as.data.table(read.csv('items_example.csv', 
                                                    header=TRUE,
                                                    stringsAsFactors=FALSE))
  archit[["action"]] <- as.data.table(read.csv('actions_example.csv', 
                                                header=TRUE,
                                                stringsAsFactors=FALSE))
  archit[["text"]] <- as.data.table(read.csv('menu_text.csv', 
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
    else if (archit[['state']] == 'title_screen'){
      plot_buffer <- plot_title(plot_buffer,archit)
      plot_buffer <- menu_grid(plot_buffer, archit)
    }
    else if (archit[['state']] == 'character_select'){
      plot_buffer <- plot_character(plot_buffer,archit)
      plot_buffer <- menu_grid(plot_buffer, archit)
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
