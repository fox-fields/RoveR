##### RoveR Application ########################################################
# RoveR 
# Application ("app.R")
# July 2019 (RoveR version 0.4: "Prop-M")
# FoxFields

#### Source Files ##############################################################

# Controllers
source("architecture_controller.R")
source("behaviour_controller.R")
source("combat_controller.R")
source("key_controller.R")
source("movement_controller.R")
source("tile_controller.R")
source("turn_controller.R")

# Generators
source("planet_generator.R")
source("station_generator.R")

# User Interface 
source("user_interface.R")

#### Packages ##################################################################
require(shinyWidgets)
require(raster)
require(gdistance)
require(dplyr)
require(shiny)
require(plotly)
require(compiler)
require(igraph)
enableJIT(3) 

#### Shiny UI ##################################################################
  ui = fillPage(
    setBackgroundColor("#353535"),
    span(textOutput('stat_panel'),style="color:#35B779"),
    span(htmlOutput('log'),style="color:white"),
    plotlyOutput('plot', height = 800, width = 1400),
    tags$script(
      "
      var keyReg = {};
      $(document).keydown(function(e) {
      Plotly.animate(plot);
       if (keyReg [e.which] === false) return;
       keyReg [e.which] = false;
        Shiny.onInputChange('key', e.which,{priority: 'event'});
      });
      $(document).keyup(function(e) { 
       keyReg [e.which] = true;
      });
  
      $(function(){
        setInterval(animationFunction, 1000);
      });
      function animationFunction() {
        Plotly.animate(plot);
      }
      "
    ),
    tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
              height: 350px;
              width: 800px;
              position:fixed;
              text-align:center;
              font-size: 40px !important;
              font-style: bold !important;
              top: calc(50% - 150px);;
              left: calc(50% - 400px);;
             }
             "
        )
      )
    )
  )

#### Shiny server ############################################################## 
server = function(input, output, session) {
  #### Temporary loading screen
  showNotification(
    "⊐═══════⊏🆁🅾🆅🅴🆁⊐══════⊏
    ⋄⋄⋄⋄⋄⋄⋄⋄⋄⋄ⓕⓞⓧⓕⓘⓔⓛⓓⓢ⋄⋄⋄⋄⋄⋄⋄⋄⋄⋄
    ⋄⋄Your battery is low and it’s getting dark⋄⋄
    ⋄⋄Performance is bad. Kill the enemies⋄⋄
    ⋄<Close window to start; WASD to move>⋄
    ⊐════════════⊏",
    duration = NA, closeButton = T,
  )
  
#### Initialization ############################################################
 
  objs <- reactiveValues()
  archit <- reactiveValues(cells = list(),
                           grid = list(),
                           log = list(log_text5 = "...", 
                                      log_text4 = "...",
                                      log_text3 = "...")
                           )
  state <- reactiveValues(turn = list())
  state[['turn']] <- 'player_turn'
  
#### Level loading #############################################################

  level_temp<-temp_level() # dungeon
  archit[["cells"]] <- data.frame(x=level_temp$x, 
                                  y=level_temp$y, 
                                  char ="▣", 
                                  color = "#FDE725",
                                  size = 1.4,
                                  blocks = TRUE)
  
  acceptable_tiles <- expand.grid(x=min(level_temp$x):max(level_temp$x), 
                                  y=min(level_temp$y):max(level_temp$y))
  acceptable_tiles <- setdiff(acceptable_tiles,level_temp)
  
  #archit[["cells"]]  <- planet_tilesetter(create_planet())
  preload_entities('entities_example.csv',objs, acceptable_tiles )

#### Plot screen ############################################################### 
   output$plot = renderPlotly({
     plot_buffer <- plot_ly() %>%  config(displayModeBar = F)
     plot_buffer <- plot_tiles(plot_buffer, archit, objs)
     plot_buffer <- plot_grid(plot_buffer, objs)
  })

#### User Interface ############################################################ 
  output$stat_panel = renderText({
    panel_stats(objs)
  })
  
  output$log = renderUI({
    HTML(panel_log(archit))
  })
  
#### Game loop #################################################################
    observeEvent(input$key, {
      
      handle_turn(input$key, state, archit, objs)
      
    })
    
  }

#### Launch application ########################################################
app<-shinyApp(ui,server)


