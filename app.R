##### RoveR ####################################################################
# RoveR
# Engine ("engine.R")
# June 2019
# FoxFields

#### Source files ##############################################################
source("entities_controller.R")
source("key_controller.R")
source("movement_controller.R")
source("tile_controller.R")

#### Packages ##################################################################
require(shinyWidgets)
require(shiny)
require(plotly)
library(compiler)
enableJIT(3) 

#### Shiny UI ##################################################################
  ui = fillPage(
    setBackgroundColor("#353535"),
    plotlyOutput('plot', height = 900, width = 1400),
    tags$script(
      "$(document).on('keypress', function (e) {
      Shiny.onInputChange('key', e.which,{priority: 'event'});
      });
      "
    ),
    tags$style(type="text/css", ".recalculating {opacity: 1.0;}") 
  )

#### Shiny server ############################################################## 
  server = function(input, output, session) {
    
#### Initialization ############################################################
    objs <- reactiveValues()
    create_entity(
      self = 'player',
      x = 1,
      y = 1,
      char = "@",
      color = "#353535",
      blocks = TRUE,
      move = move_entity,
      attack = NULL,
      fighter = NULL,
      ai = NULL,
      death = NULL,
      objs=objs
      )

#### Game loop #################################################################
    observeEvent(input$key, {
      movement_keys(input$key, objs)
      
#### Plot area ################################################################# 
      output$plot = renderPlotly({
        
       plot_buffer <- plot_ly() %>% 
                      config(displayModeBar = F)
       
       plot_buffer <- plot_grid(plot_buffer)
                         
       plot_buffer <- plot_entities(plot_buffer,
                                    font_size = 0.5,
                                    align = 'center',
                                    objs)
       
      plot_buffer
    })
  })
  }

app<-shinyApp(ui,server)

#runApp(app)
