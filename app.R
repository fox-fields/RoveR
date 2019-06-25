##### RoveR ####################################################################
# RoveR
# Engine ("engine.R")
# June 2019 (RoveR version 2: "Apollo Lunar Rover")
# FoxFields

#### Source files ##############################################################
source("architecture_controller.R")
source("key_controller.R")
source("movement_controller.R")
source("tile_controller.R")
source("level_controller.R")

#### Packages ##################################################################
require(shinyWidgets)
require(dplyr)
require(shiny)
require(plotly)
require(compiler)
enableJIT(3) 

#### Shiny UI ##################################################################
  ui = fillPage(
    setBackgroundColor("#353535"),
    plotlyOutput('plot', height = 900, width = 1400),
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
        setInterval(oneSecondFunction, 1000);
      });
      function oneSecondFunction() {
        Plotly.animate(plot);
      }
      "
    ),
    tags$style(type="text/css", ".recalculating {opacity: 1.0;}") 
  )

#### Shiny server ############################################################## 
server = function(input, output, session) {
    
#### Initialization ############################################################
  objs <- reactiveValues()
  archit <- reactiveValues(cells = list())
  
#### Level loading #############################################################  
  level_temp<-temp_level()
  archit[["cells"]]<-data.frame(x=level_temp$x, y=level_temp$y)
  preload_entities('entities_example.csv',objs)

#### Plot area ################################################################# 
    output$plot = renderPlotly({
      
      plot_buffer <- plot_ly() %>%  config(displayModeBar = F)
      plot_buffer <- plot_tiles(plot_buffer, archit, objs)
      plot_buffer <- plot_grid(plot_buffer, objs)

    })

#### Game loop #################################################################
    observeEvent(input$key, {
      
      movement_keys(input$key, archit, objs)
      
    })
    
  }

#### Launch application ########################################################
app<-shinyApp(ui,server)


