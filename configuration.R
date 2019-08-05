js_keys <-function(){
  "
  var keyReg = {}; 
  $(document).keydown(function(e) {
    if (keyReg [e.which] === false) return;
    keyReg [e.which] = false;
    Shiny.onInputChange('key', e.which,{priority: 'event'});
  });
  $(document).keyup(function(e) { 
    keyReg [e.which] = true;
  });
Plotly.newPlot('plot', data, layout);
  "
}

js_animate<-function(){
  "
  $(function(){
    setInterval(animationFunction, 1000);
   });
   function animationFunction() {
    Plotly.animate(plot);
   }
  "
}

html_notification <- function(){
  "
  .shiny-notification {
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
}


save_game <- function(archit){
  entities <- archit[['entities']]
  actions <- archit[['actions']]
  inventory <-archit[['inventory']]
  hidden <-archit[['hidden']]
  log <-archit[['log']]
  state <-archit[['state']]
  save(entities, actions, inventory, hidden, log, state,
          file = 'saved_game.Rdata')
}

load_game <- function(archit){
  load('saved_game.Rdata')

  
   archit[['entities']] <- entities
   archit[['actions']] <- actions
   archit[['inventory']] <- inventory
   archit[['hidden']] <- hidden
   archit[['log']] <- log 
   archit[['state']] <- state 
  
}