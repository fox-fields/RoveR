#### Tile Plotter ###########################################################
# RoveR
# Tile Manager Functions ("tile_plotter.R")
# July 2019 (RoveR version 0.6: "Marsokhod")
# FoxFields
#
# Functions that control the plotting of tiles.
# Contents:
# [+] Plot Tiles: plot_tiles(plot_buffer, archit)
# [+] Plot Grid: plot_grid(plot_buffer, archit)

#### [+] Plot Tiles ############################################################
# Plot all tiles on the main screen.
#
# + plot_buffer = the plot object (plot_ly plot)
# + archit = architecture lists (reactive list)
# + objs = objects list (reactive list)
# = returns an updated plot object (plot_ly plot)

plot_tiles <- function(plot_buffer, archit) {
    tiles <- archit[['entities']]
    player <- tiles[name == 'player']
    tiles<-tiles[x <= (9 + player$x)][x >= (player$x - 9)][y <= (6 + player$y)][y >= (player$y -6 )]
    tiles[x == player$x &
          y == player$y & 
          name != 'cursor'] <- player
    add_trace(
      name = "Entities",
      plot_buffer,
      x = tiles$x,
      y = tiles$y,
      type = 'scatter',
      mode = 'text',
      text = paste("<span style ='text-shadow: 1px 1px 5px ",
               tiles$color,
            ", 0px 0px 55px ",
            tiles$color,
            ", 0px 0px 15px ",
            tiles$color,
              ";'>",
            tiles$char,
            "</span>",
            sep=""),

      textposition = "center",
      hovertemplate = paste("<b>",
                            tiles$name,
                            "</b><br><br>",
                            "Integrity:",
                            tiles$integrity,
                            "/",
                            tiles$max_integrity,
                            "<br>",
                            "Power:",
                            tiles$power,
                            "<br><br><i>",
                            tiles$description,
                            "</i>"),
      textfont = list(color = tiles$color,
                      size = tiles$size * 70)
    )
}

#### [+] Plot Grid #########################################################
# Plot the grid lines and backgound color. Sets x and y axes limits. 
#
# + plot_buffer = the plot object (plot_ly plot)
# = returns an updated plot object (plot_ly plot)

plot_grid <- function (plot_buffer, archit){
  tiles <- archit[['entities']]
  player <- tiles[name == 'player']
  layout(
    plot_buffer,
    showlegend= FALSE,
    dragmode=FALSE,
    xaxis = list(
      range = c(player$x - 5, player$x + 5),
      dtick =0.1,
      tick0 = 0.5,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      gridcolor = "#282828",
      gridwidth = 2
    ),
    yaxis = list(
      range = c(player$y - 5, player$y + 5),
      dtick = 0.1,
      tick0 = 0.65,
      scaleanchor = "x",
      zeroline = FALSE,
      gridcolor = "#282828",
      gridwidth = 2,
      showline = FALSE,
      showticklabels = FALSE
    ),
    paper_bgcolor = "#282828",
    plot_bgcolor = "#282828"
  )
}


#### [+] Plot Inventory ############################################################
# Plot Inventory on the main screen.
#
# + plot_buffer = the plot object (plot_ly plot)
# + archit = architecture lists (reactive list)
# = returns an updated plot object (plot_ly plot)

plot_inventory <- function(plot_buffer, archit) {
  invent <- archit[['inventory']]
  if (archit[['state']] == 'drop_menu'){ wurd <- "[Drop Item]"}
  if (archit[['state']] == 'player_menu'){ wurd <- "[Use Item]"}  
  add_trace(
    name = "Inventory",
    plot_buffer,
    x = -3,
    y = (-(1:(length(invent$char)+2))+19)/2,
    type = 'scatter',
    mode = 'text',
    text = c("Inventory - [8 - Use][7 - Drop][9 - Exit]", 
             wurd,
             paste(head(c('[a] ','[b] ','[c] ','[d] ','[e] ','[f] ','[g] ','[h] ', '[i] ','[j] ','[k] ','[l] '), length(invent$char)),
                 invent$char, 
                 invent$name,
                 sep=" - ")),
    textposition = "right",
    hovertemplate = c("okie","okie",as.character(invent$char)),
    textfont = list(color = c("white","#FDE725",invent$color),
                    size = 30)
  )
}

#### [+] Menu Grid #########################################################
# Plot the grid lines and backgound color. Sets x and y axes limits. 
#
# + plot_buffer = the plot object (plot_ly plot)
# = returns an updated plot object (plot_ly plot)

menu_grid <- function (plot_buffer, archit){
  layout(
    plot_buffer,
    showlegend= FALSE,
    dragmode=FALSE,
    xaxis = list(
      range = c(0, 10),
      dtick = 0.1,
      tick0 = 0.5,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      gridcolor = "#282828",
      gridwidth = 2
    ),
    yaxis = list(
      range = c(0, 10),
      dtick = 0.1,
      tick0 = 0.65,
      scaleanchor = "x",
      zeroline = FALSE,
      gridcolor = "#282828",
      gridwidth = 2,
      showline = FALSE,
      showticklabels = FALSE
    ),
    paper_bgcolor = "#282828",
    plot_bgcolor = "#282828"
  )
}

#### [+] Plot Overlay ############################################################
# Plot all tiles on the main screen.
#
# + plot_buffer = the plot object (plot_ly plot)
# + archit = architecture lists (reactive list)
# + objs = objects list (reactive list)
# = returns an updated plot object (plot_ly plot)

plot_overlay <- function(plot_buffer, archit) {
  hover_buffer <- event_data("plotly_hover")
  player <- archit[['entities']][name == 'player']
  if (is.null(hover_buffer)){hover_buffer  <- player}
  hover_buffer <- select_path(hover_buffer,player)
  col_temp <- c(rep("#287C8E", (length(hover_buffer$x)-1)), "#8FD744")
  add_trace(
    name = "Overlay",
    plot_buffer,
    x = hover_buffer$x,
    y = hover_buffer$y-0.12,
    type = 'scatter',
    mode = 'text',
    text = paste("<span style ='text-shadow: 1px 1px 5px ",
                 col_temp,
                 ", 0px 0px 55px ",
                 col_temp,
                 ", 0px 0px 15px ",
                 col_temp,
                 ";'>",
                 "■",
                 "</span>",
                 sep=""),
    
    opacity = 0.5,
    textposition = "center",
    hoverinfo = "none",
    textfont = list(color = col_temp,
                    size =  120,
                    layer = 'below')
  )
}


#### [+] Plot Tiles ############################################################
# Plot all tiles on the main screen.
#
# + plot_buffer = the plot object (plot_ly plot)
# + archit = architecture lists (reactive list)
# + objs = objects list (reactive list)
# = returns an updated plot object (plot_ly plot)

plot_tiles2 <- function(plot_buffer, archit) {
  tiles <- archit[['entities']]
  player <- tiles[name == 'player']
  tiles<-tiles[x <= (9 + player$x)][x >= (player$x - 9)][y <= (6 + player$y)][y >= (player$y -6 )]
  tiles[x == player$x &
          y == player$y & 
          name != 'cursor'] <- player
  add_trace(
    name = "Entities",
    plot_buffer,
    x = tiles$x,
    y = tiles$y,
    type = 'scatter',
    mode = 'text',
    text =  paste("<span style ='text-shadow: 1px 1px 5px ",
                  tiles$color,
                  ", 0px 0px 55px ",
                  tiles$color,
                  ", 0px 0px 15px ",
                  tiles$color,
                  ";'>",
                  tiles$char,
                  "</span>",
                  sep=""),
    
    textposition = "center",
    hovertemplate = NULL,
    textfont = list(color = tiles$color,
                    size = tiles$size * 70)
  )
}



plot_sys<- function(plot_buffer, archit) {
  tiles <- archit[['planets']]

  add_trace(
    name = "Planets",
    plot_buffer,
    x = tiles$x,
    y = tiles$y,
    type = 'scatter',
    mode = 'text',
    text = tiles$char, 
    textposition = "center",
    hovertemplate = NULL,
    textfont = list(color = "green",
                    size = tiles$size * 70)
  )
}


plot_title<- function(plot_buffer, archit){
  hover_buffer <- event_data("plotly_hover")
  click_buffer <- event_data("plotly_click")
  if (is.null(hover_buffer)){hover_buffer  <- data.frame(x = 30, y = 30)}
  if (is.null(click_buffer)){click_buffer  <- data.frame(x = 30, y = 30)}

  menu_status <- archit[["menu_status"]]
  action_buffer <- archit[["action"]]
  if (length(action_buffer$color) == 3){
    pos_buffer_x <-  c(5,5,5)
    pos_buffer_y <- c(4.5,3,1.5)
  }
  if (length(action_buffer$color) == 2){
    pos_buffer_x <-  c(5,5)
    pos_buffer_y <- c(4.5,3)
  }
 
  displayed_text <- archit[["text"]][menu == menu_status]
  if (menu_status == 'poly_class' | 
      menu_status == 'poly_class2'|
      menu_status == 'alum_class' | 
      menu_status == 'alum_class2'|
      menu_status == 'tita_class' | 
      menu_status == 'tita_class2'
      ){
    
    action_buffer <- data.frame(line = action_buffer$name,
                                text = paste(
                                  "<span style='color: ",
                                  action_buffer$color,
                                  "; font-size: 100px;'>",
                                  "<sub>",
                                  action_buffer$char,
                                  " </sub>",
                                  "</span>",
                                  "<span style='font-size: 30px; color: #ebdbb2",
                                  "; text-shadow: 1px 1px 5px #ebdbb2, 0px 0px 55px #ebdbb2, 0px 0px 15px #ebdbb2;'>",
                                  "<sub>", 
                                  action_buffer$title,
                                  "</sub>",
                                  "</span>",
                                  "<br>",
                                  "<sup>",
                                  "<span style ='font-size: 30px'>",
                                  "<span>",
                                  "¤<sub>",
                                  action_buffer$cost,
                                  "</sub>",
                                  "</sup>",
                                  "<span style = 'font-size: 30px'>",
                                  "<span>",
                                  "⦻   </span>",
                                  "</span>",
                                  "</span>",
                                  
                                  
                                  "<span style = 'font-size: 30px'>",
                                  
                                  "<span style='color: #8ec07c; opacity:",
                                  action_buffer$range + 0.2,
                                  "; text-shadow: 1px 1px 5px #8ec07c, 0px 0px 55px #8ec07c, 0px 0px 15px #8ec07c;'>◱<sub>",
                                  action_buffer$range,
                                  "</sub></span>",
                                  
                                  "<span style='color: #fb4934; opacity:",
                                  action_buffer$damage + 0.2,
                                  "; text-shadow: 1px 1px 5px #fb4934, 0px 0px 55px #fb4934, 0px 0px 15px #fb4934;'>✷<sub>",
                                  action_buffer$damage,
                                  "</sub></span>",
                                  
                                  "<span style='color: #fabd2f; opacity:",
                                  action_buffer$shield + 0.2,
                                  "; text-shadow: 1px 1px 5px #fabd2f, 0px 0px 55px #fabd2f, 0px 0px 15px #fabd2f;'>⬢<sub>",
                                  action_buffer$shield,
                                  "</sub></span>",
                                  
                                  "<span style='color: #d3869b; opacity:",
                                  action_buffer$repair + 0.2,
                                  "; text-shadow: 1px 1px 5px #d3869b, 0px 0px 55px #d3869b, 0px 0px 15px #d3869b;'>⪿<sub>",
                                  action_buffer$repair,
                                  "</sub></span>",
                                  
                                  "<span style='color: #b8bb26; opacity:",
                                  action_buffer$generate + 0.2,
                                  "; text-shadow: 1px 1px 5px #b8bb26, 0px 0px 55px #b8bb26, 0px 0px 15px #b8bb26;'>⏦<sub>",
                                  action_buffer$generate,
                                  "</sub></span>",
                                  
                                  "<span style='color: #ebdbb2; opacity:",
                                  action_buffer$stall + 0.2,
                                  "; text-shadow: 1px 1px 5px #ebdbb2, 0px 0px 55px #ebdbb2, 0px 0px 15px #ebdbb2;'>⧗<sub>",
                                  action_buffer$stall,
                                  "</sub></span>",
                                  
                                  "<span style='color: #fe8019; opacity:",
                                  action_buffer$short + 0.2,
                                  "; text-shadow: 1px 1px 5px #fe8019, 0px 0px 55px #fe8019, 0px 0px 15px #fe8019;'>↯<sub>",
                                  action_buffer$short,
                                  "</sub></span>",
                                  
                                  "<span style='color: #8ec07c; opacity:",
                                  action_buffer$teleport + 0.2,
                                  "; text-shadow: 1px 1px 5px #8ec07c, 0px 0px 55px #8ec07c, 0px 0px 15px #8ec07c;'>↹<sub>",
                                  action_buffer$teleport,
                                  "</sub></span>",
                                  
                                  "<span style='color: #fb4934; opacity:",
                                  action_buffer$hack + 0.2,
                                  "; text-shadow: 1px 1px 5px #fb4934, 0px 0px 55px #fb4934, 0px 0px 15px #fb4934;'>⎆<sub>",
                                  action_buffer$hack,
                                  "</sub></span>",
                                  
                                  
                                  
                                  "   ",
                                  
                                  sep = ""),
                                size = 50,
                                x = pos_buffer_x,
                                y = pos_buffer_y,
                                color =  action_buffer$color,
                                hover_color =  action_buffer$color,
                                link = NA,
                                menu = menu_status,
                                function_call = "passive_call",
                                align = 'right',
                                stringsAsFactors = FALSE
    )
    
    
    displayed_text <- rbindlist(list(displayed_text, action_buffer))
    displayed_text <- displayed_text[menu == menu_status]
    
  }
      

  
 displayed_text$color_display <-ifelse(displayed_text$y == hover_buffer$y &
                                       displayed_text$x == hover_buffer$x, 
                                       displayed_text$hover_color,
                                       displayed_text$color
  )

  if (any(displayed_text$y == click_buffer$y &
          displayed_text$x == click_buffer$x)){
    menu_status <- displayed_text$link[displayed_text$y == click_buffer$y &
                                      displayed_text$x == click_buffer$x]
    function_call <- get(as.character(displayed_text$function_call[displayed_text$y == click_buffer$y &
                          displayed_text$x == click_buffer$x])
                         )
   

      
    archit[["menu_status"]] <- as.character(menu_status[1])
    function_call(archit)
    archit[["click_test"]] <- click_buffer
  }

  plot_buffer <- plot_ly() %>%  
  add_trace(
    name = "Title",
    x = displayed_text$x,
    y = displayed_text$y,
    type = 'scatter',
    mode = 'text',
    hoverinfo = 'skip',
    opacity = 50,
    text = paste("<span style ='text-shadow: 1px 1px 5px ",
                 displayed_text$color_display,
                 ", 0px 0px 55px ",
                 displayed_text$color_display,
                 ", 0px 0px 15px ",
                 displayed_text$color_display,
                 ";'>",
                 displayed_text$text,
                 "</span>",
                 sep=""),
    textposition = displayed_text$align,
    hovertemplate = NULL,
    textfont = list(color = displayed_text$color_display,
                    size = displayed_text$size)
  ) %>%
  config(displayModeBar = F) %>%
  menu_grid("")
  plot_buffer
}

slot_one_b <- function(archit){
  buffer <- archit[["action"]]
  hold <- as.data.table(read.csv('actions_table.csv', header=TRUE, stringsAsFactors = FALSE))
  buffer_index <- buffer$num[1] - 1 
  if (buffer_index == 0){
    buffer_index <- length(hold$id)
  }
  buffer[1,] <- hold[buffer_index,]
  if (archit[["menu_status"]] == 'poly_class'){
  archit[["menu_status"]] <- 'poly_class2'
  }
  else if (archit[["menu_status"]] == 'poly_class2'){
    archit[["menu_status"]] <- 'poly_class'
  }
  else if (archit[["menu_status"]] == 'alum_class'){
    archit[["menu_status"]] <- 'alum_class2'
  }
  else if (archit[["menu_status"]] == 'alum_class2'){
    archit[["menu_status"]] <- 'alum_class'
  }
  else if (archit[["menu_status"]] == 'tita_class'){
    archit[["menu_status"]] <- 'tita_class2'
  }
  else if (archit[["menu_status"]] == 'tita_class2'){
    archit[["menu_status"]] <- 'tita_class'
  }
  archit[["action"]] <- buffer
}

slot_one_f <- function(archit){
  buffer <- archit[["action"]]
  hold <- as.data.table(read.csv('actions_table.csv', header=TRUE, stringsAsFactors = FALSE))
  buffer_index <- buffer$num[1] + 1 
  if (buffer_index == 90){
    buffer_index <- 1
  }
  buffer[1,] <- hold[buffer_index,]
  if (archit[["menu_status"]] == 'poly_class'){
    archit[["menu_status"]] <- 'poly_class2'
  }
  else if (archit[["menu_status"]] == 'poly_class2'){
    archit[["menu_status"]] <- 'poly_class'
  }
  else if (archit[["menu_status"]] == 'alum_class'){
    archit[["menu_status"]] <- 'alum_class2'
  }
  else if (archit[["menu_status"]] == 'alum_class2'){
    archit[["menu_status"]] <- 'alum_class'
  }
  else if (archit[["menu_status"]] == 'tita_class'){
    archit[["menu_status"]] <- 'tita_class2'
  }
  else if (archit[["menu_status"]] == 'tita_class2'){
    archit[["menu_status"]] <- 'tita_class'
  }
  archit[["action"]] <- buffer
}
slot_two_b <- function(archit){
  buffer <- archit[["action"]]
  hold <- as.data.table(read.csv('actions_table.csv', header=TRUE, stringsAsFactors = FALSE))
  buffer_index <- buffer$num[2] - 1 
  if (buffer_index == 0){
    buffer_index <- length(hold$id)
  }
  buffer[2,] <- hold[buffer_index,]
  if (archit[["menu_status"]] == 'poly_class'){
    archit[["menu_status"]] <- 'poly_class2'
  }
  else if (archit[["menu_status"]] == 'poly_class2'){
    archit[["menu_status"]] <- 'poly_class'
  }
  else if (archit[["menu_status"]] == 'alum_class'){
    archit[["menu_status"]] <- 'alum_class2'
  }
  else if (archit[["menu_status"]] == 'alum_class2'){
    archit[["menu_status"]] <- 'alum_class'
  }
  else if (archit[["menu_status"]] == 'tita_class'){
    archit[["menu_status"]] <- 'tita_class2'
  }
  else if (archit[["menu_status"]] == 'tita_class2'){
    archit[["menu_status"]] <- 'tita_class'
  }
  archit[["action"]] <- buffer
}
slot_two_f <- function(archit){
  buffer <- archit[["action"]]
  hold <- as.data.table(read.csv('actions_table.csv', header=TRUE, stringsAsFactors = FALSE))
  buffer_index <- buffer$num[2] + 1 
  if (buffer_index == 90){
    buffer_index <- 1
  }
  buffer[2,] <- hold[buffer_index,]
  if (archit[["menu_status"]] == 'poly_class'){
    archit[["menu_status"]] <- 'poly_class2'
  }
  else if (archit[["menu_status"]] == 'poly_class2'){
    archit[["menu_status"]] <- 'poly_class'
  }
  else if (archit[["menu_status"]] == 'alum_class'){
    archit[["menu_status"]] <- 'alum_class2'
  }
  else if (archit[["menu_status"]] == 'alum_class2'){
    archit[["menu_status"]] <- 'alum_class'
  }
  else if (archit[["menu_status"]] == 'tita_class'){
    archit[["menu_status"]] <- 'tita_class2'
  }
  else if (archit[["menu_status"]] == 'tita_class2'){
    archit[["menu_status"]] <- 'tita_class'
  }
  archit[["action"]] <- buffer
}

slot_three_b <- function(archit){
  buffer <- archit[["action"]]
  hold <- as.data.table(read.csv('actions_table.csv', header=TRUE, stringsAsFactors = FALSE))
  buffer_index <- buffer$num[3] - 1 
  if (buffer_index == 0){
    buffer_index <- length(hold$id)
  }
  buffer[3,] <- hold[buffer_index,]
  if (archit[["menu_status"]] == 'poly_class'){
    archit[["menu_status"]] <- 'poly_class2'
  }
  else if (archit[["menu_status"]] == 'poly_class2'){
    archit[["menu_status"]] <- 'poly_class'
  }
  else if (archit[["menu_status"]] == 'alum_class'){
    archit[["menu_status"]] <- 'alum_class2'
  }
  else if (archit[["menu_status"]] == 'alum_class2'){
    archit[["menu_status"]] <- 'alum_class'
  }
  else if (archit[["menu_status"]] == 'tita_class'){
    archit[["menu_status"]] <- 'tita_class2'
  }
  else if (archit[["menu_status"]] == 'tita_class2'){
    archit[["menu_status"]] <- 'tita_class'
  }
  archit[["action"]] <- buffer
}

slot_three_f <- function(archit){
  buffer <- archit[["action"]]
  hold <- as.data.table(read.csv('actions_table.csv', header=TRUE, stringsAsFactors = FALSE))
  buffer_index <- buffer$num[3] + 1 
  if (buffer_index == 90){
    buffer_index <- 1
  }
  buffer[3,] <- hold[buffer_index,]
  if (archit[["menu_status"]] == 'poly_class'){
    archit[["menu_status"]] <- 'poly_class2'
  }
  else if (archit[["menu_status"]] == 'poly_class2'){
    archit[["menu_status"]] <- 'poly_class'
  }
  else if (archit[["menu_status"]] == 'alum_class'){
    archit[["menu_status"]] <- 'alum_class2'
  }
  else if (archit[["menu_status"]] == 'alum_class2'){
    archit[["menu_status"]] <- 'alum_class'
  }
  else if (archit[["menu_status"]] == 'tita_class'){
    archit[["menu_status"]] <- 'tita_class2'
  }
  else if (archit[["menu_status"]] == 'tita_class2'){
    archit[["menu_status"]] <- 'tita_class'
  }
  archit[["action"]] <- buffer
}


poly_select <- function(archit){
  buffer <- archit[["action"]] 
  archit[["action"]] <- buffer[1:2]
}

alum_select <- function(){
  
}
tita_select <- function(){
  
}

start_game<-function(archit){
  archit[['state']]<-'player_movement'
}