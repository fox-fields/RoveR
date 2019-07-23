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
      text = tiles$char, 
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
      dtick =1,
      tick0 = 0.5,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      gridcolor = "#353535",
      gridwidth = 2
    ),
    yaxis = list(
      range = c(player$y - 5, player$y + 5),
      dtick = 1,
      tick0 = 0.65,
      scaleanchor = "x",
      zeroline = FALSE,
      gridcolor = "#353535",
      gridwidth = 2,
      showline = FALSE,
      showticklabels = FALSE
    ),
    paper_bgcolor = "#353535",
    plot_bgcolor = "#313131"
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
      dtick = 1,
      tick0 = 0.5,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      gridcolor = "#353535",
      gridwidth = 2
    ),
    yaxis = list(
      range = c(0, 10),
      dtick = 1,
      tick0 = 0.65,
      scaleanchor = "x",
      zeroline = FALSE,
      gridcolor = "#353535",
      gridwidth = 2,
      showline = FALSE,
      showticklabels = FALSE
    ),
    paper_bgcolor = "#353535",
    plot_bgcolor = "#313131"
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
  add_trace(
    name = "Overlay",
    plot_buffer,
    x = hover_buffer$x,
    y = hover_buffer$y-0.12,
    type = 'scatter',
    mode = 'text',
    text = "◼", 
    opacity = 0.5,
    textposition = "center",
    hoverinfo = "none",
    textfont = list(color = c(rep("#287C8E", (length(hover_buffer$x)-1)), "#8FD744"),
                    size =  120)
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
    text = tiles$char, 
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


