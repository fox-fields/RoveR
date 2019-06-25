#### Tile Controller ###########################################################
# RoveR
# Tile Manager Functions ("tile_controller.R")
# June 2019 (RoveR version 2: "Apollo Lunar Rover")
# FoxFields
#
# General functions that control the plotting of tiles

# Contents:
# [+] Collate Tiles: collate_tiles(archit, objs)
# [+] Plot Tiles: plot_tiles(plot_buffer, archit, objs)
# [+] Plot Grid: plot_grid(plot_buffer, objs)

#### [+] Collate Tiles #########################################################
# Collate all architectue into tiles for plotting on the main screen.
#
# + archit = architecture lists (reactive list)
# + objs = objects list (reactive list)
# = returns a dataframe for tile ploting (dataframe)

collate_tiles <- function (archit, objs){
    
  buffer_cells <- data.frame(
    x = archit[['cells']]$x,
    y = archit[['cells']]$y,
    text = "▣",
    color = "#FDE725",
    size = 1.5
  )
  
  buffer_objs <- data.frame(
    x = sapply(objs, "[[", "x"),
    y = sapply(objs, "[[", "y"),
    text = sapply(objs, "[[", "char"),
    color = sapply(objs, "[[", "color"),
    size = 1.0
  )
  
  return(rbind(buffer_cells,buffer_objs))
  
}


#### [+] Plot Tiles #########################################################
# Plot all tiles on the main screen.
#
# + plot_buffer = the plot object (plot_ly plot)
# + archit = architecture lists (reactive list)
# + objs = objects list (reactive list)
# = returns an updated plot object (plot_ly plot)

plot_tiles <- function(plot_buffer, archit, objs) {
    tiles <- collate_tiles(archit, objs)
    add_trace(
      name = "Entities",
      plot_buffer,
      x = tiles$x[tiles$x <= 8+objs[['player']]$x &
                  tiles$x >= objs[['player']]$x-8 &
                  tiles$y <= 5+objs[['player']]$y &
                  tiles$y >= objs[['player']]$y-5] ,
      y = tiles$y[tiles$x <= 8+objs[['player']]$x &
                    tiles$x >= objs[['player']]$x-8 &
                    tiles$y <= 5+objs[['player']]$y &
                    tiles$y >= objs[['player']]$y-5] + 0.075,
      type = 'scatter',
      mode = 'text',
      text = tiles$text[tiles$x <= 8+objs[['player']]$x &
                          tiles$x >= objs[['player']]$x-8 &
                          tiles$y <= 5+objs[['player']]$y &
                          tiles$y >= objs[['player']]$y-5],
      textposition = "center",
      textfont = list(color = tiles$color[tiles$x <= 8+objs[['player']]$x &
                                          tiles$x >= objs[['player']]$x-8 &
                                          tiles$y <= 5+objs[['player']]$y &
                                          tiles$y >= objs[['player']]$y-5],
                      size = tiles$size[tiles$x <= 8+objs[['player']]$x &
                                           tiles$x >= objs[['player']]$x-8 &
                                           tiles$y <= 5+objs[['player']]$y &
                                           tiles$y >= objs[['player']]$y-5] * 70)
    )
}

#### [+] Plot Grid #########################################################
# Plot the grid lines and backgound color. Sets x and y axes limits. 
#
# + plot_buffer = the plot object (plot_ly plot)
# = returns an updated plot object (plot_ly plot)

plot_grid <- function (plot_buffer, objs){
  layout(
    plot_buffer,
    showlegend= FALSE,
    dragmode=FALSE,
    xaxis = list(
      range = c(objs[['player']]$x-5,objs[['player']]$x+5),
      dtick = 1,
      tick0 = 0.5,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      gridcolor = "#353535",
      gridwidth = 2
    ),
    yaxis = list(
      range = c(objs[['player']]$y-5,objs[['player']]$y+5),
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
    plot_bgcolor = "#313131" # 31
  )
}


