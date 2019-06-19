#### Tile Controller ###########################################################
# RoveR
# Tile Manager Functions ("tile_controller.R")
# June 2019
# FoxFields
#
# General functions that control the plotting of tiles

# Contents:
# [+] Plot Entities: plot_entities(plot_buffer, font_size=1, align='center', objs)
# [+] Plot Grid: plot_grid(plot_buffer)


#### [+] Plot Entities #########################################################
# Plot all entities on the main screen.
#
# + plot_buffer = the plot object (plot_ly plot)
# + font_size = font size modifies (proportion - numeric)
# + align = text alignment (string)
# + objs = objects list (reactive list)
# = returns an updated plot object (plot_ly plot)

plot_entities <- function(plot_buffer, font_size = 1, align = 'center', objs) {
    add_trace(
      name = "Entities",
      plot_buffer,
      x = sapply(objs, "[[", "x"),
      y = sapply(objs, "[[", "y") + 0.075,
      type = 'scatter',
      mode = 'text',
      text = sapply(objs, "[[", "char"),
      textposition = align,
      textfont = list(color = sapply(objs, "[[", "color"), size = font_size*80)
    )
}

#### [+] Plot Grid #########################################################
# Plot the grid lines and backgound color. Sets x and y axes limits. 
#
# + plot_buffer = the plot object (plot_ly plot)
# = returns an updated plot object (plot_ly plot)

plot_grid <- function (plot_buffer){
  layout(
    plot_buffer,
    showlegend= FALSE,
    xaxis = list(
      range = c(-10,10),
      dtick = 1,
      tick0 = 0.5,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      gridcolor = "#353535",
      gridwidth = 2
    ),
    yaxis = list(
      range = c(-10,10),
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
    plot_bgcolor = "#20A486FF"
  )
}

