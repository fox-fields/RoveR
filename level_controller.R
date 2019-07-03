#### Level Controller ###########################################################
# RoveR
# Tile Manager Functions ("tile_controller.R")
# July 2019 (RoveR version 3: "Lunokhod 2")
# FoxFields
#
# Generation of a (example) level.  

# Contents:
# [+] Temporary Level: temp_level()

#### [+] Temporary Level #######################################################
# Creates a generic dungeon-like level. Yikes, this is wildly bad code. 
#
# = returns x and y coordinates for the walls of a level (dataframe)

temp_level<-function(){
  
  hold_buffer <- data.frame(x = 1, y = 1)
  hallway <- data.frame(x = 1, y = 1)
  for (i in 1:sample(2:5, 1)) {
    x <- 1
    y <- 1
    dx <- 0
    dy <- 1
    
    for (i in 1:sample(3:7, 1)) {
      orient <- sample(c("onward", "turn"), 1)
      if (orient == "onward") {
        dx <- dx
        dy <- dy
        dx_d <- sample(5:10, 1)
        dy_d <- sample(5:10, 1)
        
        buffer <- expand.grid(x = x:(x + dx * dx_d),
                              y = y:(y + dy * dy_d))
        
        hold_buffer <- rbind(hold_buffer, buffer)
        
        widthL <- sample(0:1, 1)
        widthR <- sample(1:2, 1)
        hallway_buffer <- expand.grid(x = (x - (widthL - widthL * abs(dx))):((x +
                                                                                dx * dx_d) + (widthR - widthR * abs(dx))),
                                      y = (y - (widthL - widthL * abs(dy))):((y +
                                                                                dy * dy_d) + (widthR - widthR * abs(dy))))
        hallway <- rbind(hallway, hallway_buffer)
        
        x = x + dx * dx_d
        y = y + dy * dy_d
        
      }
      if (orient == "turn") {
        if (dx != 0 & dy == 0) {
          dx <- 0
          dy <- sample(c(-1, 1), 1)
          
          dx_d <- sample(5:10, 1)
          dy_d <- sample(5:10, 1)
          
          buffer <- expand.grid(x = x:(x + dx * dx_d),
                                y = y:(y + dy * dy_d))
          
          hold_buffer <- rbind(hold_buffer, buffer)
          
          widthL <- sample(0:1, 1)
          widthR <- sample(1:2, 1)
          hallway_buffer <- expand.grid(x = (x - (widthL - widthL * abs(dx))):((x +
                                                                                  dx * dx_d) + (widthR - widthR * abs(dx))),
                                        y = (y - (widthL - widthL * abs(dy))):((y +
                                                                                  dy * dy_d) + (widthR - widthR * abs(dy))))
          hallway <- rbind(hallway, hallway_buffer)
          
          x = x + dx * dx_d
          y = y + dy * dy_d
          
        }
        if (dy != 0 & dx == 0) {
          dx <- sample(c(-1, 1), 1)
          dy <- 0
          
          dx_d <- sample(5:10, 1)
          dy_d <- sample(5:10, 1)
          
          buffer <- expand.grid(x = x:(x + dx * dx_d),
                                y = y:(y + dy * dy_d))
          
          hold_buffer <- rbind(hold_buffer, buffer)
          
          widthL <- sample(0:1, 1)
          widthR <- sample(1:2, 1)
          hallway_buffer <- expand.grid(x = (x - (widthL - widthL * abs(dx))):((x +
                                                                                  dx * dx_d) + (widthR - widthR * abs(dx))),
                                        y = (y - (widthL - widthL * abs(dy))):((y +
                                                                                  dy * dy_d) + (widthR - widthR * abs(dy))))
          hallway <- rbind(hallway, hallway_buffer)
          
          x = x + dx * dx_d
          y = y + dy * dy_d
        }
      }
    }
  }
  
  hold_buffer <- rbind(hold_buffer, hallway)
  hold_buffer <- unique(hold_buffer)
  hold_buffer$col <- "grey"
  hold_buffer$type <- "hallway"
  
  for (i in 1:3) {
    end_buff <- length(hold_buffer$x) * ((i) / 3)
    start_buff <- length(hold_buffer$x) * ((i - 1) / 3)
    x_door <- sample(hold_buffer$x[start_buff:end_buff], 1)
    y_door <- max(hold_buffer$y[which(hold_buffer$x == x_door)]) + 1
    room_buffer <- expand.grid(x = 0:sample(5:10, 1),
                               y = 0:sample(5:10, 1))
    room_buffer$x <- room_buffer$x - round(median(room_buffer$x)) + x_door
    room_buffer$y <- room_buffer$y + y_door + 1
    room_buffer$col <- "red"
    room_buffer$type <- "room"
    
    hold_buffer <-
      rbind(hold_buffer,
            data.frame(
              x = x_door,
              y = y_door,
              col = "green",
              type = "door"
            ))
    hold_buffer <- rbind(room_buffer, hold_buffer)
    
    
  }
  
  for (i in 1:3) {
    end_buff <- length(hold_buffer$x) * ((i) / 3)
    start_buff <- length(hold_buffer$x) * ((i - 1) / 3)
    x_door <- sample(hold_buffer$x[start_buff:end_buff], 1)
    y_door <- min(hold_buffer$y[which(hold_buffer$x == x_door)]) - 1
    room_buffer <- expand.grid(x = 0:sample(-5:-10, 1),
                               y = 0:sample(-5:-10, 1))
    room_buffer$x <- room_buffer$x - round(median(room_buffer$x)) + x_door
    room_buffer$y <- room_buffer$y + y_door - 1
    room_buffer$col <- "red"
    room_buffer$type <- "room"
    
    hold_buffer <-
      rbind(hold_buffer,
            data.frame(
              x = x_door,
              y = y_door,
              col = "green",
              type = "door"
            ))
    hold_buffer <- rbind(room_buffer, hold_buffer)
    
    
  }
  
  
  for (i in 1:3) {
    end_buff <- length(hold_buffer$y) * ((i) / 3)
    start_buff <- length(hold_buffer$y) * ((i - 1) / 3)
    y_door <- sample(hold_buffer$y[start_buff:end_buff], 1)
    x_door <- max(hold_buffer$x[which(hold_buffer$y == y_door)]) + 1
    room_buffer <- expand.grid(x = 0:sample(5:10, 1),
                               y = 0:sample(5:10, 1))
    room_buffer$y <- room_buffer$y - round(median(room_buffer$y)) + y_door
    room_buffer$x <- room_buffer$x + x_door + 1
    room_buffer$col <- "red"
    room_buffer$type <- "room"
    
    hold_buffer <-
      rbind(hold_buffer,
            data.frame(
              x = x_door,
              y = y_door,
              col = "green",
              type = "door"
            ))
    hold_buffer <- rbind(room_buffer, hold_buffer)
    
    
  }
  
 
  for (i in 1:3) {
    end_buff <- length(hold_buffer$y) * ((i) / 3)
    start_buff <- length(hold_buffer$y) * ((i - 1) / 3)
    y_door <- sample(hold_buffer$y[start_buff:end_buff], 1)
    x_door <- min(hold_buffer$x[which(hold_buffer$y == y_door)]) - 1
    room_buffer <- expand.grid(x = 0:sample(-5:-10, 1),
                               y = 0:sample(-5:-10, 1))
    room_buffer$y <- room_buffer$y - round(median(room_buffer$y)) + y_door
    room_buffer$x <- room_buffer$x + x_door - 1
    room_buffer$col <- "red"
    room_buffer$type <- "room"
    
    hold_buffer <-
      rbind(hold_buffer,
            data.frame(
              x = x_door,
              y = y_door,
              col = "green",
              type = "door"
            ))
    hold_buffer <- rbind(room_buffer, hold_buffer)
    
    
  }
  

  grid_buffer <-
    expand.grid(x = (min(hold_buffer$x) - 3):(max(hold_buffer$x) + 3),
                y = (min(hold_buffer$y) - 3):(max(hold_buffer$y) +
                                                3))
  
  wall_buffer2 <- unique(data.frame(x = hold_buffer$x, y = hold_buffer$y))
  
  wall_buffer <- setdiff(grid_buffer, wall_buffer2)
  
  par(pty = "s")
  
  plot(
    x = hold_buffer$x,
    y = hold_buffer$y,
    col = hold_buffer$col,
    pch = 12,
    asp = 1,
    cex = 0.3
  )
  points(wall_buffer,
         pch = 12,
         asp = 1,
         cex = 0.3)
  
  return(wall_buffer)
}


