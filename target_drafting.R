
select_path <- function(target, self){
  # inefficient
  player <- self
  target <- data.frame(x=target$x, y=target$y)
  self <- data.frame(x=self$x, y=self$y)

  dx <- target$x - self$x
  dy <- target$y - self$y
  
  if (abs(dx) > abs(dy)){
    path_x <- self$x:target$x
    path_y <- rep(self$y:target$y, each=abs(dx/(dy)))
    path <- data.frame(x = path_x[1:length(path_y)], y = path_y)
    step_buffer <- sign(target - tail(path,1))
    path <- rbind(path, tail(path,1) + step_buffer)
  }
  
  if (abs(dy) > abs(dx)){
    path_x <- rep(self$x:target$x, each=abs(dy/(dx)))
    path_y <- self$y:target$y
    path <- data.frame(x = path_x, y = path_y[1:length(path_x)])
    step_buffer <- sign(target - tail(path,1))
    path <- rbind(path, tail(path,1)+step_buffer)
  }
  if (dx == 0){
    path_y <- self$y:target$y
    path_x <- rep(self$x, length(path_y))
    path <- data.frame(x = path_x, y = path_y)
  }
  if (dy == 0){
    path_x <- self$x:target$x
    path_y <- rep(self$y, length(path_x))
    path <- data.frame(x = path_x, y = path_y)
  }
  if (abs(dy) == abs(dx)){
    path_x <- self$x:target$x
    path_y <- self$y:target$y
    path<-data.frame(x = path_x, y = path_y)
  }
  
  #hit<-intersect(blocks,path)
  #hit
  return(na.omit(path))
}



baseline <- expand.grid(x = -10:10, y = -10:10)

blocks <- data.frame(x=sample(-10:10,25,replace=TRUE),
                     y=sample(-10:10,25,replace=TRUE))


target <- blocks[sample(1:25,1),]
self <- data.frame(x=1,y=1)


plot(baseline)
points(blocks, col = 'red', pch = 16)
points(target, col = 'green', pch = 16)
points(self, col = 'blue', pch = 16)

lines(select_path(target,self))
select_path(target,self)
