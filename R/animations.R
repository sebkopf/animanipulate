##' @include controls.R
NULL

## Animation controls class
AnimationControls <- setRefClass(
  "AnimationControls",
  fields = list(
    value = 'numeric', # value of the animation control
    on = 'logical', # whether animation is on
    min = 'numeric', # minimum permissible value 
    max = 'numeric', # maximum permissible value
    step = 'numeric', # what the step size of the animation is (can be positive or negative)
    speed = 'integer', # speed multiplier of the step (--> multiplication by 2^speed)
    bounce = 'logical' # whether to bounce back and forth or animate only in one direction
  ),
  methods=list(
    initialize=function(...){
      init(...)
      callSuper()
    },
    
    #' Initialize (enable multiple inhereitence)
    init=function(initial = 0, min = as.numeric(NA), max = as.numeric(NA), step = 1, speed = 0L, bounce = FALSE, ...) {
      initFields(on = FALSE, value = initial, min = min, max = max, step = step, speed = speed, bounce = bounce)
    },
    
    #' @param n how many steps to take 
    take_step = function(n = 1, step = .self$step, speed = .self$speed, ...) {
      x <- value + n * step * 2^speed
      
      # check for permissible range
      if ( (!is.na(min) && x < min) ) {
        if (on && bounce) { # during animation and bounce on, revert direction
          step <<- (-1) * step
          x <- min + .self$step * 2^speed
        } else
          x <- max
      } else if (!is.na(max) && x > max) {
        if (on && bounce) { # during animation and bounce on, revert direction
          step <<- -1 * step
          x <- max + .self$step * 2^speed
        } else
          x <- min
      }
      value <<- x
    },
    
    #' make the GUI elements that control animation
    make_animation_gui = function(cont) {
      # step control
      gbutton(" - ", tooltip = "Move one step back.", handler = function(...) take_step(-1, step = abs(step)), cont = cont)
      play <- gbutton("Play", tooltip = "Start/stop animation.", handler = function(...) {
        opts <- c("Play", "Pause")
        i <- (which(opts == svalue(play)) %% length(opts)) + 1
        svalue(play) <- opts[i]
        on <<- (i == 2)
      }, cont = cont)
      gbutton("+", tooltip = "Move one step forward.", handler = function(...) take_step(+1, step = abs(step)), cont = cont)
      
      # speed controls
      change_speed <- function(change) {
        speed <<- as.integer(speed + change)
        if (speed < 0)
          svalue(speedvalue) <- paste0("1/", 2^(-1*speed), " x")
        else
          svalue(speedvalue) <- paste0(2^speed, " x")
      }
      gbutton("<<", tooltip = "Slow down animation.", handler = function(...) change_speed(-1), cont = cont) # slower
      speedvalue <- glabel('', cont = cont)
      gbutton(">>", tooltip = "Speed up animation.", handler = function(...) change_speed(+1), cont = cont) # faster
      change_speed(0) # initalize
      
      # direction control
      direction <- gbutton("->", handler = function(...) {
        opts <- c("->", "<-", "<-->")
        i <- (which(opts == svalue(direction)) %% length(opts)) + 1
        svalue(direction) <- opts[i]
        bounce <<- (i == 3) # set bounce
        if (i == 1)
          step <<- abs(step) # move forward
        else if (i == 2)
          step <<- -1 * abs(step) # move backward
      }, cont = cont)
    }))