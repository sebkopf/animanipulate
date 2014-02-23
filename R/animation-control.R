# TODO:
# implement that the Animanipulate object controls the sleep time of all animation controls
# based on the evalution time necessary for the plot function it is rendering


## Animation control class
AnimationControl <- setRefClass(
  "AnimationControl",
  contains="ManipulateControls",
  fields = list(
    slider = "ANY", # Slider control object
    handlerID = "ANY" # changed handler of the animation text field
  ),
  methods=list(
    initialize = function(slider) {
      initFields(slider = slider)
      
      # store animation parameters in the paramter list l of the Controls object
      animation <- list(
        on = FALSE, # whether animation is on
        step = if (slider$l$step != -1) slider$l$step else 1, # what the step size of the animation is, step of the slider by default but user can change speed
        sleep = 0.1, # how long to sleep between each step (in seconds)
        x = slider$l$initial, # position of the animation
        x.discrete = slider$l$initial, # slider permissible position of the animation
        bounce = FALSE # whether to bounce back and forth or animate only in one direction
      ) 
      callSuper(l = animation, initial = slider$initial)
    },
    
    #' animate the plot
    animate = function(){
      while (l$on == TRUE) {
        # update value of the control
        set_value(l$x + l$step)
        Sys.sleep(l$sleep)
      }  
    },
    
    #' set the value of the control
    set_value=function(x) {
      
      # check for permissible range
      if (x < slider$l$min || x > slider$l$max) {
        if (l$on && l$bounce) { # if in animation, go to the max/min and simply revert direction
          l$step <<- -1 * l$step 
          if (x > slider$l$max) x <- slider$l$max + l$step
          else if (x < slider$l$min) x <- slider$l$min + l$step
        } else { # keep going in the same direction but start over
          if (x > slider$l$max) x <- slider$l$min
          else if (x < slider$l$min) x <- slider$l$max
        }
      }
      
      # update animation positions
      l$x <<- x
      
      # what is the new closest value of the step to the slider scale?
      slider.all <- slider$widget[] # the values allowed by the slider
      l$x.discrete <<- slider.all[which(abs(slider.all - x) == min(abs(slider.all - x)))][1]
      
      # set value
      if (!identical(slider$get_value(), l$x.discrete)) {
        # new whole value of slider, set change slider but only trigger change handler once (block the text field handler)
        # Note: ideally this is done the other way around (blocking the changed handler of the Slider) but at the moment not possible
        # without rewriting the Slider class from the gWidgetsManipulate package
        blockHandler(widget, handlerID)
        svalue(widget) <<- x
        svalue(slider$widget) <<- l$x.discrete
        unblockHandler(widget, handlerID)
      } else 
        # fractional value of slider, set text field only  (which triggers change handler)
        svalue(widget) <<- x
    },
    
    #' Get value of widget
    get_value=function(...) {
      # ideally this should be handled differently (by having set_value() triggered in the Slider changedHandler)
      # and then always just returning l$x --> need access to blocking the changedHandler in the Slider class
      if (!identical(l$x.discrete, slider$get_value()))
        return (slider$get_value())
      return (l$x)
    },
    
    #' Make the GUI for the control
    make_gui=function(cont, handler, ...) {
      # make slider control  
      slider$make_gui(cont, handler, ...)
      
      # make corresponding animation controls
      n <- dim(cont)[1]
      cont[n+1, 2] <- (g <- ggroup(cont=cont, horizontal=TRUE, ...))
      
      # continuous animation value text widget
      w <- gedit(NULL, cont=g, coerce.with = as.numeric)
      enabled(w) <- FALSE
      size(w) <- c(50, -1)
      widget <<- w
      
      # button controls (step back, play/pause, step forward)
      gbutton(" - ", handler = function(...) set_value(l$x - abs(l$step)), cont = g)
      play <- gbutton("Play", handler = function(...) {
        opts <- c("Play", "Pause")
        i <- (which(opts == svalue(play)) %% length(opts)) + 1
        svalue(play) <- opts[i]
        if (i == 2) {
          l$on <<- TRUE
          animate()
        } else
          l$on <<- FALSE
      }, cont = g)
      gbutton("+", handler = function(...) set_value(l$x + abs(l$step)), cont = g)
      
      # speed
      gbutton("^", handler = function(...) l$step <<- l$step*2, cont = g) # faster
      gbutton("v", handler = function(...) l$step <<- l$step/2, cont = g) # slower
      
      # direction
      direction <- gbutton("->", handler = function(...) {
        opts <- c("->", "<-", "<-->")
        i <- (which(opts == svalue(direction)) %% length(opts)) + 1
        svalue(direction) <- opts[i]
        l$bounce <<- (i == 3) # set bounce
        if (i == 1)
          l$step <<- abs(l$step) # move forward
        else if (i == 2)
          l$step <<- -1 * abs(l$step) # move backward
      }, cont = g)
      
      # slider controls the text input and can change the position of the animation
      # (but don't want to refire the changed handler)
      addHandlerChanged(slider$widget, handler = function(h, ...) {
        if (!identical(l$x.discrete, slider$get_value())) {
          blockHandler(widget, handlerID)
          w <- widget
          svalue(w) <- slider$get_value()
          l$x <<- l$x.discrete <<- slider$get_value()
          unblockHandler(widget, handlerID)
        }
      })
      
      # blockable change handler on the text field
      handlerID <<- addHandlerChanged(widget, handler = handler)
      
      # load initial value
      blockHandler(widget, handlerID)
      set_value(initial)
      unblockHandler(widget, handlerID)
    }))