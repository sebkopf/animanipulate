##' @include animations.R
##' @include slider-control.R
NULL

## Slider class
AnimatedSlider <- setRefClass(
  "AnimatedSlider",
  contains=c("Slider", "AnimationControls"),
  fields = list(
      counter = "ANY" # animation value widget
    ),
  methods=list(
    initialize=function(...) {
      callSuper(...) # initialize Slider
      init(...) # intialize AnimationControls
    },
    
    take_step = function(..., blockChangedHandler = FALSE) {
      callSuper(...)
      if (blockChangedHandler) block_handlers('changed') 
      set_value(value) # update slider as well
      if (blockChangedHandler) unblock_handlers('changed')
    },
    
    set_value=function(val) {
      block_handlers(c('slider', 'counter')) # don't retrigger update handlers 
      if (!is.na(val))
        value <<- val # update interval value if it's a valid number
      slider.all <- widget[] # set slider closests to value
      callSuper(slider.all[which(abs(slider.all - value) == min(abs(slider.all - value)))][1])
      if (!identical(value, svalue(counter)))
        svalue(counter) <<- value # update display counter
      unblock_handlers(c('slider', 'counter')) # don't retrigger update handlers 
    },
    
    get_value=function() value, # internally stored value from animation
    
    make_gui = function(cont, handler, ...){
      callSuper(cont, NULL, ...) # slider 
      cont[dim(cont)[1]+1, 2] <- (g <- ggroup(cont=cont, horizontal=TRUE, ...))
      counter <<- glabel(value, cont=g, coerce.with = as.numeric) 
      # Note: gedit is implemented to work well but slows down the animation considerably so currently using glabel instead
      # perhaps implement that a double click on the counter allows one to enter a numeric value (when not in animation!)
      size(counter) <<- c(50, -1)
      make_animation_gui(cont=g) # animation controls
      register_handler('slider', addHandlerChanged(widget, function(...) { set_value(svalue(widget)) }), widget) # slider updates counter
      register_handler('counter', addHandlerChanged(counter, function(...) { set_value(svalue(counter)) }), counter) # counter updates slider
      register_handler('changed', addHandlerChanged(counter, handler), counter) # counter triggers changed handler
    }
    )
  )

#t <- AnimatedSlider$new(min=0, max=10, label="test", validate=TRUE)
#t$make_gui(glayout(cont=gwindow("test")), handler=function(...) print(t$value))

##' Slider interface
##' 
##' @param min minimum value
##' @param max maximum value
##' @param initial initial value. Must be in min <= max
##' @param step step size. Use -1 for default
##' @param ticks logical. are ticks drawn?
##' @param label optional label for control
##' @param animated whether to make it an animated slider or not. TRUE by default
##' @return return Slider instance
##' @export
slider <- function(min, max, initial=min, 
                   step=-1, ticks=TRUE,
                   label="", animated = TRUE) {
  if (animated)
    AnimatedSlider$new(min, max, initial=initial, label=label, 
                       step=step, ticks=ticks, validate = TRUE)
  else
    Slider$new(min, max, initial=initial, label=label, 
               step=step, ticks=ticks, validate = TRUE)
}