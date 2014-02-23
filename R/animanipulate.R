# FIXME: implement proper control of the sleep function in the animation-control
# from this top level (Depending on how long the user function takes to evaluate)


#' Reference class for manipulate object
#' Expands functionality of the Manipulate object from John Verzani's gWidgetsManipulate package
Animanipulate <- setRefClass(
  "Animanipulate",
  contains = "Manipulate",
  methods = list(
    #' Generate the GUI (most of the code in this method is taken directly from John Verzani's gWidgetsManipulate package)
    #' Only extension are the save buttons.
    #' Ideally this is implemented with the parent execute function also taking the window parameter
    #' and then just adding the buttons here and then callSuper(w)
    execute = function(w = gwindow(gettext("ManipulateR"), visible=FALSE)) {
      
      # FIXME: implement save pdf and save gif buttons
      # for save gif, will probably have to ask which animation control to export as gif if there are multiple
      # implement animate function properly to do this well
      
      # Code after here is all directly from the original execute function in the Manipulate class of the gWidgetsManipulate package
      "Make the GUI"
      pg <- gpanedgroup(cont=w)
      g <- ggroup(cont=pg, expand=TRUE)
      if(gtoolkit()=="tcltk") {
        require(tkrplot)
        dev <<- tkrplot(getToolkitWidget(g), function() {})
        add(g, dev, expand=TRUE)
      } else {
        ggraphics(cont=g, expand=TRUE, fill=TRUE)
      }
      f <- gframe(gettext("Controls"), horizontal=FALSE, cont=pg)
      lyt <- glayout(cont=f)
      ## add controls using make_gui interface
      sapply(.controls, function(i) {
        i$make_gui(cont=lyt, 
                   handler=.self$change_handler)
      })
      
      visible(w) <- TRUE
      svalue(pg) <- 0.6
      change_handler() # initial
    },
   
    #' constructor
    initialize = function(...) {
      callSuper(...)
      # add animation controls for all Sliders
      for (i in which(sapply(.controls, function(i) inherits(i, "Slider")))) {
        slider <- .controls[[i]]
        .controls[[i]] <<- AnimationControl$new(slider = slider)
      }
    }
))

##' Manipulate command ala RStudio
##'
##' @param ._expr expression to produce output.
##' @param ... used to specify controls. See \code{picker},
##' \code{checkbox}, \code{slider}.
##'
##' These controls may also be specified through a object, from which the control is guessed.
##' A logical maps to \code{checkbox}.
##' A character maps to \code{picker}.
##' A numeric to \code{slider}. This mapping can be specified as an arithmetic sequence of
##' points (length 5 or greater), or as a numeric vector of length 2 to 4 with defaults
##' like: \code{c(min, max, step=1, initial=min)}
##' @return makes output, returns Manipulate object
##' @export
##' @examples
##' \dontrun{
##' ## from RStudio::manipulate
##' animanipulate(## expression
##'            plot(cars, xlim = c(x.min, x.max), type = type, 
##'                 axes = axes, ann = label),
##'            ## controls
##'            x.min = slider(0,15),
##'            x.max = slider(15,30, initial = 25),
##'            type = picker("p", "l", "b", "c", "o", "h", "s"),
##'            axes = checkbox(TRUE, label="Draw Axes"),
##'            label = checkbox(FALSE, label="Draw Labels")
##'            )
##' ## using shortcuts, ala Mathematica's manipulate
##' animanipulate(## expression
##'            plot(cars, xlim = c(x.min, x.max), type = type, 
##'                 axes = axes, ann = label),
##'            ## controls
##'            x.min = 0:15,
##'            x.max = c(15,30, 1, 25), ## min, max, step=min, initial=min
##'            type = c("p", "l", "b", "c", "o", "h", "s"),
##'            axes = TRUE,
##'            label = FALSE
##'            )
##' }
animanipulate <- function(._expr,...) {
  obj <- Animanipulate$new(substitute(._expr),...)
  obj$execute()
  invisible(obj)
}
