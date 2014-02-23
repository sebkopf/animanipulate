# FIXME: implement proper control of the sleep function in the animation-control
# from this top level (Depending on how long the user function takes to evaluate)
# implement buttons for saving pdf and gif
# --> check if animation package is installed and give warning if not (please install animation package)
# Note: I really like the mathematica manipulate shortcuts but they don't work because I don't have the resolveVariableArguments methods 
# defined in my namespace so when the Manipulate class in your package calls them it throws an error.

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
    execute = function(w = gwindow(gettext("AnimanipulateR"), width=800, height=600, visible=FALSE)) {
      
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
      svalue(pg) <- 0.55
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
##' (with added animation controls).
##'
##' @param ._expr expression to produce output.
##' @param ... used to specify controls. See \code{picker},
##' \code{checkbox}, \code{slider}.
##'
##' @return makes output, returns Animanipulate object
##' @export
##' @examples
##' \dontrun{
##' manipulate(## expression
##' {
##'   size <- sqrt.size^2
##'   y <- get(distribution)(size)
##'   plot(density(y, bw = bandwidth/100, kernel=kernel), 
##'        axes = axes, ann = label,
##'        main = paste("kernel:", kernel))
##'   points(y, rep(0, size))
##' },
##' ## controls
##' distribution = picker("rnorm", "rexp"),
##' kernel = picker("gaussian", "epanechnikov", 
##'                 "rectangular", "triangular", "cosine"),
##' sqrt.size = slider(5, 30, step = 1, init = 10), 
##' bandwidth = slider(5, 200, step = 5, init = 100, 
##'                    label = "bandwith [%]"), # using % here b/c some toolkits only allow integers (GTK is fine with real)
##' axes = checkbox(TRUE, label="Draw Axes"),
##' label = checkbox(TRUE, label="Draw Labels")
##' )
##' }
manipulate <- function(._expr,...) {
  obj <- Animanipulate$new(substitute(._expr),...)
  obj$execute()
  invisible(obj)
}
