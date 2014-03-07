#' Reference class for manipulate object
#' Expands functionality of the Manipulate object from John Verzani's gWidgetsManipulate package
Animanipulate <- setRefClass(
  "Animanipulate",
  contains = "Manipulate",
  methods = list(   
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
##' Due to the event loop structure of tcltk, this only works in the GTK
##' toolkit at the moment.
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
  options(guiToolkit = "RGtk2") # event loop for animation does not work in tcltk
  obj <- Animanipulate$new(substitute(._expr),...)
  obj$execute()
  invisible(obj)
}
