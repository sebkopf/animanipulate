#FIXME: not currently working,
#fix it up well so it can both go inside a knitr code block and works directly
#from animanipulates save as gif functionality

#' Command to go inside a knitr code chunk (or inside an actual animate call)
#' --> made by autogeneration, ... have to be just regular character arrays describing the function variables (but can vary in multiple dimension!)
#' --> all parameters for the animation should go into the surrounding knit_r code block
#' @export
animate <- function(`_expr`, ...) {
  print(substitute(`_expr`))
  print(deparse(substitute(`_expr`), control = NULL))
  params <- expand.grid(...) # paramater data frame
  manipulator <- new.env(parent = parent.frame())
  
  for (i in 1:nrow(params)) 
    do.call(substitute(`_expr`), args = params[i, , drop=TRUE]) #FIXME
  
}


#f <- function(x, y, z) message(x + y + z)
#animate(f(x, y, z), x=1, y=1:5, z=2)