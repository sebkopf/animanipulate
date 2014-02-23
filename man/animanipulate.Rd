\name{animanipulate}
\alias{animanipulate}
\title{Manipulate command ala RStudio}
\usage{
animanipulate(._expr, ...)
}
\arguments{
  \item{._expr}{expression to produce output.}

  \item{...}{used to specify controls. See \code{picker},
  \code{checkbox}, \code{slider}.

  These controls may also be specified through a object,
  from which the control is guessed. A logical maps to
  \code{checkbox}. A character maps to \code{picker}. A
  numeric to \code{slider}. This mapping can be specified
  as an arithmetic sequence of points (length 5 or
  greater), or as a numeric vector of length 2 to 4 with
  defaults like: \code{c(min, max, step=1, initial=min)}}
}
\value{
makes output, returns Manipulate object
}
\description{
Manipulate command ala RStudio
}
\examples{
\dontrun{
## from RStudio::manipulate
animanipulate(## expression
           plot(cars, xlim = c(x.min, x.max), type = type,
                axes = axes, ann = label),
           ## controls
           x.min = slider(0,15),
           x.max = slider(15,30, initial = 25),
           type = picker("p", "l", "b", "c", "o", "h", "s"),
           axes = checkbox(TRUE, label="Draw Axes"),
           label = checkbox(FALSE, label="Draw Labels")
           )
## using shortcuts, ala Mathematica's manipulate
animanipulate(## expression
           plot(cars, xlim = c(x.min, x.max), type = type,
                axes = axes, ann = label),
           ## controls
           x.min = 0:15,
           x.max = c(15,30, 1, 25), ## min, max, step=min, initial=min
           type = c("p", "l", "b", "c", "o", "h", "s"),
           axes = TRUE,
           label = FALSE
           )
}
}

