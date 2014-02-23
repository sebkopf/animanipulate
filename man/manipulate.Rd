\name{manipulate}
\alias{manipulate}
\title{Manipulate command ala RStudio
(with added animation controls).}
\usage{
manipulate(._expr, ...)
}
\arguments{
  \item{._expr}{expression to produce output.}

  \item{...}{used to specify controls. See \code{picker},
  \code{checkbox}, \code{slider}.}
}
\value{
makes output, returns Animanipulate object
}
\description{
Manipulate command ala RStudio (with added animation
controls).
}
\examples{
\dontrun{
manipulate(## expression
{
  size <- sqrt.size^2
  y <- get(distribution)(size)
  plot(density(y, bw = bandwidth/100, kernel=kernel),
       axes = axes, ann = label,
       main = paste("kernel:", kernel))
  points(y, rep(0, size))
},
## controls
distribution = picker("rnorm", "rexp"),
kernel = picker("gaussian", "epanechnikov",
                "rectangular", "triangular", "cosine"),
sqrt.size = slider(5, 30, step = 1, init = 10),
bandwidth = slider(5, 200, step = 5, init = 100,
                   label = "bandwith [\%]"), # using \% here b/c some toolkits only allow integers (GTK is fine with real)
axes = checkbox(TRUE, label="Draw Axes"),
label = checkbox(TRUE, label="Draw Labels")
)
}
}

