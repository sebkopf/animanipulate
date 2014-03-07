animanipulate
=============

Note: this is a work-in-progress, the first stable version is not released yet, please check back in a couple of weeks.

## Prerequisites

 - ```R``` (http://www.r-project.org/)
 - one of the underlying toolkits for drawing the user interface, for example ```tcltk``` (usually installed already on Windows and MacOSX) or ```GTK```

If you want to use ```GTK```, there are installayion instructions here:

[installatoin](https://gist.github.com/sebkopf/9405675)

The user interface in this package is generated using GTK+, a cross-platform toolkit for graphical user interfaces (http://www.gtk.org/). GTK needs to be installed prior to using this package but the installation is fairly straight forward (there is a known problem with the newest version of GTK on Mac OS, details in the [installation instructions](https://gist.github.com/sebkopf/9405675)). If R and GTK are already installed and running on your system, you can go straight to [installing the dfv package](#install-dfv-package).



## Installation

This package expands the ```gWidgetsManipulate``` package, which is not hosted on CRAN but on R-forge. To make sure this dependecny is installed, please run the following code in the R command line (installing from source because the binaries are not always available):
```
install.packages("gWidgetsManipulate", repos="http://R-Forge.R-project.org", type = "source")
````

To then install the ```animanipulate``` package from GitHub, the ```devtools``` package provides a super convenient way of doing it, run the following from the R command line to install ```devtools```:
```
install.packages('devtools', depen=T) # development tools
```

Then install the ```animanipulate``` package from GitHub using ```devtools```:
```
library(devtools)
install_github('animanipulate', 'sebkopf')
```

## Examples

The following example code generates the GUI pictured in the screen above. This will work with any plotting function and automatically allows animation of all ```slider``` controls.

```coffee
library(animanipulate)
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
                   label = "bandwith [%]"), # using % here b/c some toolkits only allow integers (GTK is fine with real)
axes = checkbox(TRUE, label="Draw Axes"),
label = checkbox(TRUE, label="Draw Labels")
)
```
