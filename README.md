animanipulate
=============


## Installation

This package expands the ```gWidgetsManipulate``` package, which is not hosted on CRAN but on R-forge. To make sure this dependecny is installed, please run the following code in the R command line (installing from source because the binaries are not always available):
```
install.packages("gWidgetsManipulate", repos="http://R-Forge.R-project.org", type = "source")
````

To install the ```animanipulate``` package from GitHub, the ```devtools``` package provides a super convenient way of doing it, run the following from the R command line to install ```devtools```:
```
install.packages('devtools', depen=T) # development tools
```

Then install the ```animanipulate``` package from GitHub using ```devtools```:
```
library(devtools)
install_github('dfv', 'sebkopf')
```

## Examples
