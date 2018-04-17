gmapsdistance2
=======
[![Build Status](https://travis-ci.org/rcannood/gmapsdistance2.png)](https://travis-ci.org/rcannood/gmapsdistance2) 
![](http://cranlogs.r-pkg.org/badges/gmapsdistance2?color=brightgreen)
![](https://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/gmapsdistance2)](https://cran.r-project.org/package=gmapsdistance2)

***Interface Between R and Google Maps***

The function `gmapsdistance` uses the [Google Maps Distance Matrix API](https://developers.google.com/maps/documentation/distance-matrix/intro?hl=en) to compute the distance(s) and time(s) between two points or two vectors of points. An [API key](https://developers.google.com/maps/documentation/distance-matrix/get-api-key#key) is not necessary to perform the query but the function supports its usage. If an API key is being used the Distance Matrix API should be enabled in the Google Developers Console. Google maps must be able to find both the origin and the destination in order for the function to run. If the origin or destination contains multiple words, they should be separated by a plus sign (+). The distance is returned in meters and the time in seconds. 

Four different modes of transportation are allowed: `bicycling`, `walking`, `driving`, `transit`. 


## Installation

```{r}
# Github installation
# install.packages("devtools")
# devtools::install_github("rcannood/gmapsdistance2")
```
