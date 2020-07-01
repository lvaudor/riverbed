# riverbed

riverbed is a package which is intended to facilitate the calculation of surfaces and volumes described with topological profiles (such as river long profiles or river transects)

## Installation

For now there is no stable version of this package on CRAN. If you want to install the dev version, make sure you have a very recent version of R (>3.2.2) and run:

```{r}
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("lvaudor/riverbed",build_vignette=TRUE)
```

## Guidelines

To have a general overview of the use of this package please see vignette:
```{r}
vignette("riverbed")
```
