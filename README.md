# riverbed

riverbed is a package which is intended to facilitate the calculation of surfaces and volumes described with topological profiles (such as river long profiles or river transects)

## Installation

For now there is no stable version of this package on CRAN. If you want to install the dev version, make sure you have a very recent version of R (>3.5.0) and run:

```{r}
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("lvaudor/riverbed")
```

## Documentation

You can access the documentation regarding package riverbed  [on this site](http://perso.ens-lyon.fr/lise.vaudor/Rpackages/riverbed/)

## Guidelines

To have a general overview of the use of this package please see vignette:

```{r}
vignette("riverbed")
```
