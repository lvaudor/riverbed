% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/area_between.R
\name{calc_var}
\alias{calc_var}
\title{A function to assess the variability in the area calculation}
\usage{
calc_var(dat, index, sigma_z, sigma_l)
}
\arguments{
\item{dat}{data with all trapezoids information for area calculation}

\item{index}{series identifier (1 or 2)}

\item{sigma_z}{the uncertainty in measures l (can be different for the two series)}
}
\value{
var a table regarding the variance in areas calculated.
}
\description{
This function takes two profiles defined as (l1,z1) and (l2,z2) as inputs and calculates area between them
}
