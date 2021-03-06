% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_on_AB.R
\name{project_on_AB}
\alias{project_on_AB}
\title{A function to project data on a line defined by two points A & B}
\usage{
project_on_AB(dat, datAB, datOg = datAB[1, ])
}
\arguments{
\item{x}{x coordinate of points}

\item{y}{y coordinate of points}
}
\value{
xp x coordinate of points projected on the transect

yp y coordinate of points projected on the transect

xt x coordinate along the transect (new coordinate x)

yt y coordinate corresponding to the distance to the transect (new coordinate y)
}
\description{
A function to project data on a line defined by two points A & B
}
\examples{
dat=tibble(x=c(3.3,5.2,1.6,6.3),
           y=c(2.1,6.3,2.1,1.6))
datAB=tibble(x=c(0.2,4.2),
             y=c(2.6,6.6))
dat=project_on_AB(dat,datAB)
ggplot(dat,  aes(x=x,y=y))+
  geom_point(col="red")+
  geom_line(data=datAB, aes(x=x,y=y))+
  geom_point(data=datAB)+
  geom_point(data=dat,col="blue", aes(x=xp,y=yp))+
  coord_fixed(ratio=1)
}
