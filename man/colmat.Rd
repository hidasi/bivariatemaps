% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colmat.R
\name{colmat}
\alias{colmat}
\title{colmat: Create a Color Matrix}
\usage{
colmat(
  nquantiles = 10,
  upperleft = "blue",
  upperright = "red",
  bottomleft = "grey",
  bottomright = "yellow",
  xlab = "x label",
  ylab = "y label"
)
}
\arguments{
\item{nquantiles}{numeric variable for number of quantiles in color matrix}

\item{upperleft}{upperleft color of color matrix}

\item{upperright}{upperright color of color matrix}

\item{bottomleft}{bottomleft color of color matrix}

\item{bottomright}{bottomright color of color matrix}

\item{xlab}{character variable}

\item{ylab}{character variable}
}
\value{
Two outputs: a color matrix object to be used in bivariate.map() function, and a plot of the color matrix.
}
\description{
Creates a color matrix to be used in bivariate.map() function.
}
\examples{
col.matrix<-colmat(nquantiles=10, xlab="My x label", ylab="My y label")

# https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
}
