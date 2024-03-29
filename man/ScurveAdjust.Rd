% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjustPipelineFun.R
\name{ScurveAdjust}
\alias{ScurveAdjust}
\title{ScurveAdjust}
\usage{
ScurveAdjust(
  img,
  ScurveAdjust_k = 10,
  ScurveAdjust_L = 1,
  ScurveAdjust_x0 = 0.5,
  ...
)
}
\arguments{
\item{img}{an object of \link[EBImage:Image-class]{Image}.}

\item{ScurveAdjust_k}{slope}

\item{ScurveAdjust_L}{max value}

\item{ScurveAdjust_x0}{mean value}

\item{\dots}{not used.}
}
\description{
apply S-curve adjust for a Image by formula: data = L / (1 + exp (-k * (data - x0)))
}
\examples{
library(EBImage)
img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
ScurveAdjust(img)
}
\author{
Jianhong Ou
}
