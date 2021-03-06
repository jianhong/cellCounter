% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjustPipelineFun.R
\name{GaussianBlur}
\alias{GaussianBlur}
\title{GaussianBlur}
\usage{
GaussianBlur(img, GaussianBlur_sigma = 5, ...)
}
\arguments{
\item{img}{an object of \link[EBImage:Image-class]{Image}.}

\item{GaussianBlur_sigma}{sigma for \link[EBImage:gblur]{gblur}.}

\item{\dots}{not used.}
}
\description{
apply Gaussian blur to a Image
}
\examples{
library(EBImage)
img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
GaussianBlur(img)
}
\author{
Jianhong Ou
}
