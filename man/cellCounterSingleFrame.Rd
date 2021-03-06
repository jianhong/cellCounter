% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cellCounterSingleFrame.R
\name{cellCounterSingleFrame}
\alias{cellCounterSingleFrame}
\title{cell counter}
\usage{
cellCounterSingleFrame(file, channels = c("red", "green", "blue"), formula,
  maskValue = 0.3, offset = 0.1, cellSizeRange = c(10, 1000),
  xmlfile = sub("\\\\.(tiff|tif)$", ".cellCounter.xml", file, ignore.case =
  TRUE), imageFilename = sub("\\\\.(tiff|tif)$", ".czi", basename(file),
  ignore.case = TRUE), adjustPipeline = c(ScurveAdjust),
  detectFun = detectObjects, saveAdjustImage = NULL, silence = FALSE, ...)
}
\arguments{
\item{file}{tiff file name}

\item{channels}{channel to be detected}

\item{formula}{the relationship of channels. Here '*'=='intersect' and '+'=='contain'
eg. blue*green+red means the cell must have blue and green, and red is in the cell.
    blue*green-red means the cell must have blue and green, and red is NOT in the cell.
    blue*green means the cell must have blue and green
    blue+red means the cell must have blue, and red is in the cell.}

\item{maskValue}{the cutoff value for set the mask is TRUE.}

\item{offset}{the offset of color from background, (0, 1).}

\item{cellSizeRange}{cell size range}

\item{xmlfile}{filename of xml}

\item{imageFilename}{filename of the original czi file}

\item{adjustPipeline}{adjust pipeline before cell detection}

\item{detectFun}{the function used to detect the cell, see \link{detectObjects}, \link{detectObjects2}.}

\item{saveAdjustImage}{the file name for adjusted image. NULL to ignore saveing.}

\item{silence}{output the message or not}

\item{...}{parameters could be used in the pipeline}
}
\description{
count the cells from a single frame tiff file and output xml for imageJ
}
\examples{
library(EBImage)
library(scales)
library(XML)
cellCounterSingleFrame(system.file("extdata", "low.jpg", package="cellCounter"), 
formula="blue+red", xmlfile="low.xml", imageFilename="low.jpg")
}
\author{
Jianhong Ou
}
