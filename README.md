# cellCounter

a shiny app for cell count for tiff files.

## Howto

### shiny APP

library(shiny)

runGitHub("cellCounter", "jianhong")

### local R

source("https://raw.githubusercontent.com/jianhong/cellCounter/master/cellCounter.R")

library(EBImage)

library(scales)

library(XML)

cellCounter(file.path("inst", "extdata", "sample.tiff"), xmlfile="sample.xml", imageFilename="sample.czi")

