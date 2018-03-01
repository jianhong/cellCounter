# cellCounter

a shiny app for cell count for tiff files.

## Howto

### shiny APP

library(shiny)

runGitHub("cellCounter", "jianhong")

### local R

source("https://raw.githubusercontent.com/jianhong/cellCounter/master/cellCounter.R")
source("https://raw.githubusercontent.com/jianhong/cellCounter/master/adjustPipelineFun.R")

library(EBImage)

library(scales)

library(XML)

library(RbioFormats)

cellCounter(file.path("inst", "extdata", "sample.tiff"), xmlfile="sample.xml", imageFilename="sample.czi")

cellCounterSingleFrame(file.path("inst", "extdata", "low.jpg"), formula="blue+red", xmlfile="low.xml", imageFilename="low.jpg")

[youtube tutorial](https://youtu.be/UUWOd5ys8ZY)
