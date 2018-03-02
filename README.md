# cellCounter

a shiny app for cell count for tiff files.

## Howto

### shiny APP

library(shiny)

runGitHub("cellCounter", "jianhong", subdir="inst/shinyapp/")

### local R

library(devtools)
install_github("jianhong/cellCounter")
library(cellCounter)

cellCounter(system.file("extdata", "sample.tiff", package="cellCounter"), xmlfile="sample.xml", imageFilename="sample.czi")

cellCounterSingleFrame(system.file("extdata", "low.jpg", package="cellCounter"), formula="blue+red", xmlfile="low.xml", imageFilename="low.jpg")

[youtube tutorial](https://youtu.be/UUWOd5ys8ZY)
