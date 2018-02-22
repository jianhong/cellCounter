# cellCounter

a shiny app for cell count for tiff files.

## Howto

### shiny APP

library(shiny)

runGitHub("cellCounter", "jianhong")

### local R

library(devtools)

install_github("jianhong/cellCounter")

library(EBImage)

library(scales)

library(XML)

cellCounter(file.path("inst", "extdata", "sample.tiff"), xmlfile="sample.xml", imageFilename="sample.czi")

