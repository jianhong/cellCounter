#' cell counter
#' @description count the cells from a z-stack tiff or czi file and output xml for imageJ
#' @param file tiff file name
#' @param channel channel to be detected
#' @param offset the offset of color from background, (0, 1).
#' @param cellSizeRange cell size range
#' @param distanceSameCell distance of same cell in different frame
#' @param xmlfile filename of xml
#' @param imageFilename filename of the original czi file
#' @param counterType counterType in Cell Counter of imageJ
#' @param zvalue change the z to original z.
#' @param adjustPipeline adjust pipeline before cell detection
#' @param detectFun the function used to detect the cell, see \link{detectObjects}, \link{detectObjects2}.
#' @param saveAdjustImage the file name for adjusted image. NULL to ignore saving.
#' @param silence output the message or not
#' @param ... parameters could be used in the pipeline
#' @import EBImage
#' @import scales
#' @import XML
#' @export
#' @author Jianhong Ou
#' @examples
#' library(EBImage)
#' library(scales)
#' library(XML)
#' ##library(rJava)
#' ##.jinit(parameters=c("-Xms1g","-Xmx4g"))
#' ##library(RBioFormats) ## to support czi file,
#' ## try to install by devtools::install_github("aoles/RBioFormats")
#' cellCounter(system.file("extdata", "sample.tiff", package="cellCounter"),
#' xmlfile="sample.xml", imageFilename="sample.czi")
#'
cellCounter <- function(file, channel="green", offset=0.05, cellSizeRange=c(20, 1000),
                        distanceSameCell=10,
                        xmlfile=sub("\\.(tiff|tif)$", ".cellCounter.xml", file, ignore.case = TRUE),
                        imageFilename=sub("\\.(tiff|tif)$", ".czi", basename(file), ignore.case = TRUE),
                        counterType=c(prenew=5, new=4, old=6), zvalue=fixZvalue,
                        adjustPipeline=c(GaussianBlur, ScurveAdjust),
                        detectFun=detectObjects,
                        saveAdjustImage=NULL,
                        silence=FALSE, ...){
  channel <- match.arg(channel, choices = c("green", "red", "blue"))
  stopifnot(offset<1)
  stopifnot(offset>0)
  if(!silence) message("reading tiff file")
  img <- readFile(file, channel)
  if(!is.character(imageFilename)) imageFilename <- img$imageFilename
  img <- img$img
  ## pre adjust
  if(!silence) message("adjust image file")
  for(fun in adjustPipeline){
    stopifnot(is.function(fun))
    img <- fun(img=img, ...)
  }
  if(is.character(saveAdjustImage)){
    writeImage(img, files = saveAdjustImage, type="TIFF")
  }
  ## detect the cells
  if(!silence) message("detecting cells")
  img <- detectFun(img, offset=offset, cellSizeRange=cellSizeRange, ...)
  ## coutput the xml
  if(!silence) message("output xml file")
  saveCountXML(img, xmlfile = xmlfile, file = imageFilename, distance = distanceSameCell,
               counterType=counterType, zv=zvalue, ...)
}


