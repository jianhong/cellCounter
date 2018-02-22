#' cell counter
#' @description count the cells from a tiff file and output xml for imageJ
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
#' @param silence output the message or not
#' @param ... parameters could be used in the pipeline
#' @import EBImage
#' @import scales
#' @import XML
#' @author Jianhong Ou
#' @examples 
#' library(EBImage)
#' library(scales)
#' library(XML)
#' cellCounter(file.path("inst", "extdata", "sample.tiff"), xmlfile="sample.xml", imageFilename="sample.czi")
#' 
cellCounter <- function(file, channel="green", offset=0.05, cellSizeRange=c(20, 1000), 
                        distanceSameCell=10, 
                        xmlfile=sub("\\.(tiff|tif)$", ".cellCounter.xml", file, ignore.case = TRUE),
                        imageFilename=sub("\\.(tiff|tif)$", ".czi", basename(file), ignore.case = TRUE),
                        counterType=c(new=5, old=4), zvalue=fixZvalue,
                        adjustPipeline=c(GaussianBlur, increaseContrast), 
                        silence=FALSE, ...){
  channel <- match.arg(channel, choices = c("green", "red", "blue"))
  stopifnot(offset<1)
  stopifnot(offset>0)
  if(!silence) message("reading tiff file")
  img <- readFile(file, channel)
  ## pre adjust
  if(!silence) message("adjust image file")
  for(fun in adjustPipeline){
    stopifnot(is.function(fun))
    img <- fun(img=img, ...)
  }
  ## detect the cells
  if(!silence) message("detecting cells")
  img <- detectCells(img, offset=offset, cellSizeRange=cellSizeRange, ...)
  ## coutput the xml
  if(!silence) message("output xml file")
  saveCountXML(img, xmlfile = xmlfile, file = imageFilename, distance = distanceSameCell, 
               counterType=counterType, zv=zvalue, ...)
}

readFile <- function(file, channel){
  stopifnot(file.exists(file))
  ## read image
  img <- readImage(file)
  ## remove the alpha channel
  img <- toRGB(img)
  if(length(dim(img)>3)){
    if(dim(img)[3]>3){
      img <- img[, , 1:3, ]
    }
  }
  channel(img, channel)
}

detectCells <- function(img, offset, cellSizeRange, size=31, tolerance=1, ext=1, ...){
  stopifnot(is(img, "Image"))
  stopifnot(offset<1)
  stopifnot(offset>0)
  stopifnot(length(cellSizeRange)==2)
  stopifnot(is.numeric(cellSizeRange))
  disc <- makeBrush(size, shape="disc")
  disc <- disc/sum(disc)
  sample_bg <- filter2(img, disc)
  sample_th <- img > sample_bg + offset
  nmask <- watershed(distmap(sample_th), tolerance=tolerance, ext=ext)
  nmask <- bwlabel(nmask)
  ## filter by cellSizeRange
  nmask2 <- list()
  for(i in seq.int(numberOfFrames(nmask, type="render"))){
    tbl <- table(imageData(getFrame(nmask, i, type="render")))
    tbl <- as.numeric(names(tbl)[tbl<cellSizeRange[1] | tbl>cellSizeRange[2]])
    nmask2[[i]] <- rmObjects(getFrame(nmask, i, type="render"), tbl, reenumerate=FALSE)
  }
  nmask2 <- combine(nmask2)
}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
fixZvalue <- function(n) 2*n-1
saveCountXML <- function(nmask, xmlfile="cellCounter.xml", file, distance=10, counterType=c(new=4, old=3), zv=fixZvalue, ...){
  stopifnot(is(nmask, "Image"))
  stopifnot(all(counterType>0))
  stopifnot(all(counterType<9))
  counterTypeTable <- data.frame(x=NA, y=NA, z=NA, type=NA)
  tbl.old <- NULL
  for(i in seq.int(numberOfFrames(nmask, type="render"))){
    tbl.new <- computeFeatures.moment(getFrame(nmask, i, type="render"))
    if(length(tbl.new)>0){
      checklist <- rep(TRUE, nrow(tbl.new))
      if(length(tbl.old)>0){
        for(m in seq.int(nrow(tbl.new))){
          for(n in seq.int(nrow(tbl.old))){
            if(euc.dist(tbl.new[m, c("m.cx", "m.cy")], tbl.old[n, c("m.cx", "m.cy")]) < distance){
              checklist[m] <- FALSE
              next
            }
          }
        }
      }
      cx <- round(tbl.new[, "m.cx"])
      cy <- round(tbl.new[, "m.cy"])
      counterTypeTable <- rbind(counterTypeTable,
                                data.frame(x=cx,
                                           y=cy,
                                           z=zv(i),
                                           type=checklist))
    }
    tbl.old <- tbl.new
  }
  counterTypeTable <- counterTypeTable[!is.na(counterTypeTable$type), , drop=FALSE]
  counterTypeTableNew <- counterTypeTable[counterTypeTable$type, , drop=FALSE]
  counterTypeTableOld <- counterTypeTable[!counterTypeTable$type, , drop=FALSE]
  suppressWarnings({xml <- xmlTree("CellCounter_Marker_File")})
  xml$addNode("Image_Properties", close = FALSE)
    xml$addNode("Image_Filename", file)
  xml$closeTag()
  xml$addNode("Marker_Data", close = FALSE)
    xml$addNode("Current_Type", 1)
    for(type in seq.int(8)){
      xml$addNode("Marker_Type", close = FALSE)
      xml$addNode("Type", type)
      if(counterType["new"]==type){
        for(m in seq.int(nrow(counterTypeTableNew))){
            xml$addNode("Marker", close = FALSE)
            xml$addNode("MarkerX", counterTypeTableNew$x[m])
            xml$addNode("MarkerY", counterTypeTableNew$y[m])
            xml$addNode("MarkerZ", counterTypeTableNew$z[m])
            xml$closeTag()
        }
      }
      if(counterType["old"]==type){
        for(m in seq.int(nrow(counterTypeTableOld))){
          xml$addNode("Marker", close = FALSE)
          xml$addNode("MarkerX", counterTypeTableOld$x[m])
          xml$addNode("MarkerY", counterTypeTableOld$y[m])
          xml$addNode("MarkerZ", counterTypeTableOld$z[m])
          xml$closeTag()
        }
      }
      xml$closeTag()
    }
  xml$closeTag()
  cat(saveXML(xml, prefix = '<?xml version="1.0" encoding = "UTF-8"?>'), file = xmlfile, sep="\n")
}

GaussianBlur <- function(img, sigma=5, ...){
  stopifnot(is(img, "Image"))
  gblur(img, sigma = sigma)
}

rescaleImage <- function(img, ...){
  stopifnot(is(img, "Image"))
  if(colorMode(img)!=0){
    stop("colorMode of img must be Grayscale")
  }
  if(numberOfFrames(img, type="render")==1){
    imageData(img) <- rescale(imageData(img), to=c(0, 1))
  }else{
    for(i in seq.int(numberOfFrames(img, type="render"))){
      imageData(img)[, , i] <- rescale(imageData(img)[, , i], to=c(0, 1))
    }
  }
  img
}

increaseContrast <- function(img, times=2, ...){
  stopifnot(is(img, "Image"))
  stopifnot(is.numeric(times))
  img*times
}
