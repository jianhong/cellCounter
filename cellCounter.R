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
#' @param saveAdjustImage the file name for adjusted image. NULL to ignore saving.
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
#' .jinit(parameters=c("-Xms1g","-Xmx4g"))
#' library(RBioFormats) ## to support czi file, try to install by devtools::install_github("aoles/RBioFormats")
#' cellCounter(file.path("inst", "extdata", "sample.tiff"), xmlfile="sample.xml", imageFilename="sample.czi")
#' 
cellCounter <- function(file, channel="green", offset=0.05, cellSizeRange=c(20, 1000), 
                        distanceSameCell=10, 
                        xmlfile=sub("\\.(tiff|tif)$", ".cellCounter.xml", file, ignore.case = TRUE),
                        imageFilename=sub("\\.(tiff|tif)$", ".czi", basename(file), ignore.case = TRUE),
                        counterType=c(prenew=5, new=4, old=6), zvalue=fixZvalue,
                        adjustPipeline=c(GaussianBlur, ScurveAdjust),
                        saveAdjustImage=sub("\\.(tiff|tif)$", 
                                            paste0(".adj.", channel, ".tif"), 
                                            basename(file), ignore.case = TRUE),
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
  img <- detectCells(img, offset=offset, cellSizeRange=cellSizeRange, ...)
  ## coutput the xml
  if(!silence) message("output xml file")
  saveCountXML(img, xmlfile = xmlfile, file = imageFilename, distance = distanceSameCell, 
               counterType=counterType, zv=zvalue, ...)
}

readFile <- function(file, channel=NULL){
  stopifnot(file.exists(file))
  ## read image
  if(endsWith(file, ".czi")){
    m <- read.metadata(file = file)$globalMetadata
    imageFilename <- m[["Information|Document|Title #1"]]
    setsubset <- FALSE
    if(!is.null(channel)){
      channelAvailable <-
        m[startsWith(names(m), "Experiment|AcquisitionBlock|MultiTrackSetup|TrackSetup|Detector|Color")]
      names(channelAvailable) <- 
        sub("Experiment|AcquisitionBlock|MultiTrackSetup|TrackSetup|Detector|Color #", "", 
            names(channelAvailable), fixed = TRUE)
      colorMap <- c("red"="#FF0000", "green"="#00FF00", "blue"="#0000FF")
      channelAvailable <- unlist(channelAvailable)
      if(colorMap[channel] %in% channelAvailable){
        subset <- list(C=as.numeric(names(channelAvailable)[channelAvailable==colorMap[channel]]))
        setsubset <- TRUE
      }
    }
    if(setsubset){
      img <- read.image(file = file,
                        proprietary.metadata = FALSE,
                        normalize = TRUE,
                        subset = subset)
    }else{
      img <- read.image(file = file,
                        proprietary.metadata = FALSE,
                        normalize = TRUE)
    }
    
    list(img=img, imageFilename=imageFilename)
  }else{
    img <- readImage(file)
    ## remove the alpha channel
    img <- toRGB(img)
    if(length(dim(img)>3)){
      if(dim(img)[3]>3){
        img <- img[, , 1:3, ]
      }
    }
    if(is.null(channel)) return(list(img=img, imageFilename=NULL))
    list(img=channel(img, channel), imageFilename=imageFilename)
  }
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

euc.dist <- function(x1, y1, x2, y2) sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
fixZvalue <- function(n) 2*n-1
saveCountXML <- function(nmask, xmlfile="cellCounter.xml", file, distance=10, counterType=c(prenew=6, new=4, old=3), zv=fixZvalue, ...){
  stopifnot(is(nmask, "Image"))
  stopifnot(all(counterType>0))
  stopifnot(all(counterType<9))
  counterTypeTable <- data.frame(x=NA, y=NA, z=NA, id=0)
  tbl.old <- NULL
  lastCounts <- 0
  for(i in seq.int(numberOfFrames(nmask, type="render"))){
    tbl.new <- computeFeatures.moment(getFrame(nmask, i, type="render"))
    if(length(tbl.new)>0){
      tbl.new <- cbind(tbl.new, counts=rep(0, nrow(tbl.new)))
      if(length(tbl.old)>0){
        newId <- rep(seq.int(nrow(tbl.new)), each=nrow(tbl.old))
        tbl.new1 <- tbl.new[newId, ]
        oldId <- rep(seq.int(nrow(tbl.old)), times=nrow(tbl.new))
        tbl.old1 <- tbl.old[oldId, ]
        dists <- euc.dist(tbl.new1[, "m.cx"], tbl.new1[, "m.cy"], tbl.old1[, "m.cx"], tbl.old1[, "m.cy"])
        ## select minimal distance for each new,old pair
        tbl.new1 <- data.frame(newId, oldId, dists)
        tbl.new1 <- split(tbl.new1, newId)
        tbl.new1 <- lapply(tbl.new1, function(.ele){
          .ele[which.min(.ele[, 3])[1], ]
        })
        tbl.new1 <- do.call(rbind, tbl.new1)
        oldCells <- tbl.new1[, 3]<distance
        if(sum(oldCells)>0){
          tbl.new[tbl.new1[oldCells, 1], "counts"] <- tbl.old[tbl.new1[oldCells, 2], "counts"]
        }
        if(sum(!oldCells)>0){
          tbl.new[!oldCells, "counts"] <- seq.int(sum(!oldCells)) + lastCounts
        }
      }else{
        tbl.new[, "counts"] <- seq.int(nrow(tbl.new)) + lastCounts
      }
      cx <- round(tbl.new[, "m.cx"])
      cy <- round(tbl.new[, "m.cy"])
      counterTypeTable <- rbind(counterTypeTable,
                                data.frame(x=cx,
                                           y=cy,
                                           z=zv(i),
                                           id=tbl.new[, "counts"]))
      lastCounts <- max(counterTypeTable[, "id"], na.rm = TRUE)
    }
    tbl.old <- tbl.new
  }
  counterTypeTable <- counterTypeTable[!is.na(counterTypeTable$x), , drop=FALSE]
  counterTypeTable <- split(counterTypeTable, counterTypeTable$id)
  counterTypeTable <- lapply(counterTypeTable, function(.ele){
    i <- max(floor(nrow(.ele)/2), 1)
    .ele$type <- "prenew"
    .ele$type[i:nrow(.ele)] <- "old"
    .ele$type[i] <- "new"
    .ele
  })
  counterTypeTable <- do.call(rbind, counterTypeTable)
  counterTypeTable <- split(counterTypeTable, counterTypeTable$type)
  suppressWarnings({xml <- xmlTree("CellCounter_Marker_File")})
  xml$addNode("Image_Properties", close = FALSE)
    xml$addNode("Image_Filename", file)
  xml$closeTag()
  xml$addNode("Marker_Data", close = FALSE)
    xml$addNode("Current_Type", 1)
    for(type in seq.int(8)){
      xml$addNode("Marker_Type", close = FALSE)
      xml$addNode("Type", type)
      null <- mapply(function(ctt, nameOfctt){
        if(counterType[nameOfctt]==type){
          for(m in seq.int(nrow(ctt))){
            xml$addNode("Marker", close = FALSE)
            xml$addNode("MarkerX", ctt$x[m])
            xml$addNode("MarkerY", ctt$y[m])
            xml$addNode("MarkerZ", ctt$z[m])
            xml$closeTag()
          }
        }
      },counterTypeTable, names(counterTypeTable))
      xml$closeTag()
    }
  xml$closeTag()
  cat(saveXML(xml, prefix = '<?xml version="1.0" encoding = "UTF-8"?>'), file = xmlfile, sep="\n")
}

