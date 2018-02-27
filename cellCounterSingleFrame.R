#' cell counter
#' @description count the cells from a single frame tiff file and output xml for imageJ
#' @param file tiff file name
#' @param channels channel to be detected
#' @param formula the relationship of channels. 
#'      eg. blue*green+red means the cell must have blue and green, and red is in the cell.
#'          blue*green-red means the cell must have blue and green, and red is NOT in the cell.
#'          blue*green means the cell must have blue and green
#'          blue+red means the cell must have blue, and red is in the cell.
#' @param maskValue the cutoff value for set the mask is TRUE.
#' @param offset the offset of color from background, (0, 1).
#' @param cellSizeRange cell size range
#' @param xmlfile filename of xml
#' @param imageFilename filename of the original czi file
#' @param adjustPipeline adjust pipeline before cell detection
#' @param saveAdjustImage the file name for adjusted image. NULL to ignore saveing.
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
#' cellCounterSingleFrame(file.path("inst", "extdata", "low.jpg"), formula="blue+red", xmlfile="low.xml", imageFilename="low.jpg")
cellCounterSingleFrame <- function(file, channels=c("red", "green", "blue"), formula, maskValue=0.3, 
                        offset=0.1, cellSizeRange=c(10, 1000), 
                        xmlfile=sub("\\.(tiff|tif)$", ".cellCounter.xml", file, ignore.case = TRUE),
                        imageFilename=sub("\\.(tiff|tif)$", ".czi", basename(file), ignore.case = TRUE),
                        adjustPipeline=c(ScurveAdjust),
                        saveAdjustImage=sub("\\.(tiff|tif)$", 
                                            paste0(".adj.", channel, ".tif"), 
                                            basename(file), ignore.case = TRUE),
                        silence=FALSE, ...){
  channels <- match.arg(channels, choices = c("green", "red", "blue"), several.ok = TRUE)
  stopifnot(offset<1)
  stopifnot(offset>0)
  if(!silence) message("reading tiff file")
  img <- readFile(file)
  ## pre adjust
  if(!silence) message("adjust image file")
  for(fun in adjustPipeline){
    stopifnot(is.function(fun))
    img <- fun(img=img, ...)
  }
  if(is.character(saveAdjustImage)){
    writeImage(img, files = saveAdjustImage, type="TIFF")
  }
  
  zmap <- colorZmap(channels)
  zmap <- zmap[rowSums(zmap)>0, , drop=FALSE]
  
  if(!missing(formula)){
    formula <- gsub("\\s+", "", formula[1])
    formula1 <- strsplit(formula, "\\+|-|\\*")[[1]]
    if(!all(formula1 %in% c("red", "green", "blue"))){
      stop("channels in formula is not in blue, green or red.")
    }
    if(any(table(formula1)>1)){
      stop("channels in formula can be appeared only once.")
    }
    operators <- strsplit(formula, "[a-zA-Z]+")[[1]]
    operators <- operators[operators!=""]
    if(length(operators)>2){
      stop("formula is too complicated!")
    }
    if(any(operators=="*")){
      oid <- which(operators=="*")
      if(length(oid)==1){
        oid <- zmap[, formula1[oid]] & zmap[, formula1[oid+1]] & rowSums(zmap)!=3
      }else{
        oid <- rowSums(zmap)==3
      }
      zmap <- zmap[rowSums(zmap)==1 | oid, , drop=FALSE]
    }else{
      zmap <- zmap[rowSums(zmap)==1, , drop=FALSE]
    }
  }else{
    formula1 <- NULL
    operators <- NULL
    zmap <- zmap[rowSums(zmap)==1, , drop=FALSE]
  }
  
  img <- lapply(channels, channel, x=img)
  names(img) <- channels
  img <- lapply(seq.int(nrow(zmap)), function(.ele){
    imgs <- img[colnames(zmap)[zmap[.ele, ]]]
    setImage(imgs, "intersect", maskValue)
  })
  img <- combine(img)
  ## detect the cells
  if(!silence) message("detecting cells")
  img <- detectCells(img, offset=offset, cellSizeRange=cellSizeRange, ...)
  if("+" %in% operators){
    ## remove object by `+`
    oid <- which(operators=="+")
    newChannel <- NULL
    for(ioid in oid){
      if(length(newChannel)==0){
        plusLeft <- formula1[ioid]
        if(ioid>1){
          if(operators[ioid-1]=="*"){
            plusLeft <- formula1[c(ioid-1, ioid)]
          }
        }
        idLeft <- which(rowSums(zmap[, plusLeft, drop=FALSE])==length(plusLeft))
        frameLeft <- getFrame(img, idLeft, type="render")
      }else{
        frameLeft <- newChannel
      }
      plusRight <- formula1[ioid+1]
      if(ioid+1<length(operators)){
        if(operators[ioid+2]=="*"){
          plusRight <- formula1[c(ioid+1, ioid+2)]
        }
      }
      ## plusRight must be in plusLeft
      idRight <- which(rowSums(zmap[, plusRight, drop=FALSE])==length(plusRight))
      frameRight <- getFrame(img, idRight, type="render")
      tbl0 <- table(imageData(frameLeft))
      tbl <- imageData(frameLeft) * as.numeric(imageData(frameRight)>0)
      tbl <- table(tbl)
      tbl <- as.numeric(names(tbl0)[!names(tbl0) %in% names(tbl[tbl>0])])
      newChannel <- rmObjects(frameLeft, tbl, reenumerate=FALSE)
    }
    img <- combine(img, newChannel)
  }
  ## coutput the xml
  if(!silence) message("output xml file")
  saveSingleFrameCountXML(img, xmlfile = xmlfile, file = imageFilename, zmap=zmap, ...)
  typelist <- sapply(seq.int(nrow(zmap)), function(.ele) paste(colnames(zmap)[zmap[.ele, ]], collapse=" + "))
  names(typelist) <- paste0("Type", seq_along(typelist))
  typelist
}

colorZmap <- function(channels=c("red", "green", "blue")){
  ncontrasts <- length(channels)
  noutcomes <- 2^ncontrasts
  outcomes <- matrix(0, noutcomes, ncontrasts)
  colnames(outcomes) <- channels
  for(j in 1:ncontrasts){
    outcomes[, j] <- rep(0:1, times=2^(j-1), each=2^(ncontrasts-j))
  }
  mode(outcomes) <- "logical"
  outcomes
}

avgImage <- function(imgs){
  ## imgs is a list of Image object
  if(is.list(imgs)){
    if(!all(sapply(imgs, is, class2="Image"))){
      stop("imgs should be a list of Image objects")
    }
  }else{
    stopifnot(is(imgs, "Image"))
  }
  n <- length(imgs)
  if(n==1) return(imgs[[1]])
  imgs <- lapply(imgs, function(.ele) .ele/n)
  Reduce(`+`, imgs)
}

## set operator
setImage <- function(imgs, operater=c("intersect", "union", "diff"), maskValue=0.5, ...){
  if(is.list(imgs)){
    if(!all(sapply(imgs, is, class2="Image"))){
      stop("imgs should be a list of Image objects")
    }
  }else{
    stopifnot(is(imgs, "Image"))
  }
  operater <- match.arg(operater)
  FUN <- switch(operater,
                intersect=`&`,
                union=`|`,
                diff=function(A, B){
                  A & !B
                })
  n <- length(imgs)
  if(n==1) return(imgs[[1]])
  imgs2 <- lapply(imgs, function(.ele){
    .ele > maskValue
  })
  imgs2 <- Reduce(FUN, imgs2)
  imgs <- lapply(imgs, function(.ele) .ele*imgs2)
  avgImage(imgs)
}

saveSingleFrameCountXML <- function(nmask, xmlfile="cellCounter.xml", file, zmap, ...){
  stopifnot(is(nmask, "Image"))
  counterTypeTable <- data.frame(x=NA, y=NA, z=NA, type=NA)
  for(i in seq.int(numberOfFrames(nmask, type="render"))){
    tbl <- computeFeatures.moment(getFrame(nmask, i, type="render"))
    if(length(tbl)>0){
      cx <- round(tbl[, "m.cx"])
      cy <- round(tbl[, "m.cy"])
      cz <- ifelse(i<=nrow(zmap), sum(zmap[i, ]), 3)
      counterTypeTable <- rbind(counterTypeTable,
                                data.frame(x=cx,
                                           y=cy,
                                           z=cz, 
                                           type=i))
    }
  }
  counterTypeTable <- counterTypeTable[!is.na(counterTypeTable$x), , drop=FALSE]
  suppressWarnings({xml <- xmlTree("CellCounter_Marker_File")})
  xml$addNode("Image_Properties", close = FALSE)
  xml$addNode("Image_Filename", file)
  xml$closeTag()
  xml$addNode("Marker_Data", close = FALSE)
  xml$addNode("Current_Type", 1)
  for(type in seq.int(8)){
    xml$addNode("Marker_Type", close = FALSE)
    xml$addNode("Type", type)
    counterType <- counterTypeTable[counterTypeTable$type==type, , drop=FALSE]
    if(nrow(counterType)>0){
      for(m in seq.int(nrow(counterType))){
        xml$addNode("Marker", close = FALSE)
        xml$addNode("MarkerX", counterType$x[m])
        xml$addNode("MarkerY", counterType$y[m])
        xml$addNode("MarkerZ", counterType$z[m])
        xml$closeTag()
      }
    }
    xml$closeTag()
  }
  xml$closeTag()
  cat(saveXML(xml, prefix = '<?xml version="1.0" encoding = "UTF-8"?>'), file = xmlfile, sep="\n")
}
