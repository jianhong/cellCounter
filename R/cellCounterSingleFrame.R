#' cell counter
#' @description count the cells from a single frame tiff file and output xml for imageJ
#' @param file tiff file name
#' @param channels channel to be detected
#' @param formula the relationship of channels. Here '*'=='intersect' and '+'=='contain'
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
#' @param detectFun the function used to detect the cell, see \link{detectObjects}, \link{detectObjects2}.
#' @param saveAdjustImage the file name for adjusted image. NULL to ignore saveing.
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
#' cellCounterSingleFrame(system.file("extdata", "low.jpg", package="cellCounter"), 
#' formula="blue+red", xmlfile="low.xml", imageFilename="low.jpg")
cellCounterSingleFrame <- function(file, channels=c("red", "green", "blue"), formula, maskValue=0.3, 
                        offset=0.1, cellSizeRange=c(10, 1000), 
                        xmlfile=sub("\\.(tiff|tif)$", ".cellCounter.xml", file, ignore.case = TRUE),
                        imageFilename=sub("\\.(tiff|tif)$", ".czi", basename(file), ignore.case = TRUE),
                        adjustPipeline=c(ScurveAdjust),
                        detectFun=detectObjects,
                        saveAdjustImage=NULL,
                        silence=FALSE, ...){
  channels <- match.arg(channels, choices = c("green", "red", "blue"), several.ok = TRUE)
  stopifnot(offset<1)
  stopifnot(offset>0)
  if(!silence) message("reading tiff file")
  img <- readFile(file)$img
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
    formula1 <- strsplit(formula, "\\+|-|\\*")
    operators <- strsplit(formula, "[a-zA-Z]+")
    operators <- lapply(operators, function(.ele) .ele[.ele!=""])
    keep <- rep(FALSE, nrow(zmap))
    for(i in seq_along(formula1)){
      if(!all(formula1[[i]] %in% c("red", "green", "blue"))){
        stop("channels in formula is not in blue, green or red.")
      }
      if(any(table(formula1[[i]])>1)){
        stop("channels in formula can be appeared only once.")
      }
      if(length(operators[[i]])>2){
        stop("formula is too complicated!")
      }
      if(any(operators[[i]]=="*")){
        oid <- which(operators[[i]]=="*")
        if(length(oid)==1){
          keep[zmap[, formula1[[1]][oid]] & zmap[, formula1[[1]][oid+1]] & rowSums(zmap)!=3] <- TRUE
        }else{
          keep[rowSums(zmap)==3] <- TRUE
        }
      }
    }
    zmap <- zmap[rowSums(zmap)==1 | keep, , drop=FALSE]
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
  img <- detectFun(img, offset=offset, cellSizeRange=cellSizeRange, ...)
  ## remove object for different counter type
  for(i in seq_along(formula)){
    if("+" %in% operators[[i]]){
      ## remove object by `+`
      oid <- which(operators[[i]]=="+")
      newChannel <- NULL
      for(ioid in oid){
        if(length(newChannel)==0){
          plusLeft <- formula1[[i]][ioid]
          if(ioid>1){
            if(operators[[i]][ioid-1]=="*"){
              plusLeft <- formula1[[i]][c(ioid-1, ioid)]
            }
          }
          idLeft <- which(rowSums(zmap[, plusLeft, drop=FALSE])==length(plusLeft))
          frameLeft <- getFrame(img, idLeft, type="render")
        }else{
          frameLeft <- newChannel
        }
        plusRight <- formula1[[i]][ioid+1]
        if(ioid+1<length(operators[[i]])){
          if(operators[[i]][ioid+2]=="*"){
            plusRight <- formula1[[i]][c(ioid+1, ioid+2)]
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
  }
  
  ## coutput the xml
  if(!silence) message("output xml file")
  saveSingleFrameCountXML(img, xmlfile = xmlfile, file = imageFilename, zmap=zmap, formula=formula, ...)
}
