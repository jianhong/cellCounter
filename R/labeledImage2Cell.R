#' convert the labeled imaged to a list of cell object
#' @description Convert the output of detectObjects functions into a list of cell object
#' @param imgs a list of \link[EBImage:Image-class]{Image} or \link{Image2}. It should be output of detectObjects functions.
#' @param colorChannel character(1). The color channel for the cell.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#'
labeledImage2Cell <- function(imgs, colorChannel=c(red="#FF0000", green="#00FF00", blue="#0000FF")){
  #colorChannel <- match.arg(colorChannel)
  stopifnot(is.character(colorChannel))
  stopifnot(length(colorChannel)==1)
  if(!is.list(imgs)){
    stopifnot(inherits(imgs, c("Image", "Image2")))
    stopifnot(colorMode(imgs)==Grayscale)
    if(is(imgs, "Image2")) imgs <- toImage(imgs)
    imgs <- lapply(seq.int(numberOfFrames(imgs, type="render")), 
                  getFrame, y=imgs, type = "render")
  }
  if(is.list(imgs)){
    null <- lapply(imgs, function(.ele){
      if(!inherits(.ele, c("Image", "Image2"))){
        stop("A list of Image or Image2 is required.")
      }
      if(colorMode(.ele)!=Grayscale){
        stop("Color mode of image must be Grayscale.")
      }
    })
  }
  cells <- list()
  counter <- 0
  for(i in seq_along(imgs)){
    img <- imgs[[i]]
    if(is(img, "Image2")) img <- toImage(img)
    moment <- computeFeatures.moment(img)
    if(length(moment)>0){
      moment <- round(moment)
      for(j in rownames(moment)){
        counter <- counter + 1
        cx <- moment[j, "m.cx"]
        cy <- moment[j, "m.cy"]
        xys <- getXYs(as.numeric(j), img)
        cells[[counter]] <- cell(cx=cx,
                                 cy=cy,
                                 xs=xys$x, ys=xys$y,
                                 id=counter, parent=NULL,
                                 offsprings=NULL,
                                 frame=i,
                                 color=colorChannel)
        
      }
    }
  }
  cells
}

getXYs <- function(j, img){
  ids <- which(imageData(img)==j)
  dm <- dim(img)
  stopifnot(length(dm)==2) ## Grayscale only
  #ids = (col_number - 1) * nrow + row_number
  x <- ids %% dm[1] + 1
  y <- ids %/% dm[1] + 1
  return(list(x=x, y=y))
}
