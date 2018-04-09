#' detect objects method1
#' @description detect object, good for dense objects.
#' @param img an object of \link[EBImage:Image-class]{Image} or \link{Image2}.
#' @param offset the offset of color from background, (0, 1).
#' @param objectSizeRange numeric(2). object size range in pixel.
#' @param size Brush size, see \link[EBImage:makeBrush]{makeBrush}.
#' @param tolerance,ext see \link[EBImage:watershed]{watershed}.
#' @param \dots not use.
#' @return an label Image.
#' @import EBImage
#' @importFrom scales rescale
#' @details steps: 
#' 1. use filter2 to creat local background.  
#' 2. the image is convert to 0 or 1 by local background + offset
#' 3. use fillHull to fill the holes
#' 4. use watershed to segment the objects.
#' 5. use bwlabel to label the objects
#' 6. filter the objects by size range. 
#' @export
#' @author Jianhong Ou
#'
detectObjects <- function(img, offset=.05, objectSizeRange=c(10, 1000), 
                        size=31, tolerance=1, ext=1, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")) img <- toImage(img)
  stopifnot(colorMode(img)==Grayscale)
  stopifnot(offset<1)
  stopifnot(offset>0)
  stopifnot(length(objectSizeRange)==2)
  stopifnot(is.numeric(objectSizeRange))
  disc <- makeBrush(size, shape="disc")
  disc <- disc/sum(disc)
  sample_bg <- filter2(img, disc)
  sample_th <- img > sample_bg + offset
  sample_th <- fillHull(sample_th)
  nmask <- watershed(distmap(sample_th), tolerance=tolerance, ext=ext)
  nmask <- bwlabel(nmask)
  ## filter by objectSizeRange
  filterBySize(nmask, objectSizeRange)
}
#' detect objects method2
#' @description detect object
#' @param img an object of \link[EBImage:Image-class]{Image} or \link{Image2}.
#' @param objectSizeRange numeric(2). object size range in pixel.
#' @param offset,w,h offset,w,h for \link[EBImage:thresh]{thresh}
#' @param size Brush size, see \link[EBImage:makeBrush]{makeBrush}.
#' @param tolerance,ext see \link[EBImage:watershed]{watershed}.
#' @param \dots not use.
#' @return an label Image.
#' @import EBImage
#' @importFrom scales rescale
#' @details steps: 
#' 1. all piexl - mean value, 
#' 2. rescale to increase contrast
#' detect the flys. 
#' 1. use opening to remove some samll objects 
#' 2. use fillHull to fill the holes.
#' 3. use watershed to segment the object 
#' 4. use bwlabel to label the object 
#' 5. filter the object by size range
#' @export
#' @author Jianhong Ou
#'
detectObjects2 <- function(img, offset=.2, objectSizeRange=c(10, 1000),
                           size=5, w=40, h=40, tolerance=1, ext=1, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")) img <- toImage(img)
  stopifnot(colorMode(img)==Grayscale)
  stopifnot(offset<1)
  stopifnot(offset>0)
  stopifnot(length(objectSizeRange)==2)
  stopifnot(is.numeric(objectSizeRange))
  img <- abs(img - mean(img))
  img <- medianFilter(img, size=5)
  img <- rescaleImage(img)
  nmask <- thresh(img, w=w, h=h, offset=offset)
  nmask = opening(nmask, makeBrush(size, shape='Gaussian'))
  nmask = fillHull(nmask)
  nmask <- watershed(distmap(nmask), tolerance=tolerance, ext=ext)
  nmask = bwlabel(nmask)
  filterBySize(nmask, objectSizeRange)
}


filterBySize <- function(nmask, objectSizeRange=c(10, 1000)){
  nmask2 <- list()
  for(i in seq.int(numberOfFrames(nmask, type="render"))){
    tbl <- table(imageData(getFrame(nmask, i, type="render")))
    tbl <- as.numeric(names(tbl)[tbl<objectSizeRange[1] | tbl>objectSizeRange[2]])
    nmask2[[i]] <- rmObjects(getFrame(nmask, i, type="render"), tbl, reenumerate=FALSE)
  }
  combine(nmask2)
}
#' detect objects method3
#' @description detect object, good for dense objects.
#' @param img an object of \link[EBImage:Image-class]{Image} or \link{Image2}.
#' @param offset,w,h offset,w,h for \link[EBImage:thresh]{thresh}
#' @param objectSizeRange numeric(2). object size range in pixel.
#' @param size see \link[EBImage:makeBrush]{makeBrush}
#' @param tolerance,ext see \link[EBImage:watershed]{watershed}.
#' @param \dots not use.
#' @return an label Image.
#' @import EBImage
#' @importFrom scales rescale
#' @importFrom stats fivenum
#' @details steps: 
#' 1. use watershed to segment the objects.
#' 2. use bwlabel to label the objects
#' 3. filter the objects by size range. 
#' @export
#' @author Jianhong Ou
#'
detectObjects3 <- function(img, offset=.1, objectSizeRange=c(50, 1000), 
                           size=5, w=40, h=40, tolerance=1, ext=1, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")) img <- toImage(img)
  stopifnot(colorMode(img)==Grayscale)
  stopifnot(offset<1)
  stopifnot(offset>0)
  stopifnot(length(objectSizeRange)==2)
  stopifnot(is.numeric(objectSizeRange))
  ## remove background
  img <- abs(img - mean(img))
  img <- increaseContrast(img, increaseContrast_times = 2)
  img <- medianFilter(img, size=5)
  ctmask <- thresh(img, w=w, h=h, offset=offset)
  
  ## label
  nmask <- watershed(distmap(ctmask), tolerance=tolerance, ext=ext)
  nmask <- bwlabel(nmask)
  cell.size <- table(imageData(nmask))
  cell.size <- cell.size[names(cell.size)!="0"]
  cell.size <- fivenum(cell.size)
  
  normalCells <- filterBySize(nmask, objectSizeRange = c(objectSizeRange[1], cell.size[4]))
  bigCells <- filterBySize(nmask, objectSizeRange = c(cell.size[4]+0.000001, 2*objectSizeRange[2]))
  while(sum(bigCells)>0){
    bmask <- erode(bigCells, makeBrush(size, shape='disc'))
    bmask <- bwlabel(bmask)
    normalCells <- normalCells + filterBySize(bmask, objectSizeRange = c(0, cell.size[4]))
    bigCells <- filterBySize(bmask, objectSizeRange = c(cell.size[4]+0.000001, cell.size[5]+1))
  }
  
  bwlabel(normalCells)
}
