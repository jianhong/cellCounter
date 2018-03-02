#' GaussianBlur
#' @description apply Gaussian blur to a Image
#' @param img an object of \link[EBImage:Image-class]{Image}.
#' @param GaussianBlur_sigma sigma for \link[EBImage:gblur]{gblur}.
#' @param \dots not used.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' @examples
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
#' GaussianBlur(img)
GaussianBlur <- function(img, GaussianBlur_sigma=5, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")){
    img <- toImage(img)
  }
  gblur(img, sigma = GaussianBlur_sigma)
}
#' rescaleImage
#' @description apply rescale Image data to [0, 1].
#' @param img an object of \link[EBImage:Image-class]{Image}.
#' @param \dots not used.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' @examples 
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
#' rescaleImage(channel(img, "blue"))
rescaleImage <- function(img, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")){
    img <- toImage(img)
  }
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

#' increaseContrast
#' @description increase the Image contrast by multiply the image data by a value.
#' @param img an object of \link[EBImage:Image-class]{Image}.
#' @param increaseContrast_times a numeric vector.
#' @param \dots not used.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' @examples 
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
#' increaseContrast(img)
increaseContrast <- function(img, increaseContrast_times=2, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")){
    img <- toImage(img)
  }
  stopifnot(is.numeric(increaseContrast_times))
  img*increaseContrast_times
}

#' removeBackground
#' @description substract the background from Image data. 
#' If background is not given, it will be the median of the Image.
#' @param img an object of \link[EBImage:Image-class]{Image}.
#' @param removeBackground_background background cut off value.
#' @param \dots not used.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' @examples 
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
#' removeBackground(img)
removeBackground <- function(img, removeBackground_background, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")){
    img <- toImage(img)
  }
  data <- imageData(img)
  if(missing(removeBackground_background)){
    data[data==0] <- NA
    removeBackground_background <- median(data, na.rm = TRUE)
  }
  data[is.na(data)] <- 0
  data[data<removeBackground_background[1]] <- 0
  imageData(img) <- data
  img
}

#' ScurveAdjust
#' @description apply S-curve adjust for a Image by formula: data = L / (1 + exp (-k * (data - x0)))
#' @param img an object of \link[EBImage:Image-class]{Image}.
#' @param ScurveAdjust_k slope
#' @param ScurveAdjust_L max value
#' @param ScurveAdjust_x0 mean value
#' @param \dots not used.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' @examples 
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
#' ScurveAdjust(img)
ScurveAdjust <- function(img, ScurveAdjust_k=10, ScurveAdjust_L=1, ScurveAdjust_x0=0.5, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")){
    img <- toImage(img)
  }
  data <- imageData(img)
  data <- ScurveAdjust_L / (1 + exp(-ScurveAdjust_k*(data-ScurveAdjust_x0)))
  imageData(img) <- data
  img
}

#' changeCellSize
#' @description apply rescale Image data to [0, 1]
#' @param img an object of \link[EBImage:Image-class]{Image}.
#' @param changeCellSize_direction "erode" or "dilate". erode make the cell size smaller 
#'        and dilate make the cell size bigger.
#' @param changeCellSize_targetChannel the target channel.
#' @param changeCellSize_size,changeCellSize_shape Brush size and shape, see \link[EBImage:makeBrush]{makeBrush}
#' @param \dots not used.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' @examples 
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
#' changeCellSize(img)
changeCellSize <- function(img, changeCellSize_direction=c("erode", "dilate"), 
                           changeCellSize_targetChannel=c("red", "green", 'blue'), 
                           changeCellSize_size=3, changeCellSize_shape="disc", ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  if(is(img, "Image2")){
    img <- toImage(img)
  }
  changeCellSize_direction <- match.arg(changeCellSize_direction)
  kern <- makeBrush(size=changeCellSize_size, shape=changeCellSize_shape)
  if(colorMode(img)==0){
    return(switch(changeCellSize_direction,
                  erode=erode(img, kern),
                  dilate=dilate(img, kern)))
  }
  changeCellSize_targetChannel <- match.arg(changeCellSize_targetChannel, several.ok = TRUE)
  imgs <- lapply(c("red", "green", "blue"), channel, x=img)
  names(imgs) <- c("red", "green", "blue")
  imgs.new <- lapply(imgs, function(im){
    switch(changeCellSize_direction,
           erode=erode(im, kern),
           dilate=dilate(im, kern))
  }) 
  for(i in changeCellSize_targetChannel){
    imgs[[i]] <- imgs.new[[i]]
  }
  rgbImage(imgs[["red"]], imgs[["green"]], imgs[["blue"]])
}
