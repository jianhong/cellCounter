#' crop the images
#' @description crop the image by the targeting region
#' @param imgsList output of \link{rmBackground}.
#' @param positions a list of positions of x and y for crop. They are lefttop corner and rightbottom corner.
#' @return a list of cropped alignedImage, background and backgroundRemoved.
#' @import EBImage
#' @importFrom graphics locator plot
#' @export
#' 
cropImages <- function(imgsList, positions){
  if(!is.list(imgsList) || 
     !all(c("alignedImage", "background", "backgroundRemoved") %in%
          names(imgsList))){
    stop("imgsList must be output of rmBackground")
  }
  if(missing(positions)){
    plot(imgsList$background)
    message("click the leftop and then rightbottom corner.")
    positions <- locator(2)
  }else{
    stopifnot(is.list(positions))
    stopifnot("x" %in% names(positions))
    stopifnot("y" %in% names(positions))
    stopifnot(length(positions$x)==2)
    stopifnot(length(positions$y)==2)
  }
  positions <- lapply(positions, round)
  positions <- lapply(positions, sort)
  lapply(imgsList, function(.ele){
    if(is.list(.ele)){
      lapply(.ele, function(.e) .e[positions$x[1]:positions$x[2], 
                                   positions$y[1]:positions$y[2], ])
    }else{
      .ele[positions$x[1]:positions$x[2], 
           positions$y[1]:positions$y[2], ]
    }
  })
}