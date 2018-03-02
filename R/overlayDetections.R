#' overlay detections
#' @description prepare data for display detections
#' @param imgsList output of \link{cropImages}.
#' @return an object of \link[EBImage:Image-class]{Image}.
#' @import EBImage
#' @export
#' 
overlayDetections <- function(imgsList){
  if(!is.list(imgsList) || 
     !all(c("alignedImage", "background", "backgroundRemoved", "detectedObjects") %in%
          names(imgsList))){
    stop("imgsList must be output of detectObjects")
  }
  overlay <- mapply(function(image, nmask){
    paintObjects(nmask, image)
  }, imgsList$alignedImage, imgsList$detectedObjects, SIMPLIFY = FALSE)
  overlay <- Reduce(combine, overlay)
  return(overlay)
}
