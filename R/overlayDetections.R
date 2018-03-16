#' overlay detections
#' @description prepare data for display detections
#' @param imgs a list of \link[EBImage:Image-class]{Image} or \link{Image2}.
#' @param cells a list of \link{cell} objects.
#' @param col color of the labels.
#' @param \dots the parameters for \link[graphics:text]{text}
#' @param output \link[EBImage:Image-class]{Image} or \link{Image2}
#' @return an object of \link[EBImage:Image-class]{Image}, or a list of \link{Image2}.
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' 
overlayDetections <- function(imgs, cells, col="orange", output="Image", ...){
  stopifnot(is.list(imgs))
  stopifnot(is.list(cells))
  output <- match.arg(output, choices = c("Image", "Image2"))
  null <- lapply(imgs, function(.ele){
    if(!inherits(.ele, c("Image", "Image2"))){
      stop("imgs must be a list of Image or Image2")
    }
  })
  null <- lapply(cells, function(.ele){
    if(!inherits(.ele, c("cell"))){
      stop("cells must be a list of cell")
    }
  })
  overlay <- list()
  for(i in seq_along(imgs)){
    ## get x, y and labels for each cell
    cells.sub <- cells[sapply(cells, slot, name="frame")==i]
    x <- sapply(cells.sub, slot, name="cx")
    y <- sapply(cells.sub, slot, name="cy")
    labels <- sapply(cells.sub, slot, name="id")
    ## paste number
    nmask <- rasterText(imgs[[i]], x=x, y=y, labels=labels, col="white", ...)
    colorMode(nmask) <- Grayscale
    imageData(nmask) <- imageData(nmask)[, , 1]
    if(output=="Image") {
      overlay[[i]] <- paintObjects(nmask, toImage(imgs[[i]]), col=col)
    }else{
      overlay[[i]] <- Image2(paintObjects(nmask, toImage(imgs[[i]]), col=col))
    }
  }
  if(output=="Image"){
    overlay <- Reduce(combine, overlay)
  }
  return(overlay)
}

#' raster the text in a image
#' @description add text labels in a image
#' @param img an \link[EBImage:Image-class]{Image} or \link{Image2}
#' @param \dots the parameters for \link[graphics:text]{text}
#' @return an object of \link[EBImage:Image-class]{Image}.
#' @importFrom graphics text
#' @importFrom grDevices tiff dev.off
#' @import EBImage
#' @author Jianhong Ou
rasterText <- function(img, ...){
  stopifnot(inherits(img, c("Image", "Image2")))
  tmpfile <- tempfile()
  tiff(filename = tmpfile, width = nrow(img), height = ncol(img))
  display(matrix(0, nrow = nrow(img), ncol=ncol(img)), method="raster")
  text(...)
  dev.off()
  on.exit(unlink(tmpfile))
  readImage(tmpfile, type = "tiff")
}
