#' Class cell
#' @description An object of class cell represents a cell
#' @aliases cell
#' @rdname cell-class
#' @slot cx the center position of x
#' @slot cy the center position of y
#' @slot xs the positions of x in the cell
#' @slot ys the positions of y in the cell
#' @slot id the id of the cell
#' @slot parent the parent of the cell
#' @slot offsprings the offsprings of the cell
#' @slot frame the frame of the cell
#' @slot color the color the cell
#' @import methods
#' @exportClass cell
#' @examples 
#' cell()
#' 
setClass("cell", 
         representation(cx="numeric", cy="numeric", 
                        xs="numeric", ys="numeric", 
                        id="numeric", 
                        parent="numeric",
                        offsprings="numeric",
                        frame="numeric",
                        color="character"),
         prototype(cx=0, cy=0,
                   xs=0, ys=0,
                   id=0, parent=0,
                   offsprings=0,
                   frame=0,
                   color="#000000"),
         validity=function(object){
           if(length(object@xs)!=length(object@ys))
             return("the length of xs and ys must be identical.")
           return(TRUE)
         })
#' @rdname cell-class
#' @param \dots Each argument in \dots becomes an slot in the new cell.
#' @export
cell <- function(...){
  new("cell", ...)
}

#' Class Image2
#' @description An object of class Image2 represents a Image with HDF5Array
#' @aliases Image2
#' @rdname Image2-class
#' @slot seed,index,delayed_ops see \link[DelayedArray:DelayedArray-class]{DelayedArray}
#' @slot colormode colormode of the image.
#' @import methods
#' @import EBImage
#' @import DelayedArray
#' @import HDF5Array
#' @exportClass Image2
#' @examples
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter")) 
#' Image2(img)
#' 
setClass("Image2", 
         representation(colormode="integer"),
         prototype(colormode=0L),
         contains = 'DelayedArray')
#' @rdname Image2-class
#' @export
is.Image2 <- function(x){
  is(x, "Image2")
}
#' @rdname Image2-class
#' @param img an \link[EBImage:Image-class]{Image} object.
#' @export
Image2 <- function(img){
  stopifnot(is(img, "Image"))
  new("Image2", as(imageData(img), "HDF5Array"), 
      colormode=colorMode(img))
}
#' @rdname Image2-class
#' @aliases readImage2
#' @param file file name of image.
#' @export
readImage2 <- function(file){
  Image2(readImage(file))
}
#' @rdname Image2-class
#' @aliases writeImage2
#' @param x an object of \link{Image2}
#' @param \dots parameters could passed to \link[EBImage:io]{writeImage}.
#' @export
writeImage2 <- function(x, ...){
  writeImage(toImage(x), ...)
}
#' @rdname Image2-class
#' @aliases toImage,Image2-method
#' @param img2 an \link{Image2} object.
#' @export
toImage <- function(img2){
  if(is(img2, "Image")) return(img2)
  Image(data=imageData(img2), dim=dim(img2), colormode=colorMode(img2))
}
#' Method imageData
#' @rdname Image2-class
#' @aliases imageData,Image2-method
#' @param y an \link{Image2} object.
#' @exportMethod imageData
setGeneric("imageData", function(y) sstandardGeneric("imageData"))
#' @rdname Image2-class
setMethod("imageData", "Image2", function(y){
  as(y, "array")
})
#' Method imageData<-
#' @rdname Image2-class
#' @aliases imageData<-,Image2-method
#' @param value the image data or colormode.
#' @exportMethod imageData<-
setGeneric("imageData<-", function(y, value) sstandardGeneric("imageData"))
#' @rdname Image2-class
setReplaceMethod("imageData", "Image2", function(y, value){
  if(is(value, "DelayedArray")){
    value <- as(value, "HDF5Array")
  }
  for(sn in slotNames(value)) slot(y, sn, check=TRUE) <- slot(value, sn)
  y
})
#' Method colorMode
#' @rdname Image2-class
#' @aliases colorMode,Image2-method
#' @exportMethod colorMode
setGeneric("colorMode", function(y) sstandardGeneric("colorMode"))
setMethod("colorMode", "Image2", function(y){
  slot(y, "colormode")
})
#' Method colorMode<-
#' @rdname Image2-class
#' @aliases colorMode<-,Image2-method
#' @exportMethod colorMode<-
setGeneric("colorMode<-", function(y, value) sstandardGeneric("colorMode<-"))
setReplaceMethod("colorMode", "Image2", function(y, value){
  slot(y, "colormode", check=TRUE) <- value
  y
})

#' Method show
#' @rdname Image2-class
#' @aliases show,Image2-method
#' @param object an \link{Image2} object.
#' @exportMethod show
setMethod ("show", "Image2", function(object) showImage2(object))
print.Image2 <- function(x, ...) showImage2(x, ...)
showImage2 <- function(object){
  show(toImage(object))
}

#' Method display
#' @rdname Image2-class
#' @param method see \link[EBImage:display]{display}
#' @import EBImage
#' @exportMethod display
setMethod ("display", "Image2", function(x, method, ...) EBImage::display(toImage(x), method, ...))

#' read a list of image
#' @description use reasonable memory to read a list of images
#' @param files filenames of the images
#' @param ... parameters Not used.
#' @import HDF5Array
#' @import EBImage
#' @import DelayedArray
#' @export
#' @return a list of \link{Image2}
readListImg <- function(files, ...){
  imgs <- lapply(files, readImage2)
  names(imgs) <- basename(files)
  imgs
}

