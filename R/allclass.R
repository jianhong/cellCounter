setClassUnion("numericOrNULL",members=c("numeric", "NULL"))
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
#' @author Jianhong Ou
#' @examples 
#' cell()
#' 
setClass("cell", 
         representation(cx="numeric", cy="numeric", 
                        xs="numericOrNULL", ys="numericOrNULL", 
                        id="numeric", 
                        parent="numericOrNULL",
                        offsprings="numericOrNULL",
                        frame="numeric",
                        color="character"),
         prototype(cx=0, cy=0,
                   xs=NULL, ys=NULL,
                   id=0, parent=NULL,
                   offsprings=NULL,
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
#' Method parent
#' @rdname cell-class
#' @aliases parent,cell-method
#' @param x an object of cell.
#' @exportMethod parent
setGeneric("parent", function(x) standardGeneric("parent"))
setMethod("parent", "cell", function(x){
  slot(x, "parent")
})
#' Method parent<-
#' @rdname cell-class
#' @aliases parent<-,cell-method
#' @param value the value to be applied.
#' @exportMethod parent<-
setGeneric("parent<-", function(x, value) standardGeneric("parent<-"))
setReplaceMethod("parent", "cell", function(x, value){
  slot(x, "parent", check=TRUE) <- value
  x
})
#' Method offsprings
#' @rdname cell-class
#' @aliases offsprings,cell-method
#' @exportMethod offsprings
setGeneric("offsprings", function(x) standardGeneric("offsprings"))
setMethod("offsprings", "cell", function(x){
  slot(x, "offsprings")
})
#' Method offsprings<-
#' @rdname cell-class
#' @aliases offsprings<-,cell-method
#' @exportMethod offsprings<-
setGeneric("offsprings<-", function(x, value) standardGeneric("offsprings<-"))
setReplaceMethod("offsprings", "cell", function(x, value){
  slot(x, "offsprings", check=TRUE) <- value
  x
})
#' Mothod $
#' @rdname cell-class
#' @param name slot name of cell
#' @exportMethod $
#' @aliases $,cell-method
setMethod("$", "cell", function(x, name) slot(x, name))


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
#' @author Jianhong Ou
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
#' @param frames frames of the images to display
#' @export
toImage <- function(img2, frames){
  if(is(img2, "Image")) return(img2)
  d <- dim(img2)
  if(!missing(frames)){
    if(colorMode(img2)==Grayscale){
      if(length(d)==3){
        frames <- frames[frames<=d[3]&frames>0]
        d <- c(d[1:2], length(frames))
        imageData(img2) <- img2[, , frames]
      }
    }else{
      if(length(d)==4){
        frames <- frames[frames<=d[4]&frames>0]
        d <- c(d[1:3], length(frames))
        imageData(img2) <- img2[, , , frames]
      }
    }
  }
  Image(data=imageData(img2), dim=d, colormode=colorMode(img2))
}
#' Method imageData
#' @rdname Image2-class
#' @aliases imageData,Image2-method
#' @param y an \link{Image2} object.
#' @exportMethod imageData
setGeneric("imageData", function(y) standardGeneric("imageData"))
#' @rdname Image2-class
setMethod("imageData", "Image2", function(y){
  as(y, "array")
})
#' Method imageData<-
#' @rdname Image2-class
#' @aliases imageData<-,Image2-method
#' @param value the image data or colormode.
#' @exportMethod imageData<-
setGeneric("imageData<-", function(y, value) standardGeneric("imageData<-"))
#' @rdname Image2-class
setReplaceMethod("imageData", "Image2", function(y, value){
  if(!is(value, "DelayedArray")){
    value <- as(value, "HDF5Array")
  }
  for(sn in slotNames(value)) slot(y, sn, check=TRUE) <- slot(value, sn)
  y
})
#' Method colorMode
#' @rdname Image2-class
#' @aliases colorMode,Image2-method
#' @exportMethod colorMode
setGeneric("colorMode", function(y) standardGeneric("colorMode"))
setMethod("colorMode", "Image2", function(y){
  slot(y, "colormode")
})
#' Method colorMode<-
#' @rdname Image2-class
#' @aliases colorMode<-,Image2-method
#' @exportMethod colorMode<-
setGeneric("colorMode<-", function(y, value) standardGeneric("colorMode<-"))
setReplaceMethod("colorMode", "Image2", function(y, value){
  slot(y, "colormode", check=TRUE) <- value
  y
})
#' Method channel
#' @rdname Image2-class
#' @aliases channel,Image2-method
#' @param mode A character value specifying the target mode for conversion. See \link[EBImage:channel]{channel}.
#' @exportMethod channel
setGeneric("channel", function(x, mode) standardGeneric("channel"))
setMethod("channel", "Image2", function(x, mode){
  channel(toImage(x), mode)
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
#' @aliases display,Image2-method
#' @param method see \link[EBImage:display]{display}
#' @import EBImage
#' @exportMethod display
setGeneric("display", function(x, method, frames, ...) standardGeneric("display"))
setMethod ("display", "Image2", function(x, method, frames, ...) EBImage::display(toImage(x, frames), method, ...))

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

#' combind two Image2
#' @description combind two Image2
#' @param imgs a list of \link{Image2}
#' @import HDF5Array
#' @import DelayedArray
#' @return a object of \link{Image2} 
condense <- function(imgs){
  stopifnot(is.list(imgs))
  if(length(imgs)<2) return(imgs)
  d <- dim(imgs[[1]])
  ld <- length(d)+1
  null <- lapply(imgs, function(.ele){
    if(!is(.ele, "Image2")){
      stop("imgs must be a list of Image2")
    }
    if(!all(dim(.ele)==d)){
      stop("dimension are different")
    }
    .ele <- as(array(.ele, dim = c(d, 1)), "HDF5Array")
    aperm(.ele, ld:1)
  })
  imageData(imgs[[1]]) <- aperm(do.call(arbind, null), ld:1)
  imgs[[1]]
}
