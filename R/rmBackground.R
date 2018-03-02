#' remove background
#' @description remove the background based on a batch of image
#' @param imgs a list of \link[EBImage:Image-class]{Image} or \link{Image2}.
#' @return a list of alignedImage, background and backgroundRemoved.
#' @import EBImage
#' @export
#' 
rmBackground <- function(imgs){
  stopifnot(is.list(imgs))
  stopifnot(length(imgs)>1)
  null <- lapply(imgs, function(.ele){
    if(!inherits(.ele, c("Image", "Image2"))){
      stop("imgs must be a list of Image/Image2 objects")
    }
  })
  ali <- lapply(imgs[-1], function(.ele) alignmentImageData(imgs[[1]], .ele))
  ali <- do.call(rbind, ali)
  ali.min <- apply(ali, 2, min)
  ali.max <- apply(ali, 2, max)
  ## make ali.max to be 0
  ali.adjust <- t(t(ali) - ali.max)
  ali.adjust <- rbind(ali.max, ali.adjust)
  for(i in seq.int(nrow(ali))){
    imgs[[i]] <- adjustImg(imgs[[i]], ali.adjust[i, ])
  }
  background <- getBackgroundAfterAlignment(imgs)
  imgs.rmbck <- lapply(imgs, function(.ele) .ele - background)
  list(alignedImage=imgs, background=background, backgroundRemoved=imgs.rmbck)
}

#' alignment the images
#' @description get offset of two of \link[EBImage:Image-class]{Image}.
#' Can not handle rotation now.
#' @param figureA,figureB \link[EBImage:Image-class]{Image} object.
#' @return offset of x and y
#' @import EBImage
#' @importFrom stats median ccf
alignmentImageData <- function(figureA, figureB){
  ## get features, the process to find hilight spot, low light spot and edges
  channels <- c(r="red", g="green", b="blue")
  A <- lapply(channels, function(.ele) imageData(channel(toImage(figureA), .ele)))
  B <- lapply(channels, function(.ele) imageData(channel(toImage(figureB), .ele)))
  ### hi-constraction transform
  hiCt <- function(x){
    m <- median(x)
    x[x>m] <- 1
    x[x<=m] <- 0
    x
  }
  A.hi <- lapply(A, hiCt)
  B.hi <- lapply(B, hiCt)
  ## align the features
  ## select one channel by mean
  hi <- mapply(function(.a, .b){
    abs(mean(.a - .b))
  }, A.hi, B.hi)
  hi <- names(hi)[which.min(hi)]
  row.ccf <- ccf(rowSums(A.hi[[hi]]), rowSums(B.hi[[hi]]), plot=FALSE, lag.max = nrow(A.hi[[hi]])/10)
  col.ccf <- ccf(colSums(A.hi[[hi]]), colSums(B.hi[[hi]]), plot=FALSE, lag.max = nrow(A.hi[[hi]])/10)
  .id <- which.max(row.ccf$acf[, 1, 1])
  row.ccf <- ifelse(length(.id)>0, row.ccf$lag[.id[1], 1, 1], NA)
  .id <- which.max(col.ccf$acf[, 1, 1])
  col.ccf <- ifelse(length(.id)>0, col.ccf$lag[.id[1], 1, 1], NA)
  ## export the aligned figures
  c(row.ccf, col.ccf)
}

#' adjust image
#' @description \link[EBImage:Image-class]{Image} based on output of \link{alignmentImageData}
#' @param img an object of \link[EBImage:Image-class]{Image}
#' @param offset offset of the image.
#' @return an object of \link[EBImage:Image-class]{Image}
#' @import EBImage
#' 
adjustImg <- function(img, offset){
  stopifnot(all(offset<=0))
  offset <- abs(offset)
  figure <- imageData(img)
  for(j in 1:3){
    if(offset[1]>0) figure[, , j] <- rbind(figure[(offset[1]+1):nrow(figure[, , j]), , j], 
                                              figure[1:offset[1], , j])
    if(offset[2]>0) figure[, , j] <- cbind(figure[, (offset[2]+1):ncol(figure[, , j]), j], 
                                              figure[, 1:offset[2], j])
  }
  imageData(img) <- figure
  img
}

#' get the background after alignment
#' @description Get the background after \link{alignmentImageData}
#' @param imgs a list of \link[EBImage:Image-class]{Image}.
#' @return an object of \link[EBImage:Image-class]{Image}
#' @import EBImage
#' 
getBackgroundAfterAlignment <- function(imgs){
  Reduce(`+`, imgs)/length(imgs) 
}
