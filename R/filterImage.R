#' filter image
#' @description filter pixels by conditions
#' @param img An object of \link[EBImage:Image-class]{Image} or \link{Image2}
#' @param expand.size extend the regions that meet the filter criteria by pixels. 
#' A numeric containing the size of the brush in pixels. 
#' This should be an odd number; even numbers are rounded to the next odd one, 
#' i.e., size = 4 has the same effect as size = 5. Default is 21
#' @param \dots filter criteria
#' @return An object of \link[EBImage:Image-class]{Image} or \link{Image2}
#' @import EBImage
#' @export
#' @author Jianhong Ou
#' library(EBImage)
#' img <- readImage(system.file("extdata", "low.jpg", package="cellCounter"))
#' img <- filterImage(img, red > .3, green > .3, green + .1 > red, green < red + .3)
filterImage <- function(img, ..., expand.size=21){
  stopifnot(inherits(img, c("Image", "Image2")))
  isImg2 <- FALSE
  if(is(img, "Image2")) {
    img <- toImage(img)
    isImg2 <- TRUE
  }
  
  dots <- substitute(list(...))[-1]
  criteria <- sapply(dots, deparse)
  if(any(grepl("\\(|\\)", criteria))){
    stop("Filter criteria could not contain ( or )")
  }
  ## split by ">", "<", "=", ">=", "<="
  criteria.l <- strsplit(criteria, "[><=]+")
  if(!all(lengths(criteria.l)==2)){
    stop("The filter criteria are not acceptable.")
  }
  criteria.l <- lapply(criteria.l, function(.ele) gsub(" ", "", .ele))
  criteria.o <- sub("^.*?([><=]+).*?$", "\\1", criteria)
  criteria.ll <- lapply(criteria.l, function(.ele) {
    a <- strsplit(.ele, "[+\\-\\*\\/]")
    b <- strsplit(.ele, "[^+\\-\\*\\/]*")
    mapply(function(.a, .b){
      .b[.b==""] <- .a
      .b
    }, a, b, SIMPLIFY = FALSE)
  })
  
  criteria.l <- lapply(criteria.ll, `[[`, 1)
  criteria.r <- lapply(criteria.ll, `[[`, 2)
  
  keep <- mapply(function(.l, .o, .r){
    checkParameters <- function(.ele){
      if(.ele %in% c("rgb", "gray", "grey", "luminance", "red", "green", 'blue', "asred", "asgreen", "asblue")){
        .ele <- channel(img, .ele)
      }else{
        if(!.ele %in% c("+", "-", "*", "/")){
          .ele <- as.numeric(.ele)
        }
      }
      .ele
    }
    .l <- lapply(.l, checkParameters)
    .r <- lapply(.r, checkParameters)
    doCal <- function(.list){
      .class <- sapply(.list, function(.ele) inherits(.ele, c("Image", "numeric")))
      rank <- rep(0, length(.list))
      for(i in seq_along(.list)){
        if(!.class[i]){
          if(.list[[i]] %in% c("*", "/")){
            rank[i+1] <- rank[i] <- rank[i-1] <- 1
          }
        }
      }
      .list <- .list[order(rank, decreasing = TRUE)]
      for(i in seq_along(.list)){
        if(!.class[i]){
          if(.list[[i]] %in% c("+", "-", "*", "/")){
            f <- match.fun(.list[[i]])
            .list[[i+1]] <- f(.list[[i-1]], .list[[i+1]])
          }
        }
      }
      .list[[length(.list)]]
    }
    .l <- doCal(.l)
    .r <- doCal(.r)
    f <- match.fun(.o)
    f(.l, .r)
  }, criteria.l, criteria.o, criteria.r, SIMPLIFY = FALSE)
  
  keep <- Reduce(match.fun('&'), keep)
  if(sum(keep)>0){## extend
    kern <- makeBrush(size=expand.size, shape="disc")
    keep <- dilate(keep, kern = kern)
  }
  mask <- list()
  for(i in seq.int(numberOfFrames(img))){
    mask[[i]] <- keep
  }
  mask <- combine(mask)
  img <- img * mask
  if(isImg2){
    img <- Image2(img)
  }
  img
}
