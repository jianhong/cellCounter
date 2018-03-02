euc.dist <- function(x1, y1, x2, y2) sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

fixZvalue <- function(n) 2*n-1

colorZmap <- function(channels=c("red", "green", "blue")){
  ncontrasts <- length(channels)
  noutcomes <- 2^ncontrasts
  outcomes <- matrix(0, noutcomes, ncontrasts)
  colnames(outcomes) <- channels
  for(j in 1:ncontrasts){
    outcomes[, j] <- rep(0:1, times=2^(j-1), each=2^(ncontrasts-j))
  }
  mode(outcomes) <- "logical"
  outcomes
}

avgImage <- function(imgs){
  ## imgs is a list of Image object
  if(is.list(imgs)){
    if(!all(sapply(imgs, is, class2="Image"))){
      stop("imgs should be a list of Image objects")
    }
  }else{
    stopifnot(is(imgs, "Image"))
  }
  n <- length(imgs)
  if(n==1) return(imgs[[1]])
  imgs <- lapply(imgs, function(.ele) .ele/n)
  Reduce(`+`, imgs)
}

## set operator
setImage <- function(imgs, operater=c("intersect", "union", "diff"), maskValue=0.5, ...){
  if(is.list(imgs)){
    if(!all(sapply(imgs, is, class2="Image"))){
      stop("imgs should be a list of Image objects")
    }
  }else{
    stopifnot(is(imgs, "Image"))
  }
  operater <- match.arg(operater)
  FUN <- switch(operater,
                intersect=`&`,
                union=`|`,
                diff=function(A, B){
                  A & !B
                })
  n <- length(imgs)
  if(n==1) return(imgs[[1]])
  imgs2 <- lapply(imgs, function(.ele){
    .ele > maskValue
  })
  imgs2 <- Reduce(FUN, imgs2)
  imgs <- lapply(imgs, function(.ele) .ele*imgs2)
  avgImage(imgs)
}

