#'
#'
labeledImage2Cell <- function(imgs, colorChannel=c("#FF0000", "#00FF00", "#0000FF")){
  colorChannel <- match.arg(colorChannel)
  if(!is.list(imgs)){
    stopifnot(inherits(imgs, c("Image", "Image2")))
    stopifnot(colorMode(imgs)==Grayscale)
    if(is(imgs, "Image2")) imgs <- toImage(imgs)
    imgs <- lapply(seq.int(numberOfFrames(imgs, type="render")), 
                  getFrame, y=imgs, type = "render")
  }
  cells <- list()
  counter <- 0
  for(i in seq_along(imgs)){
    img <- imgs[[i]]
    stopifnot(inherits(img, c("Image", "Image2")))
    stopifnot(colorMode(imgs)==Grayscale)
    if(is(img, "Image2")) img <- toImage(img)
    moment <- computeFeatures.moment(img)
    moment <- round(moment)
    for(j in seq.int(nrow(moment))){
      counter <- counter + 1
      cx <- moment[j, "m.cx"]
      cy <- moment[j, "m.cy"]
      xys <- getXYs(j, img)
      cells[[counter]] <- cell(cx=cx,
                               cy=cy,
                               xs=xys$x, ys=xys$y,
                               id=counter, parent=0,
                               offsprings=0,
                               frame=i,
                               color=colorChannel)
      
    }
  }
  cells
}

getXYs <- function(j, img){
  ids <- which(imageData(img)==j)
  dm <- dim(img)
  stopifnot(length(dm)==2) ## Grayscale only
  #ids = (col_number - 1) * nrow + row_number
  x <- ids %% dm[1] + 1
  y <- ids %/% dm[1] + 1
  return(list(x=x, y=y))
}