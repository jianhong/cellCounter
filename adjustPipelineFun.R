GaussianBlur <- function(img, sigma=5, ...){
  stopifnot(is(img, "Image"))
  gblur(img, sigma = sigma)
}

rescaleImage <- function(img, ...){
  stopifnot(is(img, "Image"))
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

increaseContrast <- function(img, times=2, ...){
  stopifnot(is(img, "Image"))
  stopifnot(is.numeric(times))
  img*times
}

removeBackground <- function(img, background, ...){
  stopifnot(is(img, "Image"))
  data <- imageData(img)
  if(missing(background)){
    data[data==0] <- NA
    background <- median(data, na.rm = TRUE)
  }
  data[is.na(data)] <- 0
  data[data<background[1]] <- 0
  imageData(img) <- data
  img
}

ScurveAdjust <- function(img, k=10, L=1, x0=0.5, ...){
  stopifnot(is(img, "Image"))
  data <- imageData(img)
  data <- L / (1 + exp(-k*(data-x0)))
  imageData(img) <- data
  img
}

changeCellSize <- function(img, direction=c("erode", "dilate"), 
                           targetChannel=c("red", "green", 'blue'), 
                           size=3, shape="disc", ...){
  stopifnot(is(img, "Image"))
  direction <- match.arg(direction)
  kern <- makeBrush(size=size, shape=shape)
  if(colorMode(img)==0){
    return(switch(direction,
                  erode=erode(img, kern),
                  dilate=dilate(img, kern)))
  }
  targetChannel <- match.arg(targetChannel, several.ok = TRUE)
  imgs <- lapply(c("red", "green", "blue"), channel, x=img)
  names(imgs) <- c("red", "green", "blue")
  imgs.new <- lapply(imgs, function(im){
    switch(direction,
           erode=erode(im, kern),
           dilate=dilate(im, kern))
  }) 
  for(i in targetChannel){
    imgs[[i]] <- imgs.new[[i]]
  }
  rgbImage(imgs[["red"]], imgs[["green"]], imgs[["blue"]])
}
