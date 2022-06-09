#' Read images
#' @description Read images from files.
#' @param file a character vector of file names.
#' @param channel the channel to be read in
#' @param Image2mode return should be object of Image or Image2?
#' @import EBImage
#' @export
#' @return an Image or Image2
#' @author Jianhong Ou, Yanchao Han
#' @examples
#' readFile(system.file("extdata", "sample.tiff", package="cellCounter"))
#'
readFile <- function(file, channel=NULL, Image2mode=FALSE){
  stopifnot(file.exists(file))
  imageFilename <- basename(file)
  ## read image
  if(endsWith(file, ".czi")){
    if(!requireNamespace("RBioFormats")){
      stop("RBioFormats package is required for loading czi files.",
           "Please try to install RBioFormats from https://github.com/aoles/RBioFormats")
    }
    m <- RBioFormats::read.metadata(file = file)$globalMetadata
    imageFilename <- m[["Information|Document|Title #1"]]
    setsubset <- FALSE
    if(!is.null(channel)){
      channelAvailable <-
        m[startsWith(names(m), "Experiment|AcquisitionBlock|MultiTrackSetup|TrackSetup|Detector|Color")]
      names(channelAvailable) <-
        sub("Experiment|AcquisitionBlock|MultiTrackSetup|TrackSetup|Detector|Color #", "",
            names(channelAvailable), fixed = TRUE)
      colorMap <- c("red"="#FF0000", "green"="#00FF00", "blue"="#0000FF")
      channelAvailable <- unlist(channelAvailable)
      if(colorMap[channel] %in% channelAvailable){
        subset <- list(C=as.numeric(names(channelAvailable)[channelAvailable==colorMap[channel]]))
        setsubset <- TRUE
      }
    }
    if(setsubset){
      img <- RBioFormats::read.image(file = file,
                                     proprietary.metadata = FALSE,
                                     normalize = TRUE,
                                     subset = subset)
    }else{
      img <- RBioFormats::read.image(file = file,
                                     proprietary.metadata = FALSE,
                                     normalize = TRUE)
    }
  }else{
    img <- readImage(file)
    ## remove the alpha channel
    img <- toRGB(img)
    if(length(dim(img)>3)){
      if(dim(img)[3]>3){
        img <- img[, , 1:3, ]
      }
    }
    if(!is.null(channel)) img <- channel(img, channel)
  }
  if(Image2mode) img <- Image2(img)
  list(img=img, imageFilename=imageFilename)
}
