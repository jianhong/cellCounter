#' convert list of cell to data.frame
#' @description convert list of cell object to a data frame
#' @param cells list of \link{cell} object
#' @return a data frame
#' @export
#' @author Jianhong Ou
outputCellTable <- function(cells){
  stopifnot(is.list(cells))
  m <- do.call(rbind, lapply(cells, function(.ele){
    if(!is(.ele, "cell")){
      stop("cells must be a list of cell object")
    }
    data.frame(cid=.ele@id, x=.ele@cx, y=.ele@cy, 
               parent=ifelse(length(.ele@parent), .ele@parent, 0),
               frame=.ele@frame, color=.ele@color)
  }))
  cbind(id=seq_along(cells), m)
}