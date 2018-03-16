#' given a list of cell,
#' set the parent of offsprings of the cells
#' depend on their number of frames and their position.
#' 1. distance of same cell must have overlaps
#' 2. same cell must be the nearest one
#' 3. same cell in same frame can have multiple colors, and the other informations should be merge
#' 4. is the same cell not appear in continus frames, then stop search.
#' 5. one parent can have as much as two offsprings
#' @param cells the cell list.
#' @param maxFrameDist max difference of frames for same cell
#' @param overlapPercentage minimal percentage of overlapping for same cell or parent and child
#' @importFrom utils combn
#' @export
#' @return a list of cell objects.
#' @author Jianhong Ou
#' 
setParentOfCells <- function(cells, maxFrameDist=10, overlapPercentage=0.1){
  stopifnot(is.list(cells))
  null <- lapply(cells, function(.ele){
    if(!is(.ele, "cell")){
      stop("cells must be a list of cell object")
    }
  })
  if(length(cells)<2) return(cells)
  ## change the id
  cells <- cells[order(sapply(cells, function(.ele) .ele@frame[1]))]
  for(i in seq_along(cells)){
    cells[[i]]@id <- i
  }
  ## search offsprings
  opTbl <- searchOffsprings(cells, maxFrameDist, overlapPercentage)
  ## apply the table to the cells
  if(length(opTbl)>0){
    for(i in seq.int(nrow(opTbl))){
      offsprings(cells[[opTbl[i, "id1"]]]) <- c(offsprings(cells[[opTbl[i, "id1"]]]), cells[[opTbl[i, "id2"]]]@id)
      parent(cells[[opTbl[i, "id2"]]]) <- cells[[opTbl[i, "id1"]]]@id
    }
    ## reset the ids
    counter <- 1
    for(i in seq_along(cells)){
      if(length(parent(cells[[i]]))==0){
        tmp <- resetCellIds(cells, i, counter)
        counter <- tmp$counter+1
        cells <- tmp$cells
      }
    }
  }
  
  ## return a list of cells with parent and offsprings
  cells
}

searchOffsprings <- function(cells, maxFrameDist=10, overlapPercentage=.1){
  ## get the cells count in each frame
  cells.by.frame <- do.call(rbind, lapply(cells, function(.ele) c(id=.ele@id, frame=.ele@frame)))
  cells.by.frame <- split(cells.by.frame[, "id"], cells.by.frame[, "frame"])
  if(length(cells.by.frame)==1){
    return(NULL)
  }
  ## search by loop to save memory
  cells.m <- t(combn(length(cells.by.frame), 2))
  ## make sure parent frame is smaller than offsprings and 
  ## the offspring frame is not larger than parent frame + maxFrameDist
  cells.m <- cells.m[cells.m[, 2]>cells.m[, 1] & cells.m[, 1]+maxFrameDist>=cells.m[, 2], , drop=FALSE]
  ms <- NULL
  for(i in seq.int(nrow(cells.m))){
    p <- cells.by.frame[[cells.m[i, 1]]]
    o <- cells.by.frame[[cells.m[i, 2]]]
    ## only keep the nearest frame children
    if(i!=1) p <- p[!p %in% ms[, "id1"]]
    if(length(p)>0){
      info <- do.call(rbind, lapply(cells[c(p, o)], function(.ele){
        c(id=.ele@id, frame=.ele@frame, cx=.ele@cx, cy=.ele@cy)
      }))
      m <- expand.grid(p, o)
      m <- cbind(info[match(m[, 1], info[, "id"]), ],
                 info[match(m[, 2], info[, "id"]), ])
      colnames(m) <- paste0(colnames(m), rep(1:2, each=ncol(info)))
      ## in one frame, keep the 2 smallest distance
      m <- cbind(m, dist=euc.dist(m[, "cx1"], m[, "cy1"], m[, "cx2"], m[, "cy2"]))
      m <- m[order(m[, "id1"], m[, "dist"]), ]
      m <- m[rep(c(TRUE, FALSE), c(2, length(o)-2)), ]
      ## cell must have overlaps
      jaccardIndex <- mapply(function(c1, c2){
        xy1 <- paste(c1@xs, c1@ys)
        xy2 <- paste(c2@xs, c2@ys)
        length(intersect(xy1, xy2))/length(union(xy1, xy2))
      }, cells[m[, "id1"]], cells[m[, "id2"]])
      m <- m[jaccardIndex>=overlapPercentage, ]
      ms <- rbind(ms, m)
    }
  }
  
  ## only keep the nearest parent
  ms <- ms[order(ms[, "id2"], ms[, "dist"]), ]
  ms <- ms[!duplicated(ms[, "id2"]), , drop=FALSE]
  ms <- ms[order(ms[, "frame1"], ms[, "id1"], ms[, "frame2"], ms[, "id2"]), ]
}

resetCellIds <- function(cells, index, counter=1){
  cells[[index]]@id <- counter
  o <- offsprings(cells[[index]])
  if(length(o)>0){
    offsprings(cells[[index]]) <- NULL
    if(length(o)>1){
      for(id in o){
        parent(cells[[id]]) <- cells[[index]]@id
        offsprings(cells[[index]]) <- c(offsprings(cells[[index]]), counter+1)
        tmp <- resetCellIds(cells, id, counter=counter+1)
        counter <- tmp$counter
        cells <- tmp$cells
      }
    }else{
      parent(cells[[id]]) <- cells[[index]]@id
      offsprings(cells[[index]]) <- counter
      tmp <- resetCellIds(cells, o, counter=counter)
      counter <- tmp$counter
      cells <- tmp$cells
    }
  }
  return(list(cells=cells, counter=counter))
}
