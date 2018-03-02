saveCountXML <- function(nmask, xmlfile="cellCounter.xml", file, distance=10, counterType=c(prenew=6, new=4, old=3), zv=fixZvalue, ...){
  stopifnot(is(nmask, "Image"))
  stopifnot(all(counterType>0))
  stopifnot(all(counterType<9))
  counterTypeTable <- data.frame(x=NA, y=NA, z=NA, id=0)
  tbl.old <- NULL
  lastCounts <- 0
  for(i in seq.int(numberOfFrames(nmask, type="render"))){
    tbl.new <- computeFeatures.moment(getFrame(nmask, i, type="render"))
    if(length(tbl.new)>0){
      tbl.new <- cbind(tbl.new, counts=rep(0, nrow(tbl.new)))
      if(length(tbl.old)>0){
        newId <- rep(seq.int(nrow(tbl.new)), each=nrow(tbl.old))
        tbl.new1 <- tbl.new[newId, ]
        oldId <- rep(seq.int(nrow(tbl.old)), times=nrow(tbl.new))
        tbl.old1 <- tbl.old[oldId, ]
        dists <- euc.dist(tbl.new1[, "m.cx"], tbl.new1[, "m.cy"], tbl.old1[, "m.cx"], tbl.old1[, "m.cy"])
        ## select minimal distance for each new,old pair
        tbl.new1 <- data.frame(newId, oldId, dists)
        tbl.new1 <- split(tbl.new1, newId)
        tbl.new1 <- lapply(tbl.new1, function(.ele){
          .ele[which.min(.ele[, 3])[1], ]
        })
        tbl.new1 <- do.call(rbind, tbl.new1)
        oldCells <- tbl.new1[, 3]<distance
        if(sum(oldCells)>0){
          tbl.new[tbl.new1[oldCells, 1], "counts"] <- tbl.old[tbl.new1[oldCells, 2], "counts"]
        }
        if(sum(!oldCells)>0){
          tbl.new[!oldCells, "counts"] <- seq.int(sum(!oldCells)) + lastCounts
        }
      }else{
        tbl.new[, "counts"] <- seq.int(nrow(tbl.new)) + lastCounts
      }
      cx <- round(tbl.new[, "m.cx"])
      cy <- round(tbl.new[, "m.cy"])
      counterTypeTable <- rbind(counterTypeTable,
                                data.frame(x=cx,
                                           y=cy,
                                           z=zv(i),
                                           id=tbl.new[, "counts"]))
      lastCounts <- max(counterTypeTable[, "id"], na.rm = TRUE)
    }
    tbl.old <- tbl.new
  }
  counterTypeTable <- counterTypeTable[!is.na(counterTypeTable$x), , drop=FALSE]
  counterTypeTable <- split(counterTypeTable, counterTypeTable$id)
  counterTypeTable <- lapply(counterTypeTable, function(.ele){
    i <- max(floor(nrow(.ele)/2), 1)
    .ele$type <- counterType["prenew"]
    .ele$type[i:nrow(.ele)] <- counterType["old"]
    .ele$type[i] <- counterType["new"]
    .ele
  })
  counterTypeTable <- do.call(rbind, counterTypeTable)
  #counterTypeTable <- split(counterTypeTable, counterTypeTable$type)
  writeCounterXML(counterTypeTable, xmlfile, file)
}
