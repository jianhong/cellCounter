saveSingleFrameCountXML <- function(nmask, xmlfile="cellCounter.xml", file, zmap, formula, ...){
  stopifnot(is(nmask, "Image"))
  counterTypeTable <- data.frame(x=NA, y=NA, z=NA, type=NA)
  n <- numberOfFrames(nmask, type="render")
  typelist <- n-seq.int(n)+1
  names(typelist) <- c(apply(zmap, 1, function(.ele) paste(colnames(zmap)[.ele], collapse = "*")), formula)
  for(i in seq.int(n)){
    tbl <- computeFeatures.moment(getFrame(nmask, i, type="render"))
    if(length(tbl)>0){
      cx <- round(tbl[, "m.cx"])
      cy <- round(tbl[, "m.cy"])
      cz <- ifelse(i<=nrow(zmap), sum(zmap[i, ]), 3)
      counterTypeTable <- rbind(counterTypeTable,
                                data.frame(x=cx,
                                           y=cy,
                                           z=cz, 
                                           type=typelist[i]))
    }
  }
  counterTypeTable <- counterTypeTable[!is.na(counterTypeTable$x), , drop=FALSE]
  writeCounterXML(counterTypeTable, xmlfile, file)
  return(typelist)
}
