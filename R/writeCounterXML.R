#' write counters to xml file
#' @description write counter table to xmlfile
#' @param counterTypeTable counter type table, contain columns x, y, z, type
#' @param xmlfile output xml file name
#' @param originalImageFileName original image file name
#' @import XML
#' @author Jianhong Ou
#' @export
#' @examples
#' counterTypeTable = data.frame(x=1, y=1, z=1, type=1)
#' writeCounterXML(counterTypeTable, tempfile())
writeCounterXML <- function(counterTypeTable, xmlfile, originalImageFileName){
  suppressWarnings({xml <- xmlTree("CellCounter_Marker_File")})
  xml$addNode("Image_Properties", close = FALSE)
  xml$addNode("Image_Filename", originalImageFileName)
  xml$closeTag()
  xml$addNode("Marker_Data", close = FALSE)
  xml$addNode("Current_Type", 1)
  for(type in seq.int(8)){
    xml$addNode("Marker_Type", close = FALSE)
    xml$addNode("Type", type)
    currentCounterTable <- counterTypeTable[counterTypeTable$type==type, , drop=FALSE]
    xml$addNode("Count", nrow(currentCounterTable))
    if(nrow(currentCounterTable)>0){
      for(m in seq.int(nrow(currentCounterTable))){
        xml$addNode("Marker", close = FALSE)
        xml$addNode("MarkerX", currentCounterTable$x[m])
        xml$addNode("MarkerY", currentCounterTable$y[m])
        xml$addNode("MarkerZ", currentCounterTable$z[m])
        xml$closeTag()
      }
    }
    xml$closeTag()
  }
  xml$closeTag()
  cat(saveXML(xml, prefix = '<?xml version="1.0" encoding = "UTF-8"?>'), file = xmlfile, sep="\n")
}
