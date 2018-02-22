#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# author: Jianhong Ou

if(!"EBImage" %in% rownames(installed.packages())){
  source("https://bioconductor.org/biocLite.R")
  biocLite("EBImage", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
}
if(!"DT" %in% rownames(installed.packages())){
  source("https://bioconductor.org/biocLite.R")
  biocLite("DT", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
}
if(!"scales" %in% rownames(installed.packages())){
  source("https://bioconductor.org/biocLite.R")
  biocLite("scales", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
}
if(!"XML" %in% rownames(installed.packages())){
  source("https://bioconductor.org/biocLite.R")
  biocLite("XML", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
}
# if(!"shinyjs" %in% rownames(installed.packages())){
#   source("https://bioconductor.org/biocLite.R")
#   biocLite("shinyjs", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
# }
library(shiny)
#library(shinyjs)
library(EBImage)
library(DT)
library(scales)
library(XML)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("cell counter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        tags$div(HTML("<h3>Howto</h3>",
                      "<p><ol><li>split or merge the color channel in fuji/imageJ.</li>",
                      "<li>save as gif.</li>",
                      "<li>save the gif as tiff.</li>",
                      "<li>set the parameters before upload the file.</li>",
                      "<li>upload the tiff.</li></ol></p>",
                      "<hr>")),
        
        fileInput("f",
                  "Choose tiff file",
                  multiple = FALSE,
                  accept = c(".tiff", ".TIFF", ".tif", ".TIF",
                             "image/tiff")),
        sliderInput("offset",
                    "density step",
                    min = 0.01,
                    max = 1,
                    value = 0.05,
                    step = 0.01),
        sliderInput("size",
                    "cell size range",
                    min = 0,
                    max = 2000,
                    value = c(10, 1000),
                    step = 1),
        sliderInput("distance",
                    "distance of same cell in different frame",
                    min = 1,
                    max = 100,
                    value = 10,
                    step = 1),
        radioButtons("channel",
                     "channel",
                     choiceNames = c("red", "green", "blue"),
                     choiceValues = c("red", "green", "blue"),
                     selected = "green"),
        checkboxInput("reLevel",
                      "re-level the image to increase the contrast",
                      value = TRUE),
        radioButtons("GaussianBlur",
                     "apply Gaussian blur before/after re-level",
                     choiceNames = c("No", "before", "after"),
                     choiceValues = c("No", "before", "after"),
                     selected = "No"),
        sliderInput("blurLevel",
                    "sigma value of Gaussian blur",
                    min = 0,
                    max = 20,
                    value = 2,
                    step = .1),
        radioButtons("outputXML",
                      "output to XML or labeling in figure",
                     choiceNames = c("XML", "numberLabeling"),
                     choiceValues = c("XML", "numberLabeling"),
                     selected = "XML"),
        submitButton("recount", icon("refresh")),
        downloadButton("downloadData", "Download", "disabled")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          tags$div(EBImage::displayOutput('outputviewer', width = "100%", height="600px")),
          tags$div(DT::DTOutput('countTbl', width = "100%", height = "500px"))
      )
   )
)

options(shiny.maxRequestSize=500*1024^2) 
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

# Define server logic required to draw a histogram
server <- function(input, output) {
  cnt_tbl <- data.frame()
  tbl.old <- NULL
  output$outputviewer <- EBImage::renderDisplay({
    write.table("", file = "cellCounter.xml")
    if(!is.null(input$f)){
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "loading figure", value = 0)
      
      f <- input$f$datapath
      img <- readImage(f)
      
      progress$set(message = "convert to RGB mode", value = .1)
      
      img <- toRGB(img)
      if(length(dim(img)>3)){
        if(dim(img)[3]>3){
          img <- img[, , 1:3, ]
        }
      }
      
      progress$set(message = "adjust image", value = .2)
      if(input$GaussianBlur=="before"){
        img <- gblur(img, sigma = input$blurLevel)
      }
      if(input$reLevel){
        imageData(img) <- rescale(imageData(img), to=c(0, 1))
      }
      if(input$GaussianBlur=="after"){
        img <- gblur(img, sigma = input$blurLevel)
      }
      progress$set(message = "detectings cells", value = .3)
      sample <- channel(img, input$channel)
      
      disc <- makeBrush(31, "disc")
      disc <- disc/sum(disc)
      sample_bg <- filter2(sample, disc)
      sample_th <- sample > sample_bg + input$offset
      nmask <- watershed(distmap(sample_th), 1)
      nmask <- bwlabel(nmask)
      rm(sample_bg)
      rm(sample_th)
      progress$set(value = .5, message = "counting cells")
      nmask2 <- list()
      for(i in seq.int(numberOfFrames(nmask, type="render"))){
        tbl <- table(imageData(getFrame(nmask, i, type="render")))
        tbl <- as.numeric(names(tbl)[tbl<input$size[1] | tbl>input$size[2]])
        nmask2[[i]] <- rmObjects(getFrame(nmask, i, type="render"), tbl, reenumerate=FALSE)
        progress$set(value = .5 + .2*i/numberOfFrames(nmask, type="render"), detail = paste("frame", i))
      }
      nmask2 <- combine(nmask2)
      sample <- paintObjects(nmask2, img, 
                             col = c(ifelse(input$channel=="red", "green", "red"), NA), 
                             closed = TRUE)
      rm(img)
      rm(nmask)
      gc(reset=TRUE)
      if(input$outputXML=="XML"){
        progress$set(value = .7, message = "generate xml file")
        xml <- xmlTree()
        xml$addTag("CellCounter_Marker_File", close = FALSE)
        xml$addTag("Image_Properties", close = FALSE)
        xml$addTag("Image_Filename", input$f$name)
        xml$closeTag()
        xml$addTag("Marker_Data", close = FALSE)
        xml$addTag("Current_type", 3)
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 1)
        xml$closeTag()
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 2)
        xml$closeTag()
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 3)
        xml$closeTag()
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 4)
        for(i in seq.int(numberOfFrames(nmask2, type="render"))){
          tbl.new <- computeFeatures.moment(getFrame(nmask2, i, type="render"))
          if(length(tbl.new)>0){
            checklist <- rep(TRUE, nrow(tbl.new))
            if(length(tbl.old)>0){
              for(m in seq.int(nrow(tbl.new))){
                for(n in seq.int(nrow(tbl.old))){
                  if(euc.dist(tbl.new[m, c("m.cx", "m.cy")], tbl.old[n, c("m.cx", "m.cy")]) < input$distance){
                    checklist[m] <- FALSE
                    next
                  }
                }
              }
            }
            
            for(m in seq_along(checklist)){
              if(checklist[m]){
                xml$addTag("Marker", close = FALSE)
                xml$addTag("MarkerX", round(tbl.new[m, "m.cx"]))
                xml$addTag("MarkerY", round(tbl.new[m, "m.cy"]))
                xml$addTag("MarkerZ", i)
                xml$closeTag()
              }
            }
          }
          tbl.old <- tbl.new
          progress$set(value = .7 + .2*i/numberOfFrames(nmask2, type="render"), detail = paste("frame", i))
        }
        xml$closeTag()
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 5)
        xml$closeTag()
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 6)
        xml$closeTag()
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 7)
        xml$closeTag()
        xml$addTag("Marker_Type", close = FALSE)
        xml$addTag("Type", 8)
        xml$closeTag()
        xml$closeTag()
        xml$closeTag()
        saveXML(xml, file = "cellCounter.xml")
        #removeClass("downloadData", "disabled")
      }else{
        sample2 <- list()
        progress$set(value = .7, message = "labeling cells")
        for(i in seq.int(numberOfFrames(nmask2, type="render"))){
          tbl.new <- computeFeatures.moment(getFrame(nmask2, i, type="render"))
          if(length(tbl.new)>0){
            tbl.new <- cbind(tbl.new, counts=rep(0, nrow(tbl.new)))
            if(length(tbl.old)>0){
              countsInLastFrame <- 0
              for(m in seq.int(nrow(tbl.new))){
                for(n in seq.int(nrow(tbl.old))){
                  if(euc.dist(tbl.new[m, c("m.cx", "m.cy")], tbl.old[n, c("m.cx", "m.cy")]) < input$distance){
                    countsInLastFrame <- countsInLastFrame + 1
                    tbl.new[m, "counts"] <- tbl.old[n, "counts"]
                    next
                  }
                }
              } 
              counts <- nrow(tbl.new) - countsInLastFrame
            }else{
              countsInLastFrame <- 0
              counts <- nrow(tbl.new)
            }
            lastCount <- ifelse(nrow(cnt_tbl)>0, cnt_tbl[nrow(cnt_tbl), "cumsum"], 0)
            if(sum(tbl.new[, "counts"]==0)>0){
              tbl.new[tbl.new[, "counts"]==0, "counts"] <- seq.int(sum(tbl.new[, "counts"]==0)) + lastCount
            }
            cnt_tbl <- rbind(cnt_tbl, 
                             data.frame(frame  = i,
                                        counts = counts,
                                        countsInLastFrame = countsInLastFrame,
                                        cumsum = lastCount + counts,
                                        x      = paste(round(tbl.new[, "m.cx"], digits = 1), collapse = ";"), 
                                        y      = paste(round(tbl.new[, "m.cy"], digits = 1), collapse = ";")))
            tbl.old <- tbl.new
            ## paste number
            tmpfile <- tempfile()
            png(filename = tmpfile, width = nrow(sample), height = ncol(sample))
            display(getFrame(sample, i, type="render"), method = "raster")
            text(tbl.new[, "m.cx"], tbl.new[, "m.cy"], 
                 labels = tbl.new[, "counts"], 
                 col = ifelse(tbl.new[, "counts"]>lastCount, "orange", "white"))
            dev.off()
            sample2[[i]] <- readImage(tmpfile, type = "png")[, , 1:3]
            unlink(tmpfile)
          }else{
            sample2[[i]] <- getFrame(sample, i, type="render")
          }
          progress$set(value = .7 + .2*i/numberOfFrames(nmask2, type="render"), detail = paste("frame", i))
        }
        sample <- combine(sample2)
        rm(sample2)
      }
      
      rm(nmask2)
      gc(reset=TRUE)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      progress$set(value = .9, message = "render image")
    }else{
      sample <- readImage("Blank.png")
    }
    
    output$countTbl <- DT::renderDT(cnt_tbl, options = list(
      pageLength = 10
    ))
    
    return(display(sample, method = "browser"))
  })
  output$downloadData <- downloadHandler(
    filename = "cell.counter.xml",
    content = function(file){
      file.copy("cellCounter.xml", to = file)
      },
    contentType = "application/xml"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

