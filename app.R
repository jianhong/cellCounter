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

library(shiny)
library(EBImage)
library(DT)

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
        submitButton("recount", icon("refresh"))
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
      
      progress$set(message = "detectings cells", value = .2)
      sample <- channel(img, input$channel)
      
      disc <- makeBrush(31, "disc")
      disc <- disc/sum(disc)
      sample_bg <- filter2(sample, disc)
      sample_th <- sample > sample_bg + input$offset
      nmask <- watershed(distmap(sample_th), 1)
      nmask <- bwlabel(nmask)
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
      rm(nmask)
      rm(nmask2)
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
}

# Run the application 
shinyApp(ui = ui, server = server)

