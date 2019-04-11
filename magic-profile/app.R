#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

require(magick)
require(ComplexHeatmap)

makeMatrix <- function(file, type="numeric") {
    image <- image_read(file)
    # image
    flat <- image_flatten(image, 'Modulate')
    # flat
    data <- image_data(flat)
    # data
    non_white <- data[1, , ] != "ff"
    storage.mode(non_white) <- "integer"
    non_white
}

makeHeatmap <- function(matrix) {
    if (is.null(matrix)){
        return(plot.new())
    }
    hm <- Heatmap(
        matrix = t(matrix),
        col = c("1"="black", "0"="white"),
        cluster_rows = FALSE, cluster_columns = FALSE,
        show_heatmap_legend = FALSE
    )
    draw(hm)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "imageFile", label = "Input  file", multiple = FALSE, accept = c("png"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          plotOutput(outputId = "heatmap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rObjects <- reactiveValues(
        imageData = NULL
    )
    
    observeEvent(input$imageFile, {
        rObjects$imageData <- makeMatrix(input$imageFile[1, "datapath", drop=TRUE])
    })
   
   output$heatmap <- renderPlot({
       makeHeatmap(rObjects$imageData)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

