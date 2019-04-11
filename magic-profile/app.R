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

make_matrix <- function(file, type="numeric") {
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

make_heatmap <- function(matrix) {
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

make_scatterplot <- function(matrix, point_size=1) {
    if (is.null(matrix)){
        return(plot.new())
    }
    xy_coord <- as.data.frame(which(matrix == 1, arr.ind=TRUE))
    ggplot(xy_coord) +
        geom_point(aes(row, -col), size=point_size) +
        theme_void()
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "imageFile", label = "Input  file", multiple = FALSE, accept = c("png")),
         numericInput(inputId = "point_size", label = "Point size", value = 1, min = 0.1, max = 5, step = 0.1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          column(width = 12, plotOutput(outputId = "scatterplot")),
          column(width = 12, plotOutput(outputId = "heatmap"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rObjects <- reactiveValues(
        imageData = NULL
    )
    
    observeEvent(input$imageFile, {
        rObjects$imageData <- make_matrix(file = input$imageFile[1, "datapath", drop=TRUE], type = "integer")
    })
   
   output$heatmap <- renderPlot({
       make_heatmap(matrix = rObjects$imageData)
   })
   
   output$scatterplot <- renderPlot({
       make_scatterplot(matrix = rObjects$imageData, point_size = input$point_size)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

