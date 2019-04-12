#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Profile plot"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput(
                inputId = "imageFile",
                label = "Input  file",
                multiple = FALSE,
                accept = c("png")
            ),
            numericInput(
                inputId = "downsample",
                label = "Resolution",
                value = 200,
                min = 50,
                max = 1000,
                step = 10
            ),
            numericInput(
                inputId = "point_size",
                label = "Point size",
                value = 0.1,
                min = 0.1,
                max = 5,
                step = 0.1
            ),
            numericInput(
                inputId = "point_jitter",
                label = "Jitter width/height",
                value = 5,
                min = 0,
                max = 20,
                step = 1
            ),
            textInput(
                inputId = "axis_prefix",
                label = "Axis label prefix",
                value = "TSNE"
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h1("Jitter plot"), column(width = 12, plotOutput(outputId = "jitterplot")),
            h1("Scatter plot"), column(width = 12, plotOutput(outputId = "scatterplot")),
            h1("Heat map"), column(width = 12, plotOutput(outputId = "heatmap"))
        )
    ))

defaultImageURL <- "https://res.cloudinary.com/dwccfildc/c_limit,w_1140/v1523428401/kennedy/haiku.profile/55404b0dd6a64cc5800605c46bc3b694/haiku.profiles.portrait/7f55c8cd346a42429d7a6264f752649f/image.jpg"
defaultImageObject <- image_read(path = defaultImageURL)
defaultImageMatrix <- make_matrix(defaultImageObject, type = "integer")

# Define server logic required to draw a histogram
server <- function(input, output) {
    rObjects <- reactiveValues(
        imageObject = defaultImageObject,
        imageMatrix = defaultImageMatrix
    )
    
    observeEvent(input$imageFile, {
        rObjects$imageObject <- image_read(path = input$imageFile[1, "datapath", drop = TRUE])
    })
    
    observeEvent(rObjects$imageObject, {
        rObjects$imageMatrix <- make_matrix(rObjects$imageObject)
    })
    
    output$heatmap <- renderPlot({
        make_heatmap(matrix = rObjects$imageMatrix)
    })
    
    output$scatterplot <- renderPlot({
        make_scatterplot(matrix = rObjects$imageMatrix,
                         point_size = input$point_size)
    })
    
    output$jitterplot <- renderPlot({
        make_jitterplot(
            matrix = rObjects$imageMatrix,
            downsample = input$downsample,
            point_size = input$point_size,
            jitter = input$point_jitter,
            axis_label_prefix = input$axis_prefix
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
