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
                label = "Upload an image",
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
            ),
            numericInput(
                inputId = "k_centers",
                label = "K-mean clusters",
                value = 1,
                min = 1,
                max = 20,
                step = 1
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(column(width = 12), uiOutput(outputId = "example")),
            fluidRow(
                column(
                    width = 6,
                    h1("Original image"),
                    imageOutput(outputId = "original_img")
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    h1("Jitter plot"), column(width = 12, plotOutput(outputId = "jitterplot")),
                    h1("Scatter plot"), column(width = 12, plotOutput(outputId = "scatterplot"))
                ),
                column(
                    width = 6,
                    h1("Heat map"), column(width = 12, plotOutput(outputId = "heatmap"))
                )
            )
        )
    ))

defaultImageURL <- "https://kevinrue.github.io/files/kennedy.jpg"
defaultImageObject <- image_read(path = defaultImageURL)
defaultImageMatrix <- make_matrix(defaultImageObject, type = "integer")

# Define server logic required to draw a histogram
server <- function(input, output) {
    rObjects <- reactiveValues(
        imageObject = defaultImageObject,
        imageMatrix = defaultImageMatrix
    )

    observeEvent(input$imageFile, {
        withProgress({
            rObjects$imageObject <- image_read(path = input$imageFile[1, "datapath", drop = TRUE])
        },
        message = "Loading image...")
    })

    observeEvent(rObjects$imageObject, {
        withProgress({
            rObjects$imageMatrix <- make_matrix(rObjects$imageObject)
        },
        message = "Generating matrix from image...")
    })

    output$original_img <- renderImage({
        tmpfile <- rObjects$imageObject %>%
            image_resize("30%") %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        # Return a list
        list(src = tmpfile, contentType = "image/jpeg")
    }, deleteFile=TRUE)

    output$heatmap <- renderPlot({
        withProgress({
            make_heatmap(matrix = rObjects$imageMatrix)
        },
        message = "Generating the heatmap...")
    })

    output$scatterplot <- renderPlot({
        withProgress({
            make_scatterplot(
                matrix = rObjects$imageMatrix,
                downsample = input$downsample,
                point_size = input$point_size,
                axis_label_prefix = input$axis_prefix
            )
        },
        message = "Generating the scatterplot from the matrix...")
    })

    output$jitterplot <- renderPlot({
        withProgress({
            make_jitterplot(
                matrix = rObjects$imageMatrix,
                downsample = input$downsample,
                point_size = input$point_size,
                jitter = input$point_jitter,
                axis_label_prefix = input$axis_prefix,
                k_centers = input$k_centers
            )
        },
        message = "Generating a jitterplot...")
    })

    output$example <- renderUI({
        if (is.null(input$imageFile)) {
            return(tagList(
                "Example from: ", a("https://raw.githubusercontent.com/kevinrue/kevinrue.github.io/master/files/kennedy.jpg")
            ))
        }
        return()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
