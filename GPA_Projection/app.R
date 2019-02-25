library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GPA Projection"),

   fluidRow(
     column(1,
            textInput(inputId = 'abb',
                      label = "Letter Course Name and Number",
                      value = names(total_course_info$abbreviation)),
            textOutput('abb')),
     
            br(),
            
    column(1,        
            textInput(inputId = "credit",
                      label = "Enter desired credit hour if not already provided",
                      value = total_course_info$credit)
                        )
     
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
