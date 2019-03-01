library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GPA Projection"),

    sidebarLayout(
      sidebarPanel(
        selectInput(
            inputId = "course_name",
            label = "Type in course name and numbers",
            multiple = TRUE,
            choices = c(as.character(total_course_info$abbreviation))
        ),
        
        submitButton()
        ),
     
    mainPanel(tabsetPanel(type = "tab", 
                          tabPanel("Credit", textOutput("mycredit"))))
    ))



server <- function(input, output) {

    active_df = reactive({

      subset(total_course_info_cleaned, abbreviation==as.character(input$course_name), 
            select=c(credit)) 
    })
    
    
    output$mycredit= renderPrint({
      active_df()$credit 
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
