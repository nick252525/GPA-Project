library(shinythemes)

ui= fluidPage(theme = shinytheme("united"),
              
    # Application title
    
    p(strong(titlePanel(
      "G.P.A. Projection Tool"
    ))), br(),
    
    sidebarLayout(
      sidebarPanel( 
        selectInput(
          inputId = "course_name",
          label = "Type in course name and numbers",
          multiple = TRUE,
          choices = c(as.character(total_course_info$abbreviation))
        ),
        
        submitButton("Submit")
      ),
      
      mainPanel(verticalLayout(
        h3("Summary of Course Info", style = "color:darkblue"), 
        strong(h4(tableOutput("table"))),
        
        h2(align = "center"),
        
        h3("Projected Weighted GPA", style = "color:darkblue"),
        strong(h4(textOutput("text")))
      ), align = "center")
    ))


server <- function(input, output) {

output$table = renderTable({
      validate(need(!is.null(input$course_name), "Please enter course names"))
    summary_fun(input$course_name)
})


output$text = renderText({
    validate(need(!is.null(input$course_name), "Please enter course names"))
    Semester_gpa_fun(input$course_name)    
      })
}


# Run the application 
shinyApp(ui = ui, server = server)
