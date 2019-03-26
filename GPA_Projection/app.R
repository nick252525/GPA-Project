library(shinythemes)

ui= fluidPage(theme = shinytheme("united"),

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
                          tabPanel("Projected Weighted GPA", textOutput("textt")),
                          tabPanel("Summary of Course Info", tableOutput("sum_table"))))
    ))



server <- function(input, output) {

 
  output$sum_table = renderTable({
    (summary_fun(input$course_name))
  })
  

  output$textt= renderText({Semester_gpa_fun(input$course_name)})
  
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
