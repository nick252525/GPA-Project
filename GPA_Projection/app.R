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
                          tabPanel("Course Summary", textOutput("textt"))))
    ))



server <- function(input, output) {

  active_df = reactive({
    as.numeric(total_course_info_cleaned[total_course_info$abbreviation == input$course_name,]$credit)*total_course_info[total_course_info_cleaned$abbreviation == input$course_name,]$AverageGPA
  })
    
  active2_df= reactive({
    for (i in 1:length(input$course_name))
      vector[i] <- values[i]
    
  })
  
  output$sum_table = renderTable({
    course_info =  subset(
      total_course_info,
      abbreviation == input$course_name,
      select = c(CourseTitle, credit, AverageGPA)
    )
  })
  

  output$textt= renderText({active2_df})
  
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
