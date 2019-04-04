library(shinythemes)

library("rvest")
library("stringr")
library("readxl")
library("dplyr")
library("htmlwidgets")
library("shinythemes")
library("shinyMatrix")



wade_datasetV2 <- read_excel("Wade_DatasetV2.xlsx",
                             sheet = "TestSpring2018")

course_abbreviations= unique(wade_datasetV2$Subject)

final_course_urls= paste0("http://catalog.illinois.edu/courses-of-instruction/",tolower(course_abbreviations),"/")

credit_function= function(x){
  tryCatch(
    read_html(x) %>%
      html_nodes(".courseblocktitle") %>%
      html_text() %>%
      str_extract_all(, pattern = "credit:[:blank:][:digit:][:blank:]to[:blank:][:digit:]|credit:[:blank:][:digit:]"),
    error = function(e) {NA})
}

abb_function = function(x) {
  tryCatch(
    read_html(x) %>%
      html_nodes(".courseblocktitle") %>%
      html_text() %>%
      str_extract(pattern = "[:alpha:]{2,4}[:blank:][:digit:]{3}") %>%
      str_replace_all(pattern = "[:blank:]", replacement = ""),
    error = function(e) {NA}
  )
}

all_abbreviations= mapply(FUN = abb_function, final_course_urls)
all_credits= mapply(FUN = credit_function, final_course_urls)


df_abb= data.frame(unlist(all_abbreviations))

df_credit= data.frame(unlist(all_credits))

df_abb_named= setNames(cbind(rownames(df_abb), df_abb, row.names= NULL),
                       c("url","abbreviation"))

df_credit_named= setNames(cbind(rownames(df_credit), df_credit, row.names= NULL),
                          c("url","credit"))




abb_credit_df= merge(x= df_abb_named, y= df_credit_named, by=c("url"))

total_course_info = merge(
  x = abb_credit_df,
  y = wade_datasetV2,
  by.x = "abbreviation",
  by.y = "ShortCourse",
  all.y = TRUE
)

total_course_info = subset(total_course_info, select = -c(`Credit Hours`))

total_course_info$credit = str_replace(total_course_info$credit,
                                       pattern = "credit:[:blank:]",
                                       replacement = "")

as.numeric(total_course_info$credit)

credit_avg = function(x) {
  value1 = str_extract(x, pattern = "[:digit:]")
  value2 = str_extract(x, pattern = "[:digit:]$")
  ifelse(value1 == value2, value1, (as.numeric(value1) + as.numeric(value2)) / 2)
}

avg_credits = mapply(FUN = credit_avg, total_course_info$credit)
avg_credits = as.numeric(avg_credits)

total_course_info$AverageCredit = avg_credits




raw_gpa_fun = function(x) {
  total_course_info[total_course_info$abbreviation == x,]$AverageCredit *
    total_course_info[total_course_info$abbreviation == x, ]$AverageGPA
}

raw_credit_fun= function(x){
  total_course_info[total_course_info$abbreviation == x,]$AverageCredit
}

Semester_gpa_fun= function(x){
  (sum(mapply(raw_gpa_fun, x)))/sum(mapply(raw_credit_fun, x))
  
}

subset_data_fun= function(x){subset(total_course_info, abbreviation==x, 
                                    select=c(`AverageCredit`, `AverageGPA`))}


summary_fun= function(x){
  couse_data=((t(mapply(subset_data_fun, x))))
  my_data <- cbind(rownames(couse_data), couse_data)
  rownames(my_data) <- NULL
  colnames(my_data) <- c("Course Name","Average Credit Hour","Average GPA")
  my_data     
}


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
