library(shiny)
library(riskvisrr)
library(data.table)
stroke.dt <- data.table(stroke1yr)
var.names <- c("age",
               "heartfailure",
               "hypertension",
               "diabetes",
               "vascular",
               "female",
               "stroke")
setkeyv(stroke.dt, var.names)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Stroke Risk Visualizatoin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "user_age",
                   "What is your age?", value = 55),
      radioButtons(inputId = "sex",
                   "Are you male or female?",
                   choices = c("Male" = "no",
                               "Female" = "yes")),
      checkboxInput(inputId = "hf", "Have you previously had heart failure?",
                    value = F),
      checkboxInput(inputId = "hf", "Do you have hypertension",
                    value = F),
      checkboxInput(inputId = "hf", "Do you have diabetes?",
                    value = F),
      checkboxInput(inputId = "hf", "Have you ever had a stroke?",
                    value = F)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tags$h2("Your 1-year risk of stroke is between :"),
      tags$h2(strong(textOutput("textRisk")))
      
    )
  )
)

# Define server logic
server <- function(input, output) {
 
  # output$distPlot <- renderPlot({
  #   browser()
  #   # draw the histogram with the specified number of bins
  #   hist(stroke.dt[.(input$user_age), stroke1y], breaks = 5, col = 'darkgray', border = 'white')
  # })
  output$textRisk <- renderText({
   paste0(min((stroke.dt[.(input$user_age, input$sex), stroke1y])), "%", " and ",
          max((stroke.dt[.(input$user_age, input$sex), stroke1y])), "%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

