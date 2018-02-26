library(shiny)
library(riskvisrr)
library(data.table)
library(magrittr)
stroke.dt <- data.table(stroke1yr)

# Re-order columns to the order patients will enter their variables information
# into the shiny app
new.var.order <-  c("age",
                    "female",
                    "stroke",
                    "heartfailure",
                    "diabetes",
                    "hypertension",
                    "vascular",
                    "chadsvasc",
                    "stroke1y.lower",
                    "stroke1y",
                    "stroke1y.upper",
                    "I63.lower",
                    "I63",
                    "I63.upper",
                    "thromb1y.lower",
                    "thromb1y",
                    "thromb1y.upper",
                    "year2000")

setcolorder(stroke.dt,new.var.order)

setkeyv(stroke.dt, new.var.order)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Stroke Risk Communication"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "user_age",
                   "What is your age?", value = NULL,
                   min = 20, max = 99),
      radioButtons(inputId = "sex",
                   "Are you male or female?",
                   selected = character(0),
                   choices = c("Male" = "no",
                               "Female" = "yes")),
      radioButtons(inputId = "stroke",
                   "Have you ever had a stroke?",
                   selected = character(0),
                   choices = c("No" = "no",
                               "Yes" = "yes")),
      radioButtons(inputId = "hf",
                   "Have you previously had heart failure?",
                   selected = character(0),
                   choices = c("No" = "no",
                               "Yes" = "yes")),
      radioButtons(inputId = "diabetes",
                   "Have you ever had a diabetes?",
                   selected = character(0),
                   choices = c("No" = "no",
                               "Yes" = "yes")),
      radioButtons(inputId = "hyperT",
                   "Have you ever had a hypertension?",
                   selected = character(0),
                   choices = c("No" = "no",
                               "Yes" = "yes")),
      radioButtons(inputId = "vasc",
                   "Do you have vascular disease?",
                   selected = character(0),
                   choices = c("No" = "no",
                               "Yes" = "yes")),
      numericInput(inputId = "chadsvasc",
                   "What is your CHADSVASC score?", value = 0,
                   min = 0, max = 7)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tags$h2("Your risk of having a stroke in the next year is between :"),
      tags$h2(strong(textOutput("textRisk"))),
      br(),
      hr(),
      tags$h3("Table for error checking"),
      br(),
      DT::dataTableOutput("table")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  is.valid.age <- reactive({
    # This is only run if the created expression is evaluated inside another
    # reactive function - I think
    !is.na(input$user_age) &&
      input$user_age >= 20 &&
      input$user_age <= 99
  })
  output$textRisk <- renderText({
    
    # Order of subset arguments must be same order as new.col.order variable set in
    # intro.
     # browser()
    if (is.valid.age()) {
      paste0(stroke.dt[.(input$user_age,
                            input$sex,
                            input$stroke,
                            input$hf,
                            input$diabetes,
                            input$hyperT,
                            input$vasc),
                          min(stroke1y)],

           "%", " and ",

           stroke.dt[.(input$user_age,
                            input$sex,
                            input$stroke,
                            input$hf,
                            input$diabetes,
                            input$hyperT,
                            input$vasc),
                          max(stroke1y)],
           "%")
    }
    else{
      paste0("Please enter your age (between 20 and 99)")
      }
  })

  output$table <- DT::renderDataTable(
    DT::datatable(stroke.dt[.(input$user_age,
                              input$sex,
                              input$stroke,
                              input$hf,
                              input$diabetes,
                              input$hyperT,
                              input$vasc),
                            1:10])
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

# age == input$user_age &
# female == input$sex &
# stroke == input$stroke &
# heartfailure == input$hf &
# diabetes == input$diabetes &
# hypertension == input$hyperT &
# vascular == input$vasc,
