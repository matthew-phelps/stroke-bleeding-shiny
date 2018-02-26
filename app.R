library(shiny)
library(riskvisrr)
library(data.table)
library(magrittr)
library(shinyWidgets)
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
      tags$style("#user_age {font-size:38px;height:50px; width: 110px;}"),
      numericInput(inputId = "user_age",
                   "Patient's age:", value = NULL,
                   min = 20, max = 99),

      radioGroupButtons(inputId = "sex",
                   label = "Male or female:",
                   selected = character(0),
                   choiceNames = c("Male", "Female"),
                   choiceValues = c("no", "yes")),
      radioGroupButtons(inputId = "stroke",
                   "Have you ever had a stroke?",
                   selected = character(0),
                   choiceNames = c("No ", "Yes"),
                   choiceValues = c("no", "yes"),
                   checkIcon = list(yes = icon("check")),
                   justified = TRUE, width = "140px"),
      radioGroupButtons(inputId = "hf",
                   "Have you previously had heart failure?",
                   selected = character(0),
                   choiceNames = c("No ", "Yes"),
                   choiceValues = c("no", "yes"),
                   checkIcon = list(yes = icon("check")),
                   justified = TRUE, width = "140px"),
      radioGroupButtons(inputId = "diabetes",
                   "Have you ever had a diabetes?",
                   selected = character(0),
                   choiceNames = c("No ", "Yes"),
                   choiceValues = c("no", "yes"),
                   checkIcon = list(yes = icon("check")),
                   justified = TRUE, width = "140px"),
      radioGroupButtons(inputId = "hyperT",
                   "Have you ever had a hypertension?",
                   selected = character(0),
                   choiceNames = c("No ", "Yes"),
                   choiceValues = c("no", "yes"),
                   checkIcon = list(yes = icon("check")),
                   justified = TRUE, width = "140px"),
      radioGroupButtons(inputId = "vasc",
                   "Do you have vascular disease?",
                   selected = character(0),
                   choiceNames = c("No ", "Yes"),
                   choiceValues = c("no", "yes"),
                   checkIcon = list(yes = icon("check")),
                   justified = TRUE, width = "140px"),
      # Color buttons that are higher risk. From:
      # https://github.com/dreamRs/shinyWidgets/issues/41
      tags$script("$(\"input:radio[name='stroke'][value='yes']\").parent().css('background-color', '#EDB6B2');"),
      tags$script("$(\"input:radio[name='stroke'][value='no']\").parent().css('background-color', '#B2EDB5');"),
      
      tags$script("$(\"input:radio[name='hf'][value='yes']\").parent().css('background-color', '#EDB6B2');"),
      tags$script("$(\"input:radio[name='hf'][value='no']\").parent().css('background-color', '#B2EDB5');"),
      
      tags$script("$(\"input:radio[name='diabetes'][value='yes']\").parent().css('background-color', '#EDB6B2');"),
      tags$script("$(\"input:radio[name='diabetes'][value='no']\").parent().css('background-color', '#B2EDB5');"),
      
      tags$script("$(\"input:radio[name='hyperT'][value='yes']\").parent().css('background-color', '#EDB6B2');"),
      tags$script("$(\"input:radio[name='hyperT'][value='no']\").parent().css('background-color', '#B2EDB5');"),
      
      tags$script("$(\"input:radio[name='vasc'][value='yes']\").parent().css('background-color', '#EDB6B2');"),
      tags$script("$(\"input:radio[name='vasc'][value='no']\").parent().css('background-color', '#B2EDB5');")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tags$h2("Patient's stroke risk is estimated to be:"),
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
  
  is.only.one.value <- reactive({
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
      
      dat.sub <- stroke.dt[.(input$user_age,
                            input$sex,
                            input$stroke,
                            input$hf,
                            input$diabetes,
                            input$hyperT,
                            input$vasc),
                          range(stroke1y)]
      cond <- dat.sub[2]-dat.sub[1]<0.001
      if (cond){
        paste0(dat.sub[1], "%")
      } else if (!cond) {
        paste0("between ", dat.sub[1],
               "%", " and ",
               dat.sub[2],
               "%")
  }

           
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
