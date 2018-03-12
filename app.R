library(shiny)
library(riskvisrr)
library(data.table)
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

# functions ---------------------------------------------------------------
jsButtonColor <- function(var, color, value){
  # Makes simple js code to color buttons in shinyWidgets library. From:
  # https://github.com/dreamRs/shinyWidgets/issues/41
  # var, color and yes/no need to be in quotes
  paste0("$(\"input:radio[name='",
         var,
         "'][value='",
         paste0("&quote",value, "&quote"),
         "']\").parent().css('background-color', '",
         color,
         "');")  
}
depSub <- function(a){
  # shortcut helper n
  deparse(substitute(a))
}
f <- function(x) substitute(x)
g <- function(x) {
  # browser()
  deparse(f(x))
}
class(substitute(1:10))
class(quote(1:10))

evPar <- function(x){
  # short helper fun
  eval(parse(text = x))
}
# jsButtonColor("stroke", "EDB6B2", depSub(Yes))

# Variables ---------------------------------------------------------------

button.width <- "210px"


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Stroke Risk Communication"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$style("#user_age {font-size:38px;height:50px; width: 110px;}"),
      numericInput(
        inputId = "user_age",
        "Patient's age:",
        value = NULL,
        min = 20,
        max = 99
      ),
      
      radioGroupButtons(
        inputId = "sex",
        label = "Male or female:",
        choices = c(
          "Not <br>selected" = depSub(c("no", "yes")),
          "Male<br>" = depSub("no"),
          "Female" = depSub("yes")
        ),
        selected =  depSub(c("no", "yes")),
        justified = TRUE,
        width = button.width
      ),
      
      radioGroupButtons(
        inputId = "stroke",
        "Have you ever had a stroke?",
        choiceNames = c(HTML("Not <br/>selected"), "No", "Yes"),
        selected =  depSub(c("no", "yes")),
        choiceValues = c(depSub(c("no", "yes")),
                         depSub("no"),
                         depSub("yes")),
        checkIcon = list(yes = icon("check")),
        justified = TRUE,
        width = button.width
      ),
      radioGroupButtons(
        inputId = "hf",
        "Have you previously had heart failure?",
        choiceNames = c(HTML("Not <br/>selected"), "No", "Yes"),
        selected =  depSub(c("no", "yes")),
        choiceValues = c(depSub(c("no", "yes")),
                         depSub("no"),
                         depSub("yes")),
        checkIcon = list(yes = icon("check")),
        justified = TRUE,
        width = button.width
      ),
      radioGroupButtons(
        inputId = "diabetes",
        "Have you ever had a diabetes?",
        choiceNames = c(HTML("Not <br/>selected"), "No", "Yes"),
        selected =  depSub(c("no", "yes")),
        choiceValues = c(depSub(c("no", "yes")),
                         depSub("no"),
                         depSub("yes")),
        checkIcon = list(yes = icon("check")),
        justified = TRUE,
        width = button.width
      ),
      radioGroupButtons(
        inputId = "hyperT",
        "Have you ever had a hypertension?",
        choiceNames = c(HTML("Not <br/>selected"), "No", "Yes"),
        selected =  depSub(c("no", "yes")),
        choiceValues = c(depSub(c("no", "yes")),
                         depSub("no"),
                         depSub("yes")),
        checkIcon = list(yes = icon("check")),
        justified = TRUE,
        width = button.width
      ),
      radioGroupButtons(
        inputId = "vasc",
        "Do you have vascular disease?",
        choiceNames = c(HTML("Not <br/>selected"), "No", "Yes"),
        selected =  depSub(c("no", "yes")),
        choiceValues = c(depSub(c("no", "yes")),
                         depSub("no"),
                         depSub("yes")),
        checkIcon = list(yes = icon("check")),
        justified = TRUE,
        width = button.width
      ),
      
      # Color buttons that are higher risk.
      tags$script(
        "$(\"input:radio[name='stroke'][value='&quot;yes&quot;']\").parent().css('background-color','#EDB6B2');"
      ),
      tags$script(jsButtonColor("stroke", "#B2EDB5", depSub("no"))),
      
      tags$script(jsButtonColor("hf", "#EDB6B2", "yes")),
      tags$script(jsButtonColor("hf", "#B2EDB5", "no")),
      
      tags$script(jsButtonColor("diabetes", "#EDB6B2", "yes")),
      tags$script(jsButtonColor("diabetes", "#B2EDB5", "no")),
      
      tags$script(jsButtonColor("hyperT", "#EDB6B2", "yes")),
      tags$script(jsButtonColor("hyperT", "#B2EDB5", "no")),
      
      tags$script(jsButtonColor("vasc", "#EDB6B2", "yes")),
      tags$script(jsButtonColor("vasc", "#B2EDB5", "no"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tags$h2("Patient's stroke risk is estimated to be:"),
      tags$h2(strong(textOutput("textRisk"))),
      br(),
      hr()
      # tags$h3("Table for error checking"),
      # br(),
      # DT::dataTableOutput("table")
    )
  ))
# SERVER ------------------------------------------------------------------


# Define server logic
server <- function(input, output) {
  is.valid.age <- reactive({
    # This is only run if the created expression is evaluated inside another
    # reactive function - I think.
    !is.na(input$user_age) &&
      input$user_age >= 20 &&
      input$user_age <= 99
  })
  
  is.only.one.value <- reactive({
    !is.na(input$user_age) &&
      input$user_age >= 20 &&
      input$user_age <= 99
  })
  output$textRisk <- renderText({
    
    # Order of subset arguments must be same order as new.col.order variable set in
    # intro.
    # browser()
    if (is.valid.age()) {
      # browser()
      a <- input$sex
      
      dat.sub <- stroke.dt[age == input$user_age &
                             female %in% evPar(input$sex) &
                             stroke %in% evPar(input$stroke) &
                             heartfailure%in% evPar(input$hf) &
                             diabetes %in% evPar(input$diabetes) &
                             hypertension %in% evPar(input$hyperT) &
                             vascular %in% evPar(input$vasc),
                           range(stroke1y)]
      
      one.value <- dat.sub[2]-dat.sub[1]<0.001
      if (one.value){
        paste0(dat.sub[1], "%")
      } else if (!one.value) {
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
    DT::datatable(stroke.dt[age == input$user_age &
                              female %in% evPar(input$sex) &
                              stroke %in% evPar(input$stroke) &
                              heartfailure %in% evPar(input$hf) &
                              diabetes %in% evPar(input$diabetes) &
                              hypertension %in% evPar(input$hyperT) &
                              vascular %in% evPar(input$vasc),
                            1:3])
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
