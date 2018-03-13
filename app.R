# This intro does not seem to run when script is run as a Shiny app. Must
# install before running script.
if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny", "data.table", "shinyWidgets", "devtools", "shinythemes")
if (!require("riskvisrr")) devtools::install_github("matthew-phelps/riskvisrr")

library(shiny)
library(riskvisrr)
library(data.table)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
source("functions.R")
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


# jsButtonColor("stroke", "EDB6B2", depSub(Yes))

# Variables ---------------------------------------------------------------

button.width <- "320px"


# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  # shinythemes::themeSelector(),
  titlePanel("Draft stroke risk calculator"),
  theme = shinythemes::shinytheme(theme = "yeti"),
  # Sidebar with a slider input for number of bins
  fluidRow(
    
    column(6,
           div(
             tags$head(
               tags$style(type = "text/css", ".container-fluid {  max-width:760px;}") # max width for column
             ),
             wellPanel(
               tags$style("#user_age {font-size:38px;height:50px; width: 110px;}"), # font size 
               textInput(
                 inputId = "user_age",
                 label = "Patient's age:",
                 value = NULL,
                 placeholder = "Age"
               ),
               radioGroupButtons(
                 inputId = "sex",
                 label = "Male or female:",
                 choices = c(
                   "Unknown" = depSub(c("no", "yes")),
                   "Male<br>" = depSub("no"),
                   "Female" = depSub("yes")
                 ),
                 checkIcon = list(yes = icon("check")),
                 selected =  depSub(c("no", "yes")),
                 justified = TRUE,
                 width = button.width
               ),
               
               radioGroupButtons(
                 inputId = "stroke",
                 "Have you ever had a stroke?",
                 choiceNames = c(HTML("Unknown"), "No", "Yes"),
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
                 choiceNames = c(HTML("Unknown"), "No", "Yes"),
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
                 choiceNames = c(HTML("Unknown"), "No", "Yes"),
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
                 choiceNames = c(HTML("Unknown"), "No", "Yes"),
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
                 choiceNames = c(HTML("Unknown"), "No", "Yes"),
                 selected =  depSub(c("no", "yes")),
                 choiceValues = c(depSub(c("no", "yes")),
                                  depSub("no"),
                                  depSub("yes")),
                 checkIcon = list(yes = icon("check")),
                 justified = TRUE,
                 width = button.width
               ),
               # tags$script(jsButtonColor("stroke", "#B2EDB5", depSub('no'))),
               tags$script(
                 "$(\"input:radio[name='stroke'][value= 'no']\").parent().css('background-color', '#DE6B63');"
               )
             )
           )),
    
    # Show a plot of the generated distribution
    column(
      6,
      useShinyjs(),
      
      tags$h2("1-year", strong("stroke"), "risk is estimated to be:"),
      tags$h2(strong(textOutput("strokeRisk"))),
      br(),
      tags$h2("1-year", strong("thromboembolism"), "risk is estimated to be:"),
      tags$h2(strong(textOutput("thromb1y"))),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      hr(),
      br(),
      tags$h4(("Table below only for testing - will not be included in final version")
      ),
      shinyjs::runcodeUI(),
      br(),
      DT::dataTableOutput("table")
    )
  ))
# SERVER ------------------------------------------------------------------


# Define server logic

server <- function(input, output) {
  txt2num <- reactive({
    as.numeric(input$user_age)
  })
 
  shinyjs::runcodeServer()
  output$strokeRisk <- renderText({
    
    # Order of subset arguments must be same order as new.col.order variable set in
    # intro.
    # browser()
    age_as_number <- txt2num()
    if (is.valid.age(age_as_number)) {
      # browser()
      
      
      dat.sub <- stroke.dt[age == age_as_number &
                             female %in% evPar(input$sex) &
                             stroke %in% evPar(input$stroke) &
                             heartfailure %in% evPar(input$hf) &
                             diabetes %in% evPar(input$diabetes) &
                             hypertension %in% evPar(input$hyperT) &
                             vascular %in% evPar(input$vasc),
                           range(stroke1y)]
      
      one.value <- dat.sub[2] - dat.sub[1] < 0.001
      if (one.value) {
        paste0(dat.sub[1], "%")
      } else if (!one.value) {
        paste0("between ", dat.sub[1],
               "%", " and ",
               dat.sub[2],
               "%")
      }
      
      
    }
    else {
      paste0("Please enter a whole number between 20 - 99")
    }
      
  })
  
  output$thromb1y <- renderText({
    
    # Order of subset arguments must be same order as new.col.order variable set in
    # intro.
    # browser()
    age_as_number <- txt2num()
    if (is.valid.age(age_as_number)) {
      # browser()
      
      
      dat.sub <- stroke.dt[age == age_as_number &
                             female %in% evPar(input$sex) &
                             stroke %in% evPar(input$stroke) &
                             heartfailure %in% evPar(input$hf) &
                             diabetes %in% evPar(input$diabetes) &
                             hypertension %in% evPar(input$hyperT) &
                             vascular %in% evPar(input$vasc),
                           range(thromb1y)]
      
      one.value <- dat.sub[2] - dat.sub[1] < 0.001
      if (one.value) {
        paste0(dat.sub[1], "%")
      } else if (!one.value) {
        paste0("between ", dat.sub[1],
               "%", " and ",
               dat.sub[2],
               "%")
      }
      
      
    }
    else {
      paste0("Please enter a whole number between 20 - 99")
    }
    
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(filter = "none",
                  rownames = FALSE,
                  options = list(sDom  = '<"top">rt<"bottom">ip'),
                  stroke.dt[age == input$user_age &
                              female %in% evPar(input$sex) &
                              stroke %in% evPar(input$stroke) &
                              heartfailure %in% evPar(input$hf) &
                              diabetes %in% evPar(input$diabetes) &
                              hypertension %in% evPar(input$hyperT) &
                              vascular %in% evPar(input$vasc),
                            c(1,2,3,10, 16)])
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
