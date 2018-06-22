library(caret)
library(dplyr)
library(ggplot2)
library(scales)
library(shiny)
library(shiny.semantic)
library(readr)
library(tibble)
library(xgboost)
theme_set(theme_bw())
# Final model
model <- read_rds("finalModel_xgbLinear_caret.rds")

# Lookups for inputs
lu <- read_rds("shiny_summary_data.rds")
choiceCountry <- levels(lu$Country)
choiceYearsCodingProf <- levels(lu$YearsCodingProf)
choiceSalaryType <- levels(lu$SalaryType)
choiceAge <- levels(lu$Age)
choiceNumberMonitors <- levels(lu$NumberMonitors)
choiceFormalEducation <- levels(lu$FormalEducation)

ui <- semanticPage(
  title = "Stack Overflow Developers Survey Salary Predictor",
  suppressDependencies("bootstrap"),
  
  div(class = "ui three item menu",
      div(class = "item",
          h2(class = "ui icon header", uiicon("settings"), 
             div(class = "content", "Stack Overflow Developer Survey Salary Predictor")
          )
      )
  ),
  div(class = "ui raised segment",
      a(class = "ui orange ribbon label", "Salary Predictor"),
      div(class = "ui grid",
          div(class = "four wide column",
              div(class = "ui horizontal divider",  uiicon("edit"), "Predict Your Salary!"),
              selectInput("Country", "Country", choices = choiceCountry),
              selectInput("YearsCodingProf", "YearsCodingProf", choices = choiceYearsCodingProf),
              selectInput("SalaryType", "SalaryType", choices = choiceSalaryType),
              selectInput("Age", "Age", choices = choiceAge),
              selectInput("FormalEducation", "FormalEducation", choices = choiceFormalEducation),
              selectInput("NumberMonitors", "NumberMonitors", choices = choiceNumberMonitors),
              helpText("This prediction model was built just for fun. Don't buy an extra monitor because of it! The model is built on the best six predictors of salary from the 2018 Stack Overflow Developer Survey using xgboost.")
          ),
          
          div(class = "twelve wide column",
              
              div(class = "ui horizontal divider", uiicon("chart line"), "Prediction Plot"),
              plotOutput("predHistPlt"),
              
              div(class = "ui horizontal divider", uiicon("table"), "Prediction History"),
              helpText("Current is first, predictions in USD."),
              tableOutput("predHistTbl")
          )
      )
  )
  
)

server <- shinyServer(function(input, output) {
  
  # Store prediction history as user changes profile
  rv <- reactiveValues(predictions = data.frame())
  # Single row tbl denoting currently selected profile
  currentProfile <- reactive({
    tibble(Country = input$Country,
           YearsCodingProf = input$YearsCodingProf,
           SalaryType = input$SalaryType,
           Age = input$Age,
           NumberMonitors = input$NumberMonitors,
           FormalEducation = input$FormalEducation)
  })
  
  # Make prediction and accumulate the prediction history
  observe({
    cp <- currentProfile()
    predLogSalary <- predict(model, cp)
    predSalary <- exp(1)^predLogSalary
    predictions <- cbind(cp, Prediction = predSalary)
    rv$predictions <- isolate(rbind(rv$predictions, predictions))
  })
  
  output$predHistTbl <- renderTable({
    preds <- rv$predictions
    dplyr::arrange(preds, desc(row_number(Prediction)))
  })
  
  output$predHistPlt <- renderPlot({
    ggplot(rv$predictions, aes(x = seq_along(Prediction), y = round(Prediction, 0))) +
      geom_point(size = 3, col = "orange") + geom_line(col = "darkorange") +
      labs(x = "Prediction History", y = "Predicted Salary ($)", title = "Predicted Salary in Dollars") +
      lims(x = c(1, NA), y = c(1e3, NA)) +
      scale_y_continuous(position = "right", labels = comma)
  })
  
})

shinyApp(ui = ui, server = server)
