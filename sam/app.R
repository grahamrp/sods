
# Packages
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Data
survey_data <- read_csv("../data/survey_results_public.csv")

# Defining the UI part of the app
ui <- fluidPage(
  
  column(
    width = 6,
    offset = 3,
    fluidRow(
      selectInput("cols",
                  label = "Select columns to visualise",
                  choices = colnames(survey_data),
                  multiple = TRUE,
                  width = '800px')
    )
  ),
  
  # Main output
  column(
    width = 8,
    align = "center",
    offset = 2,
    uiOutput("p")
  )
  
)


# Defining the server part of the app
server <- function(input, output) {
  
  # Define our UI placeholder using the user selection
  output$p <- renderUI({
    
    # Define an empty tagList (shiny equivalent of a list)
    plots <- tagList()
    
    # Iterate over input column selection
    for (col in input$cols) {
      plots[[col]] <- plotOutput(col)
    }
    
    plots
    
  })
  
  # Trigger everytime the input columns are changed
  observe({
    
    lapply(input$cols, function(col) {
      
      # Remove NA entries
      d <- survey_data[!is.na(survey_data[[col]]), ]
      
      # Column class
      cc <- class(d[[col]])
      
      output[[col]] <- renderPlot({
        
        # Plots only defined for chracter, numeric & integer
        validate(need(cc %in% c("character", "numeric", "integer"),
                      paste("Plots undefined for class", cc)))
        
        if (cc == "character") {
          
          # Only show barplot if we have fewer than 20 bars
          validate(need(length(unique(d[[col]])) < 20,
                        paste(col, "contains more than 20 levels")))
          
          # Basic ggplot barplot
          ggplot(d, aes_string(x = col)) +
            geom_bar() +
            coord_flip() +
            theme_bw() +
            ggtitle(col)
          
        } else {
          
          # Basic ggplot histogram
          ggplot(d, aes_string(x = col)) +
            geom_histogram() +
            theme_bw() +
            ggtitle(col)
          
        }
        
      })
      
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
