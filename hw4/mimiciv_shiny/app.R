
library(shiny)
library(ggplot2)
library(dplyr)
library(bigrquery)  # For BigQuery integration
library(readr)  # For loading RDS file


# Load ICU cohort data
cohort_data <- readRDS("mimic_icu_cohort.rds")

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("ICU Cohort Data"),

    tabsetPanel(
      
      # Summary Tab
      tabPanel(
        "Summary",
        sidebarLayout(
          
          sidebarPanel(
            # Drop down menu for selecting variable group
            selectInput(
              "variable_group",
              "Select Variable Group:",
              choices = c("Demographics", "Lab Measurements", "Vitals")
            ),
            
            # Conditional drop down menu for specific variables
            uiOutput("dynamic_input")
          ),
          
          mainPanel(
            # Placeholder for displaying summary output
            textOutput("summary_output")
          )
        )
      ),
      
      # Patient Info Tab
      tabPanel(
        "Patient Info",
        sidebarLayout(
          sidebarPanel(
            # Input for Patient ID
            textInput(
              "subject_id",
              "Enter Patient's ID:",
              value = ""
            ),
            # Submit button
            actionButton("submit_id", "Submit"),
            # Select Input for ADT or ICU
            selectInput(
              "patient_plot_type",
              "Select Plot Type:",
              choices = c("ADT", "ICU stays")
            )
          ),
          
          mainPanel(
            # Output placeholders for patient search
            textOutput("patient_info_output"),
            plotOutput("patient_plot")
          )
        )
      )
      
    )
)

# Define server
server <- function(input, output, session) {
  # Dynamically render the conditional drop down menu based on variable group
  output$dynamic_input <- renderUI({
    if (input$variable_group == "Demographics") {
      selectInput(
        "demo_variable",
        "Select Demographic Variable:",
        choices = c("Insurance", "Marital Status", "Race", "Gender", 
                    "Language", "Age Intime")
      )
    } else if (input$variable_group == "Lab Measurements") {
      selectInput(
        "lab_variable",
        "Select Lab Measurement Variable:",
        choices = c("Bicarbonate", "Chloride", "Creatinine", "Glucose", 
                    "Potassium", "Sodium", "Hematocrit", "White Blood Cells")
      )
    } else if (input$variable_group == "Vitals") {
      selectInput(
        "vitals_variable",
        "Select Vitals Variable:",
        choices = c("Respiratory Rate", "Heart Rate", 
                    "Non Invasive Blood Pressure Systolic",
                    "Non Invasive Blood Pressure Diastolic", 
                    "Temperature Fahrenheit")
      )
    }
  })

  # Placeholder for summary functionality
  output$summary_output <- renderText({
    if (input$variable_group == "Demographics") {
      paste("Selected variable group:", input$variable_group, 
            "| Selected variable:", input$demo_variable)
    } else if (input$variable_group == "Lab Measurements") {
      paste("Selected variable group:", input$variable_group, 
            "| Selected variable:", input$lab_variable)
    } else if (input$variable_group == "Vitals") {
      paste("Selected variable group:", input$variable_group, 
            "| Selected variable:", input$vitals_variable)
    } else {
      paste("Selected variable group:", input$variable_group)
    }
  })
  
  # Reactive variable to store patient data
  patient_data <- reactiveVal(NULL)
  
  # Handle patient lookup when Submit button is clicked
  observeEvent(input$submit_id, {
    patient_id <- input$patient_id
    if (patient_id == "") {
      output$patient_info_output <- renderText(
        "Please enter a valid Patient ID.")
      return()
    }
    
    # Placeholder for fetching data from BigQuery
    fetched_data <- data.frame(
      time = seq(1, 10),
      ADT_value = rnorm(10, mean = 50, sd = 10),
      ICU_value = rnorm(10, mean = 100, sd = 15)
    )
    
    patient_data(fetched_data)
    
    output$patient_info_output <- renderText(paste("Patient ID:", patient_id, "data retrieved."))
  })
  
  # Generate plot based on selected patient_info_type
  output$patient_plot <- renderPlot({
    data <- patient_data()
    if (is.null(data)) return(NULL)  # If no data, return NULL
    
    if (input$patient_info_type == "ADT") {
      ggplot(data, aes(x = time, y = ADT_value)) +
        geom_line(color = "blue") +
        geom_point(color = "blue") +
        labs(title = "ADT Data Over Time", x = "Time", y = "ADT Value") +
        theme_minimal()
    } else {
      ggplot(data, aes(x = time, y = ICU_value)) +
        geom_line(color = "red") +
        geom_point(color = "red") +
        labs(title = "ICU Data Over Time", x = "Time", y = "ICU Value") +
        theme_minimal()
    }
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
