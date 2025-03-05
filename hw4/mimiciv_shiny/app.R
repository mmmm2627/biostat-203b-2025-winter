
library(shiny)
library(ggplot2)
library(dplyr)
library(bigrquery)  # For BigQuery integration
library(readr)  # For loading RDS file


# Load ICU cohort data
cohort_data <- readRDS("mimic_icu_cohort.rds")
cohort_data <- cohort_data |>
  mutate(insurance = as.factor(insurance),
         marital_status = as.factor(marital_status),
         gender = as.factor(gender),
         language = as.factor(language))

# Mapping for user-friendly labels in plots
user_friendly_labels <- c(
  "Insurance" = "insurance", 
  "Marital Status" = "marital_status", 
  "Race" = "race", 
  "Gender" = "gender", 
  "Language" = "language", 
  "Age Intime" = "age_intime"
)

# Define UI
ui <- fluidPage(
    tags$style(HTML("
      .shiny-text-output {
        font-size: 16px;
      }
      .shiny-plot-output {
        font-size: 18px;
      }
    ")),
  
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
              "variable_group", "Select Variable Group:",
              choices = c("Demographics", "Lab Measurements", "Vitals")
            ),
            
            # Conditional drop down menu for specific variables
            uiOutput("dynamic_input"),
            
            # Numeric inputs for x-axis limits (shown only for age_intime)
            conditionalPanel(
              condition = 
                "input.variable_group == 'Demographics' && input.demo_variable == 'age_intime'",
              sliderInput("xlim_age_intime", "X-axis Limits:",
                          min = 18, max = 103, value = c(18, 103))
            )
          ),
          
          mainPanel(
            # Display summary bar plot
            plotOutput("summary_plot"),
            verbatimTextOutput("summary_stats")
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
        choices = c("Insurance" = "insurance", 
                    "Marital Status" = "marital_status", 
                    "Race" = "race", "Gender" = "gender", 
                    "Language" = "language", 
                    "Age Intime" = "age_intime")
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

  # Summary functionality
  output$summary_plot <- renderPlot({
    # Generate bar plot and summary for `Demographics`
    if (input$variable_group == "Demographics") {
      selected_var <- input$demo_variable
      variable_label <- names(
        user_friendly_labels[user_friendly_labels == selected_var])
      
      if (selected_var == "language") {
        ggplot(cohort_data, aes_string(x = selected_var, fill = selected_var)) +
          geom_bar() +
          labs(title = paste("Distribution of", variable_label),
               x = selected_var,
               y = "Count",
               fill = variable_label) +
          theme_minimal() +
          coord_flip() +
          theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 16), 
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 14)
          )
      } else if (selected_var == "age_intime") {
        ggplot(cohort_data, aes_string(x = selected_var)) +
          geom_histogram() +
          labs(title = paste("Distribution of", variable_label),
               x = "Age Intime",
               y = "Count") +
          theme_minimal() +
          xlim(input$xlim_age_intime[1], input$xlim_age_intime[2]) +
          theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14)
          )
      } else {
        ggplot(cohort_data, aes_string(x = selected_var, fill = selected_var)) +
          geom_bar() +
          labs(title = paste("Distribution of", variable_label),
               x = selected_var,
               y = "Count",
               fill = variable_label) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, face = "bold"),  
            axis.title = element_text(size = 16), 
            axis.text = element_text(size = 14),  
            legend.title = element_text(size = 18, face = "bold"),
            legend.text = element_text(size = 16)
          )
      }
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
  
  output$summary_stats <- renderPrint({
    if (input$variable_group == "Demographics") {
      selected_var <- input$demo_variable
      summary(cohort_data[[selected_var]])
    }
  })
  
  # Reactive variable to store patient data
  patient_data <- reactiveVal(NULL)
  
  # Handle patient lookup when Submit button is clicked
  observeEvent(input$submit_id, {
    subject_id <- input$subject_id
    if (is.null(subject_id) || subject_id == "") {
      output$patient_info_output <- renderText("Please enter a valid Patient ID.")
      return()
    }
    
    # Placeholder for fetching data from BigQuery
    fetched_data <- data.frame(
      time = seq(1, 10),
      ADT_value = rnorm(10, mean = 50, sd = 10),
      ICU_value = rnorm(10, mean = 100, sd = 15)
    )
    
    patient_data(fetched_data)
    
    output$patient_info_output <- renderText(paste("Patient ID:", subject_id, "data retrieved."))
  })
  
  # Generate plot based on selected patient_info_type
  output$patient_plot <- renderPlot({
    data <- patient_data()
    if (is.null(data)) return(NULL)  # If no data, return NULL
    
    if (input$patient_plot_type == "ADT") {
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
