
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
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
  "Age Intime" = "age_intime",
  "Bicarbonate" = "bicarbonate", 
  "Chloride" = "chloride", 
  "Creatinine" = "creatinine", 
  "Glucose" = "glucose", 
  "Potassium" = "potassium", 
  "Sodium" = "sodium", 
  "Hematocrit" = "hematocrit", 
  "White Blood Cells" = "wbc",
  "Respiratory Rate" = "respiratory_rate", 
  "Heart Rate" = "heart_rate", 
  "Non Invasive Blood Pressure Systolic" = "non_invasive_blood_pressure_systolic",
  "Non Invasive Blood Pressure Diastolic" = "non_invasive_blood_pressure_diastolic", 
  "Temperature Fahrenheit" = "temperature_fahrenheit"
)

# Authenticate with BigQuery
satoken <- "./../biostat-203b-2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)
# Connect to the BigQuery database `biostat-203b-2025-mimiciv_3_1`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

# Functions to retrieve ADT information and plot ADT
get_patient_info_for_title <- function(connection, id) {
  admissions <- tbl(con_bq, "admissions") |>
    filter(subject_id == id)
  
  patient_info_data <- tbl(con_bq, "patients") |>
    filter(subject_id == id) |>
    left_join(admissions, by = "subject_id") |>
    select(subject_id, gender, anchor_age, race) |>
    distinct() |>
    collect()
  
  plot_title <- paste(
    "Patient ", pull(patient_info_data, subject_id)[1], 
    ", ", pull(patient_info_data, gender)[1], 
    ", ", pull(patient_info_data, anchor_age)[1], 
    " years old, ", tolower(pull(admissions, race)[1])
  )
  
  return(plot_title)
}

get_diagnoses_for_subtitle <- function(connection, id) {
  d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses")
  
  top_3_diagnoses_data <- tbl(con_bq, "diagnoses_icd") |>
    filter(subject_id == id) |>
    count(icd_code, sort = TRUE) |>
    arrange(desc(n)) |>
    head(3) |>
    left_join(d_icd_diagnoses, by = "icd_code") |>
    select(long_title) |>
    collect()
  
  long_title <- pull(top_3_diagnoses_data, long_title)
  
  plot_subtitle <- paste(
    long_title[1], "\n",
    long_title[2], "\n",
    long_title[3],
    sep = ""
  )
  
  return(plot_subtitle)
}

get_lab_data <- function(connection, id) {
  Lab_data <- tbl(con_bq, "labevents") |>
    select(subject_id, charttime) |>
    filter(subject_id == id) |>
    distinct() |>
    collect()
  
  return(Lab_data)
}

get_ADT_data <- function(connection, id) {
  ADT_data <- tbl(con_bq, "transfers") |>
    filter(subject_id == id,
           !is.na(careunit), careunit != "UNKNOWN",
           !is.na(intime),
           !is.na(outtime)) |>
    mutate(is_icu = str_detect(careunit, 'CU')) |>
    select(subject_id, careunit, intime, outtime, is_icu) |>
    collect()
  
  return(ADT_data)
}

get_procedure_data <- function(connection, id) {
  d_icd_procedures <- tbl(con_bq, "d_icd_procedures")
  
  Procedure_data <- tbl(con_bq, "procedures_icd") |>
    filter(subject_id == id) |>
    left_join(d_icd_procedures, by = c('icd_code', 'icd_version')) |>
    select(subject_id, chartdate, long_title) |>
    collect()
  
  return(Procedure_data)
}

draw_adt <- function(connection, id) {
  plot_title <- get_patient_info_for_title(connection, id)
  plot_subtitle <- get_diagnoses_for_subtitle(connection, id)
  
  Lab <- get_lab_data(connection, id)
  ADT <- get_ADT_data(connection, id)
  Procedure <- get_procedure_data(connection, id)
  
  adt_plot <- ggplot() +
    scale_x_datetime(name = "Calendar Time",
                     limits = c(min(ADT$intime) - days(1), 
                                max(ADT$outtime))) +
    scale_y_discrete(name = NULL,
                     limits = c("Procedure", "Lab", "ADT")) +
    
    # Procedure Events as different shapes
    geom_point(
      data = Procedure,
      aes(x = as.POSIXct(chartdate), y = "Procedure", 
          shape = sub(",.*", "", long_title)),
      size = 4
    ) +
    
    # Lab Events as Crosses
    geom_point(
      data = Lab,
      aes(x = charttime, y = "Lab"),
      shape = 3, size = 3
    ) +
    
    # ADT as line segment
    geom_segment(
      data = ADT,
      aes(x = intime, xend = outtime, y = "ADT", yend = "ADT",
          color = careunit, linewidth = as.factor(is_icu))
    ) +
    
    # Formatting
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      color = "Care Unit",
      shape = "Procedure"
    ) +
    
    # Legend settings: color and shape
    guides(
      # Removes the legend for the line thickness (linewidth)
      linewidth = "none", 
      # Title for color legend
      color = guide_legend(title = "Care Unit", ncol = 3),
      # Title for shape legend
      shape = guide_legend(title = "Procedure", ncol = 2)
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",  # Stack legends vertically

      plot.title = element_text(size = 20, face = "bold"),  
      plot.subtitle = element_text(size = 16),  
      axis.title.x = element_text(size = 16),  
      axis.title.y = element_text(size = 16), 
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
      axis.text.y = element_text(size = 14),  
      legend.title = element_text(size = 14, face = "bold"), 
      legend.text = element_text(size = 12), 
    )
  
  return(adt_plot)
}

# Functions to retrieve ICU stay information and plot ICU stay
get_ICU_data <- function(connection, id) {
  d_items <- tbl(connection, "d_items") |>
    select(itemid, label, abbreviation) |>
    filter(abbreviation %in% c("HR", "NBPd", "NBPs", "RR", "Temperature F")) |>
    mutate(itemid = as.character(itemid))
  
  itemids <- d_items |>
    pull(itemid) |>
    as.character()
  
  ICU <- tbl(connection, "chartevents") |>
    mutate(itemid = as.character(itemid)) |> # convert to character
    filter(subject_id == id,
           itemid %in% itemids) |>
    select(subject_id, stay_id, charttime, itemid, valuenum) |>
    left_join(d_items, by = "itemid") |>
    collect()
  
  return(ICU)
}

draw_icu <- function(connection, id) {
  ICU <- get_ICU_data(connection, id)
  
  icu_plot <- ggplot(ICU, aes(x = charttime, y = valuenum, color = abbreviation)) +
    geom_point() +
    geom_line() +
    facet_grid(abbreviation ~ stay_id, scales = "free") +
    
    labs(
      title = paste("Patient", ICU$subject_id[1], "ICU stays - Vitals"),
      x = "",
      y = ""
    ) +
    
    theme_light() +
    theme(
      legend.position = "none",
      
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"), 
      axis.text.x = element_text(size = 14, angle = 35, hjust = 1),
      axis.text.y = element_text(size = 14), 
      strip.text = element_text(size = 16, face = "bold"), 
      strip.text.y = element_text(size = 14, face = "bold")
    ) +
    scale_x_datetime(date_labels = "%b %d %H:%M")
  
  return(icu_plot)
}

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
            ),
            
            conditionalPanel(
              condition = "input.variable_group == 'Lab Measurements' || input.variable_group == 'Vitals'",
              uiOutput("dynamic_lab_slider")
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
        choices = c("Bicarbonate" = "bicarbonate", 
                    "Chloride" = "chloride", 
                    "Creatinine" = "creatinine", 
                    "Glucose" = "glucose", 
                    "Potassium" = "potassium", 
                    "Sodium" = "sodium", 
                    "Hematocrit" = "hematocrit", 
                    "White Blood Cells" = "wbc")
      )
    } else if (input$variable_group == "Vitals") {
      selectInput(
        "vitals_variable",
        "Select Vitals Variable:",
        choices <- c(
          "Respiratory Rate" = "respiratory_rate", 
          "Heart Rate" = "heart_rate", 
          "Non Invasive Blood Pressure Systolic" = 
            "non_invasive_blood_pressure_systolic",
          "Non Invasive Blood Pressure Diastolic" = 
            "non_invasive_blood_pressure_diastolic", 
          "Temperature Fahrenheit" = "temperature_fahrenheit"
        )
        
      )
    }
  })
  
  # Dynamically render the conditional sliderInput based on variable
  output$dynamic_lab_slider <- renderUI({
    if(input$variable_group %in% c("Lab Measurements", "Vitals")) {
      if (input$variable_group == "Lab Measurements") {
        selected_lab_var <- input$lab_variable
      } else {selected_lab_var <- input$vitals_variable}
      
      data <- cohort_data[[selected_lab_var]]
      
      q1 <- quantile(data, 0.01, na.rm = TRUE)
      q99 <- quantile(data, 0.99, na.rm = TRUE)
      
      min_val <- min(data, na.rm = TRUE)
      max_val <- max(data, na.rm = TRUE)
      
      sliderInput(
        "xlim_lab", "X-axis Limits:", min = min_val, max = max_val,
        value = c(q1, q99) # default view excludes extreme outliers
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
    } 
    
    # Generate histogram and summary for lab and vitals
    else if (input$variable_group %in% c("Lab Measurements", "Vitals")) {
      if (input$variable_group == "Lab Measurements") {
        selected_var <- input$lab_variable
      } else {selected_var <- input$vitals_variable}
      
      variable_label <- names(
        user_friendly_labels[user_friendly_labels == selected_var])
      
      ggplot(cohort_data, aes_string(x = selected_var)) +
        geom_histogram() +
        labs(title = paste("Distribution of", variable_label),
             x = variable_label,
             y = "Count") +
        theme_minimal() +
        xlim(input$xlim_lab[1], input$xlim_lab[2]) +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)
        )
    }
  })
  
  output$summary_stats <- renderPrint({
    if (input$variable_group == "Demographics") {
      selected_var <- input$demo_variable
      summary(cohort_data[[selected_var]])
    }
    else if (input$variable_group  %in% c("Lab Measurements", "Vitals")) {
      if (input$variable_group == "Lab Measurements") {
        selected_var <- input$lab_variable
      } else {selected_var <- input$vitals_variable}
      summary(cohort_data[[selected_var]])
    }
  })
  
  # Handle patient lookup when Submit button is clicked
  observeEvent(input$submit_id, {
    id <- input$subject_id
    
    # check entered subject ID is valid
    if (is.null(id) || !(id %in% cohort_data$subject_id)) {
      output$patient_info_output <- renderText(
        "Please enter a valid Patient ID.")
      return()
    }
    
    output$patient_info_output <- renderText(
      paste("Patient ID:", id, "data retrieved.")
    )
  })
  
  # Generate plot based on selected patient_info_type
  output$patient_plot <- renderPlot({
    id <- input$subject_id
    
    if (is.null(id) || !(id %in% cohort_data$subject_id)) return()
    
    id <- as.integer(id)
    
    if (input$patient_plot_type == "ADT") {
      
      draw_adt(con_bq, id)
      
    } else if (input$patient_plot_type == "ICU stays"){
      
      draw_icu(con_bq, id)
    }
  }, height = 600)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
