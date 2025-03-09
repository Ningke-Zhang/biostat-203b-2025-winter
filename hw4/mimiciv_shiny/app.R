library(shiny)
library(tidyverse)
library(dplyr)
library(bigrquery)
library(shinythemes)

# Load data
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds") |>
  mutate(insurance = as.factor(insurance),
         marital_status = as.factor(marital_status),
         gender = as.factor(gender)) |>
  mutate(intime_age = anchor_age + year(intime) - anchor_year)

# Establish BigQuery connection
satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

# Load required tables
transfers <- tbl(con_bq, "transfers")
labevents <- tbl(con_bq, "labevents")
d_icd_procedures <- tbl(con_bq, "d_icd_procedures")
proc <- tbl(con_bq, "procedures_icd") |>
  left_join(d_icd_procedures, by = c("icd_code", "icd_version"))
d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses")
diag <- tbl(con_bq, "diagnoses_icd") |>
  left_join(d_icd_diagnoses, by = c("icd_code", "icd_version"))
chartevents <- tbl(con_bq, "chartevents")

patient_id <- mimic_icu_cohort |>
  select(subject_id) |>
  collect() |>
  pull(subject_id)

items <- tbl(con_bq, "d_items") |>
  select(itemid, label, abbreviation) |>
  filter(abbreviation %in% c("HR", "NBPd", 
                             "NBPs", "RR", 
                             "Temperature F")) |>
  collect()

#define var group
variable_groups <- list(
  Demo = c("first_careunit", "admission_type", "admission_location", 
           "discharge_location", "gender", "race", "intime_age", 
           "anchor_year_group", "marital_status", "language", "insurance"),
  Vital = c("heart_rate", 
            "non_invasive_blood_pressure_systolic", 
            "non_invasive_blood_pressure_diastolic", 
            "respiratory_rate", 
            "temperature_fahrenheit"),
  Lab = c("creatinine", "potassium", "sodium", "chloride", "bicarbonate",
          "hematocrit", "wbc", "glucose")
)
variable_display_names <- c(
  first_careunit = "First Care Unit",
  admission_type = "Admission Type",
  admission_location = "Admission Location",
  discharge_location = "Discharge Location",
  gender = "Gender",
  race = "Race",
  intime_age = "Age at Admission",
  anchor_year_group = "Age Group",
  marital_status = "Marital Status",
  language = "Language",
  insurance = "Insurance",
  heart_rate = "Heart Rate",
  non_invasive_blood_pressure_systolic = "Systolic Blood Pressure",
  non_invasive_blood_pressure_diastolic = "Diastolic Blood Pressure",
  respiratory_rate = "Respiratory Rate",
  temperature_fahrenheit = "Temperature (Fahrenheit)",
  creatinine = "Creatinine",
  potassium = "Potassium",
  sodium = "Sodium",
  chloride = "Chloride",
  bicarbonate = "Bicarbonate",
  hematocrit = "Hematocrit",
  wbc = "White Blood Cell Count",
  glucose = "Glucose"
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("MIMIC-IV ICU Cohort"),
  #first tab
  tabsetPanel(
    tabPanel("Patients' Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("VariableGroup", "Variable Group",
                             choices = c("Demo", "Vital", "Lab")),
                 
                 uiOutput("variable_selector"),
                 
                 uiOutput("dynamic_slider")
               ),
               
               mainPanel(
                 plotOutput("cohort_plot"),
                 verbatimTextOutput("summary_output")
               )
             )
    ),
    #second tab
    tabPanel("Patient's ADT and ICU stay information",
             sidebarLayout(
               sidebarPanel(
                 selectInput("PatientID", "Patient ID", choices = patient_id),
                 radioButtons("plot_choice", "Select Plot to Display:",
                              choices = c("ADT/ICU Timeline" = "adt_icu",
                                          "Vitals Over Time" = "vitals_line_plot"),
                              selected = "adt_icu")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.plot_choice == 'adt_icu'",
                   plotOutput("adt_icu")),
                 
                 conditionalPanel(
                   condition = "input.plot_choice == 'vitals_line_plot'",
                   plotOutput("vitals_line_plot"))
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  #first tab
  output$variable_selector <- renderUI({
    req(input$VariableGroup)
    vars <- variable_groups[[input$VariableGroup]]
    if (length(vars) == 0) return(NULL)
    default_var <- vars[1]
    selectInput("Variable", "Variable", 
                choices = setNames(vars, variable_display_names[vars]), 
                selected = default_var)
  })
  
  variable_range <- reactive({
    req(input$Variable)  
    if (!input$Variable %in% names(mimic_icu_cohort)) return(NULL)  
    data_column <- mimic_icu_cohort[[input$Variable]]
    if (is.numeric(data_column)) {
      list(min = min(data_column, na.rm = TRUE), max = max(data_column, na.rm = TRUE))
    } else {
      return(NULL)
    }
  })
  
  output$dynamic_slider <- renderUI({
    req(input$Variable, variable_range())  
    if (is.numeric(mimic_icu_cohort[[input$Variable]])) {
      sliderInput("value_range", 
                  label = paste("Select Range for", input$Variable), 
                  min = variable_range()$min, 
                  max = variable_range()$max, 
                  value = c(variable_range()$min, variable_range()$max), 
                  step = 1)
    } else {
      return(NULL)
    }
  })
  
  filtered_data <- reactive({
    data <- mimic_icu_cohort
    if (is.numeric(data[[input$Variable]])) {
      data <- data |> filter(.data[[input$Variable]] >= input$value_range[1] & 
                                .data[[input$Variable]] <= input$value_range[2])
    }
    return(data)
  })
  
  output$cohort_plot <- renderPlot({
    data <- filtered_data()
    
    if (input$VariableGroup %in% c("lab", "vital")) {
      selected_data <- data |> select(any_of(variable_groups[[input$VariableGroup]]))
      if (ncol(selected_data) == 0) {
        return(NULL)
      }
      selected_data |>  
        pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>  
        ggplot(aes(x = value, y = variable)) +  
        geom_boxplot(notch = TRUE, outlier.shape = 16) +  
        theme_minimal() +
        labs(title = paste("Distribution of", input$Variable),
             x = "Measurement Value", y = "Variable")
      
    } else {
      data |>  
        count(!!sym(input$Variable)) |>  
        ggplot(aes_string(x = input$Variable, y = "n")) +  
        geom_bar(stat = "identity", fill = "steelblue", color = "black") +  
        theme_minimal() +
        labs(title = paste("Distribution of", input$Variable),
             x = "Category", y = "Count")
    }
  })
  
  output$summary_output <- renderPrint({
    summary(filtered_data() |> select(any_of(input$Variable)))
  })
  
  #second tab
  reactiveData <- reactive({
    req(input$PatientID)
    sid <- as.numeric(input$PatientID)
    list(
      transfers = transfers |> filter(subject_id == sid) |> collect(),
      labevents = labevents |> filter(subject_id == sid) |> collect(),
      proc = proc |> filter(subject_id == sid) |> collect(),
      diag = diag |> filter(subject_id == sid) |> collect()
    )
  })
  
  output$adt_icu <- renderPlot({
    h3_list <- reactiveData()
    transfers <- h3_list$transfers
    labevents <- h3_list$labevents
    proc <- h3_list$proc
    diag <- h3_list$diag
    
    patient_info <- mimic_icu_cohort |> filter(subject_id == input$PatientID)
    
    ggplot() +
      geom_segment(data = transfers |> filter(eventtype != "discharge"),
                   aes(x = intime, xend = outtime, y = "ADT", yend = "ADT", 
                       color = careunit, 
                       linewidth = str_detect(careunit, "(ICU|CCU)")),
                   show.legend = c(linewidth = FALSE)) +
      geom_point(data = labevents |> distinct(charttime, .keep_all = TRUE),
                 aes(x = charttime, y = "Lab"), shape = '+', size = 5) +
      geom_jitter(data = proc, aes(
        x = chartdate + hours(12), y = "Procedure", 
        shape = str_sub(long_title, 1, 25)), 
        size = 3, height = 0) +
      labs(
        title = paste("Patient", input$PatientID, ",", 
                      patient_info$gender, ",", 
                      patient_info$anchor_age, "years old,", patient_info$race),
        subtitle = str_c(str_to_lower(head(unique(diag$long_title), 3)), 
                         collapse = "\n"),
        x = "Calendar Time",
        y = NULL
      ) +
      theme_light() +
      theme(
        legend.position = "bottom", 
        legend.box = "vertical"
      ) +
      guides(
        color = guide_legend(title = "Care Unit"),
        shape = guide_legend(title = "Procedure")
      )
  })
  
  output$vitals_line_plot <- renderPlot({
    req(input$PatientID)
    
    patient_id <- as.numeric(input$PatientID)
    
    chartevents <- chartevents |>
      filter(subject_id == patient_id) |>
      filter(itemid %in% c(220045, 220179, 
                           220180, 220210, 
                           223761)) |>
      select(-c(hadm_id, caregiver_id, storetime, warning)) |>
      collect() |>
      left_join(items, by = c("itemid" = "itemid")) 
    
    ggplot(chartevents,
           aes(x = charttime,
               y = valuenum,
               color = abbreviation)) +
      geom_line() +
      geom_point() +
      facet_grid(abbreviation ~ stay_id, scales = "free") +
      labs(title = paste("Patient", 
                         patient_id, 
                         "ICU stays - Vitals"),
           x = "",
           y = "") +
      theme_light(base_size = 9) +
      theme(legend.position = "none") +
      guides(fill = 'none') +
      scale_x_datetime(
        guide = guide_axis(n.dodge = 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)