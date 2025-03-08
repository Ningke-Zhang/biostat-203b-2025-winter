library(shiny)
library(tidyverse)
library(dplyr)
library(bigrquery)

# Load data
mimic_icu_cohort <- readRDS("/Users/ningkezhang/Downloads/203b-hw-revised/hw4/mimiciv_shiny/mimic_icu_cohort.rds") |>
  mutate(insurance = as.factor(insurance),
         marital_status = as.factor(marital_status),
         gender = as.factor(gender))


# Establish BigQuery connection
satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
# BigQuery authentication using service account
bq_auth(path = satoken)
# connect to the BigQuery database `biostat-203b-2025-mimiciv_3_1`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)


# Load required tables
sid_adt <- tbl(con_bq, "transfers")
sid_lab <- tbl(con_bq, "labevents")
d_icd_procedures <- tbl(con_bq, "d_icd_procedures")
sid_proc <- tbl(con_bq, "procedures_icd") |>
  left_join(d_icd_procedures, by = c("icd_code", "icd_version"))
d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses")
sid_diag <- tbl(con_bq, "diagnoses_icd") |>
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

# Define UI
ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort"),
  
  #first tab
  tabsetPanel(
    tabPanel("Patients' Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Variable", "Select Variable to Explore",
                             choices = c(
                               "First care unit" = "first_careunit",
                               "Last care unit" = "last_careunit",
                               "Admission type" = "admission_type",
                               "Admission location" = "admission_location",
                               "Discharge location" = "discharge_location",
                               "Gender" = "gender",
                               "Race" = "race",
                               "Age" = "anchor_age",
                               "Age group" = "anchor_year_group",
                               "Martial status" = "marital_status",
                               "Language" = "language",
                               "Insurance" = "insurance",
                               "Lab Events" = "labevents",
                               "Chart Events" = "chartevents"))
               ),
               mainPanel(
                 plotOutput("cohort_plot"),
                 verbatimTextOutput("summary_output")
               )
             )
    ),
    
    #second tab
    tabPanel("patient's ADT and ICU stay information",
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
server <- function(input, output) {
  
  #first tab
  output$cohort_plot <- renderPlot({
    variable <- input$Variable
    
    if(variable %in% c("labevents", "chartevents")) {
      data <- if(variable == "labevents") {
        mimic_icu_cohort |>
          select(
            potassium, sodium, glucose, creatinine, bicarbonate, chloride) |>
          pivot_longer(
            cols = everything(), names_to = "variable", values_to = "value")
      } else {
        mimic_icu_cohort |>
          select(`respiratory rate`, `heart rate`, 
                 `non invasive blood pressure systolic`, 
                 `non invasive blood pressure diastolic`, 
                 `temperature fahrenheit`) |>
          pivot_longer(cols = everything(), 
                       names_to = "variable", values_to = "value")
      }
      
      data |>
        ggplot(aes(x = value, y = variable)) +
        geom_boxplot(notch = TRUE, 
                     outlier.shape = ifelse(input$remove, NA, 16)) +
        theme_minimal()
    } else {
      mimic_icu_cohort |>
        count(!!sym(variable)) |>
        ggplot(aes_string(x = variable, y = "n")) +
        geom_bar(stat = "identity") +
        theme_minimal()
    }
  })
  
  # generate numerical summary
  output$summary_output <- renderPrint({
    variable <- input$Variable
    
    if(variable %in% c("labevents", "chartevents")) {
      summary(mimic_icu_cohort |> 
                select(any_of(c("potassium", "sodium", "glucose", 
                                "creatinine", "bicarbonate", "chloride", 
                                "respiratory rate", "heart rate", 
                                "non invasive blood pressure systolic", 
                                "non invasive blood pressure diastolic", 
                                "temperature fahrenheit"))))
    } else {
      summary(mimic_icu_cohort |> 
                select(any_of(variable)))
    }
  })
  
  #second tab
  reactiveData <- reactive({
    req(input$PatientID)
    sid <- as.numeric(input$PatientID)
    list(
      sid_adt = sid_adt |> filter(subject_id == sid) |> collect(),
      sid_lab = sid_lab |> filter(subject_id == sid) |> collect(),
      sid_proc = sid_proc |> filter(subject_id == sid) |> collect(),
      sid_diag = sid_diag |> filter(subject_id == sid) |> collect()
    )
  })
  
  output$adt_icu <- renderPlot({
    h3_list <- reactiveData()
    sid_adt <- h3_list$sid_adt
    sid_lab <- h3_list$sid_lab
    sid_proc <- h3_list$sid_proc
    sid_diag <- h3_list$sid_diag
    
    ggplot() +
      geom_segment(data = sid_adt |> filter(eventtype != "discharge"),
                   aes(x = intime, xend = outtime, y = "ADT", yend = "ADT", 
                       color = careunit, 
                       linewidth = str_detect(careunit, "(ICU|CCU)"))) +
      geom_point(data = sid_lab |> distinct(charttime, .keep_all = TRUE),
                 aes(x = charttime, y = "Lab"), shape = '+', size = 5) +
      geom_jitter(data = sid_proc, aes(
        x = chartdate + hours(12), y = "Procedure", 
                                       shape = str_sub(long_title, 1, 25)), 
                  size = 3, height = 0) +
      theme_light() +
      theme(legend.position = "bottom", legend.box = "vertical")
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
  