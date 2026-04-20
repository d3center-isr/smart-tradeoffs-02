library(shiny)
library(rhandsontable)
library(dplyr)

theme_name <- "readable"
# e.g., "superhero", "readable", "cerulean", "journal", "default", "paper"

###############################################################################
# Formulas to standardized effect size
###############################################################################

# What is standardized effect size for comparing A vs. B+C ? ------------------
GetEffectSizeStage1 <- function(type1_error_rate = 0.05, 
                                sample_size = 100,
                                power_test = 0.80,
                                prob_stage1 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - prob_stage1))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

# What is standardized effect size for comparing B vs. C ? --------------------
GetEffectSizeStage2 <- function(type1_error_rate = 0.05, 
                                sample_size = 100,
                                power_test = 0.80,
                                prob_stage1 = 0.50,
                                prob_stage2_given_0 = 0.50){
  
  q_parameter <- 1/((1 - prob_stage1)*(prob_stage2_given_0)) + 1/((1 - prob_stage1)*(1 - prob_stage2_given_0))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

# What is standardized effect size for comparing A vs. C, or A vs. B? ---------
GetEffectSizeDifferentStages <- function(type1_error_rate = 0.05, 
                                         sample_size = 100,
                                         power_test = 0.80,
                                         prob_stage1 = 0.50,
                                         prob_stage2_given_0 = 0.50,
                                         a2 = 0){
  
  if(a2 == 0){
    # Note that to compare A vs. C, set a2=0
    q_parameter <- 1/prob_stage1 + 1/((1 - prob_stage1)*(1 - prob_stage2_given_0))
  }else{
    # Note that to compare A vs. B, set a2=1
    q_parameter <- 1/prob_stage1 + 1/((1 - prob_stage1)*(prob_stage2_given_0))
  }
  
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

###############################################################################
# Shiny App UI
###############################################################################

ui <- navbarPage(
  title = "An App for Exploring Tradeoffs in Design and Sample Size Planning for a Non-Prototypical SMART",
  collapsible = TRUE,
  theme=shinythemes::shinytheme(theme_name),
  tabPanel(title = "Simple Display",
           sidebarLayout(
             sidebarPanel(
               width = 5,
               # Inputs
               h3("SMART Design Settings"),
               sliderInput(inputId = "table_simple_sample_size", 
                           label = "Total number of participants in the SMART", 
                           value = 300, 
                           ticks = FALSE,
                           min = 25, 
                           max = 1000,
                           step = 1),
               radioButtons(inputId="balance_buttons", 
                            label = "Participant allocation to first-stage and second-stage intervention options",
                            choices = list("Balance within each randomization" = 1,
                                           "Balance across all three cells" = 2), 
                            selected = 1),
               textOutput("explanation_for_simple_allocation") %>% tagAppendAttributes(style = "border: 1px solid lightgray; padding: 10px;"),
               h3("Significance Test Settings"),
               rHandsontableOutput("hot_table_simple_test"),
               helpText("Note: Type-1 error rate and power are editable values in the table. The table only accepts type-1 error rates within the range [0, 0.5] and power within the range [0.5, 1]."),
               h3("About Using the App"),
               helpText("MDSEs are automatically refreshed whenever a new SMART design setting is considered (e.g., changing the number of participants or randomization probabilities) or a new significance test setting is considered (e.g., changing type-1 error rate or power)"),
               textOutput("notification_simple") %>% tagAppendAttributes(style = "border: 4px solid black; background-color: #FFECB3; padding: 10px;")
             ),
             mainPanel(
               width = 7,
               # Outputs
               h3("Minimum Detectable Standardized Effect (MDSE)"),
               h4("Defined as Cohen's d"),
               tableOutput("table_simple"),
               
               hr(),
               
               h5("Working assumptions behind the calculated MDSEs:"),
               tags$ul(
                 tags$li("End-of-study outcome is a continuous variable."),
                 tags$li("These calculations do not account for missing data. It is best to inflate the initial sample size in order to have at least the needed sample size after expected percentage of dropout."),
                 tags$li("Residual error variances among experimental conditions being compared are equal.")
               ),
               
               hr(),
               
               h5("Design of the SMART:"),
               
               tags$img(src = file.path("images", "smart_nonprototypical.png"), width = 700, height = 307, units = "inches", alt = "Non-Prototypical SMART"),
               
               tags$ul(
                 tags$li("A1 denotes first-stage treatment assignment with two options, +1 or -1."),
                 tags$li("Among participants whose first-stage treatment assignment was A1=-1, A2 denotes second-stage treatment assignment with two options, +1 or -1."),
               )
             )
           )
  ), 
  tabPanel(title = "Flexible Display",
           sidebarLayout(
             sidebarPanel(
               width = 5,
               # Inputs
               h3("SMART Design Settings"),
               sliderInput(inputId = "table_full_sample_size", 
                           label = "Total number of participants in the SMART", 
                           value = 300, 
                           ticks = FALSE,
                           min = 25, 
                           max = 1000, 
                           step = 1),
               radioButtons(inputId="allocation_format_button", 
                            label = "Participant allocation to first-stage and second-stage intervention options",
                            choices = list("Allow me to specify allocation in terms of first and second stage randomization probabilities" = 1,
                                           "Allow me to specify allocation in terms of cell probabilities" = 2), 
                            selected = 1),
               rHandsontableOutput("hot_table_complex_allocation"),
               helpText("Note. Read-only values in the table are shaded heavily. Editable values in the table are shaded lightly. If allocation is specified in terms of first and second stage randomization probabilities (columns 1-2), the equivalent cell probabilities are calculated (columns 3-5). On the other hand, if allocation is specified in terms of cell probabilities (columns 3-5), the equivalent first and second stage randomization probabilities are calculated (columns 1-2). Since the cell probabilities should sum to 1, the probability for cell C (column 5) is automatically calculated as one minus the sum of probability for cells A and B."),
               h3("Significance Test Settings"),
               rHandsontableOutput("hot_table_complex_test"),
               helpText("Note: Type-1 error rate and power are editable values in the table. The table only accepts type-1 error rates within the range [0, 0.5] and power within the range [0.5, 1]."),
               h3("About Using the App"),
               helpText("MDSEs are automatically refreshed whenever a new SMART design setting is considered (e.g., changing the number of participants or randomization probabilities) or a new significance test setting is considered (e.g., changing type-1 error rate or power)"),
               textOutput("notification_complex") %>% tagAppendAttributes(style = "border: 4px solid black; background-color: #FFECB3; padding: 10px;")
             ),
             mainPanel(
               width = 7, # width of sidebar and main panels need to sum to 12
               # Outputs
               h2("Minimum Detectable Standardized Effect (MDSE)"),
               h4("Defined as Cohen's d"),
               tableOutput("table_full"),
               
               hr(),
               
               h5("Working assumptions behind the calculated MDSEs:"),
               tags$ul(
                 tags$li("End-of-study outcome is a continuous variable."),
                 tags$li("These calculations do not account for missing data. It is best to inflate the initial sample size in order to have at least the needed sample size after expected percentage of dropout."),
                 tags$li("Residual error variances among experimental conditions being compared are equal.")
               ),
               
               hr(),
               
               h5("Design of the SMART:"),
               
               tags$img(src = file.path("images", "smart_nonprototypical.png"), width = 700, height = 307, units = "inches", alt = "Non-Prototypical SMART"),
               
               tags$ul(
                 tags$li("A1 denotes first-stage treatment assignment with two options, +1 or -1."),
                 tags$li("Among participants whose first-stage treatment assignment was A1=-1, A2 denotes second-stage treatment assignment with two options, +1 or -1."),
               )
             )
           ),
           
  ),
  tabPanel(title="About",
           p(strong("About this R Shiny app")),
           p(strong("By Jamie R. T. Yap and John J. Dziak")),
           p(class = "text-muted",
             "(c) 2025, d3Center, University of Michigan"),
           p("Created to accompany the manuscript:"),
           p("Yap, J. R. T., Nahum-Shani, I., Dziak, J. J. (2025).",
             "Tradeoffs in Design and Sample Size Planning for",
             "Sequential Randomized Trials."
           ),
           p( "Based on formulas adapted from:"
           ),
           p("Wittes J: Sample size calculations for randomized controlled trials. Epidemiologic reviews. 2002, 24:39-53."),
           p("Julious S. A. : Sample sizes for clinical trials with normal data. Statistics in medicine. 2004, 23:1921-1986."),
           p("Funding:"),
           p("This work was supported by NIH awards P50 DA 054039 and NIH R01 DA 039901 from the National Institute on Drug Abuse.")
  ),
)

###############################################################################
# Shiny App Server
###############################################################################

server <- function(input, output, session) {
  
  #############################################################################
  # Simple view
  #############################################################################
  
  # Notification to users -----------------------------------------------------
  output$notification_simple <- renderText("BETA RELEASE: This app is currently in a testing phase. We hope to add more features. We value your input! Please send any feedback or issues to d3center-code@umich.edu")
  
  # Define text to render depending on allocation strategy selected -----------
  generate_explanation_for_simple_allocation <- reactive({
    
    if (input$balance_buttons == "1") {
      my_text <- "You selected balance within each randomization. This selection corresponds to allocating all participants equally between A1=+1 and A1=-1, and allocating participants who were previously assigned to A1=-1 at the first-stage randomization equally between A2=+1 and A2=-1 at the second-stage randomization. This selection is equivalent to allocating one-half of all participants to cell A, one-fourth of all participants to cell B, and one-fourth of all participants to cell C."
    }
    else{
      my_text <- "You selected balance across all three cells. This selection corresponds to allocating one-third of all participants to cell A, one-third of all participants to cell B, and one-third of all participants to cell C. This selection is equivalent to allocating one-third of all participants to A1=+1 and two-thirds of all participants to A1=-1 at the first-stage randomization, and allocating participants who were previously assigned to A1=-1 at the first-stage randomization equally between A2=+1 and A2=-1 at the second-stage randomization."
    }
    return(my_text)
  })
  output$explanation_for_simple_allocation <- renderText(generate_explanation_for_simple_allocation())
  
  # Define hands on table for significance test settings ----------------------
  dat_initial_simple_significance_test_settings <- data.frame(Comparison = c("First-Stage comparison: A versus B+C",
                                                                           "Pairwise comparison: B versus C",
                                                                           "Pairwise comparison: A versus B",
                                                                           "Pairwise comparison: A versus C"),
                                                            TypeOneErrorRate = rep(0.05, 4),
                                                            Power = rep(0.80, 4))
  
  dat_simple_significance_test_settings  <- reactiveValues(data = dat_initial_simple_significance_test_settings)
  
  output$hot_table_simple_test <- renderRHandsontable({
    rhandsontable(dat_simple_significance_test_settings$data, stretchH = "all", useTypes = TRUE, digits = 3, rowHeaders = FALSE)  %>%
      hot_cols(renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (cellProperties.type === 'numeric') {
             td.style.background = '#F6FAFC'; // Highlight numeric columns slightly
           }
         }
       ") %>% 
      hot_col("Comparison", readOnly = TRUE) %>%
      hot_validate_numeric(
        col = "TypeOneErrorRate",         
        min = 0,                          # Minimum allowed value (inclusive)
        max = 0.50,                       # Maximum allowed value (inclusive)
        allowInvalid = FALSE
      ) %>%
      hot_validate_numeric(
        col = "Power",                    
        min = 0.50,                       # Minimum allowed value (inclusive)
        max = 1,                          # Maximum allowed value (inclusive)
        allowInvalid = FALSE
      )
  })
  
  calculate_table_simple <- reactive({
    
    if (input$balance_buttons == "1") {
      p1 <- 0.5
      p2 <- 0.5
      pA <- 0.5
      pB <- 0.25
      pC <- 0.25
    }else{
      p1 <- 0.333
      p2 <- 0.5
      pA <- 0.333
      pB <- 0.333
      pC <- 0.333
    }
    
    my_dat <- data.frame(Comparison = c("First-Stage comparison: A versus B+C",
                                        "Pairwise comparison: B versus C",
                                        "Pairwise comparison: A versus B",
                                        "Pairwise comparison: A versus C"),
                         ParticipantsConsidered = c(input$table_simple_sample_size,
                                                    input$table_simple_sample_size*(pB + pC),
                                                    input$table_simple_sample_size*(pA + pB),
                                                    input$table_simple_sample_size*(pB + pC)),
                         MDSE = c(GetEffectSizeStage1(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[1,2]),
                                                      sample_size = as.numeric(input$table_simple_sample_size),
                                                      power_test = as.numeric(dat_simple_significance_test_settings$data[1,3]),
                                                      prob_stage1 = p1),
                                  GetEffectSizeStage2(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[2,2]),
                                                      sample_size = as.numeric(input$table_simple_sample_size),
                                                      power_test = as.numeric(dat_simple_significance_test_settings$data[2,3]),
                                                      prob_stage1 = p1,
                                                      prob_stage2_given_0 = p2),
                                  GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[3,2]),
                                                               sample_size = as.numeric(input$table_simple_sample_size),
                                                               power_test = as.numeric(dat_simple_significance_test_settings$data[3,3]),
                                                               prob_stage1 = p1,
                                                               prob_stage2_given_0 = p2,
                                                               a2 = 1),
                                  GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[4,2]),
                                                               sample_size = as.numeric(input$table_simple_sample_size),
                                                               power_test = as.numeric(dat_simple_significance_test_settings$data[4,3]),
                                                               prob_stage1 = p1,
                                                               prob_stage2_given_0 = p2,
                                                               a2 = 0))) %>%
      mutate(ParticipantsConsidered = as.integer(round(ParticipantsConsidered, 0)))
  })
  
  output$table_simple <- renderTable(calculate_table_simple(), digits = 3)
  
  observeEvent(input$hot_table_simple_test$changes$changes, {
    dat_simple_significance_test_settings$data <- hot_to_r(input$hot_table_simple_test)
    
    calculate_table_simple <- reactive({
      
      if (input$balance_buttons == "1") {
        p1 <- 0.5
        p2 <- 0.5
        pA <- 0.5
        pB <- 0.25
        pC <- 0.25
      }else{
        p1 <- 0.333
        p2 <- 0.5
        pA <- 0.333
        pB <- 0.333
        pC <- 0.333
      }
      
      my_dat <- data.frame(Comparison = c("First-Stage comparison: A versus B+C",
                                          "Pairwise comparison: B versus C",
                                          "Pairwise comparison: A versus B",
                                          "Pairwise comparison: A versus C"),
                           ParticipantsConsidered = c(input$table_simple_sample_size,
                                                      input$table_simple_sample_size*(pB + pC),
                                                      input$table_simple_sample_size*(pA + pB),
                                                      input$table_simple_sample_size*(pB + pC)),
                           MDSE = c(GetEffectSizeStage1(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[1,2]),
                                                        sample_size = as.numeric(input$table_simple_sample_size),
                                                        power_test = as.numeric(dat_simple_significance_test_settings$data[1,3]),
                                                        prob_stage1 = p1),
                                    GetEffectSizeStage2(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[2,2]),
                                                        sample_size = as.numeric(input$table_simple_sample_size),
                                                        power_test = as.numeric(dat_simple_significance_test_settings$data[2,3]),
                                                        prob_stage1 = p1,
                                                        prob_stage2_given_0 = p2),
                                    GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[3,2]),
                                                                 sample_size = as.numeric(input$table_simple_sample_size),
                                                                 power_test = as.numeric(dat_simple_significance_test_settings$data[3,3]),
                                                                 prob_stage1 = p1,
                                                                 prob_stage2_given_0 = p2,
                                                                 a2 = 1),
                                    GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_simple_significance_test_settings$data[4,2]),
                                                                 sample_size = as.numeric(input$table_simple_sample_size),
                                                                 power_test = as.numeric(dat_simple_significance_test_settings$data[4,3]),
                                                                 prob_stage1 = p1,
                                                                 prob_stage2_given_0 = p2,
                                                                 a2 = 0))) %>%
        mutate(ParticipantsConsidered = as.integer(round(ParticipantsConsidered, 0)))
    })
    
    output$table_simple <- renderTable(calculate_table_simple(), digits = 3)
  })
  
  #############################################################################
  # Power user view
  #############################################################################
  
  # Notification to users -----------------------------------------------------
  output$notification_complex <- renderText("BETA RELEASE: This app is currently in a testing phase. We hope to add more features. We value your input! Please send any feedback or issues to d3center-code@umich.edu")
  
  # Define hands on table for participant allocation --------------------------
  dat_initial_full_settings_allocation <- data.frame(p1 = 0.50, p2 = 0.50, pA = 0.50, pB = 0.25, pC = 0.25)
  dat_full_settings_allocation  <- reactiveValues(data = dat_initial_full_settings_allocation)
  
  output$hot_table_complex_allocation <- renderRHandsontable({
      ht <- rhandsontable(dat_full_settings_allocation$data, 
                          stretchH = "all", 
                          useTypes = TRUE, 
                          digits = 3,
                          height = "auto",
                          rowHeaders = FALSE, 
                          colHeaders = c("Stage 1, A1=+1", "Stage 2, A2=+1", "Cell A", "Cell B", "Cell C")) %>%
              hot_validate_numeric(
                col = c("Stage 1, A1=+1", "Stage 2, A2=+1", "Cell A", "Cell B", "Cell C"),
                min = 0,               # Minimum allowed value (inclusive)
                max = 1,               # Maximum allowed value (inclusive)
                allowInvalid = FALSE) %>%
        hot_cols(
          colWidths = c(rep(2, 10), rep(3, 5)) # Set widths in pixels for each column
        )
      
      # Apply read-only settings based on radio button input value
      if(input$allocation_format_button == "1"){
        # Make cell probabilities read-only
        ht <- ht %>% 
          hot_col(c("Stage 1, A1=+1", "Stage 2, A2=+1"), readOnly = FALSE) %>%
          hot_col(c("Cell A", "Cell B", "Cell C"), readOnly = TRUE) %>%
          hot_cols(renderer = "
                   function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);
                     if (col < 2) {
                       td.style.background = '#F6FAFC'; // Editable cells
                     }
                     else{
                       td.style.background = '#D3D3D3'; // Cells which are NOT editable
                     }
                   }
                 ")
      }else{
        # Make first and second-stage randomization probabilities read-only
        ht <- ht %>% 
          hot_col(c("Stage 1, A1=+1", "Stage 2, A2=+1"), readOnly = TRUE) %>%
          hot_col(c("Cell A", "Cell B"), readOnly = FALSE) %>%
          # We need this constraint so that cells A, B, and C all sum to 1
          hot_col(c("Cell C"), readOnly = TRUE) %>%
          hot_cols(renderer = "
                   function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);
                     if (col >=2 & col<4) {
                       td.style.background = '#F6FAFC'; // Editable cells
                     }
                     else{
                       td.style.background = '#D3D3D3'; // Cells which are NOT editable
                     }
                   }
                 ")
      }
      
      # Return modified handsontable object
      return(ht)
  })
  
  # Define hands on table for significance test settings ----------------------
  dat_initial_full_significance_test_settings <- data.frame(Comparison = c("First-Stage comparison: A versus B+C",
                                                                           "Pairwise comparison: B versus C",
                                                                           "Pairwise comparison: A versus B",
                                                                           "Pairwise comparison: A versus C"),
                                                            TypeOneErrorRate = rep(0.05, 4),
                                                            Power = rep(0.80, 4))
  
  dat_full_significance_test_settings  <- reactiveValues(data = dat_initial_full_significance_test_settings)
  
  output$hot_table_complex_test <- renderRHandsontable({
    rhandsontable(dat_full_significance_test_settings$data, stretchH = "all", useTypes = TRUE, digits = 3, rowHeaders = FALSE)  %>%
      hot_cols(renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (cellProperties.type === 'numeric') {
             td.style.background = '#F6FAFC'; // Highlight numeric columns slightly
           }
         }
       ") %>% 
      hot_col("Comparison", readOnly = TRUE) %>%
      hot_validate_numeric(
        col = "TypeOneErrorRate",         
        min = 0,                          # Minimum allowed value (inclusive)
        max = 0.50,                       # Maximum allowed value (inclusive)
        allowInvalid = FALSE
      ) %>%
      hot_validate_numeric(
        col = "Power",                    
        min = 0.50,                       # Minimum allowed value (inclusive)
        max = 1,                          # Maximum allowed value (inclusive)
        allowInvalid = FALSE
      )
  })
  
  # Specify initialization of MDSE table --------------------------------------
  calculate_table_full <- reactive({
    my_dat <- data.frame(Comparison = c("First-Stage comparison: A versus B+C",
                                        "Pairwise comparison: B versus C",
                                        "Pairwise comparison: A versus B",
                                        "Pairwise comparison: A versus C"),
                         ParticipantsConsidered = c(input$table_full_sample_size,
                                                    input$table_full_sample_size*(dat_initial_full_settings_allocation[1,4] + dat_initial_full_settings_allocation[1,5]),
                                                    input$table_full_sample_size*(dat_initial_full_settings_allocation[1,3] + dat_initial_full_settings_allocation[1,4]),
                                                    input$table_full_sample_size*(dat_initial_full_settings_allocation[1,3] + dat_initial_full_settings_allocation[1,5])),
                         MDSE = c(GetEffectSizeStage1(type1_error_rate = as.numeric(dat_initial_full_significance_test_settings[1,2]),
                                                      sample_size = as.numeric(input$table_full_sample_size),
                                                      power_test = as.numeric(dat_initial_full_significance_test_settings[1,3]),
                                                      prob_stage1 = dat_initial_full_settings_allocation[1,1]),
                                  GetEffectSizeStage2(type1_error_rate = as.numeric(dat_initial_full_significance_test_settings[2,2]),
                                                      sample_size = as.numeric(input$table_full_sample_size),
                                                      power_test = as.numeric(dat_initial_full_significance_test_settings[2,3]),
                                                      prob_stage1 = dat_initial_full_settings_allocation[1,1],
                                                      prob_stage2_given_0 = dat_initial_full_settings_allocation[1,2]),
                                  GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_initial_full_significance_test_settings[3,2]),
                                                               sample_size = as.numeric(input$table_full_sample_size),
                                                               power_test = as.numeric(dat_initial_full_significance_test_settings[3,3]),
                                                               prob_stage1 = dat_initial_full_settings_allocation[1,1],
                                                               prob_stage2_given_0 = dat_initial_full_settings_allocation[1,2],
                                                               a2 = 1),
                                  GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_initial_full_significance_test_settings[4,2]),
                                                               sample_size = as.numeric(input$table_full_sample_size),
                                                               power_test = as.numeric(dat_initial_full_significance_test_settings[4,3]),
                                                               prob_stage1 = dat_initial_full_settings_allocation[1,1],
                                                               prob_stage2_given_0 = dat_initial_full_settings_allocation[1,2],
                                                               a2 = 0))) %>%
      mutate(ParticipantsConsidered = as.integer(round(ParticipantsConsidered, 0)))
  })
  
  output$table_full <- renderTable(calculate_table_full(), digits = 3)
  
  # Specify desired behavior when hands on tables are updated -----------------
  observeEvent(input$hot_table_complex_allocation$changes$changes, {
    dat_full_settings_allocation$data <- hot_to_r(input$hot_table_complex_allocation)
    
    if(input$allocation_format_button == "1"){
      # Probability of being assigned to cell A in terms of stage-specific probabilities
      dat_full_settings_allocation$data[1,3] <- dat_full_settings_allocation$data[1,1]
      # Probability of being assigned to cell B in terms of stage-specific probabilities
      dat_full_settings_allocation$data[1,4] <- (1-dat_full_settings_allocation$data[1,1])*dat_full_settings_allocation$data[1,2]
      # Probability of being assigned to cell C in terms of stage-specific probabilities
      dat_full_settings_allocation$data[1,5] <- (1-dat_full_settings_allocation$data[1,1])*(1-dat_full_settings_allocation$data[1,2])
    }else{
      # Probability of being assigned to A1=+1 in terms of cell probabilities
      dat_full_settings_allocation$data[1,1] <- dat_full_settings_allocation$data[1,3]
      # Probability of being assigned to A2=+1 in terms of cell probabilities
      dat_full_settings_allocation$data[1,2] <- dat_full_settings_allocation$data[1,4]/(1-dat_full_settings_allocation$data[1,3])
      # Probability of being assigned to cell C
      dat_full_settings_allocation$data[1,5] <- (1-dat_full_settings_allocation$data[1,1])*(1-dat_full_settings_allocation$data[1,2])
    }
    
    calculate_table_full <- reactive({
      my_dat <- data.frame(Comparison = c("First-Stage comparison: A versus B+C",
                                          "Pairwise comparison: B versus C",
                                          "Pairwise comparison: A versus B",
                                          "Pairwise comparison: A versus C"),
                           ParticipantsConsidered = c(input$table_full_sample_size,
                                                      input$table_full_sample_size*(dat_full_settings_allocation$data[1,4] + dat_full_settings_allocation$data[1,5]),
                                                      input$table_full_sample_size*(dat_full_settings_allocation$data[1,3] + dat_full_settings_allocation$data[1,4]),
                                                      input$table_full_sample_size*(dat_full_settings_allocation$data[1,3] + dat_full_settings_allocation$data[1,5])),
                           MDSE = c(GetEffectSizeStage1(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[1,2]),
                                                        sample_size = as.numeric(input$table_full_sample_size),
                                                        power_test = as.numeric(dat_full_significance_test_settings$data[1,3]),
                                                        prob_stage1 = dat_full_settings_allocation$data[1,1]),
                                    GetEffectSizeStage2(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[2,2]),
                                                        sample_size = as.numeric(input$table_full_sample_size),
                                                        power_test = as.numeric(dat_full_significance_test_settings$data[2,3]),
                                                        prob_stage1 = dat_full_settings_allocation$data[1,1],
                                                        prob_stage2_given_0 = dat_full_settings_allocation$data[1,2]),
                                    GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[3,2]),
                                                                 sample_size = as.numeric(input$table_full_sample_size),
                                                                 power_test = as.numeric(dat_full_significance_test_settings$data[3,3]),
                                                                 prob_stage1 = dat_full_settings_allocation$data[1,1],
                                                                 prob_stage2_given_0 = dat_full_settings_allocation$data[1,2],
                                                                 a2 = 1),
                                    GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[4,2]),
                                                                 sample_size = as.numeric(input$table_full_sample_size),
                                                                 power_test = as.numeric(dat_full_significance_test_settings$data[4,3]),
                                                                 prob_stage1 = dat_full_settings_allocation$data[1,1],
                                                                 prob_stage2_given_0 = dat_full_settings_allocation$data[1,2],
                                                                 a2 = 0))) %>%
        mutate(ParticipantsConsidered = as.integer(round(ParticipantsConsidered, 0)))
    })
    
    output$table_full <- renderTable(calculate_table_full(), digits = 3)
  })
  
  observeEvent(input$hot_table_complex_test$changes$changes, {
    dat_full_significance_test_settings$data <- hot_to_r(input$hot_table_complex_test)
    
    calculate_table_full <- reactive({
      my_dat <- data.frame(Comparison = c("First-Stage comparison: A versus B+C",
                                          "Pairwise comparison: B versus C",
                                          "Pairwise comparison: A versus B",
                                          "Pairwise comparison: A versus C"),
                           ParticipantsConsidered = c(input$table_full_sample_size,
                                                      input$table_full_sample_size*(dat_full_settings_allocation$data[1,4] + dat_full_settings_allocation$data[1,5]),
                                                      input$table_full_sample_size*(dat_full_settings_allocation$data[1,3] + dat_full_settings_allocation$data[1,4]),
                                                      input$table_full_sample_size*(dat_full_settings_allocation$data[1,3] + dat_full_settings_allocation$data[1,5])),
                           MDSE = c(GetEffectSizeStage1(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[1,2]),
                                                        sample_size = as.numeric(input$table_full_sample_size),
                                                        power_test = as.numeric(dat_full_significance_test_settings$data[1,3]),
                                                        prob_stage1 = dat_full_settings_allocation$data[1,1]),
                                    GetEffectSizeStage2(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[2,2]),
                                                        sample_size = as.numeric(input$table_full_sample_size),
                                                        power_test = as.numeric(dat_full_significance_test_settings$data[2,3]),
                                                        prob_stage1 = dat_full_settings_allocation$data[1,1],
                                                        prob_stage2_given_0 = dat_full_settings_allocation$data[1,2]),
                                    GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[3,2]),
                                                                 sample_size = as.numeric(input$table_full_sample_size),
                                                                 power_test = as.numeric(dat_full_significance_test_settings$data[3,3]),
                                                                 prob_stage1 = dat_full_settings_allocation$data[1,1],
                                                                 prob_stage2_given_0 = dat_full_settings_allocation$data[1,2],
                                                                 a2 = 1),
                                    GetEffectSizeDifferentStages(type1_error_rate = as.numeric(dat_full_significance_test_settings$data[4,2]),
                                                                 sample_size = as.numeric(input$table_full_sample_size),
                                                                 power_test = as.numeric(dat_full_significance_test_settings$data[4,3]),
                                                                 prob_stage1 = dat_full_settings_allocation$data[1,1],
                                                                 prob_stage2_given_0 = dat_full_settings_allocation$data[1,2],
                                                                 a2 = 0))) %>%
        mutate(ParticipantsConsidered = as.integer(round(ParticipantsConsidered, 0)))
    })
    
    output$table_full <- renderTable(calculate_table_full(), digits = 3)
  })
}

###############################################################################
# Instantiate Shiny App
###############################################################################

shinyApp(ui, server)

