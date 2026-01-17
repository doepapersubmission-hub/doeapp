setwd("C:/Users/muqta/Desktop/doeapp")
library(shiny)
library(DT)  # Add this line
library(ggplot2)
library(agricolae)
library(multcomp)
library(DescTools)
library(nortest)
library(lmtest)
library(FSA)
library(PMCMRplus)
library(coin)
library(ggpubr)
library(car)
library(olsrr)
library(caret)
library(readxl)
library(shinyWidgets)
#install.packages("shinyWidgets")
# Define forceDunnett function with improved error handling
forceDunnett <- function(model, factor_name, data, ref_level) {
  tryCatch({
    if (!ref_level %in% levels(data[[factor_name]])) {
      stop("Reference level not found in factor levels.")
    }
    data[[factor_name]] <- relevel(data[[factor_name]], ref = ref_level)
    temp_model <- aov(as.formula(paste("response ~", factor_name)), data = data)
    mcp_args <- setNames(list("Dunnett"), factor_name)
    summary(glht(temp_model, linfct = do.call(mcp, mcp_args)))
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error in Dunnett Test",
      paste("Error:", e$message),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    return(NULL)
  })
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
  body {
    --angle: 135deg;
    background: linear-gradient(var(--angle), #e0f7fa 0%, #b2ebf2 100%);
    animation: rotateGradient 20s linear infinite;
    color: #1a3c34;
    font-family: 'Roboto', sans-serif;
    margin: 0;
  }
  .header-row {
    background: linear-gradient(90deg, #00695c 0%, #00897b 100%);
    height: 80px;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0 20px;
    margin-bottom: 30px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.15);
  }
  .app-title {
    color: #ffffff !important;
    font-size: 28px !important;
    font-weight: 500;
    letter-spacing: 1px;
    animation: slideIn 1s ease-out;
  }
  .logo-container {
    background-color: #ffffff;
    padding: 5px;
    border-radius: 8px;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    border: 2px solid #26a69a;
    height: 60px;
  }
  .sidebar-panel {
    background-color: #ffffff;
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
    margin-bottom: 20px;
  }
  .main-panel {
    background-color: #ffffff;
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  }
  .well {
    background-color: #e0f2f1 !important;
    border-radius: 10px !important;
    padding: 15px;
    border: 1px solid #b2dfdb !important;
  }
  .btn-primary {
    background: linear-gradient(90deg, #26a69a 0%, #4db6ac 100%) !important;
    border: none !important;
    border-radius: 25px;
    padding: 10px 20px;
    font-weight: 500;
    transition: all 0.3s ease;
  }
  .btn-primary:hover {
    background: linear-gradient(90deg, #00897b 0%, #26a69a 100%) !important;
    transform: scale(1.05); /* Slight zoom */
  box-shadow: 0 4px 12px rgba(0,0,0,0.3);
  }
  .error-message {
    color: #d32f2f;
    font-weight: bold;
    margin-top: 10px;
  }
  .tab-panel {
    padding: 20px;
  }
  .nav-tabs > li > a {
    color: #00695c;
    font-weight: 500;
    border: none;
    border-radius: 8px 8px 0 0;
    padding: 10px 20px;
  }
  .nav-tabs > li.active > a {
    background-color: #26a69a !important;
    color: #ffffff !important;
    border: none !important;
  }
  .nav-tabs {
    border-bottom: 2px solid #b2dfdb;
  }
  h3, h4, h5 {
    color: #00695c;
    font-weight: 500;
  }
  .help-content {
  padding: 20px;
  background-color: #f5fafa;
  border-radius: 8px;
  border: 1px solid #b2dfdb;
}
.help-content ul {
  margin-left: 20px;
  color: #004d40;
}
.help-content table {
  margin-top: 10px;
  margin-bottom: 20px;
}
.help-content table td, .help-content table th {
  padding: 8px;
  border: 1px solid #b2dfdb;
  text-align: left;
}
  select, input[type='text'], input[type='number'] {
    border-radius: 8px !important;
    border: 1px solid #b2dfdb !important;
    padding: 8px !important;
  }
  @keyframes slideIn {
    from {
      transform: translateX(-100%);
      opacity: 0;
    }
    to {
      transform: translateX(0);
      opacity: 1;
    }
  }
  @keyframes rotateGradient {
    0% { --angle: 135deg; }
    50% { --angle: 150deg; }
    100% { --angle: 135deg; }
  }
"))
  ),
  fluidRow(
    class = "header-row",
    div(style = "display: flex; align-items: center;",
        div(class = "app-title", "DOE ANALYZER"),
        div(class = "logo-container",
            tags$img(src = "kfupm.jpeg", style = "height: 50px;")
        )
    )
  ),
  tabsetPanel(
    tabPanel("Analysis",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar-panel",
                 width = 3,
                 fileInput("datafile", "Upload Data (CSV/Excel)", accept = c(".csv", ".xlsx", ".xls")),
                 div(
                   class = "well",
                   style = "margin-top: 15px;",
                   strong("NOTE:"),
                   p("ONLY the response variable can have numeric values; other columns MUST be categorical.",
                     style = "color: #004d40; font-size: 13px;")
                 ),
                 radioButtons("analysisType", "Select Analysis Type:",
                              choices = c("Single Factor Design", "Factorial Design"), inline = TRUE),
                 conditionalPanel(
                   condition = "input.analysisType == 'Factorial Design'",
                   selectInput("factorialDesignType", "Select Factorial Design:",
                               choices = c("Factorial CRD design", "Factorial RCBD design", "2^k Factorial Design", "Confounding 2^k Factorial Design", "Split-Plot Design")),
                   uiOutput("factorialOptions")
                 ),
                 conditionalPanel(
                   condition = "input.analysisType == 'Single Factor Design'",
                   selectInput("designType", "Select Design:",
                               choices = c("CRD", "RCBD", "LSD", "GLSD"))
                 ),
                 uiOutput("design_var_selection"),
                 # Consolidated conditional panel for factorInputs
                 conditionalPanel(
                   condition = "input.analysisType == 'Factorial Design' && (input.factorialDesignType == 'Factorial CRD design' || input.factorialDesignType == 'Factorial RCBD design')",
                   uiOutput("factorInputs")
                 ),
                 # Separate conditional panel for dunnett_ref_interaction_ui specific to Factorial CRD
                 conditionalPanel(
                   condition = "input.analysisType == 'Factorial Design' && input.factorialDesignType == 'Factorial CRD design'",
                   uiOutput("dunnett_ref_interaction_ui")
                 ),
                 conditionalPanel(
                   condition = "input.analysisType == 'Factorial Design' && input.factorialDesignType == 'Factorial RCBD design'",
                   uiOutput("dunnett_ref_interaction_ui")
                 ),
                 conditionalPanel(
                   condition = "input.analysisType == 'Factorial Design' && input.factorialDesignType == '2^k Factorial Design'",
                   numericInput("numFactors_2k", "Number of Factors (k):", value = 2, min = 1, step = 1),
                   uiOutput("kFactorInputs"),
                   uiOutput("dunnett_ref_ui_2k"),
                   selectInput("responseVar_2k", "Select Response Variable", choices = NULL)
                 ),
                 conditionalPanel(
                   condition = "input.analysisType == 'Factorial Design' && input.factorialDesignType == 'Confounding 2^k Factorial Design'",
                   numericInput("numFactors_confounding", "Number of Factors (k):", value = 2, min = 2, step = 1),
                   uiOutput("confoundingFactorInputs"),
                   uiOutput("confoundedInteractionUI"),
                   selectInput("responseVar_confounding", "Select Response Variable:", choices = NULL)
                 ),
                 conditionalPanel(
                   condition = "input.analysisType == 'Factorial Design' && input.factorialDesignType == 'Split-Plot Design'",
                   selectInput("wholePlotFactor", "Select Whole-Plot Factor", choices = NULL),
                   selectInput("subPlotFactor", "Select Sub-Plot Factor", choices = NULL),
                   selectInput("blockVar_split", "Select Blocking Factor", choices = NULL),
                   selectInput("responseVar_split", "Select Response Variable", choices = NULL),
                   uiOutput("dunnett_ref_ui_split_whole"),
                   uiOutput("dunnett_ref_ui_split_sub")
                 ),
                 selectInput("responseTransformation", "Response Transformation Option",
                             choices = c("None", "Log", "Square Root", "Reciprocal", "Box-Cox"), 
                             selected = "None"),
                 checkboxInput("auto_ignore", "Automatically ignore extra blocking columns", value = TRUE),
                 uiOutput("extraColumnsUI"),
                 actionButton("analyzeBtn", "Analyze", class = "btn-primary", icon = icon("rocket"))
               ),
               mainPanel(
                 class = "main-panel",
                 width = 9,
                 tabsetPanel(
                   #tabPanel("Summary", uiOutput("summary_results")),  # New tab for summary dashboard
                   tabPanel("Results",
                            uiOutput("analysis_results"),
                            uiOutput("downloadResultsUI")),  # Download button for results
                   tabPanel("Visualization",
                            uiOutput("analysis_plots"),
                            downloadButton("downloadPlots", "Download Plots as PNG", class = "btn-primary")),  # Download button for plots
                   tabPanel("Diagnostics",
                            tabsetPanel(
                              tabPanel("Plots", uiOutput("diagnostic_plots")),
                              tabPanel("Tests", verbatimTextOutput("diagnostic_tests"))
                            )
                   ),
                   tabPanel("Data",
                            DTOutput("data_head"),  # Use DTOutput instead of tableOutput for editable table
                            actionButton("resetData", "Reset to Original Data", class = "btn-secondary", style = "margin-top: 10px;"))
                 )
               )
             )
    ),
    tabPanel("Help",
             div(class = "main-panel",
                 div(class = "help-content",
                     h3("Help & Documentation"),
                     p("Welcome to the DOE Analyzer. This section provides guidance on how to use the app effectively."),
                     h4("1. Uploading Data"),
                     p("Use the 'Upload Data' button in the sidebar to upload your dataset in CSV or Excel format. Ensure that:"),
                     tags$ul(
                       tags$li("The response variable is numeric."),
                       tags$li("All other columns (factors) are categorical."),
                       tags$li("There are no missing values in the dataset.")
                     ),
                     h4("2. Selecting Analysis Type"),
                     p("Choose between 'Single Factor Design' or 'Factorial Design' using the radio buttons. Then, select the specific design type from the dropdown menu."),
                     h4("3. Running the Analysis"),
                     p("After uploading your data and selecting the design, click the 'Analyze' button to perform the analysis. Results will appear in the main panel, including ANOVA tables, diagnostic plots, and post-hoc tests (if applicable)."),
                     h4("4. Interpreting Results"),
                     p("The app provides detailed outputs, including:"),
                     tags$ul(
                       tags$li("ANOVA Table: Shows the significance of factors and interactions."),
                       tags$li("Diagnostic Plots: Check assumptions like normality and homoscedasticity."),
                       tags$li("Post-Hoc Tests: Compare means for significant factors (e.g., LSD, Tukey, Duncan).")
                     ),
                     h4("5. Troubleshooting"),
                     p("If you encounter issues:"),
                     tags$ul(
                       tags$li("Ensure your data meets the requirements (numeric response, categorical factors)."),
                       tags$li("Check for errors in the R console if the app is running locally."),
                       tags$li("Verify that all required R packages are installed (listed in the app's source code).")
                     ),
                     h4("6. Supported Designs"),
                     p("The app supports the following experimental designs:"),
                     tags$table(style = "width: 100%; border-collapse: collapse;",
                                tags$tr(style = "background-color: #b2dfdb; color: #004d40;",
                                        tags$th("Design Type"), tags$th("Description"), tags$th("Requirements")
                                ),
                                tags$tr(
                                  tags$td("CRD"), tags$td("Completely Randomized Design"), tags$td("Single factor, no blocks")
                                ),
                                tags$tr(
                                  tags$td("RCBD"), tags$td("Randomized Complete Block Design"), tags$td("Single factor with blocks")
                                ),
                                tags$tr(
                                  tags$td("LSD"), tags$td("Latin Square Design"), tags$td("Single factor with two blocking factors")
                                ),
                                tags$tr(
                                  tags$td("GLSD"), tags$td("Generalized Latin Square Design"), tags$td("Single factor with two blocking factors")
                                ),
                                tags$tr(
                                  tags$td("Factorial CRD"), tags$td("Factorial Design in CRD"), tags$td("Multiple factors, no blocks")
                                ),
                                tags$tr(
                                  tags$td("Factorial RCBD"), tags$td("Factorial Design in RCBD"), tags$td("Multiple factors with blocks")
                                ),
                                tags$tr(
                                  tags$td("2^k Factorial Design"), tags$td("2-Level Factorial Design"), tags$td("Multiple factors, each with 2 levels")
                                ),
                                tags$tr(
                                  tags$td("Confounding 2^k Factorial Design"), tags$td("Confounded 2-Level Factorial Design"), tags$td("Multiple factors with confounding")
                                ),
                                tags$tr(
                                  tags$td("Split-Plot Design"), tags$td("Split-Plot Experimental Design"), tags$td("Whole-plot and sub-plot factors with blocks")
                                )
                     ),
                     
                 )
             )
    )
  )
)
server <- function(input, output, session) {
  plotObjects <- reactiveValues(
    boxplot = NULL,
    interaction_plot = NULL,
    interaction_plot_1_2 = NULL,
    interaction_plot_1_3 = NULL,
    interaction_plot_2_3 = NULL
  )
  plotObjects <- reactiveValues(boxplot = NULL, interaction_plot = NULL)
  dataset <- reactiveVal(NULL)  # Current dataset (editable)
  # Define a reactive value to store the selected response variables
  selected_response_vars <- reactiveVal(character(0))
  
  observe({
    showModal(modalDialog(
      title = "Welcome to DOE Analyzer!",
      "This app allows you to perform statistical Experimental Design tasks As follow;
      1. Single Factor Design(CRD, RCBD, LSD, GLSD)
      2. Factorial Design of Experiments(General factorial CRD and RCBD, 2^K full and confounding, Split plot  )
       To begin, select your options and upload your data if required. For more information click help and take sometime to explore the interface.",
      easyClose = TRUE  # Allows users to close the dialog by clicking outside or pressing Esc
    ))
  }, priority = 10)
  # Update the reactive value whenever the response variable inputs change
  observe({
    # Collect all response variable inputs
    response_vars <- unique(c(
      input$responseVar,
      input$responseVar_2k,
      input$responseVar_confounding,
      input$responseVar_split
    ))
    # Remove NULL or empty values
    response_vars <- response_vars[!sapply(response_vars, is.null) & response_vars != ""]
    selected_response_vars(response_vars)
  })
  
  originalDataset <- reactiveVal(NULL)  # Store the original uploaded dataset for reset
  
  observeEvent(input$datafile, {
    req(input$datafile)
    file_ext <- tools::file_ext(input$datafile$datapath)
    if(tolower(file_ext) == "csv") {
      df <- read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
    } else if(tolower(file_ext) %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(input$datafile$datapath)
      df <- as.data.frame(df)
      for(col in names(df)) {
        if(!is.numeric(df[[col]])) df[[col]] <- as.factor(df[[col]])
      }
    } else {
      showModal(modalDialog(
        title = "Error",
        "Unsupported file format. Please upload a CSV or Excel file.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    }
    if(ncol(df) < 2) {
      showModal(modalDialog(
        title = "Error",
        "The uploaded file must contain at least two columns.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    }
    # Clean column names to avoid issues
    names(df) <- make.names(names(df), unique = TRUE)
    # Update select inputs with column names
    updateSelectInput(session, "responseVar", choices = names(df))
    updateSelectInput(session, "responseVar_2k", choices = names(df))
    updateSelectInput(session, "responseVar_split", choices = names(df))
    updateSelectInput(session, "wholePlotFactor", choices = names(df))
    updateSelectInput(session, "subPlotFactor", choices = names(df))
    updateSelectInput(session, "blockVar_split", choices = names(df))
    updateSelectInput(session, "blockVar_confounding", choices = names(df))
    updateSelectInput(session, "responseVar_confounding", choices = names(df))
    for(i in 1:10) {
      updateSelectInput(session, paste0("confounding_factor_", i), choices = names(df))
    }
    # Store the dataset
    dataset(df)
    originalDataset(df)
  })
  
  applyTransformation <- function(x) {
    trans <- input$responseTransformation
    if (trans == "None") return(x)
    else if (trans == "Log") return(log(x))
    else if (trans == "Square Root") return(sqrt(x))
    else if (trans == "Reciprocal") return(1 / x)
    else if (trans == "Box-Cox") {
      if (any(x <= 0)) stop("Box-Cox transformation requires positive values.")
      transObj <- BoxCoxTrans(x)
      return(predict(transObj, x))
    }
    return(x)
  }
  
  extraBlockCols <- reactive({
    req(dataset())
    ds <- dataset()
    block_keywords <- c("block", "blocks", "blockvar", "blockvar1", "blockvar2", "blockvar3")
    if((input$analysisType == "Single Factor Design") && (input$designType == "CRD"))
      return(names(ds)[tolower(names(ds)) %in% block_keywords])
    else if((input$analysisType == "Single Factor Design") && (input$designType == "RCBD")){
      req(input$blockVar)
      return(names(ds)[tolower(names(ds)) %in% block_keywords & !(tolower(names(ds)) %in% tolower(input$blockVar))])
    } else if((input$analysisType == "Single Factor Design") && (input$designType %in% c("LSD", "GLSD")))
      return(names(ds)[tolower(names(ds)) %in% block_keywords])
    else return(character(0))
  })
  
  output$extraColumnsUI <- renderUI({
    if (!is.null(input$auto_ignore) && !input$auto_ignore && length(extraBlockCols()) > 0) {
      checkboxGroupInput("manualIgnore", "Select extra blocking columns to ignore",
                         choices = extraBlockCols(), selected = extraBlockCols())
    }
  })
  
  output$data_head <- renderDT({
    req(dataset())
    df <- dataset()
    datatable(
      df,
      editable = "cell",  # Allow cell editing
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20),
        searching = FALSE,
        autoWidth = FALSE,  # Prevent automatic column width adjustments
        columnDefs = list(list(targets = "_all", className = "dt-center"))  # Center-align columns
      ),
      rownames = FALSE  # Disable row names to avoid index mismatches
    )
  })
  
  observeEvent(input$data_head_cell_edit, {
    info <- input$data_head_cell_edit
    req(info)
    
    # Get the current dataset
    df <- dataset()
    
    # Debug: Log the column names and indices
    cat("Column names in dataset:", names(df), "\n")
    cat("Edited column index (zero-based):", info$col, "\n")
    
    # Get the column name using the index
    col_index <- info$col + 1
    if (col_index > length(names(df))) {
      showModal(modalDialog(
        title = "Error",
        "Column index out of bounds. Please refresh the app and try again.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    col_name <- names(df)[col_index]
    cat("Target column name:", col_name, "\n")
    
    # Apply the edit
    row <- info$row
    new_value <- info$value
    
    # Determine if the column is a response variable
    is_response_var <- col_name %in% c(
      input$responseVar,
      input$responseVar_2k,
      input$responseVar_confounding,
      input$responseVar_split
    )
    
    if (is_response_var) {
      if (!is.na(suppressWarnings(as.numeric(new_value)))) {
        df[[col_name]] <- as.numeric(as.character(df[[col_name]]))
        df[row, col_name] <- as.numeric(new_value)
        df[[col_name]] <- as.numeric(df[[col_name]])
        # Debug: Check the column type after edit
        cat("After edit, is", col_name, "numeric?", is.numeric(df[[col_name]]), "\n")
      } else {
        showModal(modalDialog(
          title = "Invalid Edit",
          "Response variable must be numeric.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
    } else {
      df[[col_name]] <- as.character(df[[col_name]])
      df[row, col_name] <- as.character(new_value)
      df[[col_name]] <- as.factor(df[[col_name]])
      # Debug: Check the column type after edit
      cat("After edit, is", col_name, "factor?", is.factor(df[[col_name]]), "\n")
    }
    
    # Update the dataset
    dataset(df)
    
    showNotification("Data updated successfully. Please re-run the analysis to reflect changes.", type = "message")
  })
  
  # Reset to original dataset
  observeEvent(input$resetData, {
    if (!is.null(originalDataset())) {
      dataset(originalDataset())
      showNotification("Data reset to original upload.", type = "message")
    } else {
      showNotification("No original data to reset to.", type = "warning")
    }
  })  
  output$design_var_selection <- renderUI({
    req(dataset())
    cols <- names(dataset())
    if(input$analysisType == "Single Factor Design"){
      req(input$designType)
      note_text <- switch(input$designType,
                          "CRD" = "CRD: Treatments randomly assigned.",
                          "RCBD" = "RCBD: Includes a blocking factor.",
                          "LSD" = "LSD: Two blocking factors.",
                          "GLSD" = "GLSD: Three blocking factors.")
      note_image <- switch(input$designType,
                           "CRD" = "crd.png",
                           "RCBD" = "rcbd.png",
                           "LSD" = "lsd.png",
                           "GLSD" = "glsd.png")
      inputs_ui <- switch(input$designType,
                          "CRD" = tagList(
                            selectInput("factor1", "Select Treatment Variable", choices = cols),
                            selectInput("responseVar", "Select Response Variable", choices = cols),
                            uiOutput("dunnett_ref_ui_crd")
                          ),
                          "RCBD" = tagList(
                            selectInput("factor1", "Select Treatment Variable", choices = cols),
                            selectInput("blockVar", "Select Blocking Factor", choices = cols),
                            selectInput("responseVar", "Select Response Variable", choices = cols),
                            uiOutput("dunnett_ref_ui_crd")
                          ),
                          "LSD" = tagList(
                            selectInput("factor1", "Select Treatment Variable", choices = cols),
                            selectInput("blockVar1", "Select Blocking Factor 1", choices = cols),
                            selectInput("blockVar2", "Select Blocking Factor 2", choices = cols),
                            selectInput("responseVar", "Select Response Variable", choices = cols),
                            uiOutput("dunnett_ref_ui_crd")
                          ),
                          "GLSD" = tagList(
                            selectInput("blockVar1", "Select Blocking Factor 1", choices = cols),
                            selectInput("blockVar2", "Select Blocking Factor 2", choices = cols),
                            selectInput("blockVar3", "Select Blocking Factor 3", choices = cols),
                            selectInput("factor1", "Select Treatment Variable", choices = cols),
                            selectInput("responseVar", "Select Response Variable", choices = cols),
                            uiOutput("dunnett_ref_ui_crd")
                          )
      )
      tagList(
        inputs_ui,
        div(
          class = "well",
          style = "margin-top:15px;",
          tags$p(note_text, style = "color:#00695c; font-weight:bold;"),
          tags$img(src = note_image, style = "width:100%; max-width:250px; margin-top:10px;")
        )
      )
    } else if(input$analysisType == "Factorial Design" &&
              input$factorialDesignType != "2^k Factorial Design" &&
              input$factorialDesignType != "Split-Plot Design" &&
              input$factorialDesignType != "Confounding 2^k Factorial Design") {
      tagList(
        numericInput("numFactors", "Number of Factors:", value = 2, min = 2, step = 1),
        selectInput("responseVar", "Select Response Variable", choices = cols),
        if(input$factorialDesignType == "Factorial RCBD design") {
          selectInput("blockVar", "Select Blocking Factor", choices = cols)
        }
      )
    }
  })
  
  output$dunnett_ref_ui_crd <- renderUI({
    req(dataset(), input$factor1)
    ds <- dataset()
    if(input$factor1 %in% names(ds)) {
      factor_levels <- levels(factor(ds[[input$factor1]]))
      selectInput("dunnett_ref", "Select Reference Level for Dunnett Test:", choices = factor_levels)
    }
  })
  
  
  # Add this to the server function to render the interaction reference UI with proper dependencies
  output$dunnett_ref_interaction_ui <- renderUI({
    req(dataset(), input$numFactors)
    num <- as.integer(input$numFactors)
    if(num < 2) return(NULL)
    
    # Explicitly depend on factor inputs to ensure they are populated
    factor_inputs <- lapply(1:num, function(i) input[[paste0("factor_", i)]])
    req(all(sapply(factor_inputs, function(x) !is.null(x) && x != "")))
    
    factor_names <- sapply(factor_inputs, identity)
    ds <- dataset()
    req(all(factor_names %in% names(ds)))
    
    # Compute interaction levels for the first two factors
    interaction_levels <- levels(interaction(ds[[factor_names[1]]], ds[[factor_names[2]]]))
    if(length(interaction_levels) == 0) return(NULL)
    
    selectInput("dunnett_ref_interaction", "Select Reference for Dunnett Test (Interaction):", 
                choices = interaction_levels, selected = interaction_levels[1])
  })
  output$factorialOptions <- renderUI({
    checkboxGroupInput("analysisChoices", "Select Analyses to Perform:",
                       choices = c("Parametric ANOVA", "Parametric Post-Hoc", "Non-Parametric ANOVA and Post-Hoc"),
                       selected = c("Parametric ANOVA"))
  })
  
  output$factorInputs <- renderUI({
    req(dataset(), input$numFactors, input$factorialDesignType != "Confounding 2^k Factorial Design")
    num <- as.integer(input$numFactors)
    cols <- names(dataset())
    lapply(1:num, function(i) {
      selectInput(paste0("factor_", i), label = paste("Select Factor", i), choices = cols)
    })
  })
  
  output$kFactorInputs <- renderUI({
    req(dataset(), input$numFactors_2k)
    cols <- names(dataset())
    num <- as.integer(input$numFactors_2k)
    tagList(
      lapply(1:num, function(i) {
        selectInput(paste0("factor_", i), label = paste("Select Factor", i), choices = cols)
      })
    )
  })
  
  output$dunnett_ref_ui_2k <- renderUI({
    req(dataset(), input$numFactors_2k)
    num <- as.integer(input$numFactors_2k)
    factors_selected <- sapply(1:num, function(i) input[[paste0("factor_", i)]])
    if(any(sapply(factors_selected, is.null))) return(NULL)
    ds <- dataset()
    interaction_levels <- unique(apply(ds[, factors_selected, drop = FALSE], 1, function(x) paste(x, collapse = "_")))
    selectInput("dunnett_ref_2k", "Select Reference for Dunnett Test (Interaction):", choices = interaction_levels)
  })
  output$dunnett_ref_ui_split_whole <- renderUI({
    req(dataset(), input$wholePlotFactor)
    ds <- dataset()
    if(input$wholePlotFactor %in% names(ds)) {
      factor_levels <- levels(factor(ds[[input$wholePlotFactor]]))
      selectInput("dunnett_ref_whole", "Reference Level for Dunnett Test (Whole-Plot):", choices = factor_levels)
    }
  })
  
  output$dunnett_ref_ui_split_sub <- renderUI({
    req(dataset(), input$subPlotFactor)
    ds <- dataset()
    if(input$subPlotFactor %in% names(ds)) {
      factor_levels <- levels(factor(ds[[input$subPlotFactor]]))
      selectInput("dunnett_ref_sub", "Reference Level for Dunnett Test (Sub-Plot):", choices = factor_levels)
    }
  })
  output$confoundingFactorInputs <- renderUI({
    req(dataset(), input$numFactors_confounding)
    cols <- names(dataset())
    num <- as.integer(input$numFactors_confounding)
    tagList(
      lapply(1:num, function(i) {
        selectInput(paste0("confounding_factor_", i), label = paste("Select Factor", i), choices = cols)
      })
    )
  })
  output$confoundedInteractionUI <- renderUI({
    req(input$numFactors_confounding)
    num <- as.integer(input$numFactors_confounding)
    if(num < 2) return(NULL)
    
    # Generate all possible interactions
    factor_labels <- paste0("Factor", 1:num)
    interactions <- unlist(lapply(2:num, function(k) {
      combn(factor_labels, k, paste, collapse = "")
    }))
    
    selectInput("confoundedInteraction", "Select Interaction to Confound:", choices = interactions)
  })
  
  observeEvent(input$analyzeBtn, {
    req(dataset())
    dataset <- dataset()
    # Enhanced data validation
    response_vars <- c(input$responseVar, input$responseVar_2k, input$responseVar_confounding, input$responseVar_split)
    response_var <- response_vars[response_vars %in% names(dataset)][1]
    if (!is.null(response_var) && !is.numeric(dataset[[response_var]])) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Response variable must be numeric.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    }
    factor_vars <- c(input$factor1, input$blockVar, input$blockVar1, input$blockVar2, input$blockVar3,
                     input$wholePlotFactor, input$subPlotFactor, input$blockVar_split,
                     sapply(1:10, function(i) input[[paste0("factor_", i)]]),
                     sapply(1:10, function(i) input[[paste0("confounding_factor_", i)]]))
    factor_vars <- factor_vars[factor_vars %in% names(dataset) & !is.na(factor_vars)]
    for (f in factor_vars) {
      if (length(unique(dataset[[f]])) < 2) {
        showModal(modalDialog(
          title = "Invalid Input",
          paste("Factor", f, "has fewer than two levels."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      }
    }
    if (any(is.na(dataset))) {
      showModal(modalDialog(
        title = "Warning",
        "Dataset contains missing values. These will be removed.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      dataset <- na.omit(dataset)
    }
    
    removeExtra <- function(ds) {
      if(length(extraBlockCols()) > 0) {
        if(input$auto_ignore) {
          showModal(modalDialog(
            title = "Warning",
            paste("Your dataset contains extra blocking column(s):",
                  paste(extraBlockCols(), collapse=", "),
                  "which will be automatically ignored."),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          ds <- ds[, !(tolower(names(ds)) %in% tolower(extraBlockCols()))]
        } else {
          req(input$manualIgnore)
          ds <- ds[, !(tolower(names(ds)) %in% tolower(input$manualIgnore))]
        }
      }
      ds
    }
    
    if(input$analysisType == "Single Factor Design"){
      if(input$designType == "CRD") {
        dataset <- removeExtra(dataset)
        if(!(input$factor1 %in% names(dataset)) || !(input$responseVar %in% names(dataset))) {
          showModal(modalDialog(title = "Error", "Select valid variables.",
                                easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        if(input$factor1 == input$responseVar || is.numeric(dataset[[input$factor1]]) || !is.numeric(dataset[[input$responseVar]])) {
          showModal(modalDialog(title = "Error", "Invalid variable selection.", easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        dataset$factor1 <- factor(dataset[[input$factor1]])
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar]]))
        anova_model <- aov(response ~ factor1, data = dataset)
        dunnett_result <- forceDunnett(anova_model, "factor1", dataset, input$dunnett_ref)
        results <- list(
          anova = summary(anova_model),
          means = aggregate(response ~ factor1, data = dataset, mean),
          lsd = LSD.test(anova_model, "factor1", console = FALSE),
          tukey = TukeyHSD(anova_model, "factor1"),
          bonferroni = pairwise.t.test(dataset$response, dataset$factor1, p.adjust.method = "bonferroni"),
          scheffe = scheffe.test(anova_model, "factor1", group = FALSE, console = FALSE),
          dunnett = dunnett_result,
          kruskal = kruskal.test(response ~ factor1, data = dataset),
          dunn = dunnTest(response ~ factor1, data = dataset, method = "bonferroni")
        )
        output$boxplot <- renderPlot({
          p <- ggplot(dataset, aes(factor1, response)) +
            geom_boxplot(aes(fill = factor1)) +
            labs(title = "Response by Explanatory Factor", x = "Explanatory Factor", y = "Response Variable") +
            theme_minimal(base_size = 14) + scale_fill_brewer(palette = "Set2")
          plotObjects$boxplot <- p
          plotObjects$interaction_plot <- NULL  # Reset interaction plot since this analysis doesn't use it
          p
        })
        output$residual_plot <- renderPlot({ 
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16) 
        })
        output$qq_plot <- renderPlot({ 
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16) 
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
        output$analysis_results <- renderUI({
          anova_df <- as.data.frame(results$anova[[1]])
          if("Pr(>F)" %in% colnames(anova_df))
            anova_df$`Pr(>F)` <- format(anova_df$`Pr(>F)`, scientific = TRUE, digits = 4)
          tagList(
            h3("CRD Analysis Results", style = "color: #2c3e50;"),
            h4("Parametric ANOVA Table"), renderTable({ anova_df }, rownames = TRUE),
            h4("2. Explanatory Factor Means"), renderTable(results$means),
            div(
              h4("3. Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", renderPrint(results$lsd)),
                  tabPanel("Tukey", renderPrint(results$tukey)),
                  tabPanel("Bonferroni", renderPrint(results$bonferroni)),
                  tabPanel("Scheffé", renderPrint(results$scheffe)),
                  tabPanel("Dunnett", renderPrint(results$dunnett))
                )
              )
            ),
            div(
              h4("4. Non-Parametric ANOVA and Post Hoc Tests"),
              wellPanel(
                tagList(
                  h5("Kruskal-Wallis Test:"), renderPrint(results$kruskal),
                  h5("Dunn's Post-Hoc Test:"), renderPrint(results$dunn)
                )
              )
            )
          )
        })
        output$analysis_plots <- renderUI({
          tagList(
            h3("Visualizations", style = "color: #2c3e50;"),
            plotOutput("boxplot", height = "400px")
          )
        })
        output$diagnostic_plots <- renderUI({
          tagList(
            h3("Diagnostic Plots", style = "color: #2c3e50;"),
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        output$diagnostic_tests <- renderPrint({
          cat("Normality Tests:\n")
          cat("Shapiro-Wilk:\n"); print(shapiro.test(residuals(anova_model)))
          cat("\nAnderson-Darling:\n"); print(ad.test(residuals(anova_model)))
          cat("\nVariance Tests:\n")
          print(bptest(anova_model))
          cat("\nIndependence Tests:\n")
          cat("Durbin-Watson:\n"); print(dwtest(anova_model))
          cat("\nBreusch-Godfrey:\n"); print(bgtest(anova_model))
        })
        
      } else if(input$designType == "RCBD") {
        dataset <- removeExtra(dataset)
        req(input$factor1, input$blockVar, input$responseVar)
        if(length(unique(c(input$factor1, input$blockVar, input$responseVar))) != 3) {
          showModal(modalDialog(title = "Error", "Variables must be distinct.",
                                easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        if(is.numeric(dataset[[input$factor1]]) || is.numeric(dataset[[input$blockVar]]) || !is.numeric(dataset[[input$responseVar]])) {
          showModal(modalDialog(title = "Error", "Invalid variable types.", easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        dataset$factor1 <- factor(dataset[[input$factor1]])
        dataset$blockVar <- factor(dataset[[input$blockVar]])
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar]]))
        anova_model <- aov(response ~ factor1 + blockVar, data = dataset)
        dunnett_result <- forceDunnett(anova_model, "factor1", dataset, input$dunnett_ref)
        friedman_result <- friedman.test(response ~ factor1 | blockVar, data = dataset)
        pairwise_wilcox <- pairwise.wilcox.test(
          x = dataset$response,
          g = dataset$factor1,
          p.adjust.method = "bonferroni",
          paired = TRUE
        )
        nemenyi_result <- frdAllPairsNemenyiTest(response ~ factor1 | blockVar, data = dataset)
        results <- list(
          anova = summary(anova_model),
          means = aggregate(response ~ factor1, data = dataset, mean),
          lsd = LSD.test(anova_model, "factor1", console = FALSE),
          tukey = TukeyHSD(anova_model, "factor1"),
          bonferroni = pairwise.t.test(dataset$response, dataset$factor1, p.adjust.method = "bonferroni"),
          scheffe = scheffe.test(anova_model, "factor1", group = FALSE, console = FALSE),
          dunnett = dunnett_result,
          friedman = friedman_result,
          pairwise_wilcox = pairwise_wilcox,
          nemenyi = nemenyi_result
        )
        output$boxplot <- renderPlot({
          p <- ggplot(dataset, aes(factor1, response)) +
            geom_boxplot(aes(fill = factor1)) +
            labs(title = "Response by Explanatory Factor", x = "Explanatory Factor", y = "Response Variable") +
            theme_minimal(base_size = 14) + scale_fill_brewer(palette = "Set2")
          plotObjects$boxplot <- p
          plotObjects$interaction_plot <- NULL
          p
        })
        output$residual_plot <- renderPlot({ 
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16) 
        })
        output$qq_plot <- renderPlot({ 
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16) 
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
        output$analysis_results <- renderUI({
          anova_df <- as.data.frame(results$anova[[1]])
          if("Pr(>F)" %in% colnames(anova_df))
            anova_df$`Pr(>F)` <- format(anova_df$`Pr(>F)`, scientific = TRUE, digits = 4)
          tagList(
            h3("RCBD Analysis Results", style = "color: #2c3e50;"),
            h4("Parametric ANOVA Table"), renderTable({ anova_df }, rownames = TRUE),
            h4("2. Treatment Means"), renderTable(results$means),
            div(
              h4("Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", renderPrint(results$lsd)),
                  tabPanel("Tukey", renderPrint(results$tukey)),
                  tabPanel("Bonferroni", renderPrint(results$bonferroni)),
                  tabPanel("Scheffé", renderPrint(results$scheffe)),
                  tabPanel("Dunnett", renderPrint(results$dunnett))
                )
              )
            ),
            div(
              h4("Non-Parametric ANOVA (RCBD) and Post Hoc Tests"),
              wellPanel(
                tagList(
                  h5("Friedman Test:"), renderPrint(results$friedman),
                  h5("Wilcoxon Signed-Rank Test (Bonferroni):"), renderPrint(results$pairwise_wilcox),
                  h5("Nemenyi Test:"), renderPrint(summary(results$nemenyi))
                )
              )
            )
          )
        })
        output$analysis_plots <- renderUI({
          tagList(
            h3("Visualizations", style = "color: #2c3e50;"),
            plotOutput("boxplot", height = "400px")
          )
        })
        output$diagnostic_plots <- renderUI({
          tagList(
            h3("Diagnostic Plots", style = "color: #2c3e50;"),
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        output$diagnostic_tests <- renderPrint({
          cat("Normality Tests:\n")
          cat("Shapiro-Wilk:\n"); print(shapiro.test(residuals(anova_model)))
          cat("\nAnderson-Darling:\n"); print(ad.test(residuals(anova_model)))
          cat("\nVariance Tests:\n")
          print(bptest(anova_model))
          cat("\nIndependence Tests:\n")
          cat("Durbin-Watson:\n"); print(dwtest(anova_model))
          cat("\nBreusch-Godfrey:\n"); print(bgtest(anova_model))
        })
        
      } else if(input$designType == "LSD") {
        dataset <- removeExtra(dataset)
        req(input$factor1, input$blockVar1, input$blockVar2, input$responseVar)
        if(length(unique(c(input$factor1, input$blockVar1, input$blockVar2, input$responseVar))) != 4) {
          showModal(modalDialog(title = "Error", "Variables must be distinct.",
                                easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        if(is.numeric(dataset[[input$factor1]]) || is.numeric(dataset[[input$blockVar1]]) || 
           is.numeric(dataset[[input$blockVar2]]) || !is.numeric(dataset[[input$responseVar]])) {
          showModal(modalDialog(title = "Error", "Invalid variable types.", easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        dataset$factor1 <- factor(dataset[[input$factor1]])
        dataset$blockVar1 <- factor(dataset[[input$blockVar1]])
        dataset$blockVar2 <- factor(dataset[[input$blockVar2]])
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar]]))
        anova_model <- aov(response ~ factor1 + blockVar1 + blockVar2, data = dataset)
        dunnett_result <- forceDunnett(anova_model, "factor1", dataset, input$dunnett_ref)
        block_model <- lm(response ~ blockVar1 + blockVar2, data = dataset)
        dataset$residuals <- residuals(block_model)
        dataset$residuals_rank <- rank(dataset$residuals)
        quade_test <- tryCatch(
          oneway_test(residuals_rank ~ factor1, data = dataset, distribution = approximate(B = 10000)),
          error = function(e) e$message
        )
        pairwise_wilcox <- tryCatch(
          pairwise.wilcox.test(x = dataset$residuals_rank, g = dataset$factor1, p.adjust.method = "bonferroni"),
          error = function(e) e$message
        )
        dunn_test <- tryCatch(
          dunnTest(x = dataset$residuals_rank, g = dataset$factor1, method = "holm"),
          error = function(e) e$message
        )
        results <- list(
          anova = summary(anova_model),
          means = aggregate(response ~ factor1, data = dataset, mean),
          lsd = LSD.test(anova_model, "factor1", console = FALSE),
          tukey = TukeyHSD(anova_model, "factor1"),
          bonferroni = pairwise.t.test(dataset$response, dataset$factor1, p.adjust.method = "bonferroni"),
          scheffe = scheffe.test(anova_model, "factor1", group = FALSE, console = FALSE),
          dunnett = dunnett_result,
          quade = quade_test,
          pairwise_wilcox = pairwise_wilcox,
          dunn = dunn_test
        )
        output$boxplot <- renderPlot({
          p <- ggplot(dataset, aes(factor1, response)) +
            geom_boxplot(aes(fill = factor1)) +
            labs(title = "Response by Explanatory Factor", x = "Explanatory Factor", y = "Response Variable") +
            theme_minimal(base_size = 14) + scale_fill_brewer(palette = "Set2")
          plotObjects$boxplot <- p
          plotObjects$interaction_plot <- NULL
          p
        })
        output$residual_plot <- renderPlot({ 
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16)
        })
        output$qq_plot <- renderPlot({ 
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16)
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
        output$analysis_results <- renderUI({
          anova_df <- as.data.frame(results$anova[[1]])
          if("Pr(>F)" %in% colnames(anova_df))
            anova_df$`Pr(>F)` <- format(anova_df$`Pr(>F)`, scientific = TRUE, digits = 4)
          tagList(
            h3("LSD Analysis Results", style = "color: #2c3e50;"),
            h4("Parametric ANOVA Table"), renderTable({ anova_df }, rownames = TRUE),
            h4("2. Treatment Means"), renderTable(results$means),
            div(
              h4("Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", renderPrint(results$lsd)),
                  tabPanel("Tukey", renderPrint(results$tukey)),
                  tabPanel("Bonferroni", renderPrint(results$bonferroni)),
                  tabPanel("Scheffé", renderPrint(results$scheffe)),
                  tabPanel("Dunnett", renderPrint(results$dunnett))
                )
              )
            ),
            div(
              h4("Non-Parametric ANOVA and Post Hoc Tests"),
              wellPanel(
                tagList(
                  h5("Quade's Test:"), renderPrint({ results$quade }),
                  h5("Pairwise Wilcoxon Test (Bonferroni):"), renderPrint({ results$pairwise_wilcox }),
                  h5("Dunn's Test (Holm):"), renderPrint({ results$dunn })
                )
              )
            )
          )
        })
        output$analysis_plots <- renderUI({
          tagList(
            h3("Visualizations", style = "color: #2c3e50;"),
            plotOutput("boxplot", height = "400px")
          )
        })
        output$diagnostic_plots <- renderUI({
          tagList(
            h3("Diagnostic Plots", style = "color: #2c3e50;"),
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        output$diagnostic_tests <- renderPrint({
          cat("Normality Tests:\n")
          cat("Shapiro-Wilk:\n"); print(shapiro.test(residuals(anova_model)))
          cat("\nAnderson-Darling:\n"); print(ad.test(residuals(anova_model)))
          cat("\nVariance Tests:\n")
          print(bptest(anova_model))
          cat("\nIndependence Tests:\n")
          cat("Durbin-Watson:\n"); print(dwtest(anova_model))
          cat("\nBreusch-Godfrey:\n"); print(bgtest(anova_model))
        })
        
      } else if(input$designType == "GLSD") {
        dataset <- removeExtra(dataset)
        req(input$factor1, input$blockVar1, input$blockVar2, input$blockVar3, input$responseVar)
        if(length(unique(c(input$factor1, input$blockVar1, input$blockVar2, input$blockVar3, input$responseVar))) != 5) {
          showModal(modalDialog(title = "Error", "Variables must be distinct.",
                                easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        if(is.numeric(dataset[[input$factor1]]) || is.numeric(dataset[[input$blockVar1]]) || 
           is.numeric(dataset[[input$blockVar2]]) || is.numeric(dataset[[input$blockVar3]]) || 
           !is.numeric(dataset[[input$responseVar]])) {
          showModal(modalDialog(title = "Error", "Invalid variable types.", easyClose = TRUE, footer = modalButton("Close")))
          return(NULL)
        }
        dataset$factor1 <- factor(dataset[[input$factor1]])
        dataset$blockVar1 <- factor(dataset[[input$blockVar1]])
        dataset$blockVar2 <- factor(dataset[[input$blockVar2]])
        dataset$blockVar3 <- factor(dataset[[input$blockVar3]])
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar]]))
        anova_model <- aov(response ~ factor1 + blockVar1 + blockVar2 + blockVar3, data = dataset)
        dunnett_result <- forceDunnett(anova_model, "factor1", dataset, input$dunnett_ref)
        block_model <- lm(response ~ blockVar1 + blockVar2 + blockVar3, data = dataset)
        dataset$residuals <- residuals(block_model)
        dataset$residuals_rank <- rank(dataset$residuals)
        quade_test <- tryCatch(
          oneway_test(residuals_rank ~ factor1, data = dataset, distribution = approximate(B = 10000)),
          error = function(e) e$message
        )
        pairwise_wilcox <- tryCatch(
          pairwise.wilcox.test(x = dataset$residuals_rank, g = dataset$factor1, p.adjust.method = "bonferroni"),
          error = function(e) e$message
        )
        dunn_test <- tryCatch(
          dunnTest(x = dataset$residuals_rank, g = dataset$factor1, method = "holm"),
          error = function(e) e$message
        )
        results <- list(
          anova = summary(anova_model),
          means = aggregate(response ~ factor1, data = dataset, mean),
          lsd = LSD.test(anova_model, "factor1", console = FALSE),
          tukey = TukeyHSD(anova_model, "factor1"),
          bonferroni = pairwise.t.test(dataset$response, dataset$factor1, p.adjust.method = "bonferroni"),
          scheffe = scheffe.test(anova_model, "factor1", group = FALSE, console = FALSE),
          dunnett = dunnett_result,
          quade = quade_test,
          pairwise_wilcox = pairwise_wilcox,
          dunn = dunn_test
        )
        output$boxplot <- renderPlot({
          p <- ggplot(dataset, aes(factor1, response)) +
            geom_boxplot(aes(fill = factor1)) +
            labs(title = "Response by Explanatory Factor", x = "Explanatory Factor", y = "Response Variable") +
            theme_minimal(base_size = 14) + scale_fill_brewer(palette = "Set2")
          plotObjects$boxplot <- p
          plotObjects$interaction_plot <- NULL
          p
        })
        output$residual_plot <- renderPlot({
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16)
        })
        output$qq_plot <- renderPlot({
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16)
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
        output$analysis_results <- renderUI({
          anova_df <- as.data.frame(results$anova[[1]])
          if("Pr(>F)" %in% colnames(anova_df))
            anova_df$`Pr(>F)` <- format(anova_df$`Pr(>F)`, scientific = TRUE, digits = 4)
          tagList(
            h3("GLSD Analysis Results", style = "color: #2c3e50;"),
            h4("Parametric ANOVA Table"), renderTable({ anova_df }, rownames = TRUE),
            h4("2. Treatment Means"), renderTable(results$means),
            div(
              h4("Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", renderPrint(results$lsd)),
                  tabPanel("Tukey", renderPrint(results$tukey)),
                  tabPanel("Bonferroni", renderPrint(results$bonferroni)),
                  tabPanel("Scheffé", renderPrint(results$scheffe)),
                  tabPanel("Dunnett", renderPrint(results$dunnett))
                )
              )
            ),
            div(
              h4("Non-Parametric ANOVA and Post Hoc Tests"),
              wellPanel(
                tagList(
                  h5("Quade's Test:"), renderPrint({ results$quade }),
                  h5("Pairwise Wilcoxon Test (Bonferroni):"), renderPrint({ results$pairwise_wilcox }),
                  h5("Dunn's Test (Holm):"), renderPrint({ results$dunn })
                )
              )
            )
          )
        })
        output$analysis_plots <- renderUI({
          tagList(
            h3("Visualizations", style = "color: #2c3e50;"),
            plotOutput("boxplot", height = "400px")
          )
        })
        
        output$diagnostic_plots <- renderUI({
          tagList(
            h3("Diagnostic Plots", style = "color: #2c3e50;"),
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        output$diagnostic_tests <- renderPrint({
          cat("Normality Tests:\n")
          cat("Shapiro-Wilk:\n"); print(shapiro.test(residuals(anova_model)))
          cat("\nAnderson-Darling:\n"); print(ad.test(residuals(anova_model)))
          cat("\nVariance Tests:\n")
          print(bptest(anova_model))
          cat("\nIndependence Tests:\n")
          cat("Durbin-Watson:\n"); print(dwtest(anova_model))
          cat("\nBreusch-Godfrey:\n"); print(bgtest(anova_model))
        })
      }
      
    } else if(input$analysisType == "Factorial Design") {
      if(input$factorialDesignType == "Factorial CRD design") {
        req(input$numFactors, input$responseVar)
        num <- as.integer(input$numFactors)
        factor_names <- sapply(1:num, function(i) input[[paste0("factor_", i)]])
        if(length(unique(c(factor_names, input$responseVar))) != (num + 1)) {
          showModal(modalDialog(title = "Error", "All selected variables must be distinct.", easyClose = TRUE))
          return(NULL)
        }
        if(!is.numeric(dataset[[input$responseVar]])) {
          showModal(modalDialog(title = "Error", "Response Variable must be numeric.", easyClose = TRUE))
          return(NULL)
        }
        for(f in factor_names) dataset[[f]] <- factor(dataset[[f]])
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar]]))
        
        # Create interaction factor for Dunnett and Kruskal-Wallis tests
        if(num >= 2) {
          dataset$Interaction <- factor(interaction(dataset[[factor_names[1]]], dataset[[factor_names[2]]]))
        }
        
        formula_str <- paste("response ~", paste(factor_names, collapse = " * "))
        anova_model <- aov(as.formula(formula_str), data = dataset)
        
        # Dunnett test for main effects
        dunnett_results <- lapply(1:num, function(i) {
          f <- factor_names[i]
          temp_data <- dataset
          levels_f <- levels(temp_data[[f]])
          if(length(levels_f) > 1) {
            ref <- levels_f[1]  # Default reference level
            temp_data[[f]] <- relevel(temp_data[[f]], ref = ref)
            temp_model <- aov(as.formula(paste("response ~", f)), data = temp_data)
            mcp_args <- setNames(list("Dunnett"), f)
            tryCatch(
              summary(glht(temp_model, linfct = do.call(mcp, mcp_args))),
              error = function(e) paste("Error in Dunnett test:", e$message)
            )
          } else {
            "Factor has only one level, Dunnett test not applicable."
          }
        })
        
        # Dunnett test for interaction
        dunnett_interaction <- NULL
        if(num >= 2) {
          temp_data <- dataset
          interaction_levels <- levels(temp_data$Interaction)
          if(length(interaction_levels) > 1) {
            # Use user-selected reference if available, otherwise use the first level
            ref <- if(!is.null(input$dunnett_ref_interaction) && input$dunnett_ref_interaction %in% interaction_levels) {
              input$dunnett_ref_interaction
            } else {
              interaction_levels[1]
            }
            temp_data$Interaction <- relevel(temp_data$Interaction, ref = ref)
            temp_model <- aov(response ~ Interaction, data = temp_data)
            mcp_args <- setNames(list("Dunnett"), "Interaction")
            dunnett_interaction <- tryCatch(
              summary(glht(temp_model, linfct = do.call(mcp, mcp_args))),
              error = function(e) paste("Error in Dunnett test for interaction:", e$message)
            )
          } else {
            dunnett_interaction <- "Interaction has only one level, Dunnett test not applicable."
          }
        }
        
        lsd_tests <- lapply(factor_names, function(f) LSD.test(anova_model, f, console = FALSE))
        tukey_tests <- TukeyHSD(anova_model)
        bonf_tests <- lapply(factor_names, function(f) pairwise.t.test(dataset$response, dataset[[f]], p.adjust.method = "bonferroni"))
        scheffe_tests <- lapply(factor_names, function(f) scheffe.test(anova_model, f, group = FALSE, console = FALSE))
        duncan_tests <- lapply(factor_names, function(f) duncan.test(anova_model, f, console = FALSE))
        kruskal_tests <- lapply(factor_names, function(f) kruskal.test(response ~ dataset[[f]], data = dataset))
        dunn_tests <- lapply(factor_names, function(f) dunnTest(response ~ dataset[[f]], data = dataset, method = "bonferroni"))
        
        # Kruskal-Wallis and Dunn's test for interaction
        kruskal_interaction <- NULL
        dunn_interaction <- NULL
        if(num >= 2) {
          kruskal_interaction <- kruskal.test(response ~ Interaction, data = dataset)
          dunn_interaction <- dunnTest(response ~ Interaction, data = dataset, method = "bonferroni")
        }
        
        # Generate and store plots
        if(num == 2) {
          p_box <- ggplot(dataset, aes(x = .data[[factor_names[1]]], y = response, fill = .data[[factor_names[2]]])) +
            geom_boxplot() +
            labs(title = paste("Response by", factor_names[1], "and", factor_names[2]), x = factor_names[1], y = "Response") +
            theme_minimal(base_size = 14) +
            scale_fill_brewer(palette = "Set2")
          p_interaction <- ggplot(dataset, aes(x = .data[[factor_names[2]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot: Response by", factor_names[2], "and", factor_names[1]),
                 x = factor_names[2], y = "Response", color = factor_names[1]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          plotObjects$boxplot <- p_box
          plotObjects$interaction_plot <- p_interaction
        } else {
          plotObjects$boxplot <- NULL
          plotObjects$interaction_plot <- NULL
        }
        
        output$boxplot <- renderPlot({
          if (!is.null(plotObjects$boxplot)) {
            print(plotObjects$boxplot)
          } else {
            plot(NULL, main = "Boxplot not available for this design.")
          }
        })
        output$interaction_plot <- renderPlot({
          if (!is.null(plotObjects$interaction_plot)) {
            print(plotObjects$interaction_plot)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        
        output$analysis_results <- renderUI({
          results <- list()
          if("Parametric ANOVA" %in% input$analysisChoices) {
            results$anova <- tagList(h4("Parametric ANOVA Table"), renderPrint(summary(anova_model)))
          }
          if("Parametric Post-Hoc" %in% input$analysisChoices) {
            results$parametric <- tagList(
              h4("Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(lsd_tests[[i]])))),
                  tabPanel("Tukey", renderPrint(tukey_tests)),
                  tabPanel("Bonferroni", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(bonf_tests[[i]])))),
                  tabPanel("Scheffé", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(scheffe_tests[[i]])))),
                  tabPanel("Duncan", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(duncan_tests[[i]])))),
                  tabPanel("Dunnett (Main Effects)", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(dunnett_results[[i]])))),
                  if(num >= 2) {
                    tabPanel("Dunnett (Interaction)", renderPrint(dunnett_interaction))
                  }
                )
              )
            )
          }
          if("Non-Parametric ANOVA and Post-Hoc" %in% input$analysisChoices) {
            results$nonparametric <- tagList(
              h4("Non-Parametric ANOVA and Post-Hoc Tests"),
              wellPanel(
                lapply(1:num, function(i) tagList(
                  h5(paste("Kruskal-Wallis Test for", factor_names[i], ":")),
                  renderPrint(kruskal_tests[[i]]),
                  h5(paste("Dunn's Test for", factor_names[i], ":")),
                  renderPrint(dunn_tests[[i]])
                )),
                if(num >= 2) {
                  tagList(
                    h5("Kruskal-Wallis Test for Interaction:"),
                    renderPrint(kruskal_interaction),
                    h5("Dunn's Test for Interaction:"),
                    renderPrint(dunn_interaction)
                  )
                }
              )
            )
          }
          tagList(
            h3("Factorial CRD Analysis Results"),
            results$anova,
            results$parametric,
            results$nonparametric
          )
        })
        
        output$analysis_plots <- renderUI({
          if(num == 2) {
            tagList(
              h3("Visualizations"),
              plotOutput("boxplot", height = "400px"),
              plotOutput("interaction_plot", height = "400px")
            )
          } else {
            tagList(
              h3("Visualizations"),
              p("Visualizations are only available for designs with exactly two factors.")
            )
          }
        })
        
        output$diagnostic_plots <- renderUI({
          tagList(
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        output$diagnostic_tests <- renderPrint({
          cat("Normality Tests:\n")
          cat("Shapiro-Wilk Test:\n")
          print(shapiro.test(residuals(anova_model)))
          cat("\nAnderson-Darling Test:\n")
          print(ad.test(residuals(anova_model)))
          cat("\nVariance Tests:\n")
          cat("Breusch-Pagan Test:\n")
          print(bptest(anova_model))
          cat("\nIndependence Tests:\n")
          cat("Durbin-Watson Test:\n")
          print(tryCatch(dwtest(anova_model), error = function(e) paste("Error:", e$message)))
          cat("\nBreusch-Godfrey Test:\n")
          print(tryCatch(bgtest(anova_model), error = function(e) paste("Error:", e$message)))
        })
        
        output$residual_plot <- renderPlot({ 
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16) 
        })
        output$qq_plot <- renderPlot({ 
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16) 
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
        
        
      } else if(input$factorialDesignType == "Factorial RCBD design") {
        # Validate inputs
        req(input$numFactors, input$responseVar, input$blockVar)
        num <- as.integer(input$numFactors)
        factor_names <- sapply(1:num, function(i) input[[paste0("factor_", i)]])
        
        # Ensure all selected variables are distinct
        if(length(unique(c(factor_names, input$responseVar, input$blockVar))) != (num + 2)) {
          showModal(modalDialog(
            title = "Error",
            "All selected variables must be distinct.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        
        # Ensure response variable is numeric
        if(!is.numeric(dataset[[input$responseVar]])) {
          showModal(modalDialog(
            title = "Error",
            "Response variable must be numeric.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        
        # Check if at least one analysis type is selected
        if (length(input$analysisChoices) == 0) {
          showModal(modalDialog(
            title = "Warning",
            "No analyses selected. Please select at least one analysis type (ANOVA, Parametric Post-Hoc, or Non-Parametric Post-Hoc).",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        
        # Prepare dataset
        ds <- dataset
        for(f in factor_names) ds[[f]] <- factor(ds[[f]])
        ds$blockVar <- factor(ds[[input$blockVar]])
        ds$response <- applyTransformation(as.numeric(ds[[input$responseVar]]))
        
        # Check for missing values after transformation
        if(any(is.na(ds$response))) {
          showModal(modalDialog(
            title = "Error",
            "Response variable contains missing or invalid values after transformation.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        
        # Check for balanced design
        design_table <- table(ds$blockVar, do.call(interaction, lapply(factor_names, function(f) ds[[f]])))
        if(any(design_table == 0) || length(unique(design_table[design_table > 0])) > 1) {
          showModal(modalDialog(
            title = "Error",
            "The Factorial RCBD design is unbalanced. Each block must contain all treatment combinations exactly once.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        
        # Check number of blocks
        num_blocks <- length(unique(ds$blockVar))
        if(num_blocks < 3) {
          showModal(modalDialog(
            title = "Error",
            paste("Friedman test requires at least 3 blocks. The blocking factor", input$blockVar, "has only", num_blocks, "levels."),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        
        # Create interaction factor
        if(num >= 2) {
          ds$Interaction <- factor(interaction(ds[[factor_names[1]]], ds[[factor_names[2]]]))
          num_levels_interaction <- length(levels(ds$Interaction))
          if(num_levels_interaction < 2) {
            showModal(modalDialog(
              title = "Error",
              paste("Interaction between", factor_names[1], "and", factor_names[2], "has fewer than 2 levels."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return(NULL)
          }
        }
        
        # Validate factor levels for non-parametric tests
        for(f in factor_names) {
          num_levels <- length(unique(ds[[f]]))
          if(num_levels < 3) {
            showModal(modalDialog(
              title = "Error",
              paste("Non-parametric tests require at least 3 levels. Factor", f, "has only", num_levels, "levels."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return(NULL)
          }
        }
        if(num >= 2) {
          if(num_levels_interaction < 3) {
            showModal(modalDialog(
              title = "Error",
              paste("Non-parametric tests require at least 3 levels. The interaction between", factor_names[1], "and", factor_names[2], "has only", num_levels_interaction, "levels."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return(NULL)
          }
        }
        
        # ANOVA model with blocking
        formula_str <- paste("response ~", paste(factor_names, collapse = " * "), "+ blockVar")
        anova_model <- aov(as.formula(formula_str), data = ds)
        
        # Dunnett test for interaction (using user-selected reference)
        dunnett_interaction <- NULL
        if(num >= 2) {
          temp_data <- ds
          levels_interaction <- levels(temp_data$Interaction)
          if(length(levels_interaction) > 1) {
            ref <- if(!is.null(input$dunnett_ref_interaction) && input$dunnett_ref_interaction %in% levels_interaction) {
              input$dunnett_ref_interaction
            } else {
              levels_interaction[1]
            }
            temp_data$Interaction <- relevel(temp_data$Interaction, ref = ref)
            temp_model <- aov(response ~ Interaction + blockVar, data = temp_data)
            dunnett_interaction <- tryCatch(
              summary(glht(temp_model, linfct = mcp(Interaction = "Dunnett"))),
              error = function(e) paste("Error in Dunnett test for interaction:", e$message)
            )
          } else {
            dunnett_interaction <- "Interaction has only one level, Dunnett test not applicable."
          }
        }
        
        # Parametric post-hoc tests (LSD, Tukey, Bonferroni, Scheffé)
        posthoc_tests <- tryCatch(
          DescTools::PostHocTest(anova_model, which = NULL, method = "hsd", conf.level = 0.95),
          error = function(e) paste("Error in PostHocTest (Tukey HSD):", e$message)
        )
        posthoc_tests_bonf <- tryCatch(
          DescTools::PostHocTest(anova_model, which = NULL, method = "bonferroni", conf.level = 0.95),
          error = function(e) paste("Error in PostHocTest (Bonferroni):", e$message)
        )
        posthoc_tests_lsd <- tryCatch(
          DescTools::PostHocTest(anova_model, which = NULL, method = "lsd", conf.level = 0.95),
          error = function(e) paste("Error in PostHocTest (LSD):", e$message)
        )
        posthoc_tests_scheffe <- tryCatch(
          DescTools::PostHocTest(anova_model, which = NULL, method = "scheffe", conf.level = 0.95),
          error = function(e) paste("Error in PostHocTest (Scheffé):", e$message)
        )
        
        # Non-parametric tests (Friedman Test and Nemenyi Post-Hoc)
        non_param_results <- list()
        
        # Aggregate data to ensure one observation per block-treatment combination
        data_factor_list <- lapply(factor_names, function(f) {
          agg_data <- aggregate(response ~ get(f) + blockVar, data = ds, mean, na.rm = TRUE)
          names(agg_data) <- c(f, "blockVar", "response")
          agg_data
        })
        names(data_factor_list) <- factor_names
        
        data_interaction <- NULL
        if(num >= 2) {
          data_interaction <- aggregate(response ~ Interaction + blockVar, data = ds, mean, na.rm = TRUE)
          tab_interaction <- table(data_interaction$blockVar, data_interaction$Interaction)
          if(any(tab_interaction == 0) || length(unique(tab_interaction[tab_interaction > 0])) > 1) {
            non_param_results[["Friedman Test for Interaction"]] <- "The design is unbalanced for the interaction term after aggregation."
            non_param_results[["Nemenyi Post-Hoc for Interaction"]] <- "Nemenyi test not performed due to unbalanced design."
          }
        }
        
        # Friedman Test for each factor
        for(f in factor_names) {
          agg_data <- data_factor_list[[f]]
          num_levels <- length(unique(agg_data[[f]]))
          
          # Check balance after aggregation
          tab <- table(agg_data$blockVar, agg_data[[f]])
          if(any(tab == 0) || length(unique(tab[tab > 0])) > 1) {
            non_param_results[[paste("Friedman Test for", f)]] <- paste("The design is unbalanced for", f, "after aggregation.")
            non_param_results[[paste("Nemenyi Post-Hoc for", f)]] <- paste("Nemenyi test not performed due to unbalanced design for", f, ".")
            next
          }
          
          # Check number of levels after aggregation
          if(num_levels < 3) {
            non_param_results[[paste("Friedman Test for", f)]] <- paste("Friedman test requires at least 3 levels. Factor", f, "has only", num_levels, "levels after aggregation.")
            non_param_results[[paste("Nemenyi Post-Hoc for", f)]] <- paste("Nemenyi test not performed due to insufficient levels for", f, ".")
            next
          }
          
          formula_str <- as.formula(paste("response ~", f, "| blockVar"))
          friedman_result <- tryCatch({
            result <- friedman.test(formula_str, data = agg_data)
            result
          }, error = function(e) {
            paste("Friedman Test Error for", f, ":", e$message)
          })
          non_param_results[[paste("Friedman Test for", f)]] <- friedman_result
          
          if(is.list(friedman_result) && !is.null(friedman_result$p.value) && friedman_result$p.value < 0.05) {
            nemenyi_result <- tryCatch({
              result <- PMCMRplus::frdAllPairsNemenyiTest(y = agg_data$response, groups = agg_data[[f]], blocks = agg_data$blockVar)
              result
            }, error = function(e) {
              paste("Nemenyi Test Error for", f, ":", e$message)
            })
            non_param_results[[paste("Nemenyi Post-Hoc for", f)]] <- nemenyi_result
          } else {
            non_param_results[[paste("Nemenyi Post-Hoc for", f)]] <- "Nemenyi test not performed (Friedman test not significant or test failed)."
          }
        }
        
        # Friedman Test for interaction
        if(num >= 2 && !is.null(data_interaction)) {
          if(!("Friedman Test for Interaction" %in% names(non_param_results))) {
            formula_str <- as.formula("response ~ Interaction | blockVar")
            friedman_interaction <- tryCatch({
              result <- friedman.test(formula_str, data = data_interaction)
              result
            }, error = function(e) {
              paste("Friedman Test Error for Interaction:", e$message)
            })
            non_param_results[["Friedman Test for Interaction"]] <- friedman_interaction
            
            if(is.list(friedman_interaction) && !is.null(friedman_interaction$p.value) && friedman_interaction$p.value < 0.05) {
              nemenyi_interaction <- tryCatch({
                result <- PMCMRplus::frdAllPairsNemenyiTest(y = data_interaction$response, groups = data_interaction$Interaction, blocks = data_interaction$blockVar)
                result
              }, error = function(e) {
                paste("Nemenyi Test Error for Interaction:", e$message)
              })
              non_param_results[["Nemenyi Post-Hoc for Interaction"]] <- nemenyi_interaction
            } else {
              non_param_results[["Nemenyi Post-Hoc for Interaction"]] <- "Nemenyi test not performed (Friedman test not significant or test failed)."
            }
          }
        }
        
        # Generate and store plots
        if(num == 2) {
          p_box <- ggplot(data = ds, aes_string(x = factor_names[1], y = "response", fill = factor_names[2])) +
            geom_boxplot() +
            labs(title = paste("Economic Growth by", factor_names[1], "and", factor_names[2]),
                 x = factor_names[1], y = input$responseVar) +
            theme_minimal(base_size = 14)
          
          p_interaction <- ggplot(data = ds, aes_string(x = factor_names[2], y = "response", color = factor_names[1], group = factor_names[1])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot: Response by", factor_names[2], "and", factor_names[1]),
                 x = factor_names[2], y = input$responseVar, color = factor_names[1]) +
            theme_minimal(base_size = 14)
          
          plotObjects$boxplot <- p_box
          plotObjects$interaction_plot <- p_interaction
        } else {
          plotObjects$boxplot <- NULL
          plotObjects$interaction_plot <- NULL
        }
        
        output$boxplot <- renderPlot({
          if (!is.null(plotObjects$boxplot)) {
            print(plotObjects$boxplot)
          } else {
            plot(NULL, main = "Boxplot not available for this design.")
          }
        })
        
        output$interaction_plot <- renderPlot({
          if (!is.null(plotObjects$interaction_plot)) {
            print(plotObjects$interaction_plot)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        
        # Render analysis results
        output$analysis_results <- renderUI({
          results_ui <- list()
          
          # ANOVA Summary (only if selected)
          if ("Parametric ANOVA" %in% input$analysisChoices) {
            anova_summary <- summary(anova_model)
            if (length(anova_summary) > 0) {
              results_ui$anova <- tagList(
                h4("Parametric ANOVA Table"),
                renderPrint(anova_summary)
              )
            } else {
              results_ui$anova <- tagList(
                h4("Parametric ANOVA"),
                div(class = "error-message", "ANOVA computation failed. Please check your data.")
              )
            }
          }
          
          # Parametric Post-Hoc Tests (only if selected)
          if ("Parametric Post-Hoc" %in% input$analysisChoices) {
            results_ui$parametric <- tagList(
              h4("Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD",
                           if (is.character(posthoc_tests_lsd)) div(class = "error-message", posthoc_tests_lsd) else renderPrint(posthoc_tests_lsd)),
                  tabPanel("Tukey HSD",
                           if (is.character(posthoc_tests)) div(class = "error-message", posthoc_tests) else renderPrint(posthoc_tests)),
                  tabPanel("Bonferroni",
                           if (is.character(posthoc_tests_bonf)) div(class = "error-message", posthoc_tests_bonf) else renderPrint(posthoc_tests_bonf)),
                  tabPanel("Scheffé",
                           if (is.character(posthoc_tests_scheffe)) div(class = "error-message", posthoc_tests_scheffe) else renderPrint(posthoc_tests_scheffe)),
                  if (num >= 2 && !is.null(dunnett_interaction)) {
                    tabPanel("Dunnett (Interaction)",
                             if (is.character(dunnett_interaction)) div(class = "error-message", dunnett_interaction) else renderPrint(dunnett_interaction))
                  }
                )
              )
            )
          }
          
          # Non-Parametric Tests (only if selected)
          if ("Non-Parametric ANOVA and Post-Hoc" %in% input$analysisChoices) {
            results_ui$nonparametric <- tagList(
              h4(" Non-Parametric Tests"),
              wellPanel(
                if (length(non_param_results) == 0) {
                  div(class = "error-message", "No non-parametric results available. Please ensure the design is balanced and has sufficient levels.")
                } else {
                  non_param_ui <- lapply(names(non_param_results), function(test_name) {
                    result <- non_param_results[[test_name]]
                    tagList(
                      h5(test_name, ":"),
                      if (is.character(result)) {
                        div(class = "error-message", result)
                      } else {
                        tryCatch(
                          renderPrint(result),
                          error = function(e) div(class = "error-message", paste("Error displaying result for", test_name, ":", e$message))
                        )
                      }
                    )
                  })
                  do.call(tagList, non_param_ui)
                }
              )
            )
          }
          
          tagList(
            h3("Factorial RCBD Analysis Results"),
            results_ui$anova,
            results_ui$parametric,
            results_ui$nonparametric
          )
        })
        
        output$analysis_plots <- renderUI({
          if(num == 2) {
            tagList(
              h3("Visualizations"),
              plotOutput("boxplot", height = "400px"),
              plotOutput("interaction_plot", height = "400px")
            )
          } else {
            tagList(
              h3("Visualizations"),
              p("Visualizations are only available for designs with exactly two factors.")
            )
          }
        })
        
        output$diagnostic_plots <- renderUI({
          tagList(
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        output$diagnostic_tests <- renderPrint({
          cat("Normality Tests:\n")
          cat("Shapiro-Wilk Test:\n")
          print(shapiro.test(residuals(anova_model)))
          cat("\nAnderson-Darling Test:\n")
          print(ad.test(residuals(anova_model)))
          cat("\nVariance Tests:\n")
          cat("Breusch-Pagan Test:\n")
          print(bptest(anova_model))
          cat("\nIndependence Tests:\n")
          cat("Durbin-Watson Test:\n")
          print(tryCatch(dwtest(anova_model), error = function(e) paste("Error:", e$message)))
          cat("\nBreusch-Godfrey Test:\n")
          print(tryCatch(bgtest(anova_model), error = function(e) paste("Error:", e$message)))
        })
        
        output$residual_plot <- renderPlot({ 
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16) 
        })
        
        output$qq_plot <- renderPlot({ 
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16) 
        })
        
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
      } else if(input$factorialDesignType == "2^k Factorial Design") {
        req(input$numFactors_2k, input$responseVar_2k)
        num <- as.integer(input$numFactors_2k)
        factor_names <- sapply(1:num, function(i) input[[paste0("factor_", i)]])
        if(length(unique(c(factor_names, input$responseVar_2k))) != (num + 1)) {
          showModal(modalDialog(title = "Error", "All selected variables must be distinct.", easyClose = TRUE))
          return(NULL)
        }
        if(!is.numeric(dataset[[input$responseVar_2k]])) {
          showModal(modalDialog(title = "Error", "Response Variable must be numeric.", easyClose = TRUE))
          return(NULL)
        }
        for(f in factor_names) dataset[[f]] <- factor(dataset[[f]])
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar_2k]]))
        formula_str <- paste("response ~", paste(factor_names, collapse = " * "))
        anova_model <- aov(as.formula(formula_str), data = dataset)
        
        # Dunnett's Test for Main Effects
        dunnett_results <- lapply(1:num, function(i) {
          f <- factor_names[i]
          temp_data <- dataset
          levels_f <- levels(temp_data[[f]])
          if(length(levels_f) > 1) {
            ref <- input$dunnett_ref_2k  # Use the single reference input
            if(is.null(ref) || !(ref %in% levels(temp_data[[f]]))) ref <- levels_f[1]
            temp_data[[f]] <- relevel(temp_data[[f]], ref = ref)
            temp_model <- aov(as.formula(paste("response ~", f)), data = temp_data)
            mcp_args <- setNames(list("Dunnett"), f)
            tryCatch(
              summary(glht(temp_model, linfct = do.call(mcp, mcp_args))),
              error = function(e) paste("Error in Dunnett test:", e$message)
            )
          } else {
            "Factor has only one level, Dunnett test not applicable."
          }
        })
        
        # Dunnett's Test for Interaction Terms (New)
        dunnett_interaction <- NULL
        if(num >= 2) {
          dataset$Interaction <- factor(do.call(paste, c(lapply(factor_names, function(f) dataset[[f]]), sep = "_")))
          levels_int <- levels(dataset$Interaction)
          if(length(levels_int) > 1) {
            dataset$Interaction <- relevel(dataset$Interaction, ref = levels_int[1])
            int_model <- aov(response ~ Interaction, data = dataset)
            dunnett_interaction <- tryCatch(
              summary(glht(int_model, linfct = mcp(Interaction = "Dunnett"))),
              error = function(e) paste("Error in Dunnett test for interaction:", e$message)
            )
          } else {
            dunnett_interaction <- "Interaction has only one level, Dunnett test not applicable."
          }
        }
        
        lsd_tests <- lapply(factor_names, function(f) LSD.test(anova_model, f, console = FALSE))
        tukey_tests <- TukeyHSD(anova_model)
        bonf_tests <- lapply(factor_names, function(f) pairwise.t.test(dataset$response, dataset[[f]], p.adjust.method = "bonferroni"))
        scheffe_tests <- lapply(factor_names, function(f) scheffe.test(anova_model, f, group = FALSE, console = FALSE))
        
        kruskal_tests <- lapply(factor_names, function(f) kruskal.test(response ~ dataset[[f]], data = dataset))
        dunn_tests <- lapply(factor_names, function(f) dunnTest(response ~ dataset[[f]], data = dataset, method = "bonferroni"))
        
        # Kruskal-Wallis and Dunn's Test for Interaction Effects (New)
        kruskal_interaction <- NULL
        dunn_interaction <- NULL
        if(num >= 2) {
          dataset$Interaction <- interaction(lapply(factor_names, function(f) dataset[[f]]))
          kruskal_interaction <- kruskal.test(response ~ Interaction, data = dataset)
          dunn_interaction <- dunnTest(response ~ Interaction, data = dataset, method = "bonferroni")
        }
        
        # Generate and store plots
        if(num == 2) {
          p_box <- ggplot(dataset, aes(x = .data[[factor_names[1]]], y = response, fill = .data[[factor_names[2]]])) +
            geom_boxplot() +
            labs(title = paste("Response by", factor_names[1], "and", factor_names[2]), x = factor_names[1], y = "Response") +
            theme_minimal(base_size = 14) +
            scale_fill_brewer(palette = "Set2")
          p_interaction_1_2 <- ggplot(dataset, aes(x = .data[[factor_names[2]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[1], "and", factor_names[2]), x = factor_names[2], y = "Response", color = factor_names[1]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          plotObjects$boxplot <- p_box
          plotObjects$interaction_plot_1_2 <- p_interaction_1_2
          plotObjects$interaction_plot_1_3 <- NULL
          plotObjects$interaction_plot_2_3 <- NULL
        } else if(num == 3) {
          p_box <- ggplot(dataset, aes(x = .data[[factor_names[1]]], y = response, fill = .data[[factor_names[2]]])) +
            geom_boxplot() +
            labs(title = paste("Response by", factor_names[1], "and", factor_names[2]), x = factor_names[1], y = "Response") +
            theme_minimal(base_size = 14) +
            scale_fill_brewer(palette = "Set2")
          p_interaction_1_2 <- ggplot(dataset, aes(x = .data[[factor_names[2]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[1], "and", factor_names[2]), x = factor_names[2], y = "Response", color = factor_names[1]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          p_interaction_1_3 <- ggplot(dataset, aes(x = .data[[factor_names[3]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[1], "and", factor_names[3]), x = factor_names[3], y = "Response", color = factor_names[1]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          p_interaction_2_3 <- ggplot(dataset, aes(x = .data[[factor_names[3]]], y = response, color = .data[[factor_names[2]]], group = .data[[factor_names[2]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[2], "and", factor_names[3]), x = factor_names[3], y = "Response", color = factor_names[2]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          plotObjects$boxplot <- p_box
          plotObjects$interaction_plot_1_2 <- p_interaction_1_2
          plotObjects$interaction_plot_1_3 <- p_interaction_1_3
          plotObjects$interaction_plot_2_3 <- p_interaction_2_3
        } else {
          plotObjects$boxplot <- NULL
          plotObjects$interaction_plot_1_2 <- NULL
          plotObjects$interaction_plot_1_3 <- NULL
          plotObjects$interaction_plot_2_3 <- NULL
        }
        
        output$boxplot <- renderPlot({
          if (!is.null(plotObjects$boxplot)) {
            print(plotObjects$boxplot)
          } else {
            plot(NULL, main = "Boxplot not available for this design.")
          }
        })
        output$interaction_plot_1_2 <- renderPlot({
          if (!is.null(plotObjects$interaction_plot_1_2)) {
            print(plotObjects$interaction_plot_1_2)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        output$interaction_plot_1_3 <- renderPlot({
          if (!is.null(plotObjects$interaction_plot_1_3)) {
            print(plotObjects$interaction_plot_1_3)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        output$interaction_plot_2_3 <- renderPlot({
          if (!is.null(plotObjects$interaction_plot_2_3)) {
            print(plotObjects$interaction_plot_2_3)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        
        output$analysis_results <- renderUI({
          results <- list()
          if("Parametric ANOVA" %in% input$analysisChoices) {
            results$anova <- tagList(h4("Parametric ANOVA Table "), renderPrint(summary(anova_model)))
          }
          if("Parametric Post-Hoc" %in% input$analysisChoices) {
            results$parametric <- tagList(
              h4("Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(lsd_tests[[i]])))),
                  tabPanel("Tukey", renderPrint(tukey_tests)),
                  tabPanel("Bonferroni", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(bonf_tests[[i]])))),
                  tabPanel("Scheffé", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(scheffe_tests[[i]])))),
                  tabPanel("Dunnett", 
                           tagList(
                             h5("Main Effects:"),
                             lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(dunnett_results[[i]]))),
                             if(num >= 2) tagList(h5("Interaction Effects:"), renderPrint(dunnett_interaction))
                           )
                  )
                )
              )
            )
          }
          if("Non-Parametric ANOVA and Post-Hoc" %in% input$analysisChoices) {
            results$nonparametric <- tagList(
              h4("Non-Parametric ANOVA and Pos-Hoc Tests"),
              wellPanel(
                lapply(1:num, function(i) tagList(
                  h5(paste("Kruskal-Wallis Test for", factor_names[i], ":")),
                  renderPrint(kruskal_tests[[i]]),
                  h5(paste("Dunn's Test for", factor_names[i], ":")),
                  renderPrint(dunn_tests[[i]])
                )),
                if(num >= 2) tagList(
                  h5("Kruskal-Wallis Test for Interaction Effects:"),
                  renderPrint(kruskal_interaction),
                  h5("Dunn's Test for Interaction Effects:"),
                  renderPrint(dunn_interaction)
                )
              )
            )
          }
          tagList(
            h3("2^k Factorial Design Analysis Results"),
            results$anova,
            results$parametric,
            results$nonparametric
          )
        })
        
        output$analysis_plots <- renderUI({
          if(num == 2) {
            tagList(
              h3("Visualizations"),
              plotOutput("boxplot", height = "400px"),
              plotOutput("interaction_plot_1_2", height = "400px")
            )
          } else if(num == 3) {
            tagList(
              h3("Visualizations"),
              plotOutput("boxplot", height = "400px"),
              plotOutput("interaction_plot_1_2", height = "400px"),
              plotOutput("interaction_plot_1_3", height = "400px"),
              plotOutput("interaction_plot_2_3", height = "400px")
            )
          } else {
            tagList(
              h3("Visualizations"),
              p("Visualizations are only available for designs with 2 or 3 factors.")
            )
          }
        })
        
        output$diagnostic_plots <- renderUI({
          tagList(
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        output$diagnostic_tests <- renderPrint({
          cat("Shapiro-Wilk Test for Normality:\n")
          print(shapiro.test(residuals(anova_model)))
          cat("\nAnderson-Darling Test for Normality:\n")
          print(ad.test(residuals(anova_model)))
          cat("\nKolmogorov-Smirnov Test for Normality:\n")
          print(ks.test(residuals(anova_model), "pnorm"))
          cat("\nLilliefors Test for Normality:\n")
          print(lillie.test(residuals(anova_model)))
          cat("\nBreusch-Pagan Test for Heteroscedasticity:\n")
          print(bptest(anova_model))
          cat("\nDurbin-Watson Test for Autocorrelation:\n")
          print(tryCatch(dwtest(anova_model), error = function(e) paste("Error:", e$message)))
          cat("\nBreusch-Godfrey Test for Higher-Order Autocorrelation:\n")
          print(tryCatch(bgtest(anova_model), error = function(e) paste("Error:", e$message)))
        })
        
        output$residual_plot <- renderPlot({ 
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16)
          abline(h = c(-3, 0, 3), col = "red")  # Add horizontal lines as in the sample
        })
        output$qq_plot <- renderPlot({ 
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16) 
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
      } else if(input$factorialDesignType == "Confounding 2^k Factorial Design") {
        req(input$numFactors_confounding, input$responseVar_confounding, input$confoundedInteraction)
        num <- as.integer(input$numFactors_confounding)
        factor_names <- sapply(1:num, function(i) input[[paste0("confounding_factor_", i)]])
        
        # Validate inputs
        if (any(sapply(factor_names, is.null))) {
          showModal(modalDialog(
            title = "Error",
            "Please select all factors before analyzing.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        if (length(unique(c(factor_names, input$responseVar_confounding))) != (num + 1)) {
          showModal(modalDialog(title = "Error", "All selected variables must be distinct.", easyClose = TRUE))
          return(NULL)
        }
        if (!is.numeric(dataset[[input$responseVar_confounding]])) {
          showModal(modalDialog(title = "Error", "Response Variable must be numeric.", easyClose = TRUE))
          return(NULL)
        }
        
        # Convert factors to factor type
        for (f in factor_names) dataset[[f]] <- factor(dataset[[f]])
        
        # Create a block variable based on the confounded interaction
        # Extract the numeric indices from the confounded interaction (e.g., "Factor1Factor2Factor3" -> c(1, 2, 3))
        interaction_indices <- as.integer(unlist(regmatches(input$confoundedInteraction, gregexpr("\\d+", input$confoundedInteraction))))
        if (length(interaction_indices) == 0 || any(!interaction_indices %in% 1:num)) {
          showModal(modalDialog(
            title = "Error",
            "Invalid confounded interaction selected.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        interaction_factors <- factor_names[interaction_indices]
        
        # Create the block variable by combining the replicate and the confounded interaction
        # For each level of the interaction factors, assign +1 or -1 based on the ABC interaction
        dataset$interaction_sign <- apply(dataset[, interaction_factors, drop = FALSE], 1, function(row) {
          levels <- as.character(row)
          # Assign +1 if number of "High" levels is even, -1 if odd (for ABC confounding)
          num_high <- sum(levels == "High")
          if (num_high %% 2 == 0) 1 else -1
        })
        # Combine Rep and interaction_sign to create unique blocks (e.g., "1:1", "1:-1", "2:1", etc.)
        dataset$blockVar <- factor(paste(dataset$Rep, dataset$interaction_sign, sep = ":"))
        
        # Apply response transformation
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar_confounding]]))
        
        # Define the model including the block variable (excluding the confounded interaction)
        formula_str <- paste("response ~", paste(factor_names, collapse = " * "), "+ blockVar")
        anova_model <- aov(as.formula(formula_str), data = dataset)
        
        # Dunnett's Test for Main Effects
        dunnett_results <- lapply(1:num, function(i) {
          f <- factor_names[i]
          temp_data <- dataset
          levels_f <- levels(temp_data[[f]])
          if (length(levels_f) > 1) {
            ref <- levels_f[1]  # Use "Low" as the reference level
            temp_data[[f]] <- relevel(temp_data[[f]], ref = ref)
            temp_model <- aov(as.formula(paste("response ~", f, "+ blockVar")), data = temp_data)
            mcp_args <- setNames(list("Dunnett"), f)
            tryCatch(
              summary(glht(temp_model, linfct = do.call(mcp, mcp_args))),
              error = function(e) paste("Error in Dunnett test:", e$message)
            )
          } else {
            "Factor has only one level, Dunnett test not applicable."
          }
        })
        
        # Parametric Post-Hoc Tests
        lsd_tests <- lapply(factor_names, function(f) LSD.test(anova_model, f, console = FALSE))
        tukey_tests <- TukeyHSD(anova_model)
        bonf_tests <- lapply(factor_names, function(f) pairwise.t.test(dataset$response, dataset[[f]], p.adjust.method = "bonferroni"))
        scheffe_tests <- lapply(factor_names, function(f) scheffe.test(anova_model, f, group = FALSE, console = FALSE))
        
        # Non-Parametric Tests (Friedman and Nemenyi for each factor; Wilcoxon rank-sum for 2 levels)
        friedman_tests <- list()
        nemenyi_tests <- list()
        wilcoxon_ranksum_tests <- list()
        for (f in factor_names) {
          num_levels <- length(unique(dataset[[f]]))
          if (num_levels < 3) {
            friedman_tests[[f]] <- paste("Friedman test requires at least 3 levels in the factor (", f, " has ", num_levels, " levels).")
            nemenyi_tests[[f]] <- paste("Nemenyi test requires at least 3 levels in the factor (", f, " has ", num_levels, " levels).")
            # Perform Wilcoxon rank-sum test for 2 levels
            if (num_levels == 2) {
              levels_f <- levels(dataset[[f]])
              responses_level1 <- dataset$response[dataset[[f]] == levels_f[1]]
              responses_level2 <- dataset$response[dataset[[f]] == levels_f[2]]
              wilcoxon_ranksum_tests[[f]] <- tryCatch(
                wilcox.test(responses_level1, responses_level2, paired = FALSE),
                error = function(e) paste("Error in Wilcoxon rank-sum test for", f, ":", e$message)
              )
            } else {
              wilcoxon_ranksum_tests[[f]] <- paste("Wilcoxon rank-sum test is only applicable for factors with exactly 2 levels (", f, " has ", num_levels, " levels).")
            }
            next
          }
          tab <- table(dataset[[f]], dataset$blockVar)
          if (any(tab == 0) || length(unique(tab[tab > 0])) > 1) {
            friedman_tests[[f]] <- paste("Friedman test requires a balanced design for", f, ".")
            nemenyi_tests[[f]] <- paste("Nemenyi test requires a balanced design for", f, ".")
            next
          }
          formula_str <- as.formula(paste("response ~", f, "| blockVar"))
          friedman_tests[[f]] <- tryCatch(
            friedman.test(formula_str, data = dataset),
            error = function(e) paste("Error in Friedman test for", f, ":", e$message)
          )
          nemenyi_tests[[f]] <- tryCatch(
            frdAllPairsNemenyiTest(formula_str, data = dataset),
            error = function(e) paste("Error in Nemenyi test for", f, ":", e$message)
          )
        }
        
        # Generate and store plots
        if(num == 2) {
          p_box <- ggplot(dataset, aes(x = .data[[factor_names[1]]], y = response, fill = .data[[factor_names[2]]])) +
            geom_boxplot() +
            labs(title = paste("Response by", factor_names[1], "and", factor_names[2]), x = factor_names[1], y = "Response") +
            theme_minimal(base_size = 14) +
            scale_fill_brewer(palette = "Set2")
          p_interaction_1_2 <- ggplot(dataset, aes(x = .data[[factor_names[2]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[1], "and", factor_names[2]), x = factor_names[2], y = "Response", color = factor_names[1]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          plotObjects$boxplot <- p_box
          plotObjects$interaction_plot_1_2 <- p_interaction_1_2
          plotObjects$interaction_plot_1_3 <- NULL
          plotObjects$interaction_plot_2_3 <- NULL
          plotObjects$interaction_plot <- NULL
        } else if(num == 3) {
          p_box <- ggplot(dataset, aes(x = .data[[factor_names[1]]], y = response, fill = .data[[factor_names[2]]])) +
            geom_boxplot() +
            labs(title = paste("Response by", factor_names[1], "and", factor_names[2]), x = factor_names[1], y = "Response") +
            theme_minimal(base_size = 14) +
            scale_fill_brewer(palette = "Set2")
          p_interaction_1_2 <- ggplot(dataset, aes(x = .data[[factor_names[2]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[1], "and", factor_names[2]), x = factor_names[2], y = "Response", color = factor_names[1]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          p_interaction_1_3 <- ggplot(dataset, aes(x = .data[[factor_names[3]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[1], "and", factor_names[3]), x = factor_names[3], y = "Response", color = factor_names[1]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          p_interaction_2_3 <- ggplot(dataset, aes(x = .data[[factor_names[3]]], y = response, color = .data[[factor_names[2]]], group = .data[[factor_names[2]]])) +
            geom_point(size = 3) +
            geom_line() +
            labs(title = paste("Interaction Plot:", factor_names[2], "and", factor_names[3]), x = factor_names[3], y = "Response", color = factor_names[2]) +
            theme_minimal(base_size = 14) +
            scale_color_brewer(palette = "Set1")
          plotObjects$boxplot <- p_box
          plotObjects$interaction_plot_1_2 <- p_interaction_1_2
          plotObjects$interaction_plot_1_3 <- p_interaction_1_3
          plotObjects$interaction_plot_2_3 <- p_interaction_2_3
          plotObjects$interaction_plot <- NULL
        } else {
          plotObjects$boxplot <- NULL
          plotObjects$interaction_plot_1_2 <- NULL
          plotObjects$interaction_plot_1_3 <- NULL
          plotObjects$interaction_plot_2_3 <- NULL
          plotObjects$interaction_plot <- NULL
        }
        
        # Define plot rendering outputs
        output$boxplot <- renderPlot({
          if (!is.null(plotObjects$boxplot)) {
            print(plotObjects$boxplot)
          } else {
            plot(NULL, main = "Boxplot not available for this design.")
          }
        })
        output$interaction_plot_1_2 <- renderPlot({
          if (!is.null(plotObjects$interaction_plot_1_2)) {
            print(plotObjects$interaction_plot_1_2)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        output$interaction_plot_1_3 <- renderPlot({
          if (!is.null(plotObjects$interaction_plot_1_3)) {
            print(plotObjects$interaction_plot_1_3)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        output$interaction_plot_2_3 <- renderPlot({
          if (!is.null(plotObjects$interaction_plot_2_3)) {
            print(plotObjects$interaction_plot_2_3)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        
        # Render analysis results
        output$analysis_results <- renderUI({
          results <- list()
          if ("Parametric ANOVA" %in% input$analysisChoices) {
            anova_df <- as.data.frame(summary(anova_model)[[1]])
            if ("Pr(>F)" %in% colnames(anova_df))
              anova_df$`Pr(>F)` <- format(anova_df$`Pr(>F)`, scientific = TRUE, digits = 4)
            results$anova <- tagList(h4("Parametric ANOVA Table"), renderTable({ anova_df }, rownames = TRUE))
          }
          if ("Parametric Post-Hoc" %in% input$analysisChoices) {
            results$parametric <- tagList(
              h4(" Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(lsd_tests[[i]])))),
                  tabPanel("Tukey", renderPrint(tukey_tests)),
                  tabPanel("Bonferroni", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(bonf_tests[[i]])))),
                  tabPanel("Scheffé", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(scheffe_tests[[i]])))),
                  tabPanel("Dunnett", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(dunnett_results[[i]]))))
                )
              )
            )
          }
          if ("Non-Parametric ANOVA and Post-Hoc" %in% input$analysisChoices) {
            results$nonparametric <- tagList(
              h4("Non-Parametric Anova and  Post-Hoc Tests"),
              wellPanel(
                lapply(factor_names, function(f) tagList(
                  h5(paste("Friedman Test for", f, ":")),
                  renderPrint(friedman_tests[[f]]),
                  h5(paste("Nemenyi Test for", f, ":")),
                  renderPrint(if (is.list(nemenyi_tests[[f]])) summary(nemenyi_tests[[f]]) else nemenyi_tests[[f]]),
                  h5(paste("Wilcoxon Rank-Sum Test for", f, ":")),
                  renderPrint(if (is.list(wilcoxon_ranksum_tests[[f]])) wilcoxon_ranksum_tests[[f]] else wilcoxon_ranksum_tests[[f]])
                ))
              )
            )
          }
          tagList(
            h3("Confounding 2^k Factorial Design Analysis Results"),
            results$anova,
            results$parametric,
            results$nonparametric
          )
        })
        
        # Render visualization plots
        output$analysis_plots <- renderUI({
          if(num == 2) {
            tagList(
              h3("Visualizations"),
              plotOutput("boxplot", height = "400px"),
              plotOutput("interaction_plot_1_2", height = "400px")
            )
          } else if(num == 3) {
            tagList(
              h3("Visualizations"),
              plotOutput("boxplot", height = "400px"),
              plotOutput("interaction_plot_1_2", height = "400px"),
              plotOutput("interaction_plot_1_3", height = "400px"),
              plotOutput("interaction_plot_2_3", height = "400px")
            )
          } else {
            tagList(
              h3("Visualizations"),
              p("Visualizations are only available for designs with 2 or 3 factors.")
            )
          }
        })
        
        # Render diagnostic plots
        output$diagnostic_plots <- renderUI({
          tagList(
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        # Render diagnostic tests
        output$diagnostic_tests <- renderPrint({
          cat("Shapiro-Wilk Test for Normality:\n")
          print(shapiro.test(residuals(anova_model)))
          cat("\nBreusch-Pagan Test for Heteroscedasticity:\n")
          print(bptest(anova_model))
          cat("\nDurbin-Watson Test for Autocorrelation:\n")
          print(tryCatch(dwtest(anova_model), error = function(e) paste("Error:", e$message)))
          cat("\nBreusch-Godfrey Test for Higher-Order Autocorrelation:\n")
          print(tryCatch(bgtest(anova_model), error = function(e) paste("Error:", e$message)))
        })
        
        output$residual_plot <- renderPlot({ 
          plot(anova_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16) 
        })
        output$qq_plot <- renderPlot({ 
          plot(anova_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16) 
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(anova_model), x = seq_along(residuals(anova_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
      } else if(input$factorialDesignType == "Split-Plot Design") {
        req(input$wholePlotFactor, input$subPlotFactor, input$responseVar_split, input$blockVar_split)
        factor_names <- c(input$wholePlotFactor, input$subPlotFactor)
        num <- length(factor_names)  # Always 2 for split-plot (whole plot + subplot)
        if(length(unique(c(factor_names, input$blockVar_split, input$responseVar_split))) != 4) {
          showModal(modalDialog(title = "Error", "All selected variables must be distinct.", easyClose = TRUE))
          return(NULL)
        }
        if(!is.numeric(dataset[[input$responseVar_split]])) {
          showModal(modalDialog(title = "Error", "Response Variable must be numeric.", easyClose = TRUE))
          return(NULL)
        }
        for(f in factor_names) dataset[[f]] <- factor(dataset[[f]])
        dataset$blockVar <- factor(dataset[[input$blockVar_split]])
        dataset$response <- applyTransformation(as.numeric(dataset[[input$responseVar_split]]))
        
        # Correct formula for split-plot design with proper error structure
        formula_str <- paste("response ~", factor_names[1], "*", factor_names[2], 
                             "+ Error(blockVar/", factor_names[2], ")")
        anova_model <- aov(as.formula(formula_str), data = dataset)
        
        # Dunnett tests for main effects
        dunnett_results <- lapply(1:num, function(i) {
          f <- factor_names[i]
          temp_data <- dataset
          levels_f <- levels(temp_data[[f]])
          if(length(levels_f) > 1) {
            ref <- if(i == 1) input$dunnett_ref_whole else input$dunnett_ref_sub
            if(is.null(ref) || !(ref %in% levels_f)) ref <- levels_f[1]
            temp_data[[f]] <- relevel(temp_data[[f]], ref = ref)
            # Fit model for post-hoc test (ignoring error strata for Dunnett)
            temp_model <- aov(as.formula(paste("response ~", f)), data = temp_data)
            mcp_args <- setNames(list("Dunnett"), f)
            tryCatch(
              summary(glht(temp_model, linfct = do.call(mcp, mcp_args))),
              error = function(e) paste("Error in Dunnett test:", e$message)
            )
          } else {
            "Factor has only one level, Dunnett test not applicable."
          }
        })
        
        # Fit a fixed-effects model for other post-hoc tests (ignoring error strata)
        posthoc_model <- aov(as.formula(paste("response ~", factor_names[1], "*", factor_names[2])), data = dataset)
        
        lsd_tests <- lapply(factor_names, function(f) LSD.test(posthoc_model, f, console = FALSE))
        tukey_tests <- TukeyHSD(posthoc_model)
        bonf_tests <- lapply(factor_names, function(f) pairwise.t.test(dataset$response, dataset[[f]], p.adjust.method = "bonferroni"))
        scheffe_tests <- lapply(factor_names, function(f) scheffe.test(posthoc_model, f, group = FALSE, console = FALSE))
        
        kruskal_tests <- lapply(factor_names, function(f) kruskal.test(response ~ dataset[[f]], data = dataset))
        dunn_tests <- lapply(factor_names, function(f) dunnTest(response ~ dataset[[f]], data = dataset, method = "bonferroni"))
        
        # Generate and store plots (always num == 2 for split-plot)
        p_box <- ggplot(dataset, aes(x = .data[[factor_names[1]]], y = response, fill = .data[[factor_names[2]]])) +
          geom_boxplot() +
          labs(title = paste("Response by", factor_names[1], "and", factor_names[2]), x = factor_names[1], y = "Response") +
          theme_minimal(base_size = 14) +
          scale_fill_brewer(palette = "Set2")
        p_interaction <- ggplot(dataset, aes(x = .data[[factor_names[2]]], y = response, color = .data[[factor_names[1]]], group = .data[[factor_names[1]]])) +
          geom_point(size = 3) +
          geom_line() +
          labs(title = "Interaction Plot", x = factor_names[2], y = "Response", color = factor_names[1]) +
          theme_minimal(base_size = 14) +
          scale_color_brewer(palette = "Set1")
        plotObjects$boxplot <- p_box
        plotObjects$interaction_plot <- p_interaction
        
        # Define plot rendering outputs
        output$boxplot <- renderPlot({
          if (!is.null(plotObjects$boxplot)) {
            print(plotObjects$boxplot)
          } else {
            plot(NULL, main = "Boxplot not available for this design.")
          }
        })
        output$interaction_plot <- renderPlot({
          if (!is.null(plotObjects$interaction_plot)) {
            print(plotObjects$interaction_plot)
          } else {
            plot(NULL, main = "Interaction plot not available for this design.")
          }
        })
        
        output$analysis_results <- renderUI({
          results <- list()
          if("Parametric ANOVA" %in% input$analysisChoices) {
            results$anova <- tagList(h4("Parametric ANOVA Table"), renderPrint(summary(anova_model)))
          }
          if("Parametric Post-Hoc" %in% input$analysisChoices) {
            results$parametric <- tagList(
              h4("Parametric Post-Hoc Tests"),
              wellPanel(
                tabsetPanel(
                  tabPanel("LSD", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(lsd_tests[[i]])))),
                  tabPanel("Tukey", renderPrint(tukey_tests)),
                  tabPanel("Bonferroni", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(bonf_tests[[i]])))),
                  tabPanel("Scheffé", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(scheffe_tests[[i]])))),
                  tabPanel("Dunnett", lapply(1:num, function(i) tagList(h5(factor_names[i]), renderPrint(dunnett_results[[i]]))))
                )
              )
            )
          }
          if("Non-Parametric ANOVA and Post-Hoc" %in% input$analysisChoices) {
            results$nonparametric <- tagList(
              h4("Non-Parametric ANOVA and Post-Hoc Tests"),
              wellPanel(
                lapply(1:num, function(i) tagList(
                  h5(paste("Kruskal-Wallis Test for", factor_names[i], ":")),
                  renderPrint(kruskal_tests[[i]]),
                  h5(paste("Dunn's Test for", factor_names[i], ":")),
                  renderPrint(dunn_tests[[i]])
                ))
              )
            )
          }
          tagList(
            h3("Split-Plot Design Analysis Results"),
            results$anova,
            results$parametric,
            results$nonparametric
          )
        })
        
        output$analysis_plots <- renderUI({
          tagList(
            h3("Visualizations"),
            plotOutput("boxplot", height = "400px"),
            plotOutput("interaction_plot", height = "400px")
          )
        })
        
        output$diagnostic_plots <- renderUI({
          tagList(
            plotOutput("residual_plot", height = "400px"),
            plotOutput("qq_plot", height = "400px"),
            plotOutput("ts_plot", height = "400px")
          )
        })
        
        output$diagnostic_tests <- renderPrint({
          cat("Shapiro-Wilk Test for Normality:\n")
          print(shapiro.test(residuals(posthoc_model)))  # Use posthoc_model for diagnostics
          cat("\nBreusch-Pagan Test for Heteroscedasticity:\n")
          print(bptest(posthoc_model))
          cat("\nDurbin-Watson Test for Autocorrelation:\n")
          print(tryCatch(dwtest(posthoc_model), error = function(e) paste("Error:", e$message)))
          cat("\nBreusch-Godfrey Test for Higher-Order Autocorrelation:\n")
          print(tryCatch(bgtest(posthoc_model), error = function(e) paste("Error:", e$message)))
        })
        
        output$residual_plot <- renderPlot({ 
          plot(posthoc_model, 1, main = "Residuals vs Fitted", col = "blue", pch = 16) 
        })
        output$qq_plot <- renderPlot({ 
          plot(posthoc_model, 2, main = "Normal Q-Q Plot", col = "darkgreen", pch = 16) 
        })
        output$ts_plot <- renderPlot({
          plot(y = residuals(posthoc_model), x = seq_along(residuals(posthoc_model)),
               main = "Ordered Residual Plot", type = "o", col = "red",
               xlab = "Observation Order", ylab = "Residuals")
        })
      }
    }
  })  # Line 1747
  
  
  # Download handler for analysis results
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("analysis_results_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("Experimental Design Analysis Results\n\n")
      if (exists("anova_model", envir = environment())) {
        cat("ANOVA Summary:\n")
        print(summary(anova_model))
        cat("\n")
      }
      if (exists("tukey_tests", envir = environment())) {
        cat("Tukey HSD Post-Hoc Tests:\n")
        print(tukey_tests)
        cat("\n")
      }
      if (exists("splitplot_model", envir = environment())) {
        cat("Split-Plot ANOVA Summary:\n")
        print(summary(splitplot_model))
        cat("\n")
      }
      sink()
    }
  )
  
  # Download handler for plots
  output$downloadPlots <- downloadHandler(
    filename = function() {
      paste("analysis_plots_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      if (!is.null(plotObjects$boxplot)) {
        ggsave(file, plot = plotObjects$boxplot, device = "png", width = 8, height = 6)
      } else if (!is.null(plotObjects$interaction_plot)) {
        ggsave(file, plot = plotObjects$interaction_plot, device = "png", width = 8, height = 6)
      } else {
        writeLines("No plots available to download.", file)
      }
    }
  )
}  # End of server function
shinyApp(ui = ui, server = server)











