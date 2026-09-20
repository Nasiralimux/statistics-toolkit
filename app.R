# ============================================================
# STATISTICS TOOLKIT
# Generic Dataset-Independent Shiny Application
# ============================================================


# ============================================================
# SECTION 1 — LIBRARIES
# ============================================================

library(shiny)
library(readxl)
library(haven)
library(ggplot2)


# ============================================================
# SECTION 2 — USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  titlePanel("Statistics Toolkit"),
  
  
  # ==========================================================
  # DATA UPLOAD
  # ==========================================================
  
  h3("Upload Your Dataset"),
  
  fileInput(
    "file",
    "Choose a dataset:",
    accept = c(".csv", ".xlsx", ".xls", ".sav")
  ),
  
  
  # ==========================================================
  # DATASET PREVIEW
  # ==========================================================
  
  h3("Dataset Preview"),
  
  tableOutput("data_preview"),
  
  
  # ==========================================================
  # DESCRIPTIVE STATISTICS
  # ==========================================================
  
  h3("Descriptive Statistics"),
  
  h4("Numeric Variables"),
  
  tableOutput("numeric_stats"),
  
  h4("Categorical Variables"),
  
  tableOutput("categorical_stats"),
  
  
  # ==========================================================
  # RECOMMENDED VISUALIZATIONS
  # ==========================================================
  
  h3("Recommended Visualizations"),
  
  uiOutput("recommended_visuals"),
  
  plotOutput(
    "recommended_plot",
    height = "500px"
  ),
  
  
  # ==========================================================
  # ADVANCED VISUALIZATIONS
  # ==========================================================
  
  h3("Advanced Visualizations"),
  
  uiOutput("advanced_visual_controls"),
  
  plotOutput(
    "advanced_plot",
    height = "550px"
  ),
  
  
  # ==========================================================
  # ADVANCED STATISTICAL TOOLS
  # ==========================================================
  
  h3("Advanced Statistical Tools"),
  
  
  # ----------------------------------------------------------
  # MAIN CATEGORY
  # ----------------------------------------------------------
  
  selectInput(
    "advanced_category",
    "Category:",
    choices = c(
      "Normality Check",
      "Regression Analysis",
      "Correlation Analysis"
    ),
    selected = "Normality Check"
  ),
  
  
  # ==========================================================
  # 1. NORMALITY CHECK
  # ==========================================================
  
  conditionalPanel(
    
    condition =
      "input.advanced_category == 'Normality Check'",
    
    
    radioButtons(
      "normality_mode",
      "Variable Selection:",
      choices = c(
        "Automatic Variable Selection" = "automatic",
        "Manual Variable Selection" = "manual"
      ),
      selected = "automatic",
      inline = TRUE
    ),
    
    
    # --------------------------------------------------------
    # NORMALITY AUTOMATIC
    # --------------------------------------------------------
    
    conditionalPanel(
      
      condition =
        "input.normality_mode == 'automatic'",
      
      wellPanel(
        
        h4("Automatic Variable Selection"),
        
        uiOutput(
          "automatic_normality_variable"
        ),
        
        actionButton(
          "run_automatic_normality",
          "Check Normality",
          class = "btn-primary"
        )
      )
    ),
    
    
    # --------------------------------------------------------
    # NORMALITY MANUAL
    # --------------------------------------------------------
    
    conditionalPanel(
      
      condition =
        "input.normality_mode == 'manual'",
      
      wellPanel(
        
        h4("Manual Variable Selection"),
        
        selectInput(
          "normality_variable",
          "Numeric Variable:",
          choices = NULL
        ),
        
        actionButton(
          "run_manual_normality",
          "Check Normality",
          class = "btn-primary"
        )
      )
    ),
    
    
    br(),
    
    uiOutput(
      "normality_validation"
    ),
    
    
    # --------------------------------------------------------
    # NORMALITY RESULTS
    # --------------------------------------------------------
    
    conditionalPanel(
      
      condition =
        "(input.normality_mode == 'automatic' &&
          input.run_automatic_normality > 0) ||
         (input.normality_mode == 'manual' &&
          input.run_manual_normality > 0)",
      
      h4("Normality Results"),
      
      tableOutput(
        "normality_results"
      ),
      
      h4("Interpretation"),
      
      uiOutput(
        "normality_interpretation"
      ),
      
      h4("Q-Q Plot"),
      
      plotOutput(
        "normality_qq_plot",
        height = "450px"
      ),
      
      h4("Histogram with Density"),
      
      plotOutput(
        "normality_histogram",
        height = "450px"
      )
    )
  ),
  
  
  # ==========================================================
  # 2. REGRESSION ANALYSIS
  # ==========================================================
  
  conditionalPanel(
    
    condition =
      "input.advanced_category == 'Regression Analysis'",
    
    
    selectInput(
      "regression_method",
      "Related Tool:",
      choices = "Linear Regression",
      selected = "Linear Regression"
    ),
    
    
    conditionalPanel(
      
      condition =
        "input.regression_method == 'Linear Regression'",
      
      
      radioButtons(
        "linear_regression_mode",
        "Variable Selection:",
        
        choices = c(
          "Automatic Variable Selection" = "automatic",
          "Manual Variable Selection" = "manual"
        ),
        
        selected = "automatic",
        inline = TRUE
      ),
      
      
      # ------------------------------------------------------
      # AUTOMATIC REGRESSION
      # ------------------------------------------------------
      
      conditionalPanel(
        
        condition =
          "input.linear_regression_mode == 'automatic'",
        
        wellPanel(
          
          h4("Automatic Variable Selection"),
          
          uiOutput(
            "automatic_regression_variables"
          ),
          
          actionButton(
            "run_automatic_regression",
            "Run Linear Regression",
            class = "btn-primary"
          )
        )
      ),
      
      
      # ------------------------------------------------------
      # MANUAL REGRESSION
      # ------------------------------------------------------
      
      conditionalPanel(
        
        condition =
          "input.linear_regression_mode == 'manual'",
        
        wellPanel(
          
          h4("Manual Variable Selection"),
          
          selectInput(
            "linear_regression_dv",
            "Dependent Variable:",
            choices = NULL
          ),
          
          selectInput(
            "linear_regression_iv",
            "Independent Variable(s):",
            choices = NULL,
            multiple = TRUE
          ),
          
          actionButton(
            "run_manual_regression",
            "Run Linear Regression",
            class = "btn-primary"
          )
        )
      ),
      
      
      br(),
      
      uiOutput(
        "linear_regression_validation"
      ),
      
      
      # ------------------------------------------------------
      # AUTOMATIC RESULTS
      # ------------------------------------------------------
      
      conditionalPanel(
        
        condition =
          "input.linear_regression_mode == 'automatic' &&
           input.run_automatic_regression > 0",
        
        h4("Multicollinearity Check"),
        
        uiOutput(
          "automatic_collinearity_check"
        ),
        
        h4("Regression Results"),
        
        verbatimTextOutput(
          "automatic_regression_summary"
        ),
        
        h4("Regression Coefficients"),
        
        tableOutput(
          "automatic_regression_coefficients"
        )
      ),
      
      
      # ------------------------------------------------------
      # MANUAL RESULTS
      # ------------------------------------------------------
      
      conditionalPanel(
        
        condition =
          "input.linear_regression_mode == 'manual' &&
           input.run_manual_regression > 0",
        
        h4("Multicollinearity Check"),
        
        uiOutput(
          "manual_collinearity_check"
        ),
        
        h4("Regression Results"),
        
        verbatimTextOutput(
          "manual_regression_summary"
        ),
        
        h4("Regression Coefficients"),
        
        tableOutput(
          "manual_regression_coefficients"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # 3. CORRELATION ANALYSIS
  # ==========================================================
  
  conditionalPanel(
    
    condition =
      "input.advanced_category == 'Correlation Analysis'",
    
    
    selectInput(
      "correlation_method",
      "Correlation Method:",
      choices = c(
        "Pearson",
        "Spearman",
        "Kendall"
      ),
      selected = "Pearson"
    ),
    
    
    radioButtons(
      "correlation_mode",
      "Variable Selection:",
      choices = c(
        "Automatic Variable Selection" = "automatic",
        "Manual Variable Selection" = "manual"
      ),
      selected = "automatic",
      inline = TRUE
    ),
    
    
    # --------------------------------------------------------
    # AUTOMATIC CORRELATION
    # --------------------------------------------------------
    
    conditionalPanel(
      
      condition =
        "input.correlation_mode == 'automatic'",
      
      wellPanel(
        
        h4("Automatic Variable Selection"),
        
        uiOutput(
          "automatic_correlation_variables"
        ),
        
        actionButton(
          "run_automatic_correlation",
          "Run Correlation Analysis",
          class = "btn-primary"
        )
      )
    ),
    
    
    # --------------------------------------------------------
    # MANUAL CORRELATION
    # --------------------------------------------------------
    
    conditionalPanel(
      
      condition =
        "input.correlation_mode == 'manual'",
      
      wellPanel(
        
        h4("Manual Variable Selection"),
        
        selectInput(
          "correlation_x",
          "Variable 1:",
          choices = NULL
        ),
        
        selectInput(
          "correlation_y",
          "Variable 2:",
          choices = NULL
        ),
        
        actionButton(
          "run_manual_correlation",
          "Run Correlation Analysis",
          class = "btn-primary"
        )
      )
    ),
    
    
    br(),
    
    uiOutput(
      "correlation_validation"
    ),
    
    
    # --------------------------------------------------------
    # CORRELATION RESULTS
    # --------------------------------------------------------
    
    conditionalPanel(
      
      condition =
        "(input.correlation_mode == 'automatic' &&
          input.run_automatic_correlation > 0) ||
         (input.correlation_mode == 'manual' &&
          input.run_manual_correlation > 0)",
      
      h4("Correlation Results"),
      
      tableOutput(
        "correlation_results"
      ),
      
      uiOutput(
        "correlation_interpretation"
      )
    )
  )
)


# ============================================================
# SECTION 3 — SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # SECTION 4 — READ DATASET
  # ==========================================================
  
  data <- reactive({
    
    req(input$file)
    
    extension <- tolower(
      tools::file_ext(input$file$name)
    )
    
    switch(
      extension,
      
      "csv" = read.csv(
        input$file$datapath,
        stringsAsFactors = FALSE,
        check.names = TRUE
      ),
      
      "xlsx" = as.data.frame(
        read_excel(input$file$datapath)
      ),
      
      "xls" = as.data.frame(
        read_excel(input$file$datapath)
      ),
      
      "sav" = as.data.frame(
        read_sav(input$file$datapath)
      ),
      
      stop("Unsupported file format.")
    )
  })
  
  
  # ==========================================================
  # SECTION 5 — DATASET PREVIEW
  # ==========================================================
  
  output$data_preview <- renderTable({
    
    req(data())
    
    head(
      data(),
      10
    )
  })
  
  
  # ==========================================================
  # SECTION 6 — AUTOMATIC VARIABLE TYPE DETECTION
  # ==========================================================
  
  variable_types <- reactive({
    
    df <- data()
    
    numeric_variables <- names(df)[
      vapply(
        df,
        is.numeric,
        logical(1)
      )
    ]
    
    date_variables <- names(df)[
      vapply(
        df,
        function(x) {
          inherits(
            x,
            c(
              "Date",
              "POSIXct",
              "POSIXlt"
            )
          )
        },
        logical(1)
      )
    ]
    
    categorical_variables <- names(df)[
      vapply(
        df,
        function(x) {
          is.character(x) ||
            is.factor(x) ||
            is.logical(x)
        },
        logical(1)
      )
    ]
    
    other_variables <- setdiff(
      names(df),
      c(
        numeric_variables,
        categorical_variables,
        date_variables
      )
    )
    
    list(
      numeric = numeric_variables,
      categorical = categorical_variables,
      date = date_variables,
      other = other_variables
    )
  })
  
  
  # ==========================================================
  # SECTION 7 — DESCRIPTIVE STATISTICS
  # ==========================================================
  
  output$numeric_stats <- renderTable({
    
    df <- data()
    variables <- variable_types()$numeric
    
    if (!length(variables)) {
      return(
        data.frame(
          Message = "No numeric variables detected."
        )
      )
    }
    
    results <- lapply(
      variables,
      function(variable) {
        
        x <- df[[variable]]
        x <- x[!is.na(x)]
        
        if (!length(x)) {
          return(
            data.frame(
              Variable = variable,
              N = 0,
              Mean = NA,
              Median = NA,
              SD = NA,
              Variance = NA,
              Min = NA,
              Q1 = NA,
              Q3 = NA,
              Max = NA
            )
          )
        }
        
        data.frame(
          Variable = variable,
          N = length(x),
          Mean = round(mean(x), 3),
          Median = round(median(x), 3),
          SD = round(sd(x), 3),
          Variance = round(var(x), 3),
          Min = round(min(x), 3),
          Q1 = round(
            quantile(x, 0.25),
            3
          ),
          Q3 = round(
            quantile(x, 0.75),
            3
          ),
          Max = round(max(x), 3),
          stringsAsFactors = FALSE
        )
      }
    )
    
    do.call(
      rbind,
      results
    )
  })
  
  
  # ----------------------------------------------------------
  # CATEGORICAL STATISTICS
  # ----------------------------------------------------------
  
  output$categorical_stats <- renderTable({
    
    df <- data()
    variables <- variable_types()$categorical
    
    if (!length(variables)) {
      return(
        data.frame(
          Message = "No categorical variables detected."
        )
      )
    }
    
    results <- lapply(
      variables,
      function(variable) {
        
        x <- df[[variable]]
        x <- x[!is.na(x)]
        
        if (!length(x)) {
          return(
            data.frame(
              Variable = variable,
              Category = NA,
              Frequency = NA,
              Percentage = NA
            )
          )
        }
        
        freq <- sort(
          table(x),
          decreasing = TRUE
        )
        
        result <- data.frame(
          Variable = variable,
          Category = names(freq),
          Frequency = as.numeric(freq),
          Percentage = round(
            100 * as.numeric(freq) /
              sum(freq),
            2
          ),
          stringsAsFactors = FALSE
        )
        
        head(
          result,
          5
        )
      }
    )
    
    do.call(
      rbind,
      results
    )
  })
  
  
  # ==========================================================
  # SECTION 8 — RECOMMENDED VISUALIZATIONS
  # ==========================================================
  
  output$recommended_visuals <- renderUI({
    
    req(data())
    
    types <- variable_types()
    choices <- character(0)
    
    if (length(types$numeric) >= 1) {
      choices <- c(
        choices,
        "Histogram",
        "Boxplot",
        "Density Plot"
      )
    }
    
    if (length(types$categorical) >= 1) {
      choices <- c(
        choices,
        "Bar Chart"
      )
    }
    
    if (length(types$numeric) >= 2) {
      choices <- c(
        choices,
        "Scatter Plot"
      )
    }
    
    if (
      length(types$numeric) >= 1 &&
      length(types$categorical) >= 1
    ) {
      choices <- c(
        choices,
        "Numeric Variable by Category"
      )
    }
    
    if (!length(choices)) {
      return(
        p(
          "No suitable visualizations were detected for this dataset."
        )
      )
    }
    
    selectInput(
      "recommended_type",
      "Recommended visualization:",
      choices = choices
    )
  })
  
  
  # ==========================================================
  # SECTION 9 — RECOMMENDED VISUALIZATION
  # ==========================================================
  
  output$recommended_plot <- renderPlot({
    
    req(
      data(),
      input$recommended_type
    )
    
    df <- data()
    types <- variable_types()
    chart <- input$recommended_type
    
    if (chart == "Histogram") {
      
      variable <- types$numeric[1]
      
      return(
        ggplot(
          df,
          aes(x = .data[[variable]])
        ) +
          geom_histogram(
            bins = 30,
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              "Distribution of",
              variable
            ),
            x = variable,
            y = "Frequency"
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Boxplot") {
      
      variable <- types$numeric[1]
      
      return(
        ggplot(
          df,
          aes(y = .data[[variable]])
        ) +
          geom_boxplot(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              "Boxplot of",
              variable
            ),
            y = variable
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Density Plot") {
      
      variable <- types$numeric[1]
      
      return(
        ggplot(
          df,
          aes(x = .data[[variable]])
        ) +
          geom_density(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              "Density Plot of",
              variable
            ),
            x = variable,
            y = "Density"
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Bar Chart") {
      
      variable <- types$categorical[1]
      
      return(
        ggplot(
          df,
          aes(x = .data[[variable]])
        ) +
          geom_bar(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              "Frequency of",
              variable
            ),
            x = variable,
            y = "Frequency"
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Scatter Plot") {
      
      x <- types$numeric[1]
      y <- types$numeric[2]
      
      return(
        ggplot(
          df,
          aes(
            x = .data[[x]],
            y = .data[[y]]
          )
        ) +
          geom_point(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              y,
              "vs",
              x
            ),
            x = x,
            y = y
          ) +
          theme_minimal()
      )
    }
    
    
    if (
      chart ==
      "Numeric Variable by Category"
    ) {
      
      y <- types$numeric[1]
      x <- types$categorical[1]
      
      return(
        ggplot(
          df,
          aes(
            x = .data[[x]],
            y = .data[[y]]
          )
        ) +
          geom_boxplot(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              y,
              "by",
              x
            ),
            x = x,
            y = y
          ) +
          theme_minimal()
      )
    }
  })
  
  
  # ==========================================================
  # SECTION 10 — ADVANCED VISUALIZATION CONTROLS
  # ==========================================================
  
  output$advanced_visual_controls <- renderUI({
    
    req(data())
    
    tagList(
      
      selectInput(
        "advanced_type",
        "Visualization Type:",
        choices = c(
          "Bar Chart",
          "Multiple / Grouped Bar Chart",
          "Summary Bar Chart",
          "Histogram",
          "Boxplot",
          "Density Plot",
          "Scatter Plot"
        )
      ),
      
      radioButtons(
        "selection_mode",
        "Variable Selection Mode:",
        choices = c(
          "Automatic",
          "Manual"
        ),
        selected = "Automatic",
        inline = TRUE
      ),
      
      uiOutput(
        "advanced_variable_controls"
      )
    )
  })
  
  
  # ==========================================================
  # SECTION 11 — ADVANCED VARIABLE CONTROLS
  # ==========================================================
  
  output$advanced_variable_controls <- renderUI({
    
    req(
      data(),
      input$advanced_type,
      input$selection_mode
    )
    
    types <- variable_types()
    chart <- input$advanced_type
    
    if (
      input$selection_mode ==
      "Automatic"
    ) {
      
      if (
        chart ==
        "Multiple / Grouped Bar Chart"
      ) {
        
        if (
          length(types$categorical) >= 2
        ) {
          
          return(
            tagList(
              p(
                "Automatic mode will select two suitable categorical variables from the dataset."
              ),
              actionButton(
                "generate_advanced",
                "Generate Chart"
              )
            )
          )
        }
        
        return(
          p(
            "A grouped bar chart requires at least two categorical variables."
          )
        )
      }
      
      
      if (
        chart ==
        "Summary Bar Chart"
      ) {
        
        if (
          length(types$categorical) >= 1 &&
          length(types$numeric) >= 1
        ) {
          
          return(
            tagList(
              
              p(
                "Automatic mode will use the first available categorical and numeric variables."
              ),
              
              selectInput(
                "automatic_summary",
                "Summary Statistic:",
                choices = c(
                  "Mean",
                  "Median",
                  "Sum",
                  "Minimum",
                  "Maximum"
                ),
                selected = "Mean"
              ),
              
              actionButton(
                "generate_advanced",
                "Generate Chart"
              )
            )
          )
        }
        
        return(
          p(
            "A Summary Bar Chart requires at least one categorical and one numeric variable."
          )
        )
      }
      
      
      return(
        tagList(
          
          p(
            "Automatic mode will select suitable variables based on their detected data types."
          ),
          
          actionButton(
            "generate_advanced",
            "Generate Chart"
          )
        )
      )
    }
    
    
    if (
      input$selection_mode ==
      "Manual"
    ) {
      
      if (chart == "Bar Chart") {
        
        if (!length(types$categorical)) {
          return(
            p(
              "No categorical variables are available."
            )
          )
        }
        
        return(
          tagList(
            
            selectInput(
              "bar_x",
              "Categorical Variable:",
              choices = types$categorical
            ),
            
            actionButton(
              "generate_advanced",
              "Generate Chart"
            )
          )
        )
      }
      
      
      if (
        chart ==
        "Multiple / Grouped Bar Chart"
      ) {
        
        if (
          length(types$categorical) < 2
        ) {
          return(
            p(
              "A grouped frequency bar chart requires at least two categorical variables."
            )
          )
        }
        
        return(
          tagList(
            
            selectInput(
              "grouped_x",
              "X Variable:",
              choices = types$categorical
            ),
            
            selectInput(
              "grouped_fill",
              "Grouping Variable:",
              choices = types$categorical
            ),
            
            actionButton(
              "generate_advanced",
              "Generate Chart"
            )
          )
        )
      }
      
      
      if (
        chart ==
        "Summary Bar Chart"
      ) {
        
        if (
          !length(types$categorical) ||
          !length(types$numeric)
        ) {
          return(
            p(
              "A Summary Bar Chart requires at least one categorical and one numeric variable."
            )
          )
        }
        
        return(
          tagList(
            
            selectInput(
              "summary_x",
              "Categorical X Variable:",
              choices = types$categorical
            ),
            
            selectInput(
              "summary_y",
              "Numeric Variable:",
              choices = types$numeric
            ),
            
            selectInput(
              "summary_function",
              "Summary Statistic:",
              choices = c(
                "Mean",
                "Median",
                "Sum",
                "Minimum",
                "Maximum"
              ),
              selected = "Mean"
            ),
            
            actionButton(
              "generate_advanced",
              "Generate Chart"
            )
          )
        )
      }
      
      
      if (chart == "Histogram") {
        
        if (!length(types$numeric)) {
          return(
            p(
              "No numeric variables are available."
            )
          )
        }
        
        return(
          tagList(
            
            selectInput(
              "hist_variable",
              "Numeric Variable:",
              choices = types$numeric
            ),
            
            numericInput(
              "hist_bins",
              "Number of bins:",
              value = 30,
              min = 5,
              max = 100,
              step = 1
            ),
            
            actionButton(
              "generate_advanced",
              "Generate Chart"
            )
          )
        )
      }
      
      
      if (chart == "Boxplot") {
        
        if (!length(types$numeric)) {
          return(
            p(
              "No numeric variables are available."
            )
          )
        }
        
        return(
          tagList(
            
            selectInput(
              "box_variable",
              "Numeric Variable:",
              choices = types$numeric
            ),
            
            selectInput(
              "box_group",
              "Optional Grouping Variable:",
              choices = c(
                "None",
                types$categorical
              )
            ),
            
            actionButton(
              "generate_advanced",
              "Generate Chart"
            )
          )
        )
      }
      
      
      if (chart == "Density Plot") {
        
        if (!length(types$numeric)) {
          return(
            p(
              "No numeric variables are available."
            )
          )
        }
        
        return(
          tagList(
            
            selectInput(
              "density_variable",
              "Numeric Variable:",
              choices = types$numeric
            ),
            
            actionButton(
              "generate_advanced",
              "Generate Chart"
            )
          )
        )
      }
      
      
      if (chart == "Scatter Plot") {
        
        if (
          length(types$numeric) < 2
        ) {
          return(
            p(
              "A scatter plot requires at least two numeric variables."
            )
          )
        }
        
        return(
          tagList(
            
            selectInput(
              "scatter_x",
              "X Variable:",
              choices = types$numeric
            ),
            
            selectInput(
              "scatter_y",
              "Y Variable:",
              choices = types$numeric
            ),
            
            actionButton(
              "generate_advanced",
              "Generate Chart"
            )
          )
        )
      }
    }
  })
  
  
  # ==========================================================
  # SECTION 12 — ADVANCED VISUALIZATION
  # ==========================================================
  
  advanced_plot_data <- eventReactive(
    
    input$generate_advanced,
    
    {
      req(
        data(),
        input$advanced_type
      )
      
      data()
    },
    
    ignoreInit = TRUE
  )
  
  
  output$advanced_plot <- renderPlot({
    
    req(
      advanced_plot_data()
    )
    
    df <- advanced_plot_data()
    types <- variable_types()
    chart <- input$advanced_type
    
    if (chart == "Bar Chart") {
      
      variable <-
        if (
          input$selection_mode ==
          "Automatic"
        ) {
          types$categorical[1]
        } else {
          input$bar_x
        }
      
      validate(
        need(
          !is.null(variable) &&
            length(variable) == 1,
          "Select a categorical variable."
        )
      )
      
      return(
        ggplot(
          df,
          aes(x = .data[[variable]])
        ) +
          geom_bar(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              "Frequency of",
              variable
            ),
            x = variable,
            y = "Frequency"
          ) +
          theme_minimal()
      )
    }
    
    
    if (
      chart ==
      "Multiple / Grouped Bar Chart"
    ) {
      
      if (
        input$selection_mode ==
        "Automatic"
      ) {
        
        validate(
          need(
            length(types$categorical) >= 2,
            "A grouped bar chart requires at least two categorical variables."
          )
        )
        
        x <- types$categorical[1]
        fill <- types$categorical[2]
        
      } else {
        
        x <- input$grouped_x
        fill <- input$grouped_fill
      }
      
      validate(
        need(
          !is.null(x) &&
            !is.null(fill) &&
            x != fill,
          "X Variable and Grouping Variable must be different."
        )
      )
      
      return(
        ggplot(
          df,
          aes(
            x = .data[[x]],
            fill = .data[[fill]]
          )
        ) +
          geom_bar(
            position = "dodge",
            na.rm = TRUE
          ) +
          labs(
            title = "Multiple / Grouped Bar Chart",
            x = x,
            fill = fill,
            y = "Frequency"
          ) +
          theme_minimal()
      )
    }
    
    
    if (
      chart ==
      "Summary Bar Chart"
    ) {
      
      if (
        input$selection_mode ==
        "Automatic"
      ) {
        
        validate(
          need(
            length(types$categorical) >= 1 &&
              length(types$numeric) >= 1,
            "A Summary Bar Chart requires at least one categorical and one numeric variable."
          )
        )
        
        x <- types$categorical[1]
        y <- types$numeric[1]
        summary_function <-
          input$automatic_summary
        
      } else {
        
        x <- input$summary_x
        y <- input$summary_y
        summary_function <-
          input$summary_function
      }
      
      summary_fun <- switch(
        summary_function,
        "Mean" = mean,
        "Median" = median,
        "Sum" = sum,
        "Minimum" = min,
        "Maximum" = max
      )
      
      return(
        ggplot(
          df,
          aes(
            x = .data[[x]],
            y = .data[[y]]
          )
        ) +
          stat_summary(
            fun = summary_fun,
            geom = "bar",
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              summary_function,
              y,
              "by",
              x
            ),
            x = x,
            y = paste(
              summary_function,
              y
            )
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Histogram") {
      
      if (
        input$selection_mode ==
        "Automatic"
      ) {
        variable <- types$numeric[1]
        bins <- 30
      } else {
        variable <- input$hist_variable
        bins <- input$hist_bins
      }
      
      return(
        ggplot(
          df,
          aes(x = .data[[variable]])
        ) +
          geom_histogram(
            bins = bins,
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              "Histogram of",
              variable
            ),
            x = variable,
            y = "Frequency"
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Boxplot") {
      
      if (
        input$selection_mode ==
        "Automatic"
      ) {
        variable <- types$numeric[1]
        group <- NULL
      } else {
        variable <- input$box_variable
        group <- input$box_group
        
        if (group == "None") {
          group <- NULL
        }
      }
      
      if (is.null(group)) {
        
        return(
          ggplot(
            df,
            aes(y = .data[[variable]])
          ) +
            geom_boxplot(
              na.rm = TRUE
            ) +
            labs(
              title = paste(
                "Boxplot of",
                variable
              ),
              y = variable
            ) +
            theme_minimal()
        )
      }
      
      return(
        ggplot(
          df,
          aes(
            x = .data[[group]],
            y = .data[[variable]]
          )
        ) +
          geom_boxplot(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              variable,
              "by",
              group
            ),
            x = group,
            y = variable
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Density Plot") {
      
      variable <-
        if (
          input$selection_mode ==
          "Automatic"
        ) {
          types$numeric[1]
        } else {
          input$density_variable
        }
      
      return(
        ggplot(
          df,
          aes(x = .data[[variable]])
        ) +
          geom_density(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              "Density Plot of",
              variable
            ),
            x = variable,
            y = "Density"
          ) +
          theme_minimal()
      )
    }
    
    
    if (chart == "Scatter Plot") {
      
      if (
        input$selection_mode ==
        "Automatic"
      ) {
        
        validate(
          need(
            length(types$numeric) >= 2,
            "A scatter plot requires at least two numeric variables."
          )
        )
        
        x <- types$numeric[1]
        y <- types$numeric[2]
        
      } else {
        
        x <- input$scatter_x
        y <- input$scatter_y
      }
      
      validate(
        need(
          x != y,
          "X Variable and Y Variable must be different."
        )
      )
      
      return(
        ggplot(
          df,
          aes(
            x = .data[[x]],
            y = .data[[y]]
          )
        ) +
          geom_point(
            na.rm = TRUE
          ) +
          labs(
            title = paste(
              y,
              "vs",
              x
            ),
            x = x,
            y = y
          ) +
          theme_minimal()
      )
    }
  })
  
  
  # ==========================================================
  # SECTION 13 — NORMALITY CHECK
  # ==========================================================
  
  
  # ----------------------------------------------------------
  # NORMALITY VARIABLE DISPLAY
  # ----------------------------------------------------------
  
  output$automatic_normality_variable <- renderUI({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    if (
      !length(numeric_vars)
    ) {
      
      return(
        div(
          class = "alert alert-warning",
          tags$b(
            "No numeric variables were detected."
          )
        )
      )
    }
    
    tagList(
      
      tags$p(
        tags$b(
          "Automatic selection rule:"
        )
      ),
      
      tags$p(
        "The first detected numeric variable will be selected."
      ),
      
      tags$p(
        tags$b(
          "Selected Variable: "
        ),
        numeric_vars[1]
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # NORMALITY MANUAL VARIABLE LIST
  # ----------------------------------------------------------
  
  observe({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    updateSelectInput(
      session,
      "normality_variable",
      choices = numeric_vars
    )
  })
  
  
  # ----------------------------------------------------------
  # NORMALITY MODEL HELPER
  # ----------------------------------------------------------
  
  perform_normality_check <- function(
    variable
  ) {
    
    df <- data()
    
    validate(
      need(
        length(variable) == 1 &&
          !is.null(variable),
        "Please select one numeric variable."
      )
    )
    
    validate(
      need(
        variable %in%
          variable_types()$numeric,
        "The selected variable must be numeric."
      )
    )
    
    x <- df[[variable]]
    
    x <- x[
      is.finite(x)
    ]
    
    validate(
      need(
        length(x) >= 3,
        "At least 3 valid observations are required for normality assessment."
      )
    )
    
    validate(
      need(
        length(unique(x)) > 1,
        "The selected variable has no variation."
      )
    )
    
    list(
      variable = variable,
      values = x
    )
  }
  
  
  # ----------------------------------------------------------
  # AUTOMATIC NORMALITY
  # ----------------------------------------------------------
  
  automatic_normality <- eventReactive(
    
    input$run_automatic_normality,
    
    {
      
      numeric_vars <-
        variable_types()$numeric
      
      validate(
        need(
          length(numeric_vars) >= 1,
          "At least one numeric variable is required."
        )
      )
      
      perform_normality_check(
        numeric_vars[1]
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # MANUAL NORMALITY
  # ----------------------------------------------------------
  
  manual_normality <- eventReactive(
    
    input$run_manual_normality,
    
    {
      
      perform_normality_check(
        input$normality_variable
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # ACTIVE NORMALITY RESULT
  # ----------------------------------------------------------
  
  active_normality <- reactive({
    
    if (
      input$normality_mode ==
      "automatic"
    ) {
      automatic_normality()
    } else {
      manual_normality()
    }
  })
  
  
  # ----------------------------------------------------------
  # NORMALITY VALIDATION
  # ----------------------------------------------------------
  
  output$normality_validation <- renderUI({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    if (
      !length(numeric_vars)
    ) {
      
      return(
        div(
          class = "alert alert-danger",
          tags$b(
            "Normality Check unavailable: "
          ),
          "The uploaded dataset contains no numeric variables."
        )
      )
    }
    
    NULL
  })
  
  
  # ----------------------------------------------------------
  # NORMALITY RESULTS
  # ----------------------------------------------------------
  
  output$normality_results <- renderTable({
    
    result <-
      active_normality()
    
    x <- result$values
    
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    
    skewness <- mean(
      ((x - mean_x) / sd_x)^3
    )
    
    kurtosis <- mean(
      ((x - mean_x) / sd_x)^4
    ) - 3
    
    if (
      n <= 5000
    ) {
      
      shapiro_result <-
        shapiro.test(x)
      
      w_value <-
        unname(
          shapiro_result$statistic
        )
      
      p_value <-
        shapiro_result$p.value
      
    } else {
      
      w_value <- NA_real_
      p_value <- NA_real_
    }
    
    data.frame(
      Statistic = c(
        "N",
        "Mean",
        "SD",
        "Skewness",
        "Excess Kurtosis",
        "Shapiro-Wilk W",
        "Shapiro-Wilk p-value"
      ),
      Value = c(
        n,
        mean_x,
        sd_x,
        skewness,
        kurtosis,
        w_value,
        p_value
      ),
      stringsAsFactors = FALSE
    )
    
  }, digits = 4)
  
  
  # ----------------------------------------------------------
  # NORMALITY INTERPRETATION
  # ----------------------------------------------------------
  
  output$normality_interpretation <- renderUI({
    
    result <-
      active_normality()
    
    x <- result$values
    n <- length(x)
    
    if (
      n > 5000
    ) {
      
      return(
        div(
          class = "alert alert-info",
          
          tags$b(
            "Shapiro-Wilk test not performed: "
          ),
          
          paste(
            "The sample contains",
            n,
            "valid observations.",
            "Graphical assessment using the Q-Q plot and histogram should be used."
          )
        )
      )
    }
    
    shapiro_result <-
      shapiro.test(x)
    
    p_value <-
      shapiro_result$p.value
    
    if (
      p_value >= 0.05
    ) {
      
      return(
        div(
          class = "alert alert-success",
          
          tags$b(
            "Shapiro-Wilk result: "
          ),
          
          paste(
            "p =",
            format.pval(
              p_value,
              digits = 4
            ),
            ". The test does not provide statistically significant evidence against normality."
          ),
          
          tags$br(),
          
          "The Q-Q plot and histogram should also be considered before making a final assessment."
        )
      )
      
    }
    
    div(
      class = "alert alert-warning",
      
      tags$b(
        "Shapiro-Wilk result: "
      ),
      
      paste(
        "p =",
        format.pval(
          p_value,
          digits = 4
        ),
        ". The test provides evidence against normality."
      ),
      
      tags$br(),
      
      "The degree of departure should be assessed using the Q-Q plot and histogram."
    )
  })
  
  
  # ----------------------------------------------------------
  # NORMALITY Q-Q PLOT
  # ----------------------------------------------------------
  
  output$normality_qq_plot <- renderPlot({
    
    result <-
      active_normality()
    
    ggplot(
      data.frame(
        value = result$values
      ),
      aes(
        sample = value
      )
    ) +
      stat_qq() +
      stat_qq_line() +
      labs(
        title = paste(
          "Normal Q-Q Plot:",
          result$variable
        ),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      theme_minimal()
  })
  
  
  # ----------------------------------------------------------
  # NORMALITY HISTOGRAM
  # ----------------------------------------------------------
  
  output$normality_histogram <- renderPlot({
    
    result <-
      active_normality()
    
    ggplot(
      data.frame(
        value = result$values
      ),
      aes(
        x = value
      )
    ) +
      geom_histogram(
        aes(
          y = after_stat(density)
        ),
        bins = 30,
        na.rm = TRUE
      ) +
      geom_density(
        na.rm = TRUE
      ) +
      labs(
        title = paste(
          "Histogram with Density:",
          result$variable
        ),
        x = result$variable,
        y = "Density"
      ) +
      theme_minimal()
  })
  
  
  # ==========================================================
  # SECTION 14 — REGRESSION ANALYSIS
  # ==========================================================
  
  
  # ----------------------------------------------------------
  # AUTOMATIC VARIABLE SELECTION DISPLAY
  # ----------------------------------------------------------
  
  output$automatic_regression_variables <- renderUI({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    if (
      length(numeric_vars) < 2
    ) {
      
      return(
        div(
          class = "alert alert-warning",
          tags$b(
            "Linear regression requires at least two numeric variables."
          )
        )
      )
    }
    
    tagList(
      
      tags$p(
        tags$b(
          "Automatic selection rule:"
        )
      ),
      
      tags$p(
        paste(
          "The first detected numeric variable is used as the dependent variable,",
          "and all remaining numeric variables are used as independent variables."
        )
      ),
      
      tags$p(
        tags$b(
          "Dependent Variable: "
        ),
        numeric_vars[1]
      ),
      
      tags$p(
        tags$b(
          "Independent Variable(s): "
        ),
        paste(
          numeric_vars[-1],
          collapse = ", "
        )
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # MANUAL VARIABLE LISTS
  # ----------------------------------------------------------
  
  observe({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    updateSelectInput(
      session,
      "linear_regression_dv",
      choices = numeric_vars
    )
    
    updateSelectInput(
      session,
      "linear_regression_iv",
      choices = numeric_vars
    )
  })
  
  
  # ----------------------------------------------------------
  # REGRESSION MODEL HELPER
  # ----------------------------------------------------------
  
  build_linear_model <- function(
    dependent_variable,
    independent_variables
  ) {
    
    df <- data()
    
    validate(
      need(
        length(dependent_variable) == 1 &&
          !is.null(dependent_variable),
        "Please select one dependent variable."
      )
    )
    
    validate(
      need(
        length(independent_variables) >= 1 &&
          !is.null(independent_variables),
        "Please select at least one independent variable."
      )
    )
    
    validate(
      need(
        !(dependent_variable %in%
            independent_variables),
        "The dependent variable cannot also be selected as an independent variable."
      )
    )
    
    model_variables <- c(
      dependent_variable,
      independent_variables
    )
    
    model_data <- df[
      complete.cases(
        df[
          ,
          model_variables,
          drop = FALSE
        ]
      ),
      model_variables,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(model_data) >
          length(independent_variables) + 1,
        paste(
          "There are not enough complete observations",
          "relative to the number of model parameters."
        )
      )
    )
    
    validate(
      need(
        length(
          unique(
            model_data[[dependent_variable]]
          )
        ) > 1,
        "The dependent variable has insufficient variation for linear regression."
      )
    )
    
    invalid_predictors <- independent_variables[
      vapply(
        independent_variables,
        function(variable) {
          length(
            unique(
              model_data[[variable]]
            )
          ) <= 1
        },
        logical(1)
      )
    ]
    
    validate(
      need(
        !length(invalid_predictors),
        paste(
          "The following independent variable(s) have insufficient variation:",
          paste(
            invalid_predictors,
            collapse = ", "
          )
        )
      )
    )
    
    regression_formula <- reformulate(
      independent_variables,
      response = dependent_variable
    )
    
    x_matrix <- model.matrix(
      regression_formula,
      data = model_data
    )
    
    if (
      qr(x_matrix)$rank <
      ncol(x_matrix)
    ) {
      
      validate(
        need(
          FALSE,
          paste(
            "Perfect multicollinearity detected among the independent variables.",
            "The regression model cannot be estimated reliably."
          )
        )
      )
    }
    
    lm(
      regression_formula,
      data = model_data
    )
  }
  
  
  # ----------------------------------------------------------
  # AUTOMATIC LINEAR REGRESSION
  # ----------------------------------------------------------
  
  automatic_linear_model <- eventReactive(
    
    input$run_automatic_regression,
    
    {
      
      numeric_vars <-
        variable_types()$numeric
      
      validate(
        need(
          length(numeric_vars) >= 2,
          "At least two numeric variables are required for linear regression."
        )
      )
      
      build_linear_model(
        dependent_variable =
          numeric_vars[1],
        independent_variables =
          numeric_vars[-1]
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # MANUAL LINEAR REGRESSION
  # ----------------------------------------------------------
  
  manual_linear_model <- eventReactive(
    
    input$run_manual_regression,
    
    {
      
      build_linear_model(
        dependent_variable =
          input$linear_regression_dv,
        independent_variables =
          input$linear_regression_iv
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # VIF CALCULATION
  # ----------------------------------------------------------
  
  calculate_vif <- function(model) {
    
    x <- model.matrix(model)
    
    x <- x[
      ,
      colnames(x) != "(Intercept)",
      drop = FALSE
    ]
    
    if (
      ncol(x) < 2
    ) {
      
      return(
        data.frame(
          Variable = colnames(x),
          VIF = NA_real_,
          Interpretation =
            "VIF requires at least two independent variables.",
          stringsAsFactors = FALSE
        )
      )
    }
    
    correlation_matrix <- cor(
      x,
      use = "complete.obs"
    )
    
    vif_values <- tryCatch(
      
      diag(
        solve(correlation_matrix)
      ),
      
      error = function(e) {
        rep(
          Inf,
          ncol(x)
        )
      }
    )
    
    interpretation <- ifelse(
      is.infinite(vif_values),
      "Perfect multicollinearity",
      ifelse(
        vif_values >= 10,
        "High multicollinearity",
        ifelse(
          vif_values >= 5,
          "Potential multicollinearity",
          "No substantial multicollinearity detected"
        )
      )
    )
    
    data.frame(
      Variable = colnames(x),
      VIF = round(
        vif_values,
        3
      ),
      Interpretation = interpretation,
      stringsAsFactors = FALSE
    )
  }
  
  
  # ----------------------------------------------------------
  # COLLINEARITY STATUS
  # ----------------------------------------------------------
  
  collinearity_status <- function(
    vif_result
  ) {
    
    if (
      all(
        is.na(
          vif_result$VIF
        )
      )
    ) {
      
      return(
        list(
          class = "alert alert-info",
          title =
            "Multicollinearity check:",
          message =
            "VIF cannot be calculated because the model contains fewer than two independent variables."
        )
      )
    }
    
    if (
      any(
        vif_result$VIF >= 10,
        na.rm = TRUE
      )
    ) {
      
      return(
        list(
          class = "alert alert-warning",
          title =
            "High multicollinearity detected.",
          message =
            paste(
              "The regression has been fitted, but coefficient estimates",
              "may be unstable and standard errors may be inflated."
            )
        )
      )
    }
    
    if (
      any(
        vif_result$VIF >= 5,
        na.rm = TRUE
      )
    ) {
      
      return(
        list(
          class = "alert alert-warning",
          title =
            "Potential multicollinearity detected.",
          message =
            paste(
              "Some VIF values are elevated.",
              "Review the predictors before interpreting individual coefficients."
            )
        )
      )
    }
    
    list(
      class = "alert alert-success",
      title =
        "No substantial multicollinearity detected.",
      message = ""
    )
  }
  
  
  # ----------------------------------------------------------
  # AUTOMATIC COLLINEARITY
  # ----------------------------------------------------------
  
  output$automatic_collinearity_check <- renderUI({
    
    model <-
      automatic_linear_model()
    
    vif_result <-
      calculate_vif(model)
    
    status <-
      collinearity_status(
        vif_result
      )
    
    tagList(
      
      div(
        class = status$class,
        
        tags$b(
          status$title
        ),
        
        if (
          nzchar(status$message)
        ) {
          paste(
            " ",
            status$message
          )
        }
      ),
      
      tableOutput(
        "automatic_vif_table"
      )
    )
  })
  
  
  output$automatic_vif_table <- renderTable({
    
    calculate_vif(
      automatic_linear_model()
    )
    
  }, digits = 3)
  
  
  # ----------------------------------------------------------
  # MANUAL COLLINEARITY
  # ----------------------------------------------------------
  
  output$manual_collinearity_check <- renderUI({
    
    model <-
      manual_linear_model()
    
    vif_result <-
      calculate_vif(model)
    
    status <-
      collinearity_status(
        vif_result
      )
    
    tagList(
      
      div(
        class = status$class,
        
        tags$b(
          status$title
        ),
        
        if (
          nzchar(status$message)
        ) {
          paste(
            " ",
            status$message
          )
        }
      ),
      
      tableOutput(
        "manual_vif_table"
      )
    )
  })
  
  
  output$manual_vif_table <- renderTable({
    
    calculate_vif(
      manual_linear_model()
    )
    
  }, digits = 3)
  
  
  # ----------------------------------------------------------
  # REGRESSION SUMMARIES
  # ----------------------------------------------------------
  
  output$automatic_regression_summary <- renderPrint({
    
    summary(
      automatic_linear_model()
    )
  })
  
  
  output$manual_regression_summary <- renderPrint({
    
    summary(
      manual_linear_model()
    )
  })
  
  
  # ----------------------------------------------------------
  # COEFFICIENT TABLE
  # ----------------------------------------------------------
  
  coefficient_table <- function(model) {
    
    result <- as.data.frame(
      summary(model)$coefficients
    )
    
    result$Variable <-
      rownames(result)
    
    rownames(result) <- NULL
    
    result[
      ,
      c(
        "Variable",
        "Estimate",
        "Std. Error",
        "t value",
        "Pr(>|t|)"
      ),
      drop = FALSE
    ]
  }
  
  
  output$automatic_regression_coefficients <-
    renderTable({
      
      coefficient_table(
        automatic_linear_model()
      )
      
    }, digits = 4)
  
  
  output$manual_regression_coefficients <-
    renderTable({
      
      coefficient_table(
        manual_linear_model()
      )
      
    }, digits = 4)
  
  
  # ----------------------------------------------------------
  # REGRESSION VALIDATION
  # ----------------------------------------------------------
  
  output$linear_regression_validation <- renderUI({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    if (
      length(numeric_vars) < 2
    ) {
      
      return(
        div(
          class = "alert alert-danger",
          
          tags$b(
            "Linear Regression unavailable: "
          ),
          
          "The uploaded dataset must contain at least two numeric variables."
        )
      )
    }
    
    NULL
  })
  
  
  # ==========================================================
  # SECTION 15 — CORRELATION ANALYSIS
  # ==========================================================
  
  
  # ----------------------------------------------------------
  # AUTOMATIC VARIABLE SELECTION DISPLAY
  # ----------------------------------------------------------
  
  output$automatic_correlation_variables <- renderUI({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    if (
      length(numeric_vars) < 2
    ) {
      
      return(
        div(
          class = "alert alert-warning",
          tags$b(
            "Correlation analysis requires at least two numeric variables."
          )
        )
      )
    }
    
    tagList(
      
      tags$p(
        tags$b(
          "Automatic selection rule:"
        )
      ),
      
      tags$p(
        "The first two detected numeric variables will be selected."
      ),
      
      tags$p(
        tags$b(
          "Variable 1: "
        ),
        numeric_vars[1]
      ),
      
      tags$p(
        tags$b(
          "Variable 2: "
        ),
        numeric_vars[2]
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # MANUAL CORRELATION VARIABLE LISTS
  # ----------------------------------------------------------
  
  observe({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    updateSelectInput(
      session,
      "correlation_x",
      choices = numeric_vars
    )
    
    updateSelectInput(
      session,
      "correlation_y",
      choices = numeric_vars
    )
  })
  
  
  # ----------------------------------------------------------
  # CORRELATION HELPER
  # ----------------------------------------------------------
  
  calculate_correlation <- function(
    variable_1,
    variable_2,
    method
  ) {
    
    df <- data()
    
    validate(
      need(
        length(variable_1) == 1 &&
          length(variable_2) == 1 &&
          !is.null(variable_1) &&
          !is.null(variable_2),
        "Please select two numeric variables."
      )
    )
    
    validate(
      need(
        variable_1 != variable_2,
        "Variable 1 and Variable 2 must be different."
      )
    )
    
    validate(
      need(
        variable_1 %in%
          variable_types()$numeric &&
          variable_2 %in%
          variable_types()$numeric,
        "Both selected variables must be numeric."
      )
    )
    
    model_data <- df[
      complete.cases(
        df[
          ,
          c(
            variable_1,
            variable_2
          ),
          drop = FALSE
        ]
      ),
      c(
        variable_1,
        variable_2
      ),
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(model_data) >= 3,
        "At least 3 complete observations are required for correlation analysis."
      )
    )
    
    validate(
      need(
        length(
          unique(
            model_data[[variable_1]]
          )
        ) > 1,
        paste(
          variable_1,
          "has insufficient variation."
        )
      )
    )
    
    validate(
      need(
        length(
          unique(
            model_data[[variable_2]]
          )
        ) > 1,
        paste(
          variable_2,
          "has insufficient variation."
        )
      )
    )
    
    correlation_test <- cor.test(
      model_data[[variable_1]],
      model_data[[variable_2]],
      method = tolower(method),
      exact =
        if (
          method == "Kendall"
        ) {
          TRUE
        } else {
          NULL
        }
    )
    
    list(
      variable_1 = variable_1,
      variable_2 = variable_2,
      method = method,
      n = nrow(model_data),
      estimate =
        unname(
          correlation_test$estimate
        ),
      statistic =
        unname(
          correlation_test$statistic
        ),
      p_value =
        correlation_test$p.value,
      conf_int =
        if (
          method == "Pearson"
        ) {
          correlation_test$conf.int
        } else {
          c(
            NA_real_,
            NA_real_
          )
        }
    )
  }
  
  
  # ----------------------------------------------------------
  # AUTOMATIC CORRELATION
  # ----------------------------------------------------------
  
  automatic_correlation <- eventReactive(
    
    input$run_automatic_correlation,
    
    {
      
      numeric_vars <-
        variable_types()$numeric
      
      validate(
        need(
          length(numeric_vars) >= 2,
          "At least two numeric variables are required for correlation analysis."
        )
      )
      
      calculate_correlation(
        variable_1 =
          numeric_vars[1],
        variable_2 =
          numeric_vars[2],
        method =
          input$correlation_method
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # MANUAL CORRELATION
  # ----------------------------------------------------------
  
  manual_correlation <- eventReactive(
    
    input$run_manual_correlation,
    
    {
      
      calculate_correlation(
        variable_1 =
          input$correlation_x,
        variable_2 =
          input$correlation_y,
        method =
          input$correlation_method
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # ACTIVE CORRELATION RESULT
  # ----------------------------------------------------------
  
  active_correlation <- reactive({
    
    if (
      input$correlation_mode ==
      "automatic"
    ) {
      automatic_correlation()
    } else {
      manual_correlation()
    }
  })
  
  
  # ----------------------------------------------------------
  # CORRELATION VALIDATION
  # ----------------------------------------------------------
  
  output$correlation_validation <- renderUI({
    
    req(data())
    
    numeric_vars <-
      variable_types()$numeric
    
    if (
      length(numeric_vars) < 2
    ) {
      
      return(
        div(
          class = "alert alert-danger",
          
          tags$b(
            "Correlation Analysis unavailable: "
          ),
          
          "The uploaded dataset must contain at least two numeric variables."
        )
      )
    }
    
    NULL
  })
  
  
  # ----------------------------------------------------------
  # CORRELATION RESULTS
  # ----------------------------------------------------------
  
  output$correlation_results <- renderTable({
    
    result <-
      active_correlation()
    
    data.frame(
      Variable_1 =
        result$variable_1,
      
      Variable_2 =
        result$variable_2,
      
      Method =
        result$method,
      
      N =
        result$n,
      
      Correlation =
        round(
          result$estimate,
          4
        ),
      
      Test_Statistic =
        round(
          result$statistic,
          4
        ),
      
      P_Value =
        result$p_value,
      
      CI_Lower =
        result$conf_int[1],
      
      CI_Upper =
        result$conf_int[2],
      
      stringsAsFactors = FALSE
    )
    
  }, digits = 4)
  
  
  # ----------------------------------------------------------
  # CORRELATION INTERPRETATION
  # ----------------------------------------------------------
  
  output$correlation_interpretation <- renderUI({
    
    result <-
      active_correlation()
    
    r <-
      result$estimate
    
    p <-
      result$p_value
    
    strength <- switch(
      TRUE,
      
      abs(r) < 0.10,
      "negligible",
      
      abs(r) < 0.30,
      "weak",
      
      abs(r) < 0.50,
      "moderate",
      
      abs(r) < 0.70,
      "strong",
      
      "very strong"
    )
    
    direction <-
      if (
        r > 0
      ) {
        "positive"
      } else {
        "negative"
      }
    
    significance_text <-
      if (
        p < 0.05
      ) {
        paste(
          "The association is statistically significant at the 0.05 level",
          "(p =",
          format.pval(
            p,
            digits = 4
          ),
          ")."
        )
      } else {
        paste(
          "The association is not statistically significant at the 0.05 level",
          "(p =",
          format.pval(
            p,
            digits = 4
          ),
          ")."
        )
      }
    
    div(
      class =
        if (
          p < 0.05
        ) {
          "alert alert-info"
        } else {
          "alert alert-secondary"
        },
      
      tags$b(
        "Interpretation: "
      ),
      
      paste(
        "There is a",
        strength,
        direction,
        "association between",
        result$variable_1,
        "and",
        result$variable_2,
        "based on the",
        result$method,
        "correlation."
      ),
      
      tags$br(),
      
      significance_text,
      
      tags$br(),
      
      "Correlation does not by itself establish causation."
    )
  })
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)