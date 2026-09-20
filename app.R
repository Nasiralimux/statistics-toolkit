# ============================================================
# STATISTICS TOOLKIT
# Generic, dataset-independent Shiny application
# ============================================================
# Sections
#   1. Libraries          4. UI
#   2. Constants & CSS    5. Server
#   3. Helper functions
# ============================================================


# ============================================================
# 1. LIBRARIES
# ============================================================

library(shiny)
library(readxl)
library(haven)
library(ggplot2)
library(glmnet)   # ridge regression


# ============================================================
# 2. CONSTANTS & STYLE
# ============================================================

ACCENT <- "#1D6F8A"
MAX_AUTO_CORR_VARS <- 12   # cap for "Automatic" correlation matrix

STAT_FUNS <- list(Mean = mean, Median = median, Sum = sum,
                  Minimum = min, Maximum = max)

# Minimum number of categorical / numeric variables each chart needs.
# NOTE: for "Grouped Bar Chart (Multiple Measures)", num = 2 is a MINIMUM
# (the user can select 2 or more numeric measures at generation time).
CHARTS <- list(
  "Bar Chart"                             = c(cat = 1, num = 0),
  "Multiple / Grouped Bar Chart"          = c(cat = 2, num = 0),
  "Stacked Bar Chart"                     = c(cat = 2, num = 0),
  "Grouped Bar Chart (Multiple Measures)" = c(cat = 1, num = 2),
  "Pie Chart"                             = c(cat = 1, num = 0),
  "Summary Bar Chart"                     = c(cat = 1, num = 1),
  "Histogram"                             = c(cat = 0, num = 1),
  "Boxplot"                               = c(cat = 0, num = 1),
  "Violin Plot"                           = c(cat = 0, num = 1),
  "Density Plot"                          = c(cat = 0, num = 1),
  "Scatter Plot"                          = c(cat = 0, num = 2),
  "Line Chart"                            = c(cat = 0, num = 2)
)

app_css <- "
:root { --ink:#1f2a44; --accent:#1d6f8a; --canvas:#f3f5f7; --line:#dde3ea; --muted:#6b7686; }
body { background:var(--canvas); color:var(--ink);
       font-family:'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif; }
.app-header { background:var(--ink); color:#fff; padding:20px 28px; margin:-15px -15px 22px; }
.app-header h1 { margin:0; font-size:26px; font-weight:600; }
.app-header p { margin:4px 0 0; color:#c5cee0; }
.well { background:#fff; border:1px solid var(--line); border-radius:6px; box-shadow:none; }
.card-box { background:#fff; border:1px solid var(--line); border-radius:6px;
            padding:18px 22px; margin-bottom:18px; }
.card-box h4.card-title { margin:0 0 14px; font-size:17px; font-weight:600; }
.tiles { display:flex; flex-wrap:wrap; gap:12px; margin-bottom:18px; }
.stat-tile { flex:1 1 150px; background:#fff; border:1px solid var(--line);
             border-left:4px solid var(--accent); border-radius:6px; padding:12px 16px; }
.stat-value { font-size:22px; font-weight:600; }
.stat-label { font-size:13px; color:var(--muted); }
.hint-box { background:#eef5f8; border-left:3px solid var(--accent); border-radius:4px;
            padding:10px 14px; margin-bottom:14px; }
.welcome { padding:34px 30px; }
.nav-tabs { border-bottom:1px solid var(--line); margin-bottom:18px; }
.nav-tabs > li > a { color:var(--muted); font-weight:600; border:none;
                     border-bottom:3px solid transparent; margin-right:4px; }
.nav-tabs > li > a:hover { background:transparent; color:var(--ink); }
.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
  color:var(--accent); background:transparent; border:none; border-bottom:3px solid var(--accent); }
.btn-primary { background:var(--accent); border-color:var(--accent); }
.btn-primary:hover, .btn-primary:focus { background:#175a71; border-color:#175a71; }
.shiny-html-output table { width:100% !important; font-size:13px; }
.shiny-html-output table th { background:#f1f5f8; }
.shiny-html-output table tbody tr:nth-child(even) { background:#f8fafc; }
.shiny-html-output table tbody tr:hover { background:#eaf3f7; }
.scroll-x { overflow-x:auto; }
.scroll-x table { white-space:nowrap; }
pre { background:#f8fafc; border:1px solid var(--line); }
"


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

# ---- UI building blocks -----------------------------------

panel_card <- function(title, ...) div(class = "card-box", h4(class = "card-title", title), ...)

stat_tile <- function(label, value) {
  div(class = "stat-tile", div(class = "stat-value", value), div(class = "stat-label", label))
}

alert <- function(type, title, msg = "") {
  div(class = paste0("alert alert-", type), tags$b(title), " ", msg)
}

run_button <- function(id, label) actionButton(id, label, icon = icon("play"), class = "btn-primary")

# Automatic / Manual switch; `manual` is the UI shown in manual mode,
# `<id>_auto` is a uiOutput describing the automatic rule.
var_panel <- function(id, manual) {
  mode <- paste0(id, "_mode")
  tagList(
    radioButtons(mode, "Variable selection:",
                 c("Automatic" = "automatic", "Manual" = "manual"), inline = TRUE),
    conditionalPanel(sprintf("input.%s == 'automatic'", mode),
                     div(class = "hint-box", uiOutput(paste0(id, "_auto")))),
    conditionalPanel(sprintf("input.%s == 'manual'", mode), manual)
  )
}

# ---- Small statistical helpers ----------------------------

stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

strength <- function(r) {
  as.character(cut(abs(r), c(0, .1, .3, .5, .7, Inf),
                   c("negligible", "weak", "moderate", "strong", "very strong"),
                   right = FALSE, include.lowest = TRUE))
}

theme_app <- theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))

# ---- One plotting function for every chart ----------------
# `measures` is only used by "Grouped Bar Chart (Multiple Measures)": a
# character vector of 2+ numeric column names to compare side by side
# within each category of `x`.

plot_chart <- function(type, df, x, y = NULL, group = NULL, stat = "Mean", bins = 30,
                       measures = NULL) {
  if (is.null(bins) || is.na(bins)) bins <- 30

  p <- switch(type,
              "Histogram" = ggplot(df, aes(.data[[x]])) +
                geom_histogram(bins = bins, fill = ACCENT, colour = "white", na.rm = TRUE) +
                labs(title = paste("Histogram of", x), x = x, y = "Frequency"),
              
              "Density Plot" = ggplot(df, aes(.data[[x]])) +
                geom_density(fill = ACCENT, colour = ACCENT, alpha = 0.3, na.rm = TRUE) +
                labs(title = paste("Density plot of", x), x = x, y = "Density"),
              
              "Boxplot" = if (is.null(group)) {
                ggplot(df, aes(y = .data[[x]])) +
                  geom_boxplot(fill = ACCENT, alpha = 0.5, na.rm = TRUE) +
                  labs(title = paste("Boxplot of", x), y = x)
              } else {
                ggplot(df, aes(.data[[group]], .data[[x]])) +
                  geom_boxplot(fill = ACCENT, alpha = 0.5, na.rm = TRUE) +
                  labs(title = paste(x, "by", group), x = group, y = x)
              },
              
              "Violin Plot" = if (is.null(group)) {
                ggplot(df, aes(y = .data[[x]])) +
                  geom_violin(fill = ACCENT, alpha = 0.5, na.rm = TRUE) +
                  labs(title = paste("Violin plot of", x), y = x)
              } else {
                ggplot(df, aes(.data[[group]], .data[[x]])) +
                  geom_violin(fill = ACCENT, alpha = 0.5, na.rm = TRUE) +
                  labs(title = paste(x, "by", group), x = group, y = x)
              },
              
              "Bar Chart" = ggplot(df, aes(.data[[x]])) +
                geom_bar(fill = ACCENT, na.rm = TRUE) +
                labs(title = paste("Frequency of", x), x = x, y = "Frequency"),
              
              "Pie Chart" = {
                cnt <- as.data.frame(table(df[[x]]))
                names(cnt) <- c("category", "count")
                ggplot(cnt, aes(x = "", y = count, fill = category)) +
                  geom_col(width = 1, colour = "white", na.rm = TRUE) +
                  coord_polar(theta = "y") +
                  scale_fill_viridis_d(end = 0.85) +
                  labs(title = paste("Distribution of", x), fill = x, x = NULL, y = NULL)
              },
              
              "Multiple / Grouped Bar Chart" = ggplot(df, aes(.data[[x]], fill = .data[[group]])) +
                geom_bar(position = "dodge", na.rm = TRUE) +
                scale_fill_viridis_d(end = 0.85) +
                labs(title = paste(x, "by", group), x = x, y = "Frequency", fill = group),
              
              "Stacked Bar Chart" = ggplot(df, aes(.data[[x]], fill = .data[[group]])) +
                geom_bar(position = "stack", na.rm = TRUE) +
                scale_fill_viridis_d(end = 0.85) +
                labs(title = paste(x, "by", group, "(stacked)"), x = x, y = "Frequency", fill = group),
              
              "Grouped Bar Chart (Multiple Measures)" = {
                long_df <- do.call(rbind, lapply(measures, function(m) {
                  data.frame(.cat = df[[x]], .measure = m,
                             .value = suppressWarnings(as.numeric(df[[m]])))
                }))
                ggplot(long_df, aes(.cat, .value, fill = .measure)) +
                  stat_summary(fun = STAT_FUNS[[stat]], geom = "bar",
                               position = position_dodge(), na.rm = TRUE) +
                  scale_fill_viridis_d(end = 0.85) +
                  labs(title = paste(stat, "of selected measures by", x),
                       x = x, y = stat, fill = "Measure")
              },
              
              "Summary Bar Chart" = ggplot(df, aes(.data[[x]], .data[[y]])) +
                stat_summary(fun = STAT_FUNS[[stat]], geom = "bar", fill = ACCENT, na.rm = TRUE) +
                labs(title = paste(stat, y, "by", x), x = x, y = paste(stat, y)),
              
              "Scatter Plot" = ggplot(df, aes(.data[[x]], .data[[y]])) +
                geom_point(colour = ACCENT, alpha = 0.7, na.rm = TRUE) +
                labs(title = paste(y, "vs", x), x = x, y = y),
              
              "Line Chart" = {
                d <- df[order(df[[x]]), ]
                ggplot(d, aes(.data[[x]], .data[[y]])) +
                  geom_line(colour = ACCENT, linewidth = 1, na.rm = TRUE) +
                  geom_point(colour = ACCENT, alpha = 0.7, na.rm = TRUE) +
                  labs(title = paste(y, "over", x), x = x, y = y)
              },
              
              stop("Unknown chart type: ", type)
  )

  p <- p + theme_app
  if (type == "Pie Chart") {
    p <- p + theme(axis.text = element_blank(), axis.ticks = element_blank(),
                   panel.grid = element_blank())
  }
  p
}

# ---- Regression helpers -----------------------------------

fit_lm <- function(df, dv, ivs) {
  validate(
    need(length(dv) == 1 && length(ivs) >= 1,
         "Select one dependent variable and at least one independent variable."),
    need(!(dv %in% ivs), "The dependent variable cannot also be an independent variable.")
  )
  vars <- c(dv, ivs)
  md <- df[complete.cases(df[vars]), vars, drop = FALSE]
  
  validate(need(nrow(md) > length(ivs) + 1,
                "Not enough complete observations relative to the number of model parameters."))
  
  flat <- vars[vapply(md, function(x) length(unique(x)) <= 1, logical(1))]
  validate(need(!length(flat),
                paste("Insufficient variation in:", paste(flat, collapse = ", "))))
  
  f <- reformulate(ivs, response = dv)
  validate(need(qr(model.matrix(f, md))$rank == length(ivs) + 1,
                paste("Perfect multicollinearity detected among the independent variables.",
                      "The model cannot be estimated reliably.")))
  lm(f, data = md)
}

fit_logit <- function(df, dv, ivs) {
  validate(
    need(length(dv) == 1 && length(ivs) >= 1,
         "Select one binary dependent variable and at least one independent variable."),
    need(!(dv %in% ivs), "The dependent variable cannot also be an independent variable.")
  )
  vars <- c(dv, ivs)
  md <- df[complete.cases(df[vars]), vars, drop = FALSE]
  
  validate(need(nrow(md) > length(ivs) + 1,
                "Not enough complete observations relative to the number of model parameters."))
  
  lvl <- sort(unique(as.character(md[[dv]])))
  validate(need(length(lvl) == 2,
                paste("The dependent variable must have exactly two categories. Found:",
                      paste(lvl, collapse = ", "))))
  md[[dv]] <- factor(as.character(md[[dv]]), levels = lvl)
  
  flat <- ivs[vapply(md[ivs], function(x) length(unique(x)) <= 1, logical(1))]
  validate(need(!length(flat),
                paste("Insufficient variation in:", paste(flat, collapse = ", "))))
  
  f <- reformulate(ivs, response = dv)
  m <- glm(f, data = md, family = binomial())
  list(model = m, positive_level = lvl[2], reference_level = lvl[1], data = md, dv = dv)
}

calc_vif <- function(model) {
  x <- model.matrix(model)
  x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  
  if (ncol(x) < 2) {
    return(data.frame(Variable = colnames(x), VIF = NA_real_,
                      Interpretation = "VIF requires at least two independent variables."))
  }
  v <- tryCatch(diag(solve(cor(x))), error = function(e) rep(Inf, ncol(x)))
  data.frame(
    Variable = colnames(x), VIF = round(v, 3),
    Interpretation = ifelse(is.infinite(v), "Perfect multicollinearity",
                            ifelse(v >= 10, "High multicollinearity",
                                   ifelse(v >= 5, "Potential multicollinearity",
                                          "No substantial multicollinearity detected")))
  )
}

vif_alert <- function(v) {
  if (all(is.na(v$VIF))) {
    return(alert("info", "Multicollinearity check:", "VIF requires at least two independent variables."))
  }
  m <- max(v$VIF, na.rm = TRUE)
  if (m >= 10) {
    alert("warning", "High multicollinearity detected.",
          "The model was fitted, but coefficient estimates may be unstable and standard errors inflated.")
  } else if (m >= 5) {
    alert("warning", "Potential multicollinearity detected.",
          "Some VIF values are elevated. Review the predictors before interpreting individual coefficients.")
  } else {
    alert("success", "No substantial multicollinearity detected.")
  }
}

# ---- Correlation helpers ----------------------------------

# One cor.test per pair of variables (pairwise-complete observations)
cor_pairs <- function(df, vars, method) {
  rows <- lapply(combn(vars, 2, simplify = FALSE), function(v) {
    d <- df[complete.cases(df[v]), v]
    ct <- if (nrow(d) >= 3) {
      tryCatch(suppressWarnings(cor.test(d[[1]], d[[2]], method = tolower(method))),
               error = function(e) NULL)
    }
    ok <- !is.null(ct)
    ci <- if (ok && method == "Pearson") ct$conf.int else c(NA_real_, NA_real_)
    data.frame(
      Variable_1 = v[1], Variable_2 = v[2], Method = method, N = nrow(d),
      Correlation    = if (ok) unname(ct$estimate) else NA_real_,
      Test_Statistic = if (ok) unname(ct$statistic) else NA_real_,
      P_Value        = if (ok) ct$p.value else NA_real_,
      CI_Lower = ci[1], CI_Upper = ci[2],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Turn the pairwise table into a symmetric matrix of one column
pair_matrix <- function(pairs, vars, col, diag_value = NA_real_) {
  m <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  for (i in seq_len(nrow(pairs))) {
    a <- pairs$Variable_1[i]
    b <- pairs$Variable_2[i]
    m[a, b] <- m[b, a] <- pairs[[col]][i]
  }
  diag(m) <- diag_value
  m
}


# ============================================================
# 4. USER INTERFACE
# ============================================================

welcome_panel <- div(
  class = "card-box welcome",
  h3("Welcome to Statistics Toolkit"),
  p("Upload a CSV, Excel or SPSS file from the panel on the left to begin."),
  div(class = "tiles",
      stat_tile("Overview", "Preview data and variable types"),
      stat_tile("Descriptives", "Summaries for every variable"),
      stat_tile("Visualizations", "Recommended and custom charts"),
      stat_tile("Statistical tools", "Normality, regression, correlation"))
)

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  
  div(class = "app-header",
      h1("Statistics Toolkit"),
      p("Upload a dataset to explore, visualize and analyze it. No coding required.")),
  
  sidebarLayout(
    
    # ---------- Sidebar: data upload ----------
    sidebarPanel(
      width = 3,
      h4("Dataset"),
      fileInput("file", "Choose a file", accept = c(".csv", ".xlsx", ".xls", ".sav"),
                buttonLabel = "Browse...", placeholder = "No file selected"),
      helpText("Supported formats: .csv, .xlsx, .xls, .sav"),
      conditionalPanel("output.loaded", uiOutput("file_info"))
    ),
    
    # ---------- Main area: appears only after upload ----------
    mainPanel(
      width = 9,
      conditionalPanel("!output.loaded", welcome_panel),
      
      conditionalPanel("output.loaded", tabsetPanel(
        id = "main_tabs", type = "tabs",
        
        # ===== Overview =====
        tabPanel(
          tagList(icon("table"), "Data Overview"), br(),
          uiOutput("summary_tiles"),
          panel_card("Dataset Preview (first 10 rows)", div(class = "scroll-x", tableOutput("data_preview"))),
          panel_card("Variable Overview", div(class = "scroll-x", tableOutput("var_overview")))
        ),
        
        # ===== Descriptive statistics =====
        tabPanel(
          tagList(icon("calculator"), "Descriptive Statistics"), br(),
          panel_card("Numeric Variables", div(class = "scroll-x", tableOutput("numeric_stats"))),
          panel_card("Categorical Variables (top 5 categories)", tableOutput("categorical_stats"))
        ),
        
        # ===== Visualizations =====
        tabPanel(
          tagList(icon("chart-bar"), "Visualizations"), br(),
          panel_card("Recommended Visualizations",
                     uiOutput("rec_ui"),
                     plotOutput("rec_plot", height = "480px")),
          panel_card("Advanced Visualizations",
                     fluidRow(
                       column(6, selectInput("adv_type", "Visualization type:", names(CHARTS))),
                       column(6, radioButtons("adv_mode", "Variable selection:",
                                              c("Automatic", "Manual"), inline = TRUE))),
                     uiOutput("adv_vars"),
                     actionButton("adv_generate", "Generate Chart", icon = icon("chart-bar"),
                                  class = "btn-primary")),
          conditionalPanel("input.adv_generate > 0",
                           panel_card("Generated Chart", plotOutput("adv_plot", height = "550px")))
        ),
        
        # ===== Statistical tools =====
        tabPanel(
          tagList(icon("flask"), "Statistical Tools"), br(),
          tabsetPanel(
            id = "tools_tab", type = "tabs",
            
            # ---- Normality ----
            tabPanel(
              "Normality Check", br(),
              panel_card("Normality Check Setup",
                         var_panel("norm", selectInput("norm_var", "Numeric variable:", choices = NULL)),
                         run_button("norm_run", "Check Normality"),
                         uiOutput("norm_msg")),
              uiOutput("norm_results_ui")
            ),
            
            # ---- Regression ----
            tabPanel(
              "Regression Analysis", br(),
              panel_card("Linear Regression Setup",
                         selectInput("reg_method", "Related tool:", choices = "Linear Regression"),
                         var_panel("reg", tagList(
                           selectInput("reg_dv", "Dependent variable:", choices = NULL),
                           selectizeInput("reg_iv", "Independent variable(s):", choices = NULL,
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button"))))),
                         run_button("reg_run", "Run Linear Regression"),
                         uiOutput("reg_msg")),
              uiOutput("reg_results_ui")
            ),
            
            # ---- Correlation ----
            tabPanel(
              "Correlation Analysis", br(),
              panel_card("Correlation Setup",
                         selectInput("cor_method", "Correlation method:",
                                     c("Pearson", "Spearman", "Kendall")),
                         var_panel("cor", tagList(
                           selectizeInput("cor_vars", "Variables (select two or more):", choices = NULL,
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button"))),
                           actionLink("cor_all", "Select all numeric variables"), br(), br())),
                         run_button("cor_run", "Run Correlation Analysis"),
                         uiOutput("cor_msg")),
              uiOutput("cor_results_ui")
            ),
            
            # ---- Logistic Regression ----
            tabPanel(
              "Logistic Regression", br(),
              panel_card("Logistic Regression Setup",
                         var_panel("logit", tagList(
                           selectInput("logit_dv", "Dependent variable (2 categories):", choices = NULL),
                           selectizeInput("logit_iv", "Independent variable(s):", choices = NULL,
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button"))))),
                         run_button("logit_run", "Run Logistic Regression"),
                         uiOutput("logit_msg")),
              uiOutput("logit_results_ui")
            ),
            
            # ---- Testing (t-tests) ----
            tabPanel(
              "Testing", br(),
              panel_card("Test Setup",
                         selectInput("test_type", "Test type:",
                                     c("One-Sample t-test", "Independent Samples t-test", "Paired t-test")),
                         uiOutput("test_vars_ui"),
                         run_button("test_run", "Run Test"),
                         uiOutput("test_msg")),
              uiOutput("test_results_ui")
            ),
            
            # ---- ANOVA ----
            tabPanel(
              "ANOVA", br(),
              panel_card("One-Way ANOVA Setup",
                         var_panel("anova", tagList(
                           selectInput("anova_dv", "Numeric (dependent) variable:", choices = NULL),
                           selectInput("anova_factor", "Categorical factor:", choices = NULL))),
                         run_button("anova_run", "Run ANOVA"),
                         uiOutput("anova_msg")),
              uiOutput("anova_results_ui")
            )
          )
        )
      ))
    )
  )
)


# ============================================================
# 5. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # DATA
  # ----------------------------------------------------------
  
  data <- reactive({
    req(input$file)
    path <- input$file$datapath
    tryCatch({
      df <- switch(
        tolower(tools::file_ext(input$file$name)),
        csv  = read.csv(path, stringsAsFactors = FALSE),
        xlsx = , xls = as.data.frame(read_excel(path)),
        sav  = as.data.frame(read_sav(path)),
        stop("Unsupported file format.")
      )
      names(df) <- make.names(names(df), unique = TRUE)
      df
    }, error = function(e) {
      showNotification(paste("Could not read the file:", conditionMessage(e)), type = "error")
      NULL
    })
  })
  
  # Drives which panels are visible (welcome screen vs. analysis tabs)
  output$loaded <- reactive(!is.null(input$file) && !is.null(data()))
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  # Automatic variable-type detection
  types <- reactive({
    df <- req(data())
    nm <- names(df)
    flag <- function(f) nm[vapply(df, f, logical(1))]
    numeric     <- flag(is.numeric)
    categorical <- flag(function(x) is.character(x) || is.factor(x) || is.logical(x))
    date        <- flag(function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")))
    binary      <- categorical[vapply(df[categorical], function(x)
                    length(unique(x[!is.na(x)])) == 2, logical(1))]
    list(numeric = numeric, categorical = categorical, date = date, binary = binary,
         other = setdiff(nm, c(numeric, categorical, date)))
  })
  
  output$file_info <- renderUI({
    df <- req(data())
    div(class = "hint-box", tags$b(input$file$name), tags$br(),
        format(nrow(df), big.mark = ","), " rows \u00d7 ", ncol(df), " columns")
  })
  
  
  # ----------------------------------------------------------
  # DATA OVERVIEW
  # ----------------------------------------------------------
  
  output$summary_tiles <- renderUI({
    df <- req(data())
    t <- types()
    miss <- sum(is.na(df))
    div(class = "tiles",
        stat_tile("Rows", format(nrow(df), big.mark = ",")),
        stat_tile("Columns", ncol(df)),
        stat_tile("Numeric variables", length(t$numeric)),
        stat_tile("Categorical variables", length(t$categorical)),
        stat_tile("Missing cells", sprintf("%s (%.1f%%)", format(miss, big.mark = ","),
                                           100 * miss / max(1, prod(dim(df))))))
  })
  
  output$data_preview <- renderTable({
    d <- head(req(data()), 10)
    d[] <- lapply(d, as.character)
    d
  })
  
  output$var_overview <- renderTable({
    df <- req(data())
    t <- types()
    data.frame(
      Variable = names(df),
      Type = ifelse(names(df) %in% t$numeric, "Numeric",
                    ifelse(names(df) %in% t$categorical, "Categorical",
                           ifelse(names(df) %in% t$date, "Date", "Other"))),
      Missing = as.integer(vapply(df, function(x) sum(is.na(x)), numeric(1))),
      Unique = as.integer(vapply(df, function(x) length(unique(x)), numeric(1))),
      row.names = NULL
    )
  })
  
  
  # ----------------------------------------------------------
  # DESCRIPTIVE STATISTICS
  # ----------------------------------------------------------
  
  output$numeric_stats <- renderTable({
    df <- req(data())
    vars <- types()$numeric
    if (!length(vars)) return(data.frame(Message = "No numeric variables detected."))
    
    res <- vapply(df[vars], function(x) {
      x <- as.numeric(x[!is.na(x)])
      if (!length(x)) return(c(0, rep(NA_real_, 8)))
      c(length(x), mean(x), median(x), sd(x), var(x), min(x),
        quantile(x, .25, names = FALSE), quantile(x, .75, names = FALSE), max(x))
    }, numeric(9))
    
    out <- data.frame(Variable = vars, t(round(res, 3)), row.names = NULL)
    names(out)[-1] <- c("N", "Mean", "Median", "SD", "Variance", "Min", "Q1", "Q3", "Max")
    out$N <- as.integer(out$N)
    out
  }, digits = 3)
  
  output$categorical_stats <- renderTable({
    df <- req(data())
    vars <- types()$categorical
    if (!length(vars)) return(data.frame(Message = "No categorical variables detected."))
    
    do.call(rbind, lapply(vars, function(v) {
      x <- df[[v]][!is.na(df[[v]])]
      if (!length(x)) {
        return(data.frame(Variable = v, Category = NA, Frequency = NA, Percentage = NA))
      }
      f <- sort(table(x), decreasing = TRUE)
      head(data.frame(Variable = v, Category = names(f), Frequency = as.integer(f),
                      Percentage = round(100 * as.numeric(f) / sum(f), 2)), 5)
    }))
  })
  
  
  # ----------------------------------------------------------
  # RECOMMENDED VISUALIZATIONS
  # ----------------------------------------------------------
  
  output$rec_ui <- renderUI({
    t <- types()
    n <- length(t$numeric)
    k <- length(t$categorical)
    ch <- c(if (n >= 1) c("Histogram", "Boxplot", "Density Plot"),
            if (k >= 1) "Bar Chart",
            if (n >= 2) "Scatter Plot",
            if (n >= 1 && k >= 1) "Numeric Variable by Category")
    if (is.null(ch)) {
      return(alert("info", "No suitable visualizations were detected for this dataset."))
    }
    selectInput("rec_type", "Choose a chart:", choices = ch)
  })
  
  output$rec_plot <- renderPlot({
    df <- req(data())
    type <- req(input$rec_type)
    n <- types()$numeric
    k <- types()$categorical
    by_cat <- type == "Numeric Variable by Category"
    
    plot_chart(if (by_cat) "Boxplot" else type, df,
               x = if (type == "Bar Chart") k[1] else n[1],
               y = n[2],
               group = if (by_cat) k[1])
  })
  
  
  # ----------------------------------------------------------
  # ADVANCED VISUALIZATIONS
  # ----------------------------------------------------------
  
  output$adv_vars <- renderUI({
    req(data(), input$adv_type, input$adv_mode)
    n <- types()$numeric
    k <- types()$categorical
    type <- input$adv_type
    need_v <- CHARTS[[type]]
    
    if (length(k) < need_v["cat"] || length(n) < need_v["num"]) {
      return(alert("warning", "Not available:",
                   sprintf("%s needs at least %d categorical and %d numeric variable(s).",
                           type, need_v["cat"], need_v["num"])))
    }
    
    stat <- if (type %in% c("Summary Bar Chart", "Grouped Bar Chart (Multiple Measures)")) {
      selectInput("adv_stat", "Summary statistic:", names(STAT_FUNS))
    }
    
    if (input$adv_mode == "Automatic") {
      return(tagList(div(class = "hint-box",
                         "Suitable variables are chosen automatically from the detected variable types."),
                     stat))
    }
    
    sel <- function(id, label, ch, ...) column(6, selectInput(id, label, ch, ...))
    tagList(
      fluidRow(switch(
        type,
        "Bar Chart" = sel("adv_x", "Categorical variable:", k),
        "Pie Chart" = sel("adv_x", "Categorical variable:", k),
        "Multiple / Grouped Bar Chart" = tagList(
          sel("adv_x", "X variable:", k),
          sel("adv_group", "Grouping variable:", k, selected = k[2])),
        "Stacked Bar Chart" = tagList(
          sel("adv_x", "X variable:", k),
          sel("adv_group", "Stacking variable:", k, selected = k[2])),
        "Grouped Bar Chart (Multiple Measures)" = tagList(
          sel("adv_x", "Categorical variable:", k),
          column(6, selectizeInput("adv_measures", "Numeric measures (2 or more):", n,
                                   multiple = TRUE,
                                   options = list(plugins = list("remove_button"))))),
        "Summary Bar Chart" = tagList(
          sel("adv_x", "Categorical variable:", k),
          sel("adv_y", "Numeric variable:", n)),
        "Histogram" = tagList(
          sel("adv_x", "Numeric variable:", n),
          column(6, numericInput("adv_bins", "Number of bins:", 30, 5, 100))),
        "Boxplot" = tagList(
          sel("adv_x", "Numeric variable:", n),
          sel("adv_group", "Optional grouping variable:", c("None", k))),
        "Violin Plot" = tagList(
          sel("adv_x", "Numeric variable:", n),
          sel("adv_group", "Optional grouping variable:", c("None", k))),
        "Density Plot" = sel("adv_x", "Numeric variable:", n),
        "Scatter Plot" = tagList(
          sel("adv_x", "X variable:", n),
          sel("adv_y", "Y variable:", n, selected = n[2])),
        "Line Chart" = tagList(
          sel("adv_x", "X variable:", n),
          sel("adv_y", "Y variable:", n, selected = n[2]))
      )),
      stat
    )
  })
  
  # Captures the chart specification at the moment "Generate Chart" is clicked
  adv_spec <- eventReactive(input$adv_generate, {
    n <- types()$numeric
    k <- types()$categorical
    type <- input$adv_type
    auto <- input$adv_mode == "Automatic"
    need_v <- CHARTS[[type]]
    
    validate(need(length(k) >= need_v["cat"] && length(n) >= need_v["num"],
                  "The dataset does not have the variables this chart requires."))
    
    pick <- function(auto_value, id) if (auto) auto_value else input[[id]]
    x_is_cat <- type %in% c("Bar Chart", "Pie Chart", "Multiple / Grouped Bar Chart",
                            "Stacked Bar Chart", "Grouped Bar Chart (Multiple Measures)",
                            "Summary Bar Chart")
    
    x <- pick(if (x_is_cat) k[1] else n[1], "adv_x")
    
    group <- if (type %in% c("Multiple / Grouped Bar Chart", "Stacked Bar Chart")) {
      pick(k[2], "adv_group")
    } else if (type %in% c("Boxplot", "Violin Plot") && !auto && !identical(input$adv_group, "None")) {
      input$adv_group
    }
    
    y <- if (type %in% c("Scatter Plot", "Line Chart")) {
      pick(n[2], "adv_y")
    } else if (type == "Summary Bar Chart") {
      pick(n[1], "adv_y")
    }
    
    measures <- if (type == "Grouped Bar Chart (Multiple Measures)") {
      if (auto) head(n, 3) else input$adv_measures
    }
    
    s <- list(type = type, x = x, y = y, group = group, measures = measures,
              stat = if (is.null(input$adv_stat)) "Mean" else input$adv_stat,
              bins = if (auto) 30 else input$adv_bins)
    
    validate(
      need(isTruthy(s$x), "Select the required variables."),
      need(is.null(s$group) || !identical(s$x, s$group),
           "X variable and grouping variable must be different."),
      need(type != "Scatter Plot" || !identical(s$x, s$y),
           "X variable and Y variable must be different."),
      need(type != "Line Chart" || !identical(s$x, s$y),
           "X variable and Y variable must be different."),
      need(type != "Grouped Bar Chart (Multiple Measures)" || length(s$measures) >= 2,
           "Select at least two numeric measures.")
    )
    s
  })
  
  output$adv_plot <- renderPlot({
    df <- req(data())
    s <- adv_spec()
    validate(need(all(c(s$x, s$y, s$group, s$measures) %in% names(df)),
                  "The dataset has changed. Generate the chart again."))
    do.call(plot_chart, c(list(df = df), s))
  })
  
  
  # ----------------------------------------------------------
  # STATISTICAL TOOLS: shared setup
  # ----------------------------------------------------------
  
  # Keep the manual variable lists in sync with the dataset
  observe({
    num <- types()$numeric
    updateSelectInput(session, "norm_var", choices = num)
    updateSelectInput(session, "reg_dv", choices = num)
    updateSelectInput(session, "cor_vars", choices = num)
    updateSelectInput(session, "anova_dv", choices = num)
  })
  
  observe({
    updateSelectInput(session, "logit_dv", choices = types()$binary)
    updateSelectizeInput(session, "logit_iv", choices = types()$numeric,
                         selected = intersect(isolate(input$logit_iv), types()$numeric))
  })
  
  observe({
    updateSelectInput(session, "anova_factor", choices = types()$categorical)
  })
  
  # Regression predictors exclude the chosen dependent variable
  observe({
    ivs <- setdiff(types()$numeric, input$reg_dv)
    updateSelectInput(session, "reg_iv", choices = ivs,
                      selected = intersect(isolate(input$reg_iv), ivs))
  })
  
  observeEvent(input$cor_all, {
    updateSelectInput(session, "cor_vars", selected = types()$numeric)
  })
  
  # Warning shown when the dataset cannot support a tool
  min_vars_msg <- function(k, tool) {
    if (length(types()$numeric) < k) {
      alert("danger", paste(tool, "unavailable:"),
            paste("the dataset must contain at least", k, "numeric variable(s)."))
    }
  }
  output$norm_msg <- renderUI(min_vars_msg(1, "Normality Check"))
  output$reg_msg  <- renderUI(min_vars_msg(2, "Linear Regression"))
  output$cor_msg  <- renderUI(min_vars_msg(2, "Correlation Analysis"))
  
  output$logit_msg <- renderUI({
    if (length(types()$binary) < 1 || length(types()$numeric) < 1) {
      alert("danger", "Logistic Regression unavailable:",
            "the dataset must contain at least one two-category variable and one numeric variable.")
    }
  })
  
  output$test_msg <- renderUI({
    if (length(types()$numeric) < 1) {
      alert("danger", "Testing unavailable:", "the dataset must contain at least one numeric variable.")
    } else if (input$test_type == "Independent Samples t-test" && length(types()$binary) < 1) {
      alert("danger", "Independent Samples t-test unavailable:",
            "the dataset must contain a two-category grouping variable.")
    } else if (input$test_type == "Paired t-test" && length(types()$numeric) < 2) {
      alert("danger", "Paired t-test unavailable:",
            "the dataset must contain at least two numeric variables.")
    }
  })
  
  output$anova_msg <- renderUI({
    if (length(types()$numeric) < 1 || length(types()$categorical) < 1) {
      alert("danger", "ANOVA unavailable:",
            "the dataset must contain at least one numeric variable and one categorical variable.")
    }
  })
  
  # Description of the automatic selection rule for each tool
  output$norm_auto <- renderUI({
    v <- types()$numeric
    req(length(v) >= 1)
    tagList(tags$b("Automatic rule: "), "the first numeric variable is tested.", tags$br(),
            tags$b("Selected variable: "), v[1])
  })
  
  output$reg_auto <- renderUI({
    v <- types()$numeric
    req(length(v) >= 2)
    tagList(tags$b("Automatic rule: "),
            "the first numeric variable is the dependent variable; all other numeric variables are predictors.",
            tags$br(), tags$b("Dependent variable: "), v[1],
            tags$br(), tags$b("Independent variable(s): "), paste(v[-1], collapse = ", "))
  })
  
  output$cor_auto <- renderUI({
    v <- head(types()$numeric, MAX_AUTO_CORR_VARS)
    req(length(v) >= 2)
    tagList(tags$b("Automatic rule: "),
            sprintf("all numeric variables are correlated (first %d at most).", MAX_AUTO_CORR_VARS),
            tags$br(), tags$b("Selected variables: "), paste(v, collapse = ", "))
  })
  
  output$logit_auto <- renderUI({
    bin <- types()$binary
    num <- types()$numeric
    req(length(bin) >= 1, length(num) >= 1)
    tagList(tags$b("Automatic rule: "),
            "the first two-category variable is the outcome; all numeric variables are predictors.",
            tags$br(), tags$b("Dependent variable: "), bin[1],
            tags$br(), tags$b("Independent variable(s): "), paste(num, collapse = ", "))
  })
  
  output$anova_auto <- renderUI({
    num <- types()$numeric
    cat <- types()$categorical
    req(length(num) >= 1, length(cat) >= 1)
    tagList(tags$b("Automatic rule: "),
            "the first numeric variable is compared across the levels of the first categorical variable.",
            tags$br(), tags$b("Dependent variable: "), num[1],
            tags$br(), tags$b("Factor: "), cat[1])
  })
  
  
  # ----------------------------------------------------------
  # NORMALITY CHECK
  # ----------------------------------------------------------
  
  norm_result <- eventReactive(input$norm_run, {
    num <- types()$numeric
    validate(need(length(num) >= 1, "At least one numeric variable is required."))
    
    v <- if (input$norm_mode == "automatic") num[1] else input$norm_var
    validate(need(length(v) == 1 && v %in% num, "Please select one numeric variable."))
    
    x <- data()[[v]]
    x <- as.numeric(x[is.finite(x)])
    validate(
      need(length(x) >= 3, "At least 3 valid observations are required for normality assessment."),
      need(length(unique(x)) > 1, "The selected variable has no variation.")
    )
    
    z <- (x - mean(x)) / sd(x)
    sw <- if (length(x) <= 5000) shapiro.test(x) else list(statistic = NA_real_, p.value = NA_real_)
    list(variable = v, x = x, n = length(x),
         skew = mean(z^3), kurt = mean(z^4) - 3,
         w = unname(sw$statistic), p = sw$p.value)
  })
  
  output$norm_results_ui <- renderUI({
    req(norm_result())
    tagList(
      panel_card("Normality Results", tableOutput("norm_table"),
                 h5("Interpretation"), uiOutput("norm_interp")),
      panel_card("Diagnostic Plots", fluidRow(
        column(6, plotOutput("norm_qq", height = "400px")),
        column(6, plotOutput("norm_hist", height = "400px"))))
    )
  })
  
  output$norm_table <- renderTable({
    r <- norm_result()
    data.frame(
      Statistic = c("N", "Mean", "SD", "Skewness", "Excess Kurtosis",
                    "Shapiro-Wilk W", "Shapiro-Wilk p-value"),
      Value = c(as.character(r$n),
                sprintf("%.4f", c(mean(r$x), sd(r$x), r$skew, r$kurt, r$w)),
                format.pval(r$p, digits = 4))
    )
  })
  
  output$norm_interp <- renderUI({
    r <- norm_result()
    if (is.na(r$p)) {
      return(alert("info", "Shapiro-Wilk test not performed:",
                   paste("The sample contains", r$n, "valid observations.",
                         "Use the Q-Q plot and histogram for graphical assessment.")))
    }
    p_txt <- paste0("p = ", format.pval(r$p, digits = 4), ". ")
    if (r$p >= 0.05) {
      alert("success", "Shapiro-Wilk result:",
            paste0(p_txt, "The test does not provide statistically significant evidence against normality. ",
                   "The Q-Q plot and histogram should also be considered before making a final assessment."))
    } else {
      alert("warning", "Shapiro-Wilk result:",
            paste0(p_txt, "The test provides evidence against normality. ",
                   "The degree of departure should be assessed using the Q-Q plot and histogram."))
    }
  })
  
  output$norm_qq <- renderPlot({
    r <- norm_result()
    ggplot(data.frame(v = r$x), aes(sample = v)) +
      stat_qq(colour = ACCENT) + stat_qq_line() +
      labs(title = paste("Normal Q-Q plot:", r$variable),
           x = "Theoretical quantiles", y = "Sample quantiles") +
      theme_app
  })
  
  output$norm_hist <- renderPlot({
    r <- norm_result()
    ggplot(data.frame(v = r$x), aes(v)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = ACCENT, alpha = 0.5, colour = "white") +
      geom_density(colour = ACCENT, linewidth = 1) +
      labs(title = paste("Histogram with density:", r$variable), x = r$variable, y = "Density") +
      theme_app
  })
  
  
  # ----------------------------------------------------------
  # LINEAR REGRESSION
  # ----------------------------------------------------------
  
  reg_model <- eventReactive(input$reg_run, {
    num <- types()$numeric
    validate(need(length(num) >= 2, "At least two numeric variables are required for linear regression."))
    auto <- input$reg_mode == "automatic"
    fit_lm(data(),
           dv  = if (auto) num[1]  else input$reg_dv,
           ivs = if (auto) num[-1] else input$reg_iv)
  })
  
  output$reg_results_ui <- renderUI({
    m <- req(reg_model())
    s <- summary(m)
    f <- s$fstatistic
    tagList(
      div(class = "tiles",
          stat_tile("Observations", nobs(m)),
          stat_tile("R\u00b2", sprintf("%.3f", s$r.squared)),
          stat_tile("Adjusted R\u00b2", sprintf("%.3f", s$adj.r.squared)),
          stat_tile("Residual SE", sprintf("%.3f", s$sigma)),
          stat_tile("F-test p-value",
                    format.pval(pf(f[1], f[2], f[3], lower.tail = FALSE), digits = 3))),
      panel_card("Multicollinearity Check", uiOutput("reg_collinearity")),
      panel_card("Regression Results", verbatimTextOutput("reg_summary")),
      panel_card("Regression Coefficients", tableOutput("reg_coefs")),
      uiOutput("ridge_results_ui")
    )
  })
  
  output$reg_collinearity <- renderUI({
    v <- calc_vif(reg_model())
    show_ridge <- !all(is.na(v$VIF)) && max(v$VIF, na.rm = TRUE) >= 5
    tagList(
      vif_alert(v), tableOutput("reg_vif"),
      if (show_ridge) tagList(
        br(),
        div(class = "hint-box",
            "Multicollinearity is affecting the ordinary least squares estimates. ",
            "Ridge regression shrinks the coefficients toward zero and can produce more ",
            "stable estimates when predictors are highly correlated."),
        actionButton("reg_ridge_run", "Run Ridge Regression", icon = icon("play"),
                     class = "btn-primary")
      )
    )
  })
  
  ridge_model <- eventReactive(input$reg_ridge_run, {
    m <- req(reg_model())
    md <- model.frame(m)
    y <- md[[1]]
    x <- as.matrix(md[-1])
    validate(need(nrow(x) > ncol(x) + 1, "Not enough observations to fit a ridge regression."))
    cv <- glmnet::cv.glmnet(x, y, alpha = 0, standardize = TRUE)
    fit <- glmnet::glmnet(x, y, alpha = 0, standardize = TRUE, lambda = cv$lambda.min)
    list(fit = fit, lambda = cv$lambda.min, ols = coef(m))
  })
  
  output$ridge_results_ui <- renderUI({
    req(ridge_model())
    tagList(
      panel_card("Ridge Regression Results",
                 p(sprintf("Penalty strength (\u03bb) chosen by 10-fold cross-validation: %.5f",
                          ridge_model()$lambda)),
                 tableOutput("ridge_coefs"),
                 div(class = "hint-box",
                     "Ridge coefficients are shrunk to reduce the instability caused by ",
                     "multicollinearity, and do not have standard errors or p-values in the ",
                     "usual OLS sense. Compare their signs and relative magnitudes to the OLS ",
                     "coefficients above rather than testing them individually."))
    )
  })
  
  output$ridge_coefs <- renderTable({
    r <- ridge_model()
    rc <- as.matrix(coef(r$fit))
    data.frame(Variable = rownames(rc),
               OLS_Coefficient   = round(as.numeric(r$ols[rownames(rc)]), 4),
               Ridge_Coefficient = round(as.numeric(rc[, 1]), 4),
               row.names = NULL)
  }, digits = 4)
  
  output$reg_vif <- renderTable(calc_vif(reg_model()), digits = 3)
  
  output$reg_summary <- renderPrint(summary(reg_model()))
  
  output$reg_coefs <- renderTable({
    co <- as.data.frame(summary(reg_model())$coefficients)
    co[["Pr(>|t|)"]] <- format.pval(co[["Pr(>|t|)"]], digits = 3, eps = 1e-4)
    cbind(Variable = rownames(co), co, row.names = NULL)
  }, digits = 4)
  
  
  # ----------------------------------------------------------
  # CORRELATION ANALYSIS (two variables or a full matrix)
  # ----------------------------------------------------------
  
  cor_result <- eventReactive(input$cor_run, {
    num <- types()$numeric
    method <- input$cor_method
    vars <- if (input$cor_mode == "automatic") head(num, MAX_AUTO_CORR_VARS) else input$cor_vars
    
    validate(need(length(vars) >= 2, "Select at least two numeric variables."))
    validate(need(all(vars %in% num), "All selected variables must be numeric."))
    
    df <- data()[vars]
    flat <- vars[vapply(df, function(x) length(unique(x[!is.na(x)])) <= 1, logical(1))]
    validate(need(!length(flat),
                  paste("Insufficient variation in:", paste(flat, collapse = ", "))))
    
    pairs <- cor_pairs(df, vars, method)
    validate(need(!anyNA(pairs$Correlation),
                  "At least 3 complete observations are required for every pair of variables."))
    
    list(vars = vars, method = method, pairs = pairs,
         r = pair_matrix(pairs, vars, "Correlation", diag_value = 1),
         p = pair_matrix(pairs, vars, "P_Value"))
  })
  
  output$cor_results_ui <- renderUI({
    res <- req(cor_result())
    multi <- length(res$vars) > 2
    tagList(
      panel_card(
        if (multi) "Correlation Matrix" else "Correlation Results",
        if (multi) tagList(
          div(class = "scroll-x", tableOutput("cor_matrix")),
          p(class = "text-muted", "* p < .05, ** p < .01, *** p < .001"),
          h5("Pairwise tests")),
        div(class = "scroll-x", tableOutput("cor_pairs"))
      ),
      if (multi) panel_card(
        "Correlation Heatmap",
        plotOutput("cor_heatmap",
                   height = paste0(min(900, max(420, 70 * length(res$vars) + 160)), "px"))
      ),
      panel_card("Interpretation", uiOutput("cor_interp"))
    )
  })
  
  output$cor_matrix <- renderTable({
    res <- cor_result()
    as.data.frame(matrix(paste0(formatC(res$r, digits = 3, format = "f"), stars(res$p)),
                         nrow(res$r), dimnames = dimnames(res$r)))
  }, rownames = TRUE)
  
  output$cor_pairs <- renderTable({
    p <- cor_result()$pairs
    p$N <- as.integer(p$N)
    p$Test_Statistic <- format(round(p$Test_Statistic, 4), trim = TRUE, drop0trailing = TRUE)
    p$P_Value <- format.pval(p$P_Value, digits = 3, eps = 1e-4)
    p
  }, digits = 4, na = "-")
  
  output$cor_heatmap <- renderPlot({
    res <- cor_result()
    hm <- as.data.frame(as.table(res$r), responseName = "r")
    hm$p <- as.vector(res$p)
    hm$Var2 <- factor(hm$Var2, levels = rev(res$vars))   # diagonal runs top-left to bottom-right
    hm$label <- paste0(sprintf("%.2f", hm$r), stars(hm$p))
    
    ggplot(hm, aes(Var1, Var2, fill = r)) +
      geom_tile(colour = "white", linewidth = 0.6) +
      geom_text(aes(label = label), size = if (length(res$vars) > 8) 3 else 4) +
      scale_fill_gradient2(low = "#3B6FA0", mid = "white", high = "#C8553D",
                           midpoint = 0, limits = c(-1, 1), name = res$method) +
      coord_fixed() +
      labs(title = paste(res$method, "correlation heatmap"), x = NULL, y = NULL) +
      theme_app +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
  })
  
  output$cor_interp <- renderUI({
    res <- cor_result()
    p <- res$pairs
    coef_name <- c(Pearson = "r", Spearman = "rho", Kendall = "tau")[[res$method]]
    
    msg <- if (nrow(p) == 1) {
      tagList(
        sprintf("There is a %s %s association between %s and %s based on the %s correlation.",
                strength(p$Correlation), if (p$Correlation > 0) "positive" else "negative",
                p$Variable_1, p$Variable_2, res$method),
        tags$br(),
        sprintf("The association is %sstatistically significant at the 0.05 level (p = %s).",
                if (p$P_Value < 0.05) "" else "not ", format.pval(p$P_Value, digits = 4)))
    } else {
      top <- p[which.max(abs(p$Correlation)), ]
      tagList(
        sprintf("Across %d variables (%d pairs), %d pair(s) are statistically significant at the 0.05 level.",
                length(res$vars), nrow(p), sum(p$P_Value < 0.05, na.rm = TRUE)),
        tags$br(),
        sprintf("Strongest association: %s and %s (%s, %s %s).",
                top$Variable_1, top$Variable_2, sprintf("%s = %.3f", coef_name, top$Correlation),
                strength(top$Correlation), if (top$Correlation > 0) "positive" else "negative"))
    }
    alert("info", "Interpretation:",
          tagList(msg, tags$br(), "Correlation does not by itself establish causation."))
  })
  
  
  # ----------------------------------------------------------
  # LOGISTIC REGRESSION
  # ----------------------------------------------------------
  
  logit_model <- eventReactive(input$logit_run, {
    bin <- types()$binary
    num <- types()$numeric
    validate(need(length(bin) >= 1, "At least one two-category variable is required."))
    validate(need(length(num) >= 1, "At least one numeric variable is required."))
    auto <- input$logit_mode == "automatic"
    fit_logit(data(),
              dv  = if (auto) bin[1] else input$logit_dv,
              ivs = if (auto) num    else input$logit_iv)
  })
  
  output$logit_results_ui <- renderUI({
    r <- req(logit_model())
    m <- r$model
    mcfadden <- 1 - m$deviance / m$null.deviance
    pred <- ifelse(predict(m, type = "response") >= 0.5, r$positive_level, r$reference_level)
    acc <- mean(pred == as.character(r$data[[r$dv]]))
    
    tagList(
      div(class = "tiles",
          stat_tile("Observations", nobs(m)),
          stat_tile("McFadden's R\u00b2", sprintf("%.3f", mcfadden)),
          stat_tile("AIC", sprintf("%.1f", AIC(m))),
          stat_tile("Accuracy (0.5 cutoff)", sprintf("%.1f%%", 100 * acc)),
          stat_tile("Predicting", sprintf('"%s" vs "%s"', r$positive_level, r$reference_level))),
      panel_card("Logistic Regression Results", verbatimTextOutput("logit_summary")),
      panel_card("Coefficients & Odds Ratios", tableOutput("logit_coefs")),
      panel_card("Confusion Matrix (0.5 cutoff)", tableOutput("logit_confusion"))
    )
  })
  
  output$logit_summary <- renderPrint(summary(logit_model()$model))
  
  output$logit_coefs <- renderTable({
    co <- as.data.frame(summary(logit_model()$model)$coefficients)
    co$`Odds Ratio` <- exp(co$Estimate)
    co[["Pr(>|z|)"]] <- format.pval(co[["Pr(>|z|)"]], digits = 3, eps = 1e-4)
    cbind(Variable = rownames(co), co, row.names = NULL)
  }, digits = 4)
  
  output$logit_confusion <- renderTable({
    r <- logit_model()
    lvls <- c(r$reference_level, r$positive_level)
    pred <- ifelse(predict(r$model, type = "response") >= 0.5, r$positive_level, r$reference_level)
    actual <- as.character(r$data[[r$dv]])
    tab <- table(Actual = factor(actual, lvls), Predicted = factor(pred, lvls))
    d <- as.data.frame.matrix(tab)
    cbind(Actual = rownames(d), d, row.names = NULL)
  })
  
  
  # ----------------------------------------------------------
  # TESTING (t-tests)
  # ----------------------------------------------------------
  
  output$test_vars_ui <- renderUI({
    req(input$test_type)
    num <- types()$numeric
    bin <- types()$binary
    switch(
      input$test_type,
      "One-Sample t-test" = tagList(
        selectInput("test_var", "Numeric variable:", num),
        numericInput("test_mu", "Test value (\u03bc\u2080):", value = 0)),
      "Independent Samples t-test" = tagList(
        selectInput("test_var", "Numeric variable:", num),
        selectInput("test_group", "Grouping variable (2 categories):", bin)),
      "Paired t-test" = tagList(
        selectInput("test_var1", "Variable 1:", num),
        selectInput("test_var2", "Variable 2:", num, selected = num[2]))
    )
  })
  
  test_result <- eventReactive(input$test_run, {
    df <- data()
    type <- input$test_type
    
    if (type == "One-Sample t-test") {
      validate(need(isTruthy(input$test_var), "Select a numeric variable."))
      x <- as.numeric(df[[input$test_var]]); x <- x[is.finite(x)]
      validate(need(length(x) >= 2, "At least 2 valid observations are required."))
      tt <- t.test(x, mu = input$test_mu)
      list(type = type, test = tt, label = input$test_var, mu = input$test_mu)
      
    } else if (type == "Independent Samples t-test") {
      validate(need(isTruthy(input$test_var) && isTruthy(input$test_group),
                    "Select a numeric variable and a two-category grouping variable."))
      x <- as.numeric(df[[input$test_var]])
      g <- df[[input$test_group]]
      ok <- is.finite(x) & !is.na(g)
      x <- x[ok]; g <- g[ok]
      validate(need(length(unique(g)) == 2, "The grouping variable must have exactly two categories."))
      validate(need(all(table(g) >= 2), "Each group needs at least 2 valid observations."))
      tt <- t.test(x ~ g)
      list(type = type, test = tt, label = input$test_var, group = input$test_group)
      
    } else {
      validate(need(isTruthy(input$test_var1) && isTruthy(input$test_var2) &&
                      !identical(input$test_var1, input$test_var2),
                    "Select two different numeric variables."))
      d <- df[complete.cases(df[c(input$test_var1, input$test_var2)]), ]
      validate(need(nrow(d) >= 2, "At least 2 paired observations are required."))
      tt <- t.test(d[[input$test_var1]], d[[input$test_var2]], paired = TRUE)
      list(type = type, test = tt, label = paste(input$test_var1, "vs", input$test_var2))
    }
  })
  
  output$test_results_ui <- renderUI({
    req(test_result())
    panel_card("Test Results", tableOutput("test_table"),
               h5("Interpretation"), uiOutput("test_interp"))
  })
  
  output$test_table <- renderTable({
    tt <- test_result()$test
    data.frame(
      Statistic = c("t", "df", "p-value", "95% CI Lower", "95% CI Upper",
                    if (length(tt$estimate) == 2) c("Mean (Group 1)", "Mean (Group 2)")
                    else "Mean / Mean Difference"),
      Value = c(sprintf("%.4f", tt$statistic), sprintf("%.2f", tt$parameter),
                format.pval(tt$p.value, digits = 4),
                sprintf("%.4f", tt$conf.int[1]), sprintf("%.4f", tt$conf.int[2]),
                sprintf("%.4f", tt$estimate))
    )
  })
  
  output$test_interp <- renderUI({
    r <- test_result()
    tt <- r$test
    sig <- tt$p.value < 0.05
    p_txt <- format.pval(tt$p.value, digits = 4)
    msg <- switch(
      r$type,
      "One-Sample t-test" = sprintf(
        "The mean of %s is %sstatistically different from %.4g (p = %s).",
        r$label, if (sig) "" else "not ", r$mu, p_txt),
      "Independent Samples t-test" = sprintf(
        "%s is %sstatistically different between the two levels of %s (p = %s).",
        r$label, if (sig) "" else "not ", r$group, p_txt),
      "Paired t-test" = sprintf(
        "The mean difference between the paired measurements is %sstatistically significant (p = %s).",
        if (sig) "" else "not ", p_txt)
    )
    alert(if (sig) "warning" else "success", "t-test result:", msg)
  })
  
  
  # ----------------------------------------------------------
  # ANOVA (one-way)
  # ----------------------------------------------------------
  
  anova_model <- eventReactive(input$anova_run, {
    num <- types()$numeric
    cat <- types()$categorical
    validate(need(length(num) >= 1 && length(cat) >= 1,
                  "At least one numeric variable and one categorical variable are required."))
    auto <- input$anova_mode == "automatic"
    dv     <- if (auto) num[1] else input$anova_dv
    factor_var <- if (auto) cat[1] else input$anova_factor
    validate(need(isTruthy(dv) && isTruthy(factor_var),
                  "Select a numeric variable and a categorical factor."))
    validate(need(!identical(dv, factor_var),
                  "The dependent variable and the factor must be different."))
    
    d <- data()[complete.cases(data()[c(dv, factor_var)]), c(dv, factor_var)]
    d[[factor_var]] <- factor(d[[factor_var]])
    validate(need(nlevels(d[[factor_var]]) >= 2, "The factor must have at least two levels."))
    validate(need(all(table(d[[factor_var]]) >= 2), "Each group needs at least 2 observations."))
    
    f <- reformulate(factor_var, response = dv)
    list(fit = aov(f, data = d), dv = dv, factor = factor_var, data = d)
  })
  
  output$anova_results_ui <- renderUI({
    req(anova_model())
    tagList(
      panel_card("ANOVA Table", tableOutput("anova_table"), uiOutput("anova_interp")),
      panel_card("Tukey HSD Post-Hoc Comparisons", tableOutput("anova_tukey")),
      panel_card("Group Distributions", plotOutput("anova_plot", height = "420px"))
    )
  })
  
  output$anova_table <- renderTable({
    s <- summary(anova_model()$fit)[[1]]
    out <- data.frame(Source = trimws(rownames(s)), s, row.names = NULL)
    names(out) <- c("Source", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    out$`Pr(>F)` <- format.pval(out$`Pr(>F)`, digits = 4, eps = 1e-4)
    out
  }, digits = 4, na = "")
  
  output$anova_interp <- renderUI({
    r <- anova_model()
    p <- summary(r$fit)[[1]][["Pr(>F)"]][1]
    if (is.na(p)) return(NULL)
    sig <- p < 0.05
    alert(if (sig) "warning" else "success", "Interpretation:",
          sprintf("The mean of %s %s significantly across the levels of %s (p = %s).",
                  r$dv, if (sig) "differs" else "does not differ significantly",
                  r$factor, format.pval(p, digits = 4)))
  })
  
  output$anova_tukey <- renderTable({
    tk <- TukeyHSD(anova_model()$fit)[[1]]
    out <- data.frame(Comparison = rownames(tk), tk, row.names = NULL)
    names(out) <- c("Comparison", "Difference", "Lower CI", "Upper CI", "p adj")
    out$`p adj` <- format.pval(out$`p adj`, digits = 4, eps = 1e-4)
    out
  }, digits = 4)
  
  output$anova_plot <- renderPlot({
    r <- anova_model()
    plot_chart("Boxplot", r$data, x = r$dv, group = r$factor)
  })
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
