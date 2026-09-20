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
library(glmnet)
library(shiny)
library(readxl)
library(haven)
library(ggplot2)


# ============================================================
# 2. CONSTANTS & STYLE
# ============================================================

ACCENT <- "#1D6F8A"
MAX_AUTO_CORR_VARS <- 12   # cap for "Automatic" correlation matrix
MAX_AUTO_MV_VARS   <- 15   # cap for "Automatic" multivariate analysis
VIF_FLAG_THRESHOLD <- 5    # VIF at/above this triggers the ridge-regression option

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

# Minimum categorical / numeric variables each hypothesis test needs.
TESTS <- list(
  "One-Sample t-test"  = c(cat = 0, num = 1),
  "Independent t-test" = c(cat = 1, num = 1),
  "Paired t-test"      = c(cat = 0, num = 2),
  "Chi-Square Test"    = c(cat = 2, num = 0)
)

# Minimum categorical / numeric variables each non-parametric test needs.
NP_TESTS <- list(
  "Mann-Whitney U (2 groups)"     = c(cat = 1, num = 1),
  "Kruskal-Wallis (3+ groups)"    = c(cat = 1, num = 1),
  "Wilcoxon Signed-Rank (paired)" = c(cat = 0, num = 2)
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
# `<id>_auto` is a uiOutput describing the automatic rule. `manual` may
# itself be a uiOutput() when the manual UI depends on other inputs.
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

# Build a two-column, complete-cases data frame for a numeric variable `x`
# grouped by categorical variable `g` (used by t-tests, ANOVA, non-parametric
# tests). `g` is coerced to a factor with unused levels dropped.
grouped_xy <- function(df, x, g) {
  d <- df[c(x, g)]
  d <- d[complete.cases(d), ]
  d[[g]] <- droplevels(as.factor(d[[g]]))
  d
}

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

# Same validity checks as fit_lm(), for a binary (two-level) outcome.
fit_logit <- function(df, dv, ivs) {
  validate(
    need(length(dv) == 1 && length(ivs) >= 1,
         "Select one dependent variable and at least one independent variable."),
    need(!(dv %in% ivs), "The dependent variable cannot also be an independent variable.")
  )
  vars <- c(dv, ivs)
  md <- df[complete.cases(df[vars]), vars, drop = FALSE]
  md[[dv]] <- as.factor(md[[dv]])
  validate(need(nlevels(md[[dv]]) == 2, "The dependent variable must have exactly two levels."))
  validate(need(nrow(md) > length(ivs) + 1,
                "Not enough complete observations relative to the number of model parameters."))
  flat <- ivs[vapply(md[ivs], function(x) length(unique(x)) <= 1, logical(1))]
  validate(need(!length(flat),
                paste("Insufficient variation in:", paste(flat, collapse = ", "))))
  glm(reformulate(ivs, dv), data = md, family = binomial())
}

# Works for both lm and glm fits (model.matrix() is generic).
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

# Ridge regression (glmnet, alpha = 0) fitted on the same design matrix as an
# already-fitted lm model. Only meant to be called when VIF has flagged
# multicollinearity; not a general-purpose fitting routine.
fit_ridge <- function(model) {
  x <- model.matrix(model)[, -1, drop = FALSE]
  y <- model.response(model.frame(model))
  nf <- max(3, min(10, nrow(x)))
  cvfit <- cv.glmnet(x, y, alpha = 0, standardize = TRUE, nfolds = nf)
  list(lambda = cvfit$lambda.min, coef = as.matrix(coef(cvfit, s = "lambda.min")))
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

# ---- Outlier detection helpers -----------------------------

outlier_bounds <- function(x, method = "IQR") {
  x <- x[is.finite(x)]
  if (method == "IQR") {
    q <- quantile(x, c(.25, .75), na.rm = TRUE); iqr <- diff(q)
    c(lower = q[[1]] - 1.5 * iqr, upper = q[[2]] + 1.5 * iqr)
  } else {
    m <- mean(x); s <- sd(x)
    c(lower = m - 3 * s, upper = m + 3 * s)
  }
}

outlier_summary <- function(df, vars, method = "IQR") {
  do.call(rbind, lapply(vars, function(v) {
    x <- df[[v]]; xf <- x[is.finite(x)]
    b <- outlier_bounds(xf, method)
    flag <- is.finite(x) & (x < b[["lower"]] | x > b[["upper"]])
    data.frame(Variable = v, N = length(xf), N_Outliers = sum(flag),
               Pct_Outliers = round(100 * sum(flag) / max(1, length(xf)), 2),
               Lower_Bound = round(b[["lower"]], 3), Upper_Bound = round(b[["upper"]], 3))
  }))
}

# ---- Reliability (Cronbach's alpha) helper -----------------

cronbach_alpha <- function(df) {
  df <- df[complete.cases(df), , drop = FALSE]
  k <- ncol(df)
  item_var <- vapply(df, var, numeric(1))
  total_var <- var(rowSums(df))
  a <- (k / (k - 1)) * (1 - sum(item_var) / total_var)
  it  <- vapply(seq_len(k), function(i) cor(df[[i]], rowSums(df[-i])), numeric(1))
  aid <- vapply(seq_len(k), function(i) {
    sub <- df[-i]; kk <- ncol(sub)
    (kk / (kk - 1)) * (1 - sum(vapply(sub, var, numeric(1))) / var(rowSums(sub)))
  }, numeric(1))
  list(alpha = a, n = nrow(df), k = k,
       items = data.frame(Item = names(df), Item_Total_Cor = round(it, 3),
                          Alpha_If_Dropped = round(aid, 3)))
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
      stat_tile("Statistical tools", "Normality, regression, correlation and more"))
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
            
            # ---- 1. Normality ----
            tabPanel(
              "Normality Check", br(),
              panel_card("Normality Check Setup",
                         var_panel("norm", selectInput("norm_var", "Numeric variable:", choices = NULL)),
                         run_button("norm_run", "Check Normality"),
                         uiOutput("norm_msg")),
              uiOutput("norm_results_ui")
            ),
            
            # ---- 2. Regression (+ ridge when collinearity is flagged) ----
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
            
            # ---- 3. Correlation ----
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
            
            # ---- 4. Logistic Regression ----
            tabPanel(
              "Logistic Regression", br(),
              panel_card("Logistic Regression Setup",
                         var_panel("log", tagList(
                           selectInput("log_dv", "Dependent variable (binary):", choices = NULL),
                           selectizeInput("log_iv", "Independent variable(s):", choices = NULL,
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button"))))),
                         run_button("log_run", "Run Logistic Regression"),
                         uiOutput("log_msg")),
              uiOutput("log_results_ui")
            ),
            
            # ---- 5. Hypothesis Testing ----
            tabPanel(
              "Testing", br(),
              panel_card("Hypothesis Test Setup",
                         fluidRow(
                           column(6, selectInput("test_type", "Test:", names(TESTS))),
                           column(6, conditionalPanel(
                             "input.test_type == 'One-Sample t-test'",
                             numericInput("test_mu", "Test value (\u03bc):", 0)))),
                         var_panel("test", uiOutput("test_manual_ui")),
                         run_button("test_run", "Run Test"),
                         uiOutput("test_msg")),
              uiOutput("test_results_ui")
            ),
            
            # ---- 6. ANOVA ----
            tabPanel(
              "ANOVA", br(),
              panel_card("One-Way ANOVA Setup",
                         var_panel("anova", tagList(
                           selectInput("anova_y", "Numeric (dependent) variable:", choices = NULL),
                           selectInput("anova_g", "Grouping (factor) variable:", choices = NULL))),
                         run_button("anova_run", "Run ANOVA"),
                         uiOutput("anova_msg")),
              uiOutput("anova_results_ui")
            ),
            
            # ---- 7. Non-Parametric Tests ----
            tabPanel(
              "Non-Parametric", br(),
              panel_card("Non-Parametric Test Setup",
                         selectInput("np_type", "Test:", names(NP_TESTS)),
                         var_panel("np", uiOutput("np_manual_ui")),
                         run_button("np_run", "Run Test"),
                         uiOutput("np_msg")),
              uiOutput("np_results_ui")
            ),
            
            # ---- 8. Time Series ----
            tabPanel(
              "Time Series", br(),
              panel_card("Time Series Setup",
                         var_panel("ts", tagList(
                           selectInput("ts_var", "Numeric (value) variable:", choices = NULL),
                           selectInput("ts_time", "Time / date variable (optional):", choices = NULL),
                           numericInput("ts_freq",
                                        "Seasonal frequency (1 = none, 12 = monthly, 4 = quarterly, 7 = weekly):",
                                        1, 1, 365))),
                         run_button("ts_run", "Run Time Series Analysis"),
                         uiOutput("ts_msg")),
              uiOutput("ts_results_ui")
            ),
            
            # ---- 9. Reliability Analysis ----
            tabPanel(
              "Reliability Analysis", br(),
              panel_card("Reliability (Cronbach's Alpha) Setup",
                         var_panel("rel", selectizeInput("rel_items", "Scale items (numeric, 2 or more):",
                                                         choices = NULL, multiple = TRUE,
                                                         options = list(plugins = list("remove_button")))),
                         run_button("rel_run", "Run Reliability Analysis"),
                         uiOutput("rel_msg")),
              uiOutput("rel_results_ui")
            ),
            
            # ---- 10. Multivariate Analysis ----
            tabPanel(
              "Multivariate Analysis", br(),
              panel_card("Principal Component Analysis & Clustering Setup",
                         var_panel("mv", selectizeInput("mv_vars", "Numeric variables (2 or more):",
                                                        choices = NULL, multiple = TRUE,
                                                        options = list(plugins = list("remove_button")))),
                         fluidRow(
                           column(6, checkboxInput("mv_scale", "Standardize variables (recommended)", TRUE)),
                           column(6, numericInput("mv_k", "Number of clusters (k):", 3, 2, 10))),
                         run_button("mv_run", "Run Multivariate Analysis"),
                         uiOutput("mv_msg")),
              uiOutput("mv_results_ui")
            ),
            
            # ---- 11. Outlier Detection ----
            tabPanel(
              "Outlier Detection", br(),
              panel_card("Outlier Detection Setup",
                         var_panel("out", selectizeInput("out_vars", "Numeric variable(s):", choices = NULL,
                                                         multiple = TRUE,
                                                         options = list(plugins = list("remove_button")))),
                         radioButtons("out_method", "Method:",
                                      c("IQR (1.5\u00d7)" = "IQR", "Z-score (|z| > 3)" = "Z"), inline = TRUE),
                         run_button("out_run", "Detect Outliers"),
                         uiOutput("out_msg")),
              uiOutput("out_results_ui")
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
    list(numeric = numeric, categorical = categorical, date = date,
         other = setdiff(nm, c(numeric, categorical, date)))
  })
  
  # Any column (of any type) with exactly two distinct non-missing values;
  # used as the candidate outcome list for logistic regression.
  binary_vars <- reactive({
    df <- req(data())
    names(df)[vapply(df, function(x) length(unique(x[!is.na(x)])) == 2, logical(1))]
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
    cat <- types()$categorical
    updateSelectInput(session, "norm_var", choices = num)
    updateSelectInput(session, "reg_dv", choices = num)
    updateSelectInput(session, "cor_vars", choices = num)
    updateSelectInput(session, "log_dv", choices = binary_vars())
    updateSelectInput(session, "anova_y", choices = num)
    updateSelectInput(session, "anova_g", choices = cat)
    updateSelectInput(session, "ts_var", choices = num)
    updateSelectInput(session, "ts_time", choices = c("(row order)" = "", types()$date, num))
    updateSelectizeInput(session, "rel_items", choices = num)
    updateSelectizeInput(session, "mv_vars", choices = num)
    updateSelectizeInput(session, "out_vars", choices = num)
  })
  
  # Regression / logistic-regression predictors exclude the chosen outcome
  observe({
    ivs <- setdiff(types()$numeric, input$reg_dv)
    updateSelectInput(session, "reg_iv", choices = ivs,
                      selected = intersect(isolate(input$reg_iv), ivs))
  })
  observe({
    ivs <- setdiff(c(types()$numeric, types()$categorical), input$log_dv)
    updateSelectizeInput(session, "log_iv", choices = ivs,
                         selected = intersect(isolate(input$log_iv), ivs))
  })
  
  observeEvent(input$cor_all, {
    updateSelectInput(session, "cor_vars", selected = types()$numeric)
  })
  
  # Warning shown when the dataset cannot support a tool. Replaces the
  # dataset-specific checks each tool used to duplicate.
  avail_msg <- function(tool, need_num = 0, need_cat = 0, need_bin = 0) {
    t <- types()
    if (length(t$numeric) < need_num || length(t$categorical) < need_cat ||
        length(binary_vars()) < need_bin) {
      req_txt <- paste(c(
        if (need_num) paste(need_num, "numeric"),
        if (need_cat) paste(need_cat, "categorical"),
        if (need_bin) paste(need_bin, "binary")
      ), collapse = " and ")
      return(alert("danger", paste0(tool, " unavailable:"),
                   paste("the dataset must contain at least", req_txt, "variable(s).")))
    }
    NULL
  }
  output$norm_msg  <- renderUI(avail_msg("Normality Check", need_num = 1))
  output$reg_msg   <- renderUI(avail_msg("Linear Regression", need_num = 2))
  output$cor_msg   <- renderUI(avail_msg("Correlation Analysis", need_num = 2))
  output$log_msg   <- renderUI(avail_msg("Logistic Regression", need_bin = 1))
  output$test_msg  <- renderUI(avail_msg("Hypothesis Testing", need_num = 1))
  output$anova_msg <- renderUI(avail_msg("ANOVA", need_num = 1, need_cat = 1))
  output$np_msg    <- renderUI(avail_msg("Non-Parametric Tests", need_num = 1))
  output$ts_msg    <- renderUI(avail_msg("Time Series Analysis", need_num = 1))
  output$rel_msg   <- renderUI(avail_msg("Reliability Analysis", need_num = 2))
  output$mv_msg    <- renderUI(avail_msg("Multivariate Analysis", need_num = 2))
  output$out_msg   <- renderUI(avail_msg("Outlier Detection", need_num = 1))
  
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
  
  output$log_auto <- renderUI({
    v <- binary_vars(); n <- types()$numeric
    req(length(v) >= 1)
    tagList(tags$b("Automatic rule: "),
            "the first binary variable is the outcome; all numeric variables are predictors.",
            tags$br(), tags$b("Dependent variable: "), v[1],
            tags$br(), tags$b("Independent variable(s): "), paste(setdiff(n, v[1]), collapse = ", "))
  })
  
  output$test_auto <- renderUI({
    n <- types()$numeric; k <- types()$categorical; type <- input$test_type
    req(type)
    need_v <- TESTS[[type]]
    req(length(k) >= need_v["cat"], length(n) >= need_v["num"])
    desc <- switch(type,
      "One-Sample t-test"  = sprintf("Testing %s against \u03bc = %s.", n[1], input$test_mu),
      "Independent t-test" = sprintf("Comparing %s across the two groups of %s.", n[1], k[1]),
      "Paired t-test"      = sprintf("Comparing %s and %s (paired).", n[1], n[min(2, length(n))]),
      "Chi-Square Test"    = sprintf("Testing independence between %s and %s.", k[1], k[min(2, length(k))])
    )
    tagList(tags$b("Automatic rule: "), desc)
  })
  
  output$anova_auto <- renderUI({
    n <- types()$numeric; k <- types()$categorical
    req(length(n) >= 1, length(k) >= 1)
    tagList(tags$b("Automatic rule: "), "the first numeric variable is compared across the first categorical variable.",
            tags$br(), tags$b("Dependent variable: "), n[1], tags$br(), tags$b("Grouping variable: "), k[1])
  })
  
  output$np_auto <- renderUI({
    n <- types()$numeric; k <- types()$categorical; type <- input$np_type
    req(type)
    need_v <- NP_TESTS[[type]]
    req(length(k) >= need_v["cat"], length(n) >= need_v["num"])
    desc <- if (type == "Wilcoxon Signed-Rank (paired)") {
      sprintf("Comparing %s and %s (paired).", n[1], n[min(2, length(n))])
    } else {
      sprintf("Comparing %s across the groups of %s.", n[1], k[1])
    }
    tagList(tags$b("Automatic rule: "), desc)
  })
  
  output$ts_auto <- renderUI({
    n <- types()$numeric
    req(length(n) >= 1)
    tagList(tags$b("Automatic rule: "),
            "the first numeric variable is analyzed in row order (or by the first date variable, if present).",
            tags$br(), tags$b("Variable: "), n[1])
  })
  
  output$rel_auto <- renderUI({
    n <- types()$numeric
    req(length(n) >= 2)
    tagList(tags$b("Automatic rule: "), "all numeric variables are treated as scale items.",
            tags$br(), tags$b("Items: "), paste(n, collapse = ", "))
  })
  
  output$mv_auto <- renderUI({
    n <- head(types()$numeric, MAX_AUTO_MV_VARS)
    req(length(n) >= 2)
    tagList(tags$b("Automatic rule: "), sprintf("all numeric variables are used (first %d at most).", MAX_AUTO_MV_VARS),
            tags$br(), tags$b("Variables: "), paste(n, collapse = ", "))
  })
  
  output$out_auto <- renderUI({
    n <- types()$numeric
    req(length(n) >= 1)
    tagList(tags$b("Automatic rule: "), "outliers are flagged in every numeric variable.",
            tags$br(), tags$b("Variables: "), paste(n, collapse = ", "))
  })
  
  
  # ----------------------------------------------------------
  # 1. NORMALITY CHECK
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
  # 2. LINEAR REGRESSION (+ ridge)
  # ----------------------------------------------------------
  
  reg_model <- eventReactive(input$reg_run, {
    num <- types()$numeric
    validate(need(length(num) >= 2, "At least two numeric variables are required for linear regression."))
    auto <- input$reg_mode == "automatic"
    fit_lm(data(),
           dv  = if (auto) num[1]  else input$reg_dv,
           ivs = if (auto) num[-1] else input$reg_iv)
  })
  
  reg_vif <- reactive(calc_vif(reg_model()))
  
  # Ridge is only offered once VIF has actually flagged multicollinearity.
  reg_show_ridge <- reactive({
    v <- reg_vif()$VIF
    length(v) > 0 && !all(is.na(v)) && max(v, na.rm = TRUE) >= VIF_FLAG_THRESHOLD
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
      if (reg_show_ridge()) panel_card(
        "Ridge Regression (suggested due to multicollinearity)",
        p("Ridge regression shrinks correlated predictors' coefficients toward zero, trading a small",
          "amount of bias for lower variance and more stable estimates than ordinary least squares."),
        tableOutput("reg_ridge"), uiOutput("reg_ridge_note")
      )
    )
  })
  
  output$reg_collinearity <- renderUI({
    tagList(vif_alert(reg_vif()), tableOutput("reg_vif"))
  })
  
  output$reg_vif <- renderTable(reg_vif(), digits = 3)
  
  output$reg_summary <- renderPrint(summary(reg_model()))
  
  output$reg_coefs <- renderTable({
    co <- as.data.frame(summary(reg_model())$coefficients)
    co[["Pr(>|t|)"]] <- format.pval(co[["Pr(>|t|)"]], digits = 3, eps = 1e-4)
    cbind(Variable = rownames(co), co, row.names = NULL)
  }, digits = 4)
  
  reg_ridge <- reactive({
    m <- reg_model()
    validate(need(nrow(model.matrix(m)) >= 10,
                  "At least 10 observations are recommended for ridge regression cross-validation."))
    fit_ridge(m)
  })
  
  output$reg_ridge <- renderTable({
    r <- reg_ridge()
    ols <- coef(reg_model())
    nm <- rownames(r$coef)
    data.frame(Variable = nm, OLS = round(ols[nm], 4), Ridge = round(r$coef[, 1], 4))
  }, digits = 4)
  
  output$reg_ridge_note <- renderUI({
    p(class = "text-muted",
      sprintf("Optimal penalty (\u03bb, chosen via cross-validation): %.4f", reg_ridge()$lambda))
  })
  
  
  # ----------------------------------------------------------
  # 3. CORRELATION ANALYSIS (two variables or a full matrix)
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
  # 4. LOGISTIC REGRESSION
  # ----------------------------------------------------------
  
  log_model <- eventReactive(input$log_run, {
    validate(need(length(binary_vars()) >= 1, "At least one binary variable is required as the outcome."))
    auto <- input$log_mode == "automatic"
    dv  <- if (auto) binary_vars()[1] else input$log_dv
    ivs <- if (auto) setdiff(types()$numeric, dv) else input$log_iv
    fit_logit(data(), dv, ivs)
  })
  
  output$log_results_ui <- renderUI({
    m <- req(log_model())
    nullmod <- glm(m$model[[1]] ~ 1, family = binomial())
    mcfadden <- 1 - as.numeric(logLik(m)) / as.numeric(logLik(nullmod))
    tagList(
      div(class = "tiles",
          stat_tile("Observations", nobs(m)),
          stat_tile("McFadden R\u00b2", sprintf("%.3f", mcfadden)),
          stat_tile("AIC", sprintf("%.1f", AIC(m))),
          stat_tile("Null Deviance", sprintf("%.1f", m$null.deviance)),
          stat_tile("Residual Deviance", sprintf("%.1f", m$deviance))),
      panel_card("Multicollinearity Check", uiOutput("log_collinearity")),
      panel_card("Regression Results", verbatimTextOutput("log_summary")),
      panel_card("Coefficients & Odds Ratios", tableOutput("log_coefs")),
      panel_card("Classification (0.5 cutoff)", tableOutput("log_confmat"), uiOutput("log_accuracy"))
    )
  })
  
  output$log_collinearity <- renderUI({
    v <- calc_vif(log_model())
    tagList(vif_alert(v), tableOutput("log_vif"))
  })
  output$log_vif <- renderTable(calc_vif(log_model()), digits = 3)
  
  output$log_summary <- renderPrint(summary(log_model()))
  
  output$log_coefs <- renderTable({
    m <- log_model()
    co <- as.data.frame(summary(m)$coefficients)
    ci <- suppressWarnings(confint.default(m))
    out <- data.frame(Variable = rownames(co), Estimate = round(co[["Estimate"]], 4),
                      SE = round(co[["Std. Error"]], 4), Z = round(co[["z value"]], 3),
                      P = format.pval(co[["Pr(>|z|)"]], digits = 3, eps = 1e-4),
                      OR = round(exp(co[["Estimate"]]), 3),
                      OR_CI = sprintf("[%.3f, %.3f]", exp(ci[, 1]), exp(ci[, 2])), row.names = NULL)
    names(out) <- c("Variable", "Estimate", "Std. Error", "Z value", "p value", "Odds Ratio", "OR 95% CI")
    out
  })
  
  output$log_confmat <- renderTable({
    m <- log_model()
    y <- m$model[[1]]; lv <- levels(y)
    pred <- factor(ifelse(fitted(m) > 0.5, lv[2], lv[1]), levels = lv)
    as.data.frame.matrix(table(Actual = y, Predicted = pred))
  }, rownames = TRUE)
  
  output$log_accuracy <- renderUI({
    m <- log_model()
    y <- m$model[[1]]; lv <- levels(y)
    pred <- factor(ifelse(fitted(m) > 0.5, lv[2], lv[1]), levels = lv)
    tab <- table(y, pred)
    acc <- sum(diag(tab)) / sum(tab)
    sens <- tab[2, 2] / sum(tab[2, ])
    spec <- tab[1, 1] / sum(tab[1, ])
    alert("info", "Classification performance:",
          sprintf("Accuracy = %.1f%%, Sensitivity = %.1f%%, Specificity = %.1f%% (positive class: %s).",
                  100 * acc, 100 * sens, 100 * spec, lv[2]))
  })
  
  
  # ----------------------------------------------------------
  # 5. HYPOTHESIS TESTING (t-tests, chi-square)
  # ----------------------------------------------------------
  
  output$test_manual_ui <- renderUI({
    n <- types()$numeric; k <- types()$categorical; type <- req(input$test_type)
    need_v <- TESTS[[type]]
    if (length(k) < need_v["cat"] || length(n) < need_v["num"]) {
      return(alert("warning", "Not available:",
                   sprintf("%s needs at least %d categorical and %d numeric variable(s).",
                           type, need_v["cat"], need_v["num"])))
    }
    switch(type,
      "One-Sample t-test"  = selectInput("test_x", "Numeric variable:", n),
      "Independent t-test" = tagList(selectInput("test_x", "Numeric variable:", n),
                                     selectInput("test_g", "Grouping variable (2 levels):", k)),
      "Paired t-test"      = tagList(selectInput("test_x", "Variable 1:", n),
                                     selectInput("test_y", "Variable 2:", n, selected = n[min(2, length(n))])),
      "Chi-Square Test"    = tagList(selectInput("test_x", "Variable 1:", k),
                                     selectInput("test_y", "Variable 2:", k, selected = k[min(2, length(k))]))
    )
  })
  
  test_result <- eventReactive(input$test_run, {
    df <- data(); n <- types()$numeric; k <- types()$categorical
    type <- input$test_type; auto <- input$test_mode == "automatic"
    need_v <- TESTS[[type]]
    validate(need(length(k) >= need_v["cat"] && length(n) >= need_v["num"],
                  "The dataset does not have the variables this test requires."))
    pick <- function(auto_val, id) if (auto) auto_val else input[[id]]
    
    res <- switch(type,
      "One-Sample t-test" = {
        x <- pick(n[1], "test_x")
        v <- df[[x]]; v <- v[is.finite(v)]
        validate(need(length(v) >= 2, "At least two valid observations are required."))
        list(test = t.test(v, mu = input$test_mu),
             label = sprintf("One-sample t-test: %s vs \u03bc = %s", x, input$test_mu))
      },
      "Independent t-test" = {
        x <- pick(n[1], "test_x"); g <- pick(k[1], "test_g")
        d <- grouped_xy(df, x, g)
        lv <- levels(d[[g]])
        validate(need(length(lv) == 2, "The grouping variable must have exactly two levels."))
        list(test = t.test(d[[x]][d[[g]] == lv[1]], d[[x]][d[[g]] == lv[2]]),
             label = sprintf("Independent t-test: %s by %s", x, g))
      },
      "Paired t-test" = {
        x <- pick(n[1], "test_x"); y <- pick(n[min(2, length(n))], "test_y")
        validate(need(!identical(x, y), "Select two different variables."))
        d <- df[c(x, y)]; d <- d[complete.cases(d), ]
        validate(need(nrow(d) >= 2, "At least two complete paired observations are required."))
        list(test = t.test(d[[x]], d[[y]], paired = TRUE),
             label = sprintf("Paired t-test: %s vs %s", x, y))
      },
      "Chi-Square Test" = {
        x <- pick(k[1], "test_x"); y <- pick(k[min(2, length(k))], "test_y")
        validate(need(!identical(x, y), "Select two different variables."))
        tab <- table(df[[x]], df[[y]])
        validate(need(all(dim(tab) >= 2), "Both variables must have at least two categories."))
        list(test = suppressWarnings(chisq.test(tab)), label = sprintf("Chi-square test: %s vs %s", x, y))
      }
    )
    res
  })
  
  output$test_results_ui <- renderUI({
    r <- req(test_result())
    panel_card(r$label, verbatimTextOutput("test_print"), h5("Interpretation"), uiOutput("test_interp"))
  })
  output$test_print <- renderPrint(test_result()$test)
  output$test_interp <- renderUI({
    p <- test_result()$test$p.value
    alert(if (p < 0.05) "warning" else "success", "Result:",
          sprintf("p = %s. The difference is %sstatistically significant at the 0.05 level.",
                  format.pval(p, digits = 4), if (p < 0.05) "" else "not "))
  })
  
  
  # ----------------------------------------------------------
  # 6. ANOVA (one-way, with Levene's test and Tukey HSD)
  # ----------------------------------------------------------
  
  anova_result <- eventReactive(input$anova_run, {
    n <- types()$numeric; k <- types()$categorical
    validate(need(length(n) >= 1 && length(k) >= 1,
                  "At least one numeric and one categorical variable are required."))
    auto <- input$anova_mode == "automatic"
    y <- if (auto) n[1] else input$anova_y
    g <- if (auto) k[1] else input$anova_g
    d <- grouped_xy(data(), y, g)
    validate(need(nlevels(d[[g]]) >= 2, "The grouping variable must have at least two levels with data."))
    validate(need(nrow(d) > nlevels(d[[g]]), "Not enough observations relative to the number of groups."))
    list(y = y, g = g, d = d, fit = aov(reformulate(g, y), data = d))
  })
  
  output$anova_results_ui <- renderUI({
    r <- req(anova_result())
    tagList(
      panel_card(paste("ANOVA:", r$y, "by", r$g), tableOutput("anova_table"), uiOutput("anova_interp")),
      panel_card("Homogeneity of Variance (Levene's Test)", uiOutput("anova_levene")),
      panel_card("Tukey HSD Post-Hoc Comparisons", tableOutput("anova_tukey")),
      panel_card("Group Distribution", plotOutput("anova_plot", height = "420px"))
    )
  })
  
  output$anova_table <- renderTable({
    s <- summary(anova_result()$fit)[[1]]
    eta2 <- s[1, "Sum Sq"] / sum(s[["Sum Sq"]])
    data.frame(Source = trimws(rownames(s)), Df = s[["Df"]],
               `Sum Sq` = round(s[["Sum Sq"]], 3), `Mean Sq` = round(s[["Mean Sq"]], 3),
               `F value` = c(round(s[["F value"]][1], 3), NA),
               `p value` = c(format.pval(s[["Pr(>F)"]][1], digits = 4), NA),
               `Eta Squared` = c(round(eta2, 3), NA), check.names = FALSE)
  }, na = "")
  
  output$anova_interp <- renderUI({
    p <- summary(anova_result()$fit)[[1]][["Pr(>F)"]][1]
    alert(if (p < 0.05) "warning" else "success", "Result:",
          sprintf("p = %s. Group means are %sstatistically significantly different at the 0.05 level.",
                  format.pval(p, digits = 4), if (p < 0.05) "" else "not "))
  })
  
  output$anova_levene <- renderUI({
    r <- anova_result(); d <- r$d
    dev <- abs(d[[r$y]] - ave(d[[r$y]], d[[r$g]], FUN = median))
    p <- summary(aov(dev ~ d[[r$g]]))[[1]][["Pr(>F)"]][1]
    alert(if (p < 0.05) "warning" else "success", "Levene's test (Brown-Forsythe):",
          sprintf("p = %s. Variances are %shomogeneous across groups at the 0.05 level.",
                  format.pval(p, digits = 4), if (p < 0.05) "not " else ""))
  })
  
  output$anova_tukey <- renderTable({
    r <- anova_result()
    validate(need(nlevels(r$d[[r$g]]) >= 2, "Not enough groups for post-hoc comparisons."))
    tk <- as.data.frame(TukeyHSD(r$fit)[[1]])
    data.frame(Comparison = rownames(tk), Diff = round(tk$diff, 3),
               Lower = round(tk$lwr, 3), Upper = round(tk$upr, 3),
               `p adj` = format.pval(tk$`p adj`, digits = 4), check.names = FALSE)
  })
  
  output$anova_plot <- renderPlot({
    r <- anova_result()
    plot_chart("Boxplot", r$d, x = r$y, group = r$g)
  })
  
  
  # ----------------------------------------------------------
  # 7. NON-PARAMETRIC TESTS
  # ----------------------------------------------------------
  
  output$np_manual_ui <- renderUI({
    n <- types()$numeric; k <- types()$categorical; type <- req(input$np_type)
    need_v <- NP_TESTS[[type]]
    if (length(k) < need_v["cat"] || length(n) < need_v["num"]) {
      return(alert("warning", "Not available:",
                   sprintf("%s needs at least %d categorical and %d numeric variable(s).",
                           type, need_v["cat"], need_v["num"])))
    }
    if (type == "Wilcoxon Signed-Rank (paired)") {
      tagList(selectInput("np_x", "Variable 1:", n),
              selectInput("np_y", "Variable 2:", n, selected = n[min(2, length(n))]))
    } else {
      tagList(selectInput("np_x", "Numeric variable:", n),
              selectInput("np_g", "Grouping variable:", k))
    }
  })
  
  np_result <- eventReactive(input$np_run, {
    df <- data(); n <- types()$numeric; k <- types()$categorical
    type <- input$np_type; auto <- input$np_mode == "automatic"
    need_v <- NP_TESTS[[type]]
    validate(need(length(k) >= need_v["cat"] && length(n) >= need_v["num"],
                  "The dataset does not have the variables this test requires."))
    pick <- function(auto_val, id) if (auto) auto_val else input[[id]]
    
    if (type == "Wilcoxon Signed-Rank (paired)") {
      x <- pick(n[1], "np_x"); y <- pick(n[min(2, length(n))], "np_y")
      validate(need(!identical(x, y), "Select two different variables."))
      d <- df[c(x, y)]; d <- d[complete.cases(d), ]
      validate(need(nrow(d) >= 2, "At least two complete paired observations are required."))
      return(list(test = suppressWarnings(wilcox.test(d[[x]], d[[y]], paired = TRUE)),
                  label = sprintf("Wilcoxon signed-rank test: %s vs %s", x, y), effect = NULL, post = NULL))
    }
    
    x <- pick(n[1], "np_x"); g <- pick(k[1], "np_g")
    d <- grouped_xy(df, x, g)
    lv <- levels(d[[g]])
    validate(need(length(lv) >= 2, "The grouping variable must have at least two levels with data."))
    
    if (type == "Mann-Whitney U (2 groups)") {
      validate(need(length(lv) == 2, "This test requires exactly two groups. Use Kruskal-Wallis for 3+ groups."))
      a <- d[[x]][d[[g]] == lv[1]]; b <- d[[x]][d[[g]] == lv[2]]
      wt <- suppressWarnings(wilcox.test(a, b))
      list(test = wt, label = sprintf("Mann-Whitney U test: %s by %s", x, g),
           effect = sprintf("Rank-biserial correlation r = %.3f",
                            1 - (2 * unname(wt$statistic)) / (length(a) * length(b))),
           post = NULL)
    } else {
      kt <- kruskal.test(d[[x]], d[[g]])
      eps2 <- unname(kt$statistic) / (nrow(d) - 1)
      post <- pairwise.wilcox.test(d[[x]], d[[g]], p.adjust.method = "bonferroni")
      list(test = kt, label = sprintf("Kruskal-Wallis test: %s by %s", x, g),
           effect = sprintf("Epsilon squared = %.3f", eps2), post = post)
    }
  })
  
  output$np_results_ui <- renderUI({
    r <- req(np_result())
    tagList(
      panel_card(r$label, verbatimTextOutput("np_print"),
                 if (!is.null(r$effect)) p(tags$b("Effect size: "), r$effect),
                 h5("Interpretation"), uiOutput("np_interp")),
      if (!is.null(r$post)) panel_card("Pairwise Comparisons (Bonferroni-adjusted)", tableOutput("np_post"))
    )
  })
  output$np_print <- renderPrint(np_result()$test)
  output$np_interp <- renderUI({
    p <- np_result()$test$p.value
    alert(if (p < 0.05) "warning" else "success", "Result:",
          sprintf("p = %s. The difference is %sstatistically significant at the 0.05 level.",
                  format.pval(p, digits = 4), if (p < 0.05) "" else "not "))
  })
  output$np_post <- renderTable({
    pm <- np_result()$post$p.value
    d <- as.data.frame(as.table(pm))
    names(d) <- c("Group 1", "Group 2", "p_value")
    d <- d[!is.na(d$p_value), ]
    d$p_value <- format.pval(d$p_value, digits = 4)
    d
  })
  
  
  # ----------------------------------------------------------
  # 8. TIME SERIES ANALYSIS
  # ----------------------------------------------------------
  
  ts_result <- eventReactive(input$ts_run, {
    df <- data(); n <- types()$numeric
    validate(need(length(n) >= 1, "At least one numeric variable is required."))
    auto <- input$ts_mode == "automatic"
    v <- if (auto) n[1] else input$ts_var
    tvar <- if (auto) "" else input$ts_time
    freq <- if (auto) 1 else input$ts_freq
    x <- df[[v]]
    ord <- if (nzchar(tvar) && tvar %in% names(df)) order(df[[tvar]]) else seq_along(x)
    x <- x[ord]; x <- x[is.finite(x)]
    validate(need(length(x) >= 4, "At least 4 valid, ordered observations are required."))
    
    lb <- Box.test(x, lag = max(1, min(10, floor(length(x) / 5))), type = "Ljung-Box")
    decomp <- if (freq > 1 && length(x) >= 2 * freq) {
      tryCatch(decompose(ts(x, frequency = freq)), error = function(e) NULL)
    }
    list(var = v, time = tvar, freq = freq, x = x, n = length(x), lb = lb, decomp = decomp)
  })
  
  output$ts_results_ui <- renderUI({
    r <- req(ts_result())
    tagList(
      panel_card(paste("Time Series:", r$var), plotOutput("ts_line", height = "350px")),
      panel_card("Autocorrelation (ACF / PACF)",
                 fluidRow(column(6, plotOutput("ts_acf", height = "320px")),
                         column(6, plotOutput("ts_pacf", height = "320px"))),
                 uiOutput("ts_lb")),
      if (!is.null(r$decomp)) panel_card("Seasonal Decomposition", plotOutput("ts_decomp", height = "480px"))
      else panel_card("Seasonal Decomposition",
                      alert("info", "Not available:",
                            "set a seasonal frequency greater than 1 (with enough observations) to decompose the series."))
    )
  })
  
  output$ts_line <- renderPlot({
    r <- ts_result()
    d <- data.frame(t = seq_along(r$x), v = r$x)
    ggplot(d, aes(t, v)) + geom_line(colour = ACCENT, linewidth = 1) + geom_point(colour = ACCENT, alpha = 0.6) +
      labs(title = paste(r$var, "over time"), x = if (nzchar(r$time)) r$time else "Observation order", y = r$var) +
      theme_app
  })
  
  output$ts_acf <- renderPlot({
    r <- ts_result(); a <- acf(r$x, plot = FALSE)
    d <- data.frame(lag = as.numeric(a$lag)[-1], acf = as.numeric(a$acf)[-1])
    ci <- qnorm(0.975) / sqrt(r$n)
    ggplot(d, aes(lag, acf)) + geom_col(fill = ACCENT, width = 0.4) +
      geom_hline(yintercept = c(-ci, ci), linetype = "dashed", colour = "#C8553D") +
      labs(title = "ACF", x = "Lag", y = "Autocorrelation") + theme_app
  })
  
  output$ts_pacf <- renderPlot({
    r <- ts_result(); a <- pacf(r$x, plot = FALSE)
    d <- data.frame(lag = as.numeric(a$lag), pacf = as.numeric(a$acf))
    ci <- qnorm(0.975) / sqrt(r$n)
    ggplot(d, aes(lag, pacf)) + geom_col(fill = ACCENT, width = 0.4) +
      geom_hline(yintercept = c(-ci, ci), linetype = "dashed", colour = "#C8553D") +
      labs(title = "PACF", x = "Lag", y = "Partial autocorrelation") + theme_app
  })
  
  output$ts_lb <- renderUI({
    lb <- ts_result()$lb
    alert("info", "Ljung-Box test for autocorrelation:",
          sprintf("p = %s. %s", format.pval(lb$p.value, digits = 4),
                  if (lb$p.value < 0.05) "Significant autocorrelation is present in the series."
                  else "No significant autocorrelation was detected."))
  })
  
  output$ts_decomp <- renderPlot({
    dc <- ts_result()$decomp; req(dc)
    plot(dc)
  })
  
  
  # ----------------------------------------------------------
  # 9. RELIABILITY ANALYSIS (Cronbach's alpha)
  # ----------------------------------------------------------
  
  rel_result <- eventReactive(input$rel_run, {
    n <- types()$numeric
    validate(need(length(n) >= 2, "At least two numeric variables are required."))
    auto <- input$rel_mode == "automatic"
    items <- if (auto) n else input$rel_items
    validate(need(length(items) >= 2, "Select at least two scale items."))
    d <- data()[items]; d <- d[complete.cases(d), ]
    validate(need(nrow(d) >= 3, "At least 3 complete observations are required."))
    flat <- items[vapply(d, function(x) length(unique(x)) <= 1, logical(1))]
    validate(need(!length(flat), paste("Insufficient variation in:", paste(flat, collapse = ", "))))
    cronbach_alpha(d)
  })
  
  output$rel_results_ui <- renderUI({
    r <- req(rel_result())
    tagList(
      div(class = "tiles",
          stat_tile("Cronbach's Alpha", sprintf("%.3f", r$alpha)),
          stat_tile("Items", r$k), stat_tile("N (complete cases)", r$n)),
      panel_card("Interpretation", uiOutput("rel_interp")),
      panel_card("Item Statistics", tableOutput("rel_items_table"))
    )
  })
  
  output$rel_interp <- renderUI({
    a <- rel_result()$alpha
    lvl <- if (a >= .9) "excellent" else if (a >= .8) "good" else if (a >= .7) "acceptable" else
      if (a >= .6) "questionable" else if (a >= .5) "poor" else "unacceptable"
    alert(if (a >= .7) "success" else "warning", "Internal consistency:",
          sprintf("Cronbach's alpha = %.3f, which is generally considered %s.", a, lvl))
  })
  
  output$rel_items_table <- renderTable(rel_result()$items, digits = 3)
  
  
  # ----------------------------------------------------------
  # 10. MULTIVARIATE ANALYSIS (PCA + hierarchical clustering)
  # ----------------------------------------------------------
  
  mv_result <- eventReactive(input$mv_run, {
    n <- types()$numeric
    validate(need(length(n) >= 2, "At least two numeric variables are required."))
    auto <- input$mv_mode == "automatic"
    vars <- if (auto) head(n, MAX_AUTO_MV_VARS) else input$mv_vars
    validate(need(length(vars) >= 2, "Select at least two numeric variables."))
    d <- data()[vars]; d <- d[complete.cases(d), ]
    validate(need(nrow(d) >= length(vars) + 1,
                  "Not enough complete observations relative to the number of variables."))
    validate(need(nrow(d) >= 3, "At least three observations are required for clustering."))
    flat <- vars[vapply(d, function(x) length(unique(x)) <= 1, logical(1))]
    validate(need(!length(flat), paste("Insufficient variation in:", paste(flat, collapse = ", "))))
    
    pca <- prcomp(d, scale. = input$mv_scale)
    hc <- hclust(dist(scale(d)))
    k <- max(2, min(input$mv_k, nrow(d) - 1))
    list(vars = vars, d = d, pca = pca, hc = hc, k = k, clusters = cutree(hc, k))
  })
  
  output$mv_results_ui <- renderUI({
    req(mv_result())
    tagList(
      panel_card("Variance Explained", tableOutput("mv_var_table"), plotOutput("mv_scree", height = "320px")),
      panel_card("Component Loadings", tableOutput("mv_loadings")),
      panel_card("PCA Biplot (PC1 vs PC2, coloured by cluster)", plotOutput("mv_biplot", height = "480px")),
      panel_card("Hierarchical Clustering", plotOutput("mv_dendro", height = "400px"),
                 tableOutput("mv_cluster_sizes"))
    )
  })
  
  output$mv_var_table <- renderTable({
    s <- summary(mv_result()$pca)$importance
    data.frame(Component = colnames(s), t(round(s, 3)), row.names = NULL, check.names = FALSE)
  })
  
  output$mv_scree <- renderPlot({
    s <- summary(mv_result()$pca)$importance
    d <- data.frame(PC = factor(colnames(s), levels = colnames(s)),
                    Variance = s["Proportion of Variance", ], Cumulative = s["Cumulative Proportion", ])
    ggplot(d, aes(PC, Variance, group = 1)) +
      geom_col(fill = ACCENT) + geom_line(aes(y = Cumulative), colour = "#C8553D", linewidth = 1) +
      geom_point(aes(y = Cumulative), colour = "#C8553D") +
      labs(title = "Scree plot", y = "Proportion of variance", x = NULL) + theme_app
  })
  
  output$mv_loadings <- renderTable({
    l <- mv_result()$pca$rotation
    data.frame(Variable = rownames(l), round(l, 3), row.names = NULL)
  }, digits = 3)
  
  output$mv_biplot <- renderPlot({
    r <- mv_result()
    sc <- as.data.frame(r$pca$x[, 1:2, drop = FALSE])
    sc$Cluster <- factor(r$clusters)
    ld <- as.data.frame(r$pca$rotation[, 1:2, drop = FALSE])
    ld$Variable <- rownames(ld)
    mul <- 0.8 * max(abs(sc[c("PC1", "PC2")])) / max(abs(ld[c("PC1", "PC2")]))
    ggplot() +
      geom_point(data = sc, aes(PC1, PC2, colour = Cluster), alpha = 0.7) +
      geom_segment(data = ld, aes(x = 0, y = 0, xend = PC1 * mul, yend = PC2 * mul),
                  arrow = arrow(length = grid::unit(0.2, "cm")), colour = "#33414f") +
      geom_text(data = ld, aes(PC1 * mul * 1.1, PC2 * mul * 1.1, label = Variable),
               size = 3.3, colour = "#33414f") +
      scale_colour_viridis_d(end = 0.85) +
      labs(title = "PCA biplot") + theme_app
  })
  
  output$mv_dendro <- renderPlot({
    r <- mv_result()
    plot(r$hc, main = "Hierarchical clustering dendrogram", xlab = "", sub = "")
    rect.hclust(r$hc, k = r$k, border = ACCENT)
  })
  
  output$mv_cluster_sizes <- renderTable({
    cl <- mv_result()$clusters
    data.frame(Cluster = names(table(cl)), Size = as.integer(table(cl)))
  })
  
  
  # ----------------------------------------------------------
  # 11. OUTLIER DETECTION
  # ----------------------------------------------------------
  
  out_result <- eventReactive(input$out_run, {
    n <- types()$numeric
    validate(need(length(n) >= 1, "At least one numeric variable is required."))
    auto <- input$out_mode == "automatic"
    vars <- if (auto) n else input$out_vars
    validate(need(length(vars) >= 1, "Select at least one numeric variable."))
    list(vars = vars, method = input$out_method, summary = outlier_summary(data(), vars, input$out_method))
  })
  
  output$out_results_ui <- renderUI({
    r <- req(out_result())
    tagList(
      panel_card(paste0("Outlier Summary (", r$method, " method)"), tableOutput("out_table")),
      panel_card("Visual Inspection",
                 selectInput("out_plot_var", "Variable:", r$vars),
                 plotOutput("out_plot", height = "400px"))
    )
  })
  
  output$out_table <- renderTable(out_result()$summary, digits = 2)
  
  output$out_plot <- renderPlot({
    v <- req(input$out_plot_var)
    validate(need(v %in% out_result()$vars, "Generate the summary again for this variable."))
    plot_chart("Boxplot", data(), x = v)
  })
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
