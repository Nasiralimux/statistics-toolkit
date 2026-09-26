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
library(shinyjs)
options(shiny.maxRequestSize = 25 * 1024^2)


# ============================================================
# 2. CONSTANTS & STYLE
# ============================================================

# Shiny's own upload cap defaults to 5 MB regardless of app-level checks;
# raise it to match MAX_UPLOAD_MB (used later in read_uploaded_file()).
options(shiny.maxRequestSize = 25 * 1024^2)

# Logo shown in the header. Set LOGO_URL to ONE of:
#  1) A website link:   "https://yoursite.com/logo.png"
#  2) A Google Drive link: share the image, then use
#     "https://drive.google.com/uc?export=view&id=FILE_ID" (FILE_ID from the share link)
#  3) A local file: put the image in a "www" folder next to app.R, then set e.g. "logo.png"
LOGO_URL <- ""

SAMPLE_DESC <- list(
  iris = list(rows = "150 rows, 5 columns",
              cols = "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width (numeric)",
              target = "Species \u2014 3 classes: setosa, versicolor, virginica",
              note = "Good for classification, ANOVA, correlation."),
  mtcars = list(rows = "32 rows, 11 columns (all numeric)",
                cols = "mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb",
                target = "No fixed target \u2014 mpg is commonly used for regression",
                note = "Good for regression, correlation, PCA."),
  airquality = list(rows = "153 rows, 6 columns",
                    cols = "Ozone, Solar.R, Wind, Temp (numeric), Month, Day",
                    target = "No fixed target \u2014 Ozone is commonly predicted",
                    note = "Contains real missing values (Ozone, Solar.R) \u2014 good for Data Wrangling."),
  PlantGrowth = list(rows = "30 rows, 2 columns",
                     cols = "weight (numeric)",
                     target = "group \u2014 3 levels: ctrl, trt1, trt2",
                     note = "Good for small ANOVA / t-test examples."),
  ToothGrowth = list(rows = "60 rows, 3 columns",
                     cols = "len (numeric), dose (0.5, 1, 2)",
                     target = "supp \u2014 2 levels: OJ, VC",
                     note = "Good for small ANOVA / t-test examples.")
)

ACCENT <- "#1D6F8A"
MAX_AUTO_CORR_VARS <- 12
MAX_AUTO_MV_VARS   <- 15
VIF_FLAG_THRESHOLD <- 5

STAT_FUNS <- list(Mean = mean, Median = median, Sum = sum,
                  Minimum = min, Maximum = max)

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

TESTS <- list(
  "One-Sample t-test"  = c(cat = 0, num = 1),
  "Independent t-test" = c(cat = 1, num = 1),
  "Paired t-test"      = c(cat = 0, num = 2),
  "Chi-Square Test"    = c(cat = 2, num = 0)
)

NP_TESTS <- list(
  "Mann-Whitney U (2 groups)"     = c(cat = 1, num = 1),
  "Kruskal-Wallis (3+ groups)"    = c(cat = 1, num = 1),
  "Wilcoxon Signed-Rank (paired)" = c(cat = 0, num = 2)
)

app_css <- "
:root { color-scheme: light; --ink:#1f2a44; --accent:#1d6f8a; --canvas:#f3f5f7; --line:#dde3ea; --muted:#6b7686; }
body { background:var(--canvas); color:var(--ink);
       font-family:'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif; }
.app-header { position:sticky; top:0; z-index:1000; background:var(--ink); color:#fff;
              padding:14px 28px; margin:-15px -15px 22px;
              border-bottom:3px solid transparent;
              border-image:linear-gradient(90deg,var(--accent),#5fb8d6) 1; }
.app-header-inner { display:grid; grid-template-columns:auto 1fr auto; align-items:center; gap:16px; }
.app-header .app-logo { height:38px; width:auto; }
.app-header .title-block { text-align:center; }
.app-header h1 { margin:0; font-size:24px; font-weight:600; }
.app-header p { margin:2px 0 0; color:#c5cee0; font-size:13px; }
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
.dataTables_wrapper { font-size:13px; }
table.dataTable thead th { background:#f1f5f8; }
.grid-table { background:#fff; }
.grid-table th, .grid-table td { vertical-align:middle; }
.grid-rownum { color:var(--muted); text-align:center; width:36px; }
.grid-table input.grid-cell { width:100%; min-width:90px; border:1px solid var(--line);
             border-radius:3px; padding:4px 6px; font-size:13px; }
.grid-table input.grid-cell:focus { outline:none; border-color:var(--accent);
             box-shadow:0 0 0 2px rgba(29,111,138,0.25); }
.dl-link { font-size:12px; font-weight:normal; float:right; color:var(--muted); }
@media print {
  .no-print, .col-sm-3, .btn, .nav-tabs, .dl-link { display:none !important; }
  .card-box { break-inside:avoid; border:1px solid #ccc; }
  body { background:#fff; }
  .col-sm-9 { width:100% !important; }
}
@media (max-width: 768px) {
  .app-header { padding:10px 14px; }
  .app-header-inner { grid-template-columns:1fr; text-align:center; }
  .app-header h1 { font-size:19px; }
  .app-header .app-logo { margin:0 auto; }
  .card-box { padding:12px 14px; }
  .stat-tile { flex-basis:100%; }
  .shiny-html-output table, .dataTables_wrapper { font-size:12px; }
  .nav-tabs > li > a { padding:8px 10px; font-size:13px; }
}
"

app_js <- "
$(document).on('keydown', 'input.grid-cell', function(e) {
  if (e.key !== 'Enter') return;
  e.preventDefault();
  var input = $(this);
  var row = parseInt(input.data('row'));
  var col = parseInt(input.data('col'));
  Shiny.setInputValue('entry_cell_edit',
    {row: row, col: col, value: input.val()}, {priority: 'event'});
  var nextRow = row + (e.shiftKey ? -1 : 1);
  var next = $('input.grid-cell[data-row=\"' + nextRow + '\"][data-col=\"' + col + '\"]');
  if (next.length) { next.focus(); next[0].select(); }
});
$(document).on('change', 'input.grid-cell', function() {
  var input = $(this);
  Shiny.setInputValue('entry_cell_edit',
    {row: parseInt(input.data('row')), col: parseInt(input.data('col')), value: input.val()},
    {priority: 'event'});
});
"


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

panel_card <- function(title, ..., dl = NULL) {
  head <- if (is.null(dl)) title else tagList(title, downloadLink(paste0(dl, "_dl"), "Export", class = "dl-link"))
  div(class = "card-box", h4(class = "card-title", head), ...)
}

stat_tile <- function(label, value) {
  div(class = "stat-tile", div(class = "stat-value", value), div(class = "stat-label", label))
}

alert <- function(type, title, msg = "") {
  div(class = paste0("alert alert-", type), tags$b(title), " ", msg)
}

run_button <- function(id, label) actionButton(id, label, icon = icon("play"), class = "btn-primary")

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

unavailable_warning <- function(type, need_v, n, k) {
  if (length(k) < need_v["cat"] || length(n) < need_v["num"]) {
    return(alert("warning", "Not available:",
                 sprintf("%s needs at least %d categorical and %d numeric variable(s).",
                         type, need_v["cat"], need_v["num"])))
  }
  NULL
}

render_entry_grid <- function(df) {
  header <- tags$tr(tags$th(""), lapply(names(df), tags$th))
  body <- lapply(seq_len(nrow(df)), function(r) {
    tags$tr(
      tags$td(class = "grid-rownum", r),
      lapply(seq_len(ncol(df)), function(cl) {
        tags$td(tags$input(type = "text", class = "grid-cell form-control input-sm",
                            `data-row` = r - 1, `data-col` = cl - 1, value = df[r, cl]))
      })
    )
  })
  div(class = "scroll-x",
      tags$table(class = "table table-bordered table-condensed grid-table",
                 tags$thead(header), tags$tbody(body)))
}

stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

strength <- function(r) {
  as.character(cut(abs(r), c(0, .1, .3, .5, .7, Inf),
                   c("negligible", "weak", "moderate", "strong", "very strong"),
                   right = FALSE, include.lowest = TRUE))
}

theme_app <- theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))

grouped_xy <- function(df, x, g) {
  d <- df[c(x, g)]
  d <- d[complete.cases(d), ]
  d[[g]] <- droplevels(as.factor(d[[g]]))
  d
}

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

fit_model <- function(df, dv, ivs, logistic = FALSE) {
  validate(
    need(length(dv) == 1 && length(ivs) >= 1,
         "Select one dependent variable and at least one independent variable."),
    need(!(dv %in% ivs), "The dependent variable cannot also be an independent variable.")
  )
  vars <- c(dv, ivs)
  md <- df[complete.cases(df[vars]), vars, drop = FALSE]

  if (logistic) {
    md[[dv]] <- as.factor(md[[dv]])
    validate(need(nlevels(md[[dv]]) == 2, "The dependent variable must have exactly two levels."))
  }

  validate(need(nrow(md) > length(ivs) + 1,
                "Not enough complete observations relative to the number of model parameters."))

  flat <- if (logistic) ivs[vapply(md[ivs], function(x) length(unique(x)) <= 1, logical(1))]
          else vars[vapply(md, function(x) length(unique(x)) <= 1, logical(1))]
  validate(need(!length(flat),
                paste("Insufficient variation in:", paste(flat, collapse = ", "))))

  f <- reformulate(ivs, response = dv)
  if (logistic) return(glm(f, data = md, family = binomial()))

  validate(need(qr(model.matrix(f, md))$rank == length(ivs) + 1,
                paste("Perfect multicollinearity detected among the independent variables.",
                      "The model cannot be estimated reliably.")))
  lm(f, data = md)
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

fit_ridge <- function(model) {
  x <- model.matrix(model)[, -1, drop = FALSE]
  y <- model.response(model.frame(model))
  nf <- max(3, min(10, nrow(x)))
  cvfit <- cv.glmnet(x, y, alpha = 0, standardize = TRUE, nfolds = nf)
  list(lambda = cvfit$lambda.min, coef = as.matrix(coef(cvfit, s = "lambda.min")))
}

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

outlier_bounds <- function(x, method = "IQR") {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(c(lower = NA_real_, upper = NA_real_))
  if (method == "IQR") {
    q <- quantile(x, c(.25, .75), na.rm = TRUE); iqr <- unname(diff(q))
    c(lower = q[[1]] - 1.5 * iqr, upper = q[[2]] + 1.5 * iqr)
  } else {
    m <- mean(x); s <- sd(x)
    if (!is.finite(s) || s == 0) return(c(lower = NA_real_, upper = NA_real_))
    c(lower = m - 3 * s, upper = m + 3 * s)
  }
}

outlier_summary <- function(df, vars, method = "IQR") {
  do.call(rbind, lapply(vars, function(v) {
    x <- df[[v]]; xf <- x[is.finite(x)]
    b <- outlier_bounds(xf, method)
    flag <- is.finite(x) & !is.na(b[["lower"]]) & (x < b[["lower"]] | x > b[["upper"]])
    data.frame(Variable = v, N = length(xf), N_Outliers = sum(flag),
               Pct_Outliers = round(100 * sum(flag) / max(1, length(xf)), 2),
               Lower_Bound = round(b[["lower"]], 3), Upper_Bound = round(b[["upper"]], 3))
  }))
}

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

info <- function(label, tip) tagList(label, tags$abbr(title = tip,
  style = "cursor:help;color:var(--muted);margin-left:3px;text-decoration:none;", "\u24d8"))

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
}

clean_data <- function(df, tokens, trim, redetect, dedup, dropcols, rules, strategy, threshold) {
  dropcols <- intersect(dropcols, names(df))
  if (length(dropcols)) df <- df[, setdiff(names(df), dropcols), drop = FALSE]

  for (v in names(df)) {
    x <- df[[v]]
    if (is.numeric(x)) {
      num_tok <- suppressWarnings(as.numeric(tokens))
      x[x %in% num_tok[!is.na(num_tok)]] <- NA
    } else {
      x <- as.character(x)
      if (trim) x <- trimws(x)
      x[x %in% trimws(tokens)] <- NA
      x[x == ""] <- NA
    }
    df[[v]] <- x
  }

  if (redetect) for (v in names(df)) {
    x <- df[[v]]
    if (is.character(x)) {
      nv <- suppressWarnings(as.numeric(x))
      if (all(is.na(x) == is.na(nv))) df[[v]] <- nv
    }
  }

  if (dedup) df <- df[!duplicated(df), , drop = FALSE]

  rules <- rules[intersect(names(rules), names(df))]
  for (v in names(rules)) {
    r <- rules[[v]]; x <- df[[v]]
    if (identical(r$action, "Drop rows with missing here")) {
      df <- df[!is.na(df[[v]]), , drop = FALSE]; next
    }
    df[[v]] <- switch(r$action,
      "Impute Mean"         = { x[is.na(x)] <- mean(suppressWarnings(as.numeric(x)), na.rm = TRUE); x },
      "Impute Median"       = { x[is.na(x)] <- median(suppressWarnings(as.numeric(x)), na.rm = TRUE); x },
      "Impute Mode"         = { x[is.na(x)] <- mode_value(x); x },
      "Fill with constant"  = { x[is.na(x)] <- r$constant; x },
      x)
  }

  if (identical(strategy, "listwise")) {
    df <- df[complete.cases(df), , drop = FALSE]
  } else if (identical(strategy, "drop_high_missing")) {
    keep <- vapply(df, function(x) mean(is.na(x)) * 100 <= threshold, logical(1))
    df <- df[, keep, drop = FALSE]
  } else if (identical(strategy, "auto_impute")) {
    for (v in names(df)) {
      x <- df[[v]]; if (!anyNA(x)) next
      df[[v]] <- if (is.numeric(x)) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
                 else { x[is.na(x)] <- mode_value(x); x }
    }
  }
  df
}

apply_type_overrides <- function(df, overrides) {
  overrides <- overrides[intersect(names(overrides), names(df))]
  for (v in names(overrides)) {
    df[[v]] <- switch(overrides[[v]],
      "Numeric"     = suppressWarnings(as.numeric(as.character(df[[v]]))),
      "Categorical" = as.character(df[[v]]),
      "Date"        = suppressWarnings(as.Date(as.character(df[[v]]))),
      df[[v]])
  }
  df
}

# Converts text/category columns to numeric so tools that need numeric input
# (correlation, regression, etc.) can use them. Applied last in the pipeline,
# after cleaning and type overrides.
apply_encoding <- function(df, enc) {
  enc <- enc[intersect(names(enc), names(df))]
  for (v in names(enc)) {
    x <- as.character(df[[v]])
    if (identical(enc[[v]], "Label Encode (0,1,2...)")) {
      df[[v]] <- as.integer(factor(x)) - 1L
    } else if (identical(enc[[v]], "One-Hot Encode")) {
      lv <- sort(unique(x[!is.na(x)]))
      for (l in lv) df[[paste0(v, "_", make.names(l))]] <- as.integer(x == l)
      df[[v]] <- NULL
    }
  }
  df
}

run_with_lock <- function(input, btn_id, result_reactive) {
  observeEvent(input[[btn_id]], shinyjs::disable(btn_id), priority = 1000, ignoreInit = TRUE)
  observe({
    tryCatch(result_reactive(), error = function(e) NULL)
    shinyjs::enable(btn_id)
  })
}

odds_ratio_2x2 <- function(tab) {
  m <- tab
  corrected <- any(m == 0)
  if (corrected) m <- m + 0.5
  est <- (m[1, 1] * m[2, 2]) / (m[1, 2] * m[2, 1])
  se  <- sqrt(1 / m[1, 1] + 1 / m[1, 2] + 1 / m[2, 1] + 1 / m[2, 2])
  ci  <- exp(log(est) + c(-1, 1) * qnorm(0.975) * se)
  list(estimate = unname(est), lower = unname(ci[1]), upper = unname(ci[2]),
       corrected = corrected, rows = rownames(tab), cols = colnames(tab))
}

register_table <- function(output, id, data_fun, group = NULL, registry = NULL, ...) {
  output[[id]]           <- renderTable(data_fun(), ...)
  output[[paste0(id, "_dl")]] <- downloadHandler(
    filename = function() paste0(id, ".csv"),
    content = function(file) write.csv(data_fun(), file, row.names = FALSE)
  )
  if (!is.null(registry)) isolate(registry(c(registry(), setNames(list(list(type = "table", group = group, fun = data_fun)), id))))
}
# Wraps a ggplot object so build errors (e.g. quantile "subscript out of
# bounds" on a too-small group) surface as a friendly message instead of
# crashing the whole app; forces the build now, inside the tryCatch.
safe_plot <- function(plot_fun) {
  tryCatch({
    p <- plot_fun()
    ggplot2::ggplot_build(p)
    p
  }, error = function(e) validate(need(FALSE, paste("Could not draw this chart:", conditionMessage(e)))))
}

register_plot <- function(output, id, plot_fun, height = NULL, w = 8, h = 5, group = NULL, registry = NULL) {
  output[[id]] <- renderPlot(safe_plot(plot_fun))
  output[[paste0(id, "_dl")]] <- downloadHandler(
    filename = function() paste0(id, ".png"),
    content = function(file) ggsave(file, plot = safe_plot(plot_fun), width = w, height = h, dpi = 150)
  )
  if (!is.null(registry)) isolate(registry(c(registry(), setNames(list(list(type = "plot", group = group, fun = plot_fun)), id))))
}
register_base_plot <- function(output, id, draw_fun, height = NULL, w = 900, h = 600, group = NULL, registry = NULL) {
  output[[id]] <- renderPlot(tryCatch(draw_fun(),
    error = function(e) validate(need(FALSE, paste("Could not draw this chart:", conditionMessage(e))))))
  output[[paste0(id, "_dl")]] <- downloadHandler(
    filename = function() paste0(id, ".png"),
    content = function(file) { png(file, width = w, height = h); on.exit(dev.off()); draw_fun() }
  )
  if (!is.null(registry)) {
    wrap <- function() { tf <- tempfile(fileext = ".png"); png(tf, width = w, height = h); draw_fun(); dev.off(); tf }
    isolate(registry(c(registry(), setNames(list(list(type = "baseplot", group = group, fun = wrap)), id))))
  }
}

make_group_zip_handler <- function(registry, group_input) {
  function(file) {
    items <- Filter(function(it) identical(it$group, group_input()), registry())
    validate(need(length(items) > 0, "No results yet for this section \u2014 run that tool first."))
    tmp <- tempfile(); dir.create(tmp); paths <- character(0)
    for (nm in names(items)) {
      it <- items[[nm]]
      ok <- tryCatch({
        if (it$type == "table") {
          fp <- file.path(tmp, paste0(nm, ".csv")); write.csv(it$fun(), fp, row.names = FALSE)
        } else if (it$type == "plot") {
          fp <- file.path(tmp, paste0(nm, ".png")); ggsave(fp, plot = it$fun(), width = 8, height = 5, dpi = 150)
        } else {
          src <- it$fun(); fp <- file.path(tmp, paste0(nm, ".png")); file.copy(src, fp)
        }
        paths <<- c(paths, fp); TRUE
      }, error = function(e) FALSE)
    }
    validate(need(length(paths) > 0, "Nothing could be exported."))
    old <- setwd(tmp); on.exit(setwd(old))
    utils::zip(file, basename(paths))
  }
}


# ============================================================
# 4. USER INTERFACE
# ============================================================

welcome_panel <- div(
  class = "card-box welcome",
  h3("Welcome to Statistics Toolkit"),
  p("Upload a CSV, Excel or SPSS file, or enter your own data directly, from the panel on the left to begin."),
  div(class = "tiles",
      stat_tile("Overview", "Preview data and variable types"),
      stat_tile("Descriptives", "Summaries for every variable"),
      stat_tile("Visualizations", "Recommended and custom charts"),
      stat_tile("Statistical tools", "Normality, regression, correlation and more"))
)

entry_modal <- function() {
  modalDialog(
    title = "Manual Data Entry", size = "l", easyClose = FALSE,
    fluidRow(
      column(3, numericInput("entry_rows", "Rows:", 10, 1, 500)),
      column(3, numericInput("entry_cols", "Columns:", 5, 1, 50)),
      column(6, br(), actionButton("entry_reset", "Reset Grid", icon = icon("rotate")))
    ),
    fluidRow(
      column(9, textInput("entry_colnames", "Column names (comma-separated):",
                          placeholder = "e.g. Age, Gender, Income")),
      column(3, br(), actionButton("entry_rename", "Rename Columns"))
    ),
    div(class = "hint-box",
        "Click a cell and type. Press Enter to move to the cell below (Shift+Enter to move up), Tab to move right.",
        "Each column is read as numeric automatically if every entered value in it is numeric; otherwise it is kept as text."),
    fluidRow(
      column(6, actionButton("grid_undo", "Undo", icon = icon("rotate-left"))),
      column(6, actionButton("grid_redo", "Redo", icon = icon("rotate-right")))
    ), br(),
    uiOutput("entry_table"),
    br(),
    fluidRow(
      column(6, actionButton("entry_add_row", "Add Row", icon = icon("plus"))),
      column(6, actionButton("entry_add_col", "Add Column", icon = icon("plus")))
    ),
    uiOutput("entry_msg"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("manual_use", "Use This Data", icon = icon("check"), class = "btn-primary")
    )
  )
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(app_js))),

  div(class = "app-header no-print",
      div(class = "app-header-inner",
          if (nzchar(LOGO_URL)) tags$img(src = LOGO_URL, class = "app-logo") else div(),
          div(class = "title-block", h1("Statistics Toolkit"), p("Explore. Visualize. Analyze.")),
          tags$button("Print this page", class = "btn btn-sm btn-primary", onclick = "window.print()"))),

  sidebarLayout(

    sidebarPanel(
      width = 3,
      h4("Dataset"),
      radioButtons("data_source_choice", NULL,
                   c("Upload a file" = "file", "Enter data manually" = "manual", "Sample dataset" = "sample"),
                   inline = TRUE),
      conditionalPanel(
        "input.data_source_choice == 'file'",
        fileInput("file", "Choose a file", accept = c(".csv", ".xlsx", ".xls", ".sav"),
                  buttonLabel = "Browse...", placeholder = "No file selected"),
        helpText("Supported formats: .csv, .xlsx, .xls, .sav (up to 25 MB)")
      ),
      conditionalPanel(
        "input.data_source_choice == 'manual'",
        actionButton("open_entry", "Open Data Entry Grid", icon = icon("table"), class = "btn-primary"),
        helpText("Build a small dataset by typing values directly into a grid.")
      ),
      conditionalPanel(
        "input.data_source_choice == 'sample'",
        selectInput("sample_choice", "Choose a dataset:",
                    c("Iris (flowers, categorical target)" = "iris",
                      "Motor Trend Cars (mtcars, all numeric)" = "mtcars",
                      "Air Quality (has real missing values)" = "airquality",
                      "Plant Growth (small, one factor)" = "PlantGrowth",
                      "Tooth Growth (small, one factor)" = "ToothGrowth")),
        actionButton("load_sample", "Load Sample Dataset", icon = icon("database"), class = "btn-primary"),
        uiOutput("sample_desc")
      ),
      conditionalPanel("output.loaded", uiOutput("file_info")),
      conditionalPanel(
        "output.loaded",
        hr(),
        h4("Export"),
        selectInput("dl_group_choice", "Download all results from:",
                    c("Data Overview" = "overview", "Descriptives" = "desc", "Visualizations" = "viz",
                      "Normality" = "norm", "Regression" = "reg", "Correlation" = "cor",
                      "Logistic Regression" = "log", "Testing" = "test", "ANOVA" = "anova",
                      "Non-Parametric" = "np", "Time Series" = "ts", "Reliability" = "rel",
                      "Multivariate" = "mv", "Outliers" = "out", "Machine Learning" = "ml")),
        downloadButton("dl_group_btn", "Download All (ZIP)", class = "btn-sm")
      )
    ),

    mainPanel(
      width = 9,
      conditionalPanel("!output.loaded", welcome_panel),

      conditionalPanel("output.loaded", tabsetPanel(
        id = "main_tabs", type = "tabs",

        tabPanel(
          tagList(icon("table"), "Data Overview"), br(),
          uiOutput("summary_tiles"),
          panel_card("Dataset Preview (first 10 rows)", dl = "data_preview",
                     div(class = "scroll-x", tableOutput("data_preview"))),
          panel_card("Variable Overview", div(class = "scroll-x", tableOutput("var_overview")))
        ),

        tabPanel(
          tagList(icon("broom"), "Data Wrangling"), br(),
          uiOutput("wr_summary_tiles"),

          panel_card("Missing-Value Tokens",
            p(class = "text-muted",
              "List any values that actually mean \u201cmissing\u201d in your file (e.g. many public datasets use \u201c?\u201d, \u201c-999\u201d, or the word \u201cunknown\u201d instead of a blank cell)."),
            textInput("wr_tokens", "Treat these values as missing (comma-separated):",
                      value = "?, NA, N/A, na, n/a, unknown, -999"),
            fluidRow(
              column(4, checkboxInput("wr_trim", "Trim leading/trailing spaces in text values", TRUE)),
              column(4, checkboxInput("wr_redetect", "Re-detect numeric columns after cleaning", TRUE)),
              column(4, checkboxInput("wr_dedup", "Remove duplicate rows", FALSE))
            ),
            actionButton("wr_apply", "Apply Cleaning", icon = icon("broom"), class = "btn-primary")
          ),

          panel_card("Missing-Data Summary", tableOutput("wr_missing_table")),

          panel_card("Global Missing-Data Strategy",
            radioButtons("wr_strategy", NULL,
              c("Leave as missing (no action)" = "none",
                "Drop rows with any missing value (listwise deletion)" = "listwise",
                "Impute automatically (mean for numeric, mode for categorical)" = "auto_impute",
                "Drop columns above a missing-value threshold" = "drop_high_missing")),
            conditionalPanel("input.wr_strategy == 'drop_high_missing'",
                             numericInput("wr_threshold", "Drop columns with more than this % missing:", 50, 1, 100)),
            actionButton("wr_apply_strategy", "Apply Strategy", class = "btn-primary")
          ),

          panel_card("Per-Column Rules (override the global strategy for specific columns)",
            fluidRow(
              column(4, selectInput("wr_col", "Column:", choices = NULL)),
              column(4, selectInput("wr_action", "Action:",
                       c("Impute Mean", "Impute Median", "Impute Mode",
                         "Fill with constant", "Drop rows with missing here"))),
              column(4, conditionalPanel("input.wr_action == 'Fill with constant'",
                       textInput("wr_const", "Constant value:")))
            ),
            actionButton("wr_add_rule", "Add Rule", icon = icon("plus")),
            actionButton("wr_clear_rules", "Clear All Rules"),
            br(), br(),
            tableOutput("wr_rules_table")
          ),

          panel_card("Drop Columns",
            selectizeInput("wr_dropcols_sel", "Columns to remove entirely:", choices = NULL, multiple = TRUE),
            actionButton("wr_drop_apply", "Remove Selected Columns", class = "btn-primary")
          ),

          panel_card("Variable Type Override",
            p(class = "text-muted", "Auto-detected types are usually correct; override here only if a column was misread (e.g. a numeric ID column, or a category coded as 0/1/2)."),
            uiOutput("type_override_ui"),
            actionButton("type_apply", "Apply Type Changes", class = "btn-primary")
          ),

          panel_card("Categorical Encoding",
            p(class = "text-muted",
              "Convert text categories into numbers for tools that need numeric input (correlation, regression, etc.). ",
              "Example: Iris' \u201cSpecies\u201d (setosa/versicolor/virginica) can be label- or one-hot encoded here."),
            fluidRow(
              column(5, selectInput("wr_enc_col", "Categorical column:", choices = NULL)),
              column(5, selectInput("wr_enc_type", "Encoding:",
                       c("Label Encode (0,1,2...)", "One-Hot Encode"))),
              column(2, br(), actionButton("wr_enc_add", "Add", icon = icon("plus")))
            ),
            actionButton("wr_enc_clear", "Clear All Encodings"),
            br(), br(),
            tableOutput("wr_enc_table")
          ),

          panel_card("Reset & Export",
            actionButton("wr_reset_all", "Reset All Wrangling", icon = icon("rotate-left")),
            downloadButton("wr_export_csv", "Download Cleaned Dataset (CSV)", class = "btn-sm")
          )
        ),

        tabPanel(
          tagList(icon("calculator"), "Descriptive Statistics"), br(),
          panel_card("Numeric Variables", dl = "numeric_stats",
                     div(class = "scroll-x", tableOutput("numeric_stats"))),
          panel_card("Categorical Variables (top 5 categories)", dl = "categorical_stats",
                     tableOutput("categorical_stats"))
        ),

        tabPanel(
          tagList(icon("chart-bar"), "Visualizations"), br(),
          panel_card("Recommended Visualizations",
                     uiOutput("rec_ui"),
                     plotOutput("rec_plot", height = "480px"), dl = "rec_plot"),
          panel_card("Advanced Visualizations",
                     fluidRow(
                       column(6, selectInput("adv_type", "Visualization type:", names(CHARTS))),
                       column(6, radioButtons("adv_mode", "Variable selection:",
                                              c("Automatic", "Manual"), inline = TRUE))),
                     uiOutput("adv_vars"),
                     actionButton("adv_generate", "Generate Chart", icon = icon("chart-bar"),
                                  class = "btn-primary")),
          conditionalPanel("input.adv_generate > 0",
                           panel_card("Generated Chart", plotOutput("adv_plot", height = "550px"), dl = "adv_plot"))
        ),

        tabPanel(
          tagList(icon("flask"), "Statistical Tools"), br(),
          tabsetPanel(
            id = "tools_tab", type = "tabs",

            tabPanel(
              "Normality Check", br(),
              panel_card("Normality Check Setup",
                         var_panel("norm", selectInput("norm_var", "Numeric variable:", choices = NULL)),
                         run_button("norm_run", "Check Normality"),
                         uiOutput("norm_msg")),
              uiOutput("norm_results_ui")
            ),

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

            tabPanel(
              "Non-Parametric", br(),
              panel_card("Non-Parametric Test Setup",
                         selectInput("np_type", "Test:", names(NP_TESTS)),
                         var_panel("np", uiOutput("np_manual_ui")),
                         run_button("np_run", "Run Test"),
                         uiOutput("np_msg")),
              uiOutput("np_results_ui")
            ),

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
        ),

        tabPanel(
          "Machine Learning", br(),
          panel_card("Machine Learning Setup",
                     selectInput("ml_problem", "Problem type:",
                                 c("Classification", "Regression")),
                     uiOutput("ml_target_ui"),
                     uiOutput("ml_predictors_ui"),
                     fluidRow(
                       column(4, sliderInput("ml_train", "Training proportion:",
                                            min = 0.60, max = 0.90, value = 0.80, step = 0.05)),
                       column(4, numericInput("ml_seed", "Random seed:", 123, min = 1)),
                       column(4, checkboxInput("ml_scale", "Standardize numeric predictors", TRUE))),
                     uiOutput("ml_models_ui"),
                     run_button("ml_run", "Run Machine Learning"),
                     uiOutput("ml_msg")),
          uiOutput("ml_results_ui")
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
  # DATA (file upload OR manual entry)
  # ----------------------------------------------------------

  data_source  <- reactiveVal(NULL)
  manual_data  <- reactiveVal(NULL)
  grid_df      <- reactiveVal(NULL)
  grid_version <- reactiveVal(0)
  grid_history <- reactiveVal(list())
  grid_future  <- reactiveVal(list())
  entry_error  <- reactiveVal(NULL)

  dl_registry    <- reactiveVal(list())
  type_overrides <- reactiveVal(list())
  wr_tokens_v    <- reactiveVal(c("?", "NA", "N/A", "na", "n/a", "unknown", "-999"))
  wr_trim_v      <- reactiveVal(TRUE)
  wr_redetect_v  <- reactiveVal(TRUE)
  wr_dedup_v     <- reactiveVal(FALSE)
  wr_strategy_v  <- reactiveVal("none")
  wr_threshold_v <- reactiveVal(50)
  wr_dropcols_v  <- reactiveVal(character(0))
  wr_rules_v     <- reactiveVal(list())
  wr_encode_v    <- reactiveVal(list())

  # Resets every data-wrangling setting; called whenever a NEW dataset is
  # loaded so stale column names/rules from a previous dataset can't leak in.
  reset_dataset_state <- function() {
    type_overrides(list())
    wr_dropcols_v(character(0))
    wr_rules_v(list())
    wr_encode_v(list())
    wr_tokens_v(c("?", "NA", "N/A", "na", "n/a", "unknown", "-999"))
    wr_trim_v(TRUE); wr_redetect_v(TRUE); wr_dedup_v(FALSE)
    wr_strategy_v("none"); wr_threshold_v(50)
  }

  bump_grid <- function() grid_version(grid_version() + 1)

  set_grid <- function(new_df, redraw = TRUE) {
    h <- grid_history(); h[[length(h) + 1]] <- grid_df()
    if (length(h) > 30) h <- h[-1]
    grid_history(h)
    grid_future(list())
    grid_df(new_df)
    if (redraw) bump_grid()
  }

  observeEvent(input$file, {
    reset_dataset_state()
    data_source("file")
  }, ignoreInit = TRUE)

  MAX_UPLOAD_MB <- 25

  read_uploaded_file <- function(file) {
    tryCatch({
      if (file$size > MAX_UPLOAD_MB * 1024^2) stop(sprintf("File exceeds the %d MB limit.", MAX_UPLOAD_MB))
      df <- switch(
        tolower(tools::file_ext(file$name)),
        csv  = read.csv(file$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
        xlsx = , xls = as.data.frame(read_excel(file$datapath)),
        sav  = as.data.frame(read_sav(file$datapath)),
        stop("Unsupported file format.")
      )
      if (!nrow(df) || !ncol(df)) stop("The file contains no readable rows or columns.")
      names(df)[1] <- sub("^\ufeff", "", names(df)[1])
      names(df) <- make.names(names(df), unique = TRUE)
      df
    }, error = function(e) {
      ext <- toupper(tools::file_ext(file$name))
      showNotification(
        paste0("Could not read \u201c", file$name, "\u201d: ", conditionMessage(e),
               ". Double-check it's a valid ", ext, " file, not password-protected, ",
               "and under ", MAX_UPLOAD_MB, " MB."),
        type = "error", duration = 12)
      NULL
    })
  }

  data_raw <- reactive({
    req(data_source())
    if (data_source() %in% c("manual", "sample")) {
      req(manual_data())
    } else {
      req(input$file)
      withProgress(message = "Reading file...", value = 0.5, req(read_uploaded_file(input$file)))
    }
  })

  # data_pre_encode: cleaned + type-overridden, BEFORE categorical encoding
  # (encoding choices are offered from this stage's text/category columns).
  data_pre_encode <- reactive({
    df <- req(data_raw())
    df <- clean_data(df, wr_tokens_v(), wr_trim_v(), wr_redetect_v(), wr_dedup_v(),
                     wr_dropcols_v(), wr_rules_v(), wr_strategy_v(), wr_threshold_v())
    validate(need(ncol(df) >= 1 && nrow(df) >= 1,
                  "All rows or columns were removed by the current data-wrangling settings."))
    apply_type_overrides(df, type_overrides())
  })

  data <- reactive(apply_encoding(req(data_pre_encode()), wr_encode_v()))

  output$loaded <- reactive(!is.null(data_source()) && !is.null(data_raw()))
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)

  observeEvent(input$clear_data, {
    data_source(NULL); manual_data(NULL); grid_df(NULL)
    reset_dataset_state()
    shinyjs::reset("file")
  })

  output$dl_group_btn <- downloadHandler(
    filename = function() paste0(input$dl_group_choice, "_results.zip"),
    content = make_group_zip_handler(dl_registry, reactive(input$dl_group_choice))
  )

  output$sample_desc <- renderUI({
    req(input$sample_choice)
    d <- SAMPLE_DESC[[input$sample_choice]]
    div(class = "hint-box",
        tags$b(d$rows), tags$br(),
        tags$b("Columns: "), d$cols, tags$br(),
        tags$b("Target: "), d$target, tags$br(),
        tags$em(d$note))
  })

  observeEvent(input$load_sample, {
    reset_dataset_state()
    df <- as.data.frame(get(input$sample_choice))
    names(df) <- make.names(names(df), unique = TRUE)
    manual_data(df)
    data_source("sample")
  })

  # ---- Data Wrangling -----------------------------------------

  observe({
    nm <- names(req(data_raw()))
    updateSelectInput(session, "wr_col", choices = nm)
    updateSelectizeInput(session, "wr_dropcols_sel", choices = nm)
  })

  observe({
    df <- req(data_pre_encode())
    cat_nm <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
    updateSelectInput(session, "wr_enc_col", choices = cat_nm)
  })

  observeEvent(input$wr_enc_add, {
    req(input$wr_enc_col)
    a <- wr_encode_v(); a[[input$wr_enc_col]] <- input$wr_enc_type
    wr_encode_v(a)
  })
  observeEvent(input$wr_enc_clear, wr_encode_v(list()))
  output$wr_enc_table <- renderTable({
    a <- wr_encode_v(); req(length(a) > 0)
    data.frame(Column = names(a), Encoding = unlist(a))
  })

  observeEvent(input$wr_apply, {
    toks <- trimws(strsplit(input$wr_tokens, ",")[[1]])
    wr_tokens_v(toks[nzchar(toks)])
    wr_trim_v(input$wr_trim); wr_redetect_v(input$wr_redetect); wr_dedup_v(input$wr_dedup)
  })

  observeEvent(input$wr_apply_strategy, {
    wr_strategy_v(input$wr_strategy); wr_threshold_v(input$wr_threshold)
  })

  observeEvent(input$wr_add_rule, {
    req(input$wr_col)
    a <- wr_rules_v(); a[[input$wr_col]] <- list(action = input$wr_action, constant = input$wr_const)
    wr_rules_v(a)
  })
  observeEvent(input$wr_clear_rules, wr_rules_v(list()))
  output$wr_rules_table <- renderTable({
    a <- wr_rules_v(); req(length(a) > 0)
    data.frame(Column = names(a), Action = vapply(a, function(x) x$action, character(1)))
  })

  observeEvent(input$wr_drop_apply, {
    req(input$wr_dropcols_sel)
    wr_dropcols_v(union(wr_dropcols_v(), input$wr_dropcols_sel))
  })

  observeEvent(input$wr_reset_all, reset_dataset_state())

  output$wr_summary_tiles <- renderUI({
    raw <- req(data_raw()); df <- req(data())
    div(class = "tiles",
        stat_tile("Rows (raw \u2192 clean)", paste(nrow(raw), "\u2192", nrow(df))),
        stat_tile("Columns (raw \u2192 clean)", paste(ncol(raw), "\u2192", ncol(df))),
        stat_tile("Missing cells (raw)", sum(is.na(raw))),
        stat_tile("Missing cells (clean)", sum(is.na(df))))
  })

  output$wr_missing_table <- renderTable({
    df <- req(data())
    data.frame(Variable = names(df),
               Type = ifelse(vapply(df, is.numeric, logical(1)), "Numeric", "Categorical/Other"),
               Missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
               Pct_Missing = round(100 * vapply(df, function(x) mean(is.na(x)), numeric(1)), 2))
  })

  output$type_override_ui <- renderUI({
    df <- req(data_raw())
    ov <- isolate(type_overrides())
    rows <- lapply(names(df), function(v) {
      cur <- if (!is.null(ov[[v]])) ov[[v]] else "Auto"
      fluidRow(column(6, tags$b(v)),
               column(6, selectInput(paste0("ov_", make.names(v)), NULL,
                                     c("Auto", "Numeric", "Categorical", "Date"), selected = cur)))
    })
    tagList(rows)
  })

  observeEvent(input$type_apply, {
    df <- req(data_raw()); ov <- list()
    for (v in names(df)) {
      val <- input[[paste0("ov_", make.names(v))]]
      if (!is.null(val) && val != "Auto") ov[[v]] <- val
    }
    type_overrides(ov)
  })

  output$wr_export_csv <- downloadHandler(
    filename = function() "cleaned_data.csv",
    content = function(file) write.csv(req(data()), file, row.names = FALSE)
  )

  # ---- Manual data entry grid --------------------------------

  init_grid <- function(nr, nc) {
    d <- as.data.frame(matrix("", nrow = nr, ncol = nc), stringsAsFactors = FALSE)
    names(d) <- paste0("V", seq_len(nc))
    d
  }

  observeEvent(input$open_entry, {
    if (is.null(grid_df())) grid_df(init_grid(10, 5))
    entry_error(NULL)
    bump_grid()
    showModal(entry_modal())
  })

  observeEvent(input$open_entry_again, {
    entry_error(NULL)
    bump_grid()
    showModal(entry_modal())
  })

  observeEvent(input$entry_reset, set_grid(init_grid(input$entry_rows, input$entry_cols)))

  observeEvent(input$entry_rename, {
    d <- req(grid_df())
    nm <- trimws(strsplit(input$entry_colnames, ",")[[1]])
    nm <- nm[nzchar(nm)]
    req(length(nm) > 0)
    n <- ncol(d)
    nm <- if (length(nm) >= n) nm[seq_len(n)] else c(nm, names(d)[(length(nm) + 1):n])
    names(d) <- make.names(nm, unique = TRUE)
    set_grid(d)
  })

  observeEvent(input$entry_add_row, {
    d <- req(grid_df()); d[nrow(d) + 1, ] <- ""
    set_grid(d)
  })

  observeEvent(input$entry_add_col, {
    d <- req(grid_df()); d[[paste0("V", ncol(d) + 1)]] <- ""
    set_grid(d)
  })

  observeEvent(input$grid_undo, {
    h <- grid_history(); req(length(h) > 0)
    f <- grid_future(); f[[length(f) + 1]] <- grid_df(); grid_future(f)
    grid_df(h[[length(h)]]); grid_history(h[-length(h)])
    bump_grid()
  })

  observeEvent(input$grid_redo, {
    f <- grid_future(); req(length(f) > 0)
    h <- grid_history(); h[[length(h) + 1]] <- grid_df(); grid_history(h)
    grid_df(f[[length(f)]]); grid_future(f[-length(f)])
    bump_grid()
  })

  output$entry_table <- renderUI({
    grid_version()
    render_entry_grid(isolate(req(grid_df())))
  })

  observeEvent(input$entry_cell_edit, {
    d <- req(grid_df())
    info <- input$entry_cell_edit
    r <- info$row + 1; c <- info$col + 1
    req(r <= nrow(d), c <= ncol(d))
    d[r, c] <- as.character(info$value)
    set_grid(d, redraw = FALSE)
  })

  output$entry_msg <- renderUI({
    e <- entry_error()
    if (is.null(e)) NULL else alert("danger", "Cannot use this data:", e)
  })

  observeEvent(input$manual_use, {
    d <- req(grid_df())
    d <- d[apply(d, 1, function(r) any(nzchar(trimws(r)))), , drop = FALSE]
    if (!nrow(d) || !ncol(d)) {
      entry_error("add at least one row and one column with data.")
      return()
    }
    mixed <- character(0)
    for (v in names(d)) {
      x <- trimws(d[[v]])
      x[x == ""] <- NA
      num <- suppressWarnings(as.numeric(x))
      is_num <- !all(is.na(x)) && all(is.na(x) == is.na(num))
      if (!is_num && sum(!is.na(x) & is.na(num)) < sum(!is.na(x))) mixed <- c(mixed, v)
      d[[v]] <- if (is_num) num else x
    }
    if (length(mixed)) {
      showNotification(paste("Kept as text (mix of numbers and non-numbers):", paste(mixed, collapse = ", ")),
                       type = "warning", duration = 8)
    }
    names(d) <- make.names(names(d), unique = TRUE)
    entry_error(NULL)
    reset_dataset_state()
    manual_data(d)
    data_source("manual")
    removeModal()
  })

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

  binary_vars <- reactive({
    df <- req(data())
    names(df)[vapply(df, function(x) length(unique(x[!is.na(x)])) == 2, logical(1))]
  })

  output$file_info <- renderUI({
    df <- req(data())
    raw <- req(data_raw())
    lbl <- switch(data_source(),
      manual = tagList(tags$b("Manually entered data"), tags$br(), actionLink("open_entry_again", "Edit this data")),
      sample = tags$b(paste("Sample dataset:", input$sample_choice)),
      tags$b(input$file$name))
    changed <- isTRUE(nrow(df) != nrow(raw)) || isTRUE(ncol(df) != ncol(raw))
    div(class = "hint-box", lbl, tags$br(),
        format(nrow(df), big.mark = ","), " rows \u00d7 ", ncol(df), " columns",
        if (changed) tagList(tags$br(), tags$em(class = "text-muted", "(after data wrangling)")),
        tags$br(), actionButton("clear_data", "Clear Dataset", icon = icon("trash"), class = "btn-sm"))
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
                                           100 * miss / max(1, prod(dim(df))))),
        stat_tile("Duplicate rows", sum(duplicated(df))))
  })

  data_preview_fun <- function() {
    d <- head(req(data()), 10)
    d[] <- lapply(d, as.character)
    d
  }
  register_table(output, "data_preview", data_preview_fun, group = "overview", registry = dl_registry)

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

  numeric_stats_fun <- function() {
    df <- req(data())
    vars <- types()$numeric
    if (!length(vars)) return(data.frame(Message = "No numeric variables detected."))

    res <- vapply(df[vars], function(x) {
      x <- as.numeric(x[!is.na(x)])
      if (!length(x)) return(c(0, rep(NA_real_, 8)))
      tryCatch(
        c(length(x), mean(x), median(x), sd(x), var(x), min(x),
          quantile(x, .25, names = FALSE), quantile(x, .75, names = FALSE), max(x)),
        error = function(e) c(length(x), rep(NA_real_, 8))
      )
    }, numeric(9))

    out <- data.frame(Variable = vars, t(round(res, 3)), row.names = NULL)
    names(out)[-1] <- c("N", "Mean", "Median", "SD", "Variance", "Min", "Q1", "Q3", "Max")
    out$N <- as.integer(out$N)
    out
  }
  register_table(output, "numeric_stats", numeric_stats_fun, digits = 3, group = "desc", registry = dl_registry)

  categorical_stats_fun <- function() {
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
  }
  register_table(output, "categorical_stats", categorical_stats_fun, group = "desc", registry = dl_registry)


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

  rec_plot_fun <- function() {
    df <- req(data())
    type <- req(input$rec_type)
    n <- types()$numeric
    k <- types()$categorical
    by_cat <- type == "Numeric Variable by Category"

    plot_chart(if (by_cat) "Boxplot" else type, df,
               x = if (type == "Bar Chart") k[1] else n[1],
               y = n[2],
               group = if (by_cat) k[1])
  }
  register_plot(output, "rec_plot", rec_plot_fun, height = "480px", group = "viz", registry = dl_registry)


  # ----------------------------------------------------------
  # ADVANCED VISUALIZATIONS
  # ----------------------------------------------------------

  output$adv_vars <- renderUI({
    req(data(), input$adv_type, input$adv_mode)
    n <- types()$numeric
    k <- types()$categorical
    type <- input$adv_type
    need_v <- CHARTS[[type]]

    w <- unavailable_warning(type, need_v, n, k)
    if (!is.null(w)) return(w)

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

  adv_plot_fun <- function() {
    df <- req(data())
    s <- adv_spec()
    validate(need(all(c(s$x, s$y, s$group, s$measures) %in% names(df)),
                  "The dataset has changed. Generate the chart again."))
    do.call(plot_chart, c(list(df = df), s))
  }
  register_plot(output, "adv_plot", adv_plot_fun, height = "550px", group = "viz", registry = dl_registry)


  # ----------------------------------------------------------
  # STATISTICAL TOOLS: shared setup
  # ----------------------------------------------------------

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

  auto_desc <- list(
    norm = function() {
      v <- types()$numeric
      req(length(v) >= 1)
      tagList(tags$b("Automatic rule: "), "the first numeric variable is tested.", tags$br(),
              tags$b("Selected variable: "), v[1])
    },
    reg = function() {
      v <- types()$numeric
      req(length(v) >= 2)
      tagList(tags$b("Automatic rule: "),
              "the first numeric variable is the dependent variable; all other numeric variables are predictors.",
              tags$br(), tags$b("Dependent variable: "), v[1],
              tags$br(), tags$b("Independent variable(s): "), paste(v[-1], collapse = ", "))
    },
    cor = function() {
      v <- head(types()$numeric, MAX_AUTO_CORR_VARS)
      req(length(v) >= 2)
      tagList(tags$b("Automatic rule: "),
              sprintf("all numeric variables are correlated (first %d at most).", MAX_AUTO_CORR_VARS),
              tags$br(), tags$b("Selected variables: "), paste(v, collapse = ", "))
    },
    log = function() {
      v <- binary_vars(); n <- types()$numeric
      req(length(v) >= 1)
      tagList(tags$b("Automatic rule: "),
              "the first binary variable is the outcome; all numeric variables are predictors.",
              tags$br(), tags$b("Dependent variable: "), v[1],
              tags$br(), tags$b("Independent variable(s): "), paste(setdiff(n, v[1]), collapse = ", "))
    },
    test = function() {
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
    },
    anova = function() {
      n <- types()$numeric; k <- types()$categorical
      req(length(n) >= 1, length(k) >= 1)
      tagList(tags$b("Automatic rule: "), "the first numeric variable is compared across the first categorical variable.",
              tags$br(), tags$b("Dependent variable: "), n[1], tags$br(), tags$b("Grouping variable: "), k[1])
    },
    np = function() {
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
    },
    ts = function() {
      n <- types()$numeric
      req(length(n) >= 1)
      tagList(tags$b("Automatic rule: "),
              "the first numeric variable is analyzed in row order (or by the first date variable, if present).",
              tags$br(), tags$b("Variable: "), n[1])
    },
    rel = function() {
      n <- types()$numeric
      req(length(n) >= 2)
      tagList(tags$b("Automatic rule: "), "all numeric variables are treated as scale items.",
              tags$br(), tags$b("Items: "), paste(n, collapse = ", "))
    },
    mv = function() {
      n <- head(types()$numeric, MAX_AUTO_MV_VARS)
      req(length(n) >= 2)
      tagList(tags$b("Automatic rule: "), sprintf("all numeric variables are used (first %d at most).", MAX_AUTO_MV_VARS),
              tags$br(), tags$b("Variables: "), paste(n, collapse = ", "))
    },
    out = function() {
      n <- types()$numeric
      req(length(n) >= 1)
      tagList(tags$b("Automatic rule: "), "outliers are flagged in every numeric variable.",
              tags$br(), tags$b("Variables: "), paste(n, collapse = ", "))
    }
  )
  for (auto_id in names(auto_desc)) {
    local({
      id_ <- auto_id
      output[[paste0(id_, "_auto")]] <- renderUI(auto_desc[[id_]]())
    })
  }


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
      panel_card("Normality Results", tableOutput("norm_table"), dl = "norm_table",
                 h5("Interpretation"), uiOutput("norm_interp")),
      panel_card("Diagnostic Plots", fluidRow(
        column(6, plotOutput("norm_qq", height = "400px")),
        column(6, plotOutput("norm_hist", height = "400px"))))
    )
  })

  norm_table_fun <- function() {
    r <- norm_result()
    data.frame(
      Statistic = c("N", "Mean", "SD", "Skewness", "Excess Kurtosis",
                    "Shapiro-Wilk W", "Shapiro-Wilk p-value"),
      Value = c(as.character(r$n),
                sprintf("%.4f", c(mean(r$x), sd(r$x), r$skew, r$kurt, r$w)),
                format.pval(r$p, digits = 4))
    )
  }
  register_table(output, "norm_table", norm_table_fun, group = "norm", registry = dl_registry)

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
    fit_model(data(),
              dv  = if (auto) num[1]  else input$reg_dv,
              ivs = if (auto) num[-1] else input$reg_iv)
  })

  reg_vif <- reactive(calc_vif(reg_model()))

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
          stat_tile(info("Adjusted R\u00b2", "R\u00b2 penalized for the number of predictors; better for comparing models with different numbers of variables."), sprintf("%.3f", s$adj.r.squared)),
          stat_tile("Residual SE", sprintf("%.3f", s$sigma)),
          stat_tile("F-test p-value",
                    format.pval(pf(f[1], f[2], f[3], lower.tail = FALSE), digits = 3))),
      panel_card("Multicollinearity Check", uiOutput("reg_collinearity")),
      panel_card("Regression Results", verbatimTextOutput("reg_summary")),
      panel_card("Regression Coefficients", tableOutput("reg_coefs"), dl = "reg_coefs"),
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

  reg_coefs_fun <- function() {
    co <- as.data.frame(summary(reg_model())$coefficients)
    co[["Pr(>|t|)"]] <- format.pval(co[["Pr(>|t|)"]], digits = 3, eps = 1e-4)
    cbind(Variable = rownames(co), co, row.names = NULL)
  }
  register_table(output, "reg_coefs", reg_coefs_fun, digits = 4, group = "reg", registry = dl_registry)

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
  # 3. CORRELATION ANALYSIS
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
        dl = "cor_pairs",
        if (multi) tagList(
          div(class = "scroll-x", tableOutput("cor_matrix")),
          p(class = "text-muted", "* p < .05, ** p < .01, *** p < .001"),
          h5("Pairwise tests")),
        div(class = "scroll-x", tableOutput("cor_pairs"))
      ),
      if (multi) panel_card(
        "Correlation Heatmap", dl = "cor_heatmap",
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

  cor_pairs_fun <- function() {
    p <- cor_result()$pairs
    p$N <- as.integer(p$N)
    p$Test_Statistic <- format(round(p$Test_Statistic, 4), trim = TRUE, drop0trailing = TRUE)
    p$P_Value <- format.pval(p$P_Value, digits = 3, eps = 1e-4)
    p
  }
  register_table(output, "cor_pairs", cor_pairs_fun, digits = 4, na = "-", group = "cor", registry = dl_registry)

  cor_heatmap_fun <- function() {
    res <- cor_result()
    hm <- as.data.frame(as.table(res$r), responseName = "r")
    hm$p <- as.vector(res$p)
    hm$Var2 <- factor(hm$Var2, levels = rev(res$vars))
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
  }
  register_plot(output, "cor_heatmap", cor_heatmap_fun, height = "600px", w = 8, h = 7, group = "cor", registry = dl_registry)

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
    fit_model(data(), dv, ivs, logistic = TRUE)
  })

  output$log_results_ui <- renderUI({
    m <- req(log_model())
    nullmod <- glm(m$model[[1]] ~ 1, family = binomial())
    mcfadden <- 1 - as.numeric(logLik(m)) / as.numeric(logLik(nullmod))
    tagList(
      div(class = "tiles",
          stat_tile("Observations", nobs(m)),
          stat_tile(info("McFadden R\u00b2", "Pseudo-R\u00b2 for logistic models; 0.2-0.4 is often considered a good fit (it is not directly comparable to linear-regression R\u00b2)."), sprintf("%.3f", mcfadden)),
          stat_tile(info("AIC", "Akaike Information Criterion: balances model fit against complexity. Lower is better when comparing models on the same data."), sprintf("%.1f", AIC(m))),
          stat_tile("Null Deviance", sprintf("%.1f", m$null.deviance)),
          stat_tile("Residual Deviance", sprintf("%.1f", m$deviance))),
      panel_card("Multicollinearity Check", uiOutput("log_collinearity")),
      panel_card("Regression Results", verbatimTextOutput("log_summary")),
      panel_card("Coefficients & Odds Ratios", tableOutput("log_coefs"), dl = "log_coefs"),
      panel_card("Classification (0.5 cutoff)", tableOutput("log_confmat"), uiOutput("log_accuracy"))
    )
  })

  output$log_collinearity <- renderUI({
    v <- calc_vif(log_model())
    tagList(vif_alert(v), tableOutput("log_vif"))
  })
  output$log_vif <- renderTable(calc_vif(log_model()), digits = 3)

  output$log_summary <- renderPrint(summary(log_model()))

  log_coefs_fun <- function() {
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
  }
  register_table(output, "log_coefs", log_coefs_fun, group = "log", registry = dl_registry)

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
  # 5. HYPOTHESIS TESTING
  # ----------------------------------------------------------

  output$test_manual_ui <- renderUI({
    n <- types()$numeric; k <- types()$categorical; type <- req(input$test_type)
    need_v <- TESTS[[type]]
    w <- unavailable_warning(type, need_v, n, k)
    if (!is.null(w)) return(w)
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
                    or <- if (all(dim(tab) == 2)) odds_ratio_2x2(tab)
                    list(test = suppressWarnings(chisq.test(tab)),
                         label = sprintf("Chi-square test: %s vs %s", x, y), or = or, tab = tab)
                  }
    )
    res
  })

  output$test_results_ui <- renderUI({
    r <- req(test_result())
    tagList(
      panel_card(r$label, verbatimTextOutput("test_print"),
                 if (!is.null(r$or)) uiOutput("test_or"),
                 h5("Interpretation"), uiOutput("test_interp"))
    )
  })
  output$test_print <- renderPrint(test_result()$test)
  output$test_or <- renderUI({
    r <- test_result(); o <- r$or; req(o)
    tagList(
      p(tags$b("Odds ratio: "),
        sprintf("%.3f (95%% CI: %.3f to %.3f)", o$estimate, o$lower, o$upper),
        if (o$corrected) " \u2014 a zero cell was present; a 0.5 correction was applied to all cells."),
      p(class = "text-muted",
        sprintf("Table rows: \u201c%s\u201d vs \u201c%s\u201d; columns: \u201c%s\u201d vs \u201c%s\u201d. The odds ratio compares the odds of \u201c%s\u201d between the two rows.",
                o$rows[1], o$rows[2], o$cols[1], o$cols[2], o$cols[1]))
    )
  })
  output$test_interp <- renderUI({
    p <- test_result()$test$p.value
    alert(if (p < 0.05) "warning" else "success", "Result:",
          sprintf("p = %s. The difference is %sstatistically significant at the 0.05 level.",
                  format.pval(p, digits = 4), if (p < 0.05) "" else "not "))
  })


  # ----------------------------------------------------------
  # 6. ANOVA
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
      panel_card(paste("ANOVA:", r$y, "by", r$g), tableOutput("anova_table"), dl = "anova_table",
                 uiOutput("anova_interp")),
      panel_card("Homogeneity of Variance (Levene's Test)", uiOutput("anova_levene")),
      panel_card("Tukey HSD Post-Hoc Comparisons", tableOutput("anova_tukey"), dl = "anova_tukey"),
      panel_card("Group Distribution", plotOutput("anova_plot", height = "420px"), dl = "anova_plot")
    )
  })

  anova_table_fun <- function() {
    s <- summary(anova_result()$fit)[[1]]
    eta2 <- s[1, "Sum Sq"] / sum(s[["Sum Sq"]])
    data.frame(Source = trimws(rownames(s)), Df = s[["Df"]],
               `Sum Sq` = round(s[["Sum Sq"]], 3), `Mean Sq` = round(s[["Mean Sq"]], 3),
               `F value` = c(round(s[["F value"]][1], 3), NA),
               `p value` = c(format.pval(s[["Pr(>F)"]][1], digits = 4), NA),
               `Eta Squared` = c(round(eta2, 3), NA), check.names = FALSE)
  }
  register_table(output, "anova_table", anova_table_fun, na = "", group = "anova", registry = dl_registry)

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

  anova_tukey_fun <- function() {
    r <- anova_result()
    validate(need(nlevels(r$d[[r$g]]) >= 2, "Not enough groups for post-hoc comparisons."))
    tk <- as.data.frame(TukeyHSD(r$fit)[[1]])
    data.frame(Comparison = rownames(tk), Diff = round(tk$diff, 3),
               Lower = round(tk$lwr, 3), Upper = round(tk$upr, 3),
               `p adj` = format.pval(tk$`p adj`, digits = 4), check.names = FALSE)
  }
  register_table(output, "anova_tukey", anova_tukey_fun, group = "anova", registry = dl_registry)

  anova_plot_fun <- function() {
    r <- anova_result()
    plot_chart("Boxplot", r$d, x = r$y, group = r$g)
  }
  register_plot(output, "anova_plot", anova_plot_fun, height = "420px", group = "anova", registry = dl_registry)


  # ----------------------------------------------------------
  # 7. NON-PARAMETRIC TESTS
  # ----------------------------------------------------------

  output$np_manual_ui <- renderUI({
    n <- types()$numeric; k <- types()$categorical; type <- req(input$np_type)
    need_v <- NP_TESTS[[type]]
    w <- unavailable_warning(type, need_v, n, k)
    if (!is.null(w)) return(w)
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
      if (!is.null(r$post)) panel_card("Pairwise Comparisons (Bonferroni-adjusted)", tableOutput("np_post"), dl = "np_post")
    )
  })
  output$np_print <- renderPrint(np_result()$test)
  output$np_interp <- renderUI({
    p <- np_result()$test$p.value
    alert(if (p < 0.05) "warning" else "success", "Result:",
          sprintf("p = %s. The difference is %sstatistically significant at the 0.05 level.",
                  format.pval(p, digits = 4), if (p < 0.05) "" else "not "))
  })
  np_post_fun <- function() {
    pm <- np_result()$post$p.value
    d <- as.data.frame(as.table(pm))
    names(d) <- c("Group 1", "Group 2", "p_value")
    d <- d[!is.na(d$p_value), ]
    d$p_value <- format.pval(d$p_value, digits = 4)
    d
  }
  register_table(output, "np_post", np_post_fun, group = "np", registry = dl_registry)


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
      panel_card(paste("Time Series:", r$var), plotOutput("ts_line", height = "350px"), dl = "ts_line"),
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

  ts_line_fun <- function() {
    r <- ts_result()
    d <- data.frame(t = seq_along(r$x), v = r$x)
    ggplot(d, aes(t, v)) + geom_line(colour = ACCENT, linewidth = 1) + geom_point(colour = ACCENT, alpha = 0.6) +
      labs(title = paste(r$var, "over time"), x = if (nzchar(r$time)) r$time else "Observation order", y = r$var) +
      theme_app
  }
  register_plot(output, "ts_line", ts_line_fun, height = "350px", group = "ts", registry = dl_registry)

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
  # 9. RELIABILITY ANALYSIS
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
          stat_tile(info("Cronbach's Alpha", "Internal-consistency reliability of the scale items; 0.7+ is generally acceptable, 0.9+ excellent."), sprintf("%.3f", r$alpha)),
          stat_tile("Items", r$k), stat_tile("N (complete cases)", r$n)),
      panel_card("Interpretation", uiOutput("rel_interp")),
      panel_card("Item Statistics", tableOutput("rel_items_table"), dl = "rel_items_table")
    )
  })

  output$rel_interp <- renderUI({
    a <- rel_result()$alpha
    lvl <- if (a >= .9) "excellent" else if (a >= .8) "good" else if (a >= .7) "acceptable" else
      if (a >= .6) "questionable" else if (a >= .5) "poor" else "unacceptable"
    alert(if (a >= .7) "success" else "warning", "Internal consistency:",
          sprintf("Cronbach's alpha = %.3f, which is generally considered %s.", a, lvl))
  })

  register_table(output, "rel_items_table", function() rel_result()$items, digits = 3, group = "rel", registry = dl_registry)


  # ----------------------------------------------------------
  # 10. MULTIVARIATE ANALYSIS
  # ----------------------------------------------------------

  mv_result <- eventReactive(input$mv_run, withProgress(message = "Running PCA & clustering...", value = 0.5, {
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
  }))

  output$mv_results_ui <- renderUI({
    req(mv_result())
    tagList(
      panel_card("Variance Explained", tableOutput("mv_var_table"), plotOutput("mv_scree", height = "320px")),
      panel_card("Component Loadings", tableOutput("mv_loadings"), dl = "mv_loadings"),
      panel_card("PCA Biplot (PC1 vs PC2, coloured by cluster)", plotOutput("mv_biplot", height = "480px"), dl = "mv_biplot"),
      panel_card("Hierarchical Clustering", plotOutput("mv_dendro", height = "400px"), dl = "mv_dendro",
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

  register_table(output, "mv_loadings", function() {
    l <- mv_result()$pca$rotation
    data.frame(Variable = rownames(l), round(l, 3), row.names = NULL)
  }, digits = 3, group = "mv", registry = dl_registry)

  mv_biplot_fun <- function() {
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
  }
  register_plot(output, "mv_biplot", mv_biplot_fun, height = "480px", w = 8, h = 6, group = "mv", registry = dl_registry)

  mv_dendro_fun <- function() {
    r <- mv_result()
    plot(r$hc, main = "Hierarchical clustering dendrogram", xlab = "", sub = "")
    rect.hclust(r$hc, k = r$k, border = ACCENT)
  }
  register_base_plot(output, "mv_dendro", mv_dendro_fun, height = "400px", w = 900, h = 600, group = "mv", registry = dl_registry)

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
      panel_card(paste0("Outlier Summary (", r$method, " method)"), tableOutput("out_table"), dl = "out_table"),
      panel_card("Visual Inspection",
                 selectInput("out_plot_var", "Variable:", r$vars),
                 plotOutput("out_plot", height = "400px"), dl = "out_plot")
    )
  })

  register_table(output, "out_table", function() out_result()$summary, digits = 2, group = "out", registry = dl_registry)

  out_plot_fun <- function() {
    v <- req(input$out_plot_var)
    validate(need(v %in% out_result()$vars, "Generate the summary again for this variable."))
    plot_chart("Boxplot", data(), x = v)
  }
  register_plot(output, "out_plot", out_plot_fun, height = "400px", group = "out", registry = dl_registry)


  for (btn in c("norm_run", "reg_run", "cor_run", "log_run", "test_run", "anova_run",
                "np_run", "ts_run", "rel_run", "mv_run", "out_run")) {
    local({
      b <- btn
      r <- switch(b, norm_run = norm_result, reg_run = reg_model, cor_run = cor_result,
                  log_run = log_model, test_run = test_result, anova_run = anova_result,
                  np_run = np_result, ts_run = ts_result, rel_run = rel_result,
                  mv_run = mv_result, out_run = out_result)
      run_with_lock(input, b, r)
    })
  }

  # ----------------------------------------------------------
  # 12. MACHINE LEARNING
  # ----------------------------------------------------------

  ml_available <- reactive({
    c(
      "Logistic Regression" = TRUE,
      "K-Nearest Neighbors" = requireNamespace("class", quietly = TRUE),
      "Decision Tree" = requireNamespace("rpart", quietly = TRUE),
      "Random Forest" = requireNamespace("randomForest", quietly = TRUE),
      "Support Vector Machine" = requireNamespace("e1071", quietly = TRUE),
      "XGBoost" = requireNamespace("xgboost", quietly = TRUE),
      "Linear Regression" = TRUE,
      "Ridge Regression" = requireNamespace("glmnet", quietly = TRUE),
      "Lasso Regression" = requireNamespace("glmnet", quietly = TRUE)
    )
  })

  output$ml_target_ui <- renderUI({
    df <- req(data())
    if (input$ml_problem == "Classification") {
      tagList(
        selectInput("ml_target", "Target variable (categorical):",
                    choices = names(df), selected = if (length(types()$categorical)) types()$categorical[1] else NULL),
        uiOutput("ml_force_cat_ui")
      )
    } else {
      selectInput("ml_target", "Target variable (numeric):",
                  choices = types()$numeric, selected = if (length(types()$numeric)) types()$numeric[1] else NULL)
    }
  })

  output$ml_force_cat_ui <- renderUI({
    req(input$ml_problem == "Classification", input$ml_target)
    df <- req(data())
    x <- df[[input$ml_target]]
    if (!is.numeric(x)) return(NULL)
    nuniq <- length(unique(x[!is.na(x)]))
    checkboxInput("ml_force_cat",
                  sprintf("Treat \"%s\" as categorical (%d distinct numeric values found)",
                          input$ml_target, nuniq),
                  value = nuniq <= 20)
  })

  output$ml_predictors_ui <- renderUI({
    df <- req(data())
    target <- input$ml_target
    choices <- setdiff(names(df), target)
    selectizeInput("ml_predictors", "Predictor variables:", choices = choices,
                   selected = choices, multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  })

  output$ml_models_ui <- renderUI({
    a <- ml_available()
    if (input$ml_problem == "Classification") {
      models <- c("Logistic Regression", "K-Nearest Neighbors", "Decision Tree",
                  "Random Forest", "Support Vector Machine", "XGBoost")
    } else {
      models <- c("Linear Regression", "Ridge Regression", "Lasso Regression",
                  "Decision Tree", "Random Forest", "Support Vector Machine", "XGBoost")
    }
    disabled <- models[!unname(a[models])]
    tagList(
      checkboxGroupInput("ml_models", "Models to compare:", choices = models,
                         selected = models[unname(a[models])]),
      if (length(disabled))
        helpText("Optional models unavailable because their R package is not installed: ",
                 paste(disabled, collapse = ", "), ".")
    )
  })

  ml_mode_value <- function(x, force_cat = FALSE) {
    if (is.factor(x)) return(x)
    if (is.character(x) || is.logical(x)) return(factor(x))
    if (is.numeric(x) && force_cat) return(factor(x))
    x
  }

  ml_prepare <- function(df, target, predictors, problem, train_prop, seed, scale_numeric, force_cat = FALSE) {
    validate(need(target %in% names(df), "Select a valid target variable."))
    validate(need(length(predictors) >= 1, "Select at least one predictor variable."))
    d <- df[, unique(c(target, predictors)), drop = FALSE]
    names(d) <- make.names(names(d), unique = TRUE)
    names(d)[1] <- "__target__"

    keep <- !is.na(d$`__target__`)
    d <- d[keep, , drop = FALSE]
    validate(need(nrow(d) >= 20, "At least 20 observations are recommended for machine learning."))

    for (v in names(d)[-1]) {
      x <- d[[v]]
      if (is.character(x) || is.factor(x) || is.logical(x)) {
        x <- as.character(x)
        mode_x <- names(sort(table(x[x != "" & !is.na(x)]), decreasing = TRUE))[1]
        x[is.na(x) | x == ""] <- if (length(mode_x)) mode_x else "Missing"
        d[[v]] <- factor(x)
      } else {
        med <- median(x, na.rm = TRUE)
        if (!is.finite(med)) med <- 0
        x[is.na(x)] <- med
        d[[v]] <- x
      }
    }

    if (problem == "Classification") {
      d$`__target__` <- ml_mode_value(d$`__target__`, force_cat)
      validate(need(is.factor(d$`__target__`), "Classification target must be categorical."))
      d$`__target__` <- droplevels(d$`__target__`)
      validate(need(nlevels(d$`__target__`) >= 2, "The classification target needs at least two classes."))
      validate(need(nlevels(d$`__target__`) <= 20, "The target has too many classes for this interface."))
      if (nlevels(d$`__target__`) == 2 && any(table(d$`__target__`) < 2))
        validate(need(FALSE, "Each class must contain at least two observations."))
    } else {
      d$`__target__` <- suppressWarnings(as.numeric(d$`__target__`))
      validate(need(all(is.finite(d$`__target__`)), "Regression target must be numeric."))
    }

    set.seed(seed)
    if (problem == "Classification") {
      idx <- unlist(lapply(split(seq_len(nrow(d)), d$`__target__`), function(ii) {
        ntr <- max(1, min(length(ii) - 1, floor(length(ii) * train_prop)))
        sample(ii, ntr)
      }), use.names = FALSE)
      idx <- sort(unique(idx))
    } else {
      idx <- sample(seq_len(nrow(d)), floor(nrow(d) * train_prop))
    }
    train <- d[idx, , drop = FALSE]
    test <- d[-idx, , drop = FALSE]
    validate(need(nrow(test) >= 5, "The test set must contain at least 5 observations."))

    form <- as.formula("`__target__` ~ .")
    x_train <- model.matrix(form, train)[, -1, drop = FALSE]
    x_test  <- model.matrix(form, test)[, -1, drop = FALSE]
    if (ncol(x_train) == 0) validate(need(FALSE, "No usable predictor columns remain after encoding."))
    common <- intersect(colnames(x_train), colnames(x_test))
    x_train <- x_train[, common, drop = FALSE]
    x_test <- x_test[, common, drop = FALSE]

    if (scale_numeric) {
      mu <- colMeans(x_train)
      sig <- apply(x_train, 2, sd)
      sig[!is.finite(sig) | sig == 0] <- 1
      x_train <- sweep(sweep(x_train, 2, mu, "-"), 2, sig, "/")
      x_test <- sweep(sweep(x_test, 2, mu, "-"), 2, sig, "/")
    }

    list(train = train, test = test, x_train = x_train, x_test = x_test,
         y_train = train$`__target__`, y_test = test$`__target__`,
         predictors = predictors, target = target, problem = problem,
         train_n = nrow(train), test_n = nrow(test))
  }

  ml_fit_one <- function(name, prep) {
    y <- prep$y_train
    xt <- prep$x_train; xv <- prep$x_test
    if (prep$problem == "Classification") {
      pred <- NULL; prob <- NULL; fit <- NULL
      if (name == "Logistic Regression") {
        validate(need(nlevels(y) == 2, "Logistic Regression requires exactly two classes."))
        fit <- glm(y ~ ., data = data.frame(y = y, xt), family = binomial())
        pr <- predict(fit, newdata = data.frame(xv), type = "response")
        lev <- levels(y); pred <- factor(ifelse(pr >= 0.5, lev[2], lev[1]), levels = lev)
        prob <- pr
      } else if (name == "K-Nearest Neighbors") {
        k <- max(3, min(15, floor(sqrt(nrow(xt)))))
        pred <- class::knn(xt, xv, y, k = k)
      } else if (name == "Decision Tree") {
        fit <- rpart::rpart(y ~ ., data = data.frame(y = y, xt), method = "class")
        pred <- predict(fit, newdata = data.frame(xv), type = "class")
      } else if (name == "Random Forest") {
        validate(need(requireNamespace("randomForest", quietly = TRUE), "Install package 'randomForest' to use Random Forest."))
        fit <- randomForest::randomForest(x = xt, y = y, ntree = 300)
        pred <- predict(fit, xv)
      } else if (name == "Support Vector Machine") {
        validate(need(requireNamespace("e1071", quietly = TRUE), "Install package 'e1071' to use SVM."))
        fit <- e1071::svm(x = xt, y = y, probability = TRUE)
        pred <- predict(fit, xv)
        pr <- attr(pred, "probabilities")
        if (!is.null(pr) && nlevels(y) == 2) prob <- pr[, levels(y)[2]]
      } else if (name == "XGBoost") {
        validate(need(requireNamespace("xgboost", quietly = TRUE), "Install package 'xgboost' to use XGBoost."))
        if (nlevels(y) == 2) {
          yy <- as.integer(y) - 1
          fit <- xgboost::xgboost(data = xt, label = yy, objective = "binary:logistic",
                                  nrounds = 100, max_depth = 4, eta = 0.08,
                                  subsample = 0.8, colsample_bytree = 0.8, verbose = 0)
          prob <- as.numeric(predict(fit, xv))
          lev <- levels(y); pred <- factor(ifelse(prob >= 0.5, lev[2], lev[1]), levels = lev)
        } else {
          yy <- as.integer(y) - 1
          fit <- xgboost::xgboost(data = xt, label = yy, objective = "multi:softprob",
                                  num_class = nlevels(y), nrounds = 100, max_depth = 4,
                                  eta = 0.08, subsample = 0.8, colsample_bytree = 0.8, verbose = 0)
          pp <- matrix(as.numeric(predict(fit, xv)), ncol = nlevels(y), byrow = TRUE)
          pred <- factor(levels(y)[max.col(pp)], levels = levels(y))
        }
      }
      pred <- factor(as.character(pred), levels = levels(y))
      cm <- table(Actual = prep$y_test, Predicted = pred)
      acc <- sum(diag(cm)) / sum(cm)
      precision <- mean(sapply(seq_len(nlevels(y)), function(i) {
        den <- sum(cm[, i]); if (!den) NA_real_ else cm[i, i] / den
      }), na.rm = TRUE)
      recall <- mean(sapply(seq_len(nlevels(y)), function(i) {
        den <- sum(cm[i, ]); if (!den) NA_real_ else cm[i, i] / den
      }), na.rm = TRUE)
      f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else NA_real_
      list(model = name, prediction = pred, probability = prob, confusion = cm,
           metrics = c(Accuracy = acc, Precision = precision, Recall = recall, F1 = f1), fit = fit)
    } else {
      fit <- NULL; pred <- NULL
      if (name == "Linear Regression") {
        fit <- lm(y ~ ., data = data.frame(y = y, xt)); pred <- predict(fit, data.frame(xv))
      } else if (name %in% c("Ridge Regression", "Lasso Regression")) {
        alpha <- if (name == "Lasso Regression") 1 else 0
        fit <- glmnet::cv.glmnet(xt, y, alpha = alpha, standardize = FALSE)
        pred <- as.numeric(predict(fit, xv, s = "lambda.min"))
      } else if (name == "Decision Tree") {
        fit <- rpart::rpart(y ~ ., data = data.frame(y = y, xt), method = "anova")
        pred <- as.numeric(predict(fit, data.frame(xv)))
      } else if (name == "Random Forest") {
        validate(need(requireNamespace("randomForest", quietly = TRUE), "Install package 'randomForest' to use Random Forest."))
        fit <- randomForest::randomForest(x = xt, y = y, ntree = 300)
        pred <- as.numeric(predict(fit, xv))
      } else if (name == "Support Vector Machine") {
        validate(need(requireNamespace("e1071", quietly = TRUE), "Install package 'e1071' to use SVM."))
        fit <- e1071::svm(x = xt, y = y)
        pred <- as.numeric(predict(fit, xv))
      } else if (name == "XGBoost") {
        validate(need(requireNamespace("xgboost", quietly = TRUE), "Install package 'xgboost' to use XGBoost."))
        fit <- xgboost::xgboost(data = xt, label = y, objective = "reg:squarederror",
                                nrounds = 100, max_depth = 4, eta = 0.08,
                                subsample = 0.8, colsample_bytree = 0.8, verbose = 0)
        pred <- as.numeric(predict(fit, xv))
      }
      actual <- prep$y_test
      rmse <- sqrt(mean((actual - pred)^2)); mae <- mean(abs(actual - pred))
      r2 <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
      list(model = name, prediction = pred, metrics = c(RMSE = rmse, MAE = mae, R2 = r2), fit = fit)
    }
  }

  ml_result <- eventReactive(input$ml_run, withProgress(message = "Training models...", value = 0, {
    validate(need(length(input$ml_models), "Select at least one model."))
    incProgress(0.15, detail = "Preparing data")
    prep <- ml_prepare(data(), input$ml_target, input$ml_predictors, input$ml_problem,
                       input$ml_train, input$ml_seed, input$ml_scale,
                       force_cat = isTRUE(input$ml_force_cat))
    step <- 0.8 / length(input$ml_models)
    results <- lapply(input$ml_models, function(m) {
      incProgress(step, detail = m)
      tryCatch(ml_fit_one(m, prep), error = function(e) list(model = m, error = conditionMessage(e)))
    })
    ok <- results[vapply(results, function(x) is.null(x$error), logical(1))]
    list(prep = prep, results = ok,
         errors = results[!vapply(results, function(x) is.null(x$error), logical(1))])
  }))
  run_with_lock(input, "ml_run", ml_result)

  output$ml_msg <- renderUI({
    if (input$ml_run < 1) return(NULL)
    r <- req(ml_result())
    if (length(r$errors)) alert("warning", "Some models could not be run:",
                                paste(vapply(r$errors, function(x) paste(x$model, x$error, sep = ": "), character(1)), collapse = " | "))
  })

  output$ml_results_ui <- renderUI({
    r <- req(ml_result())
    validate(need(length(r$results) >= 1, "No selected model completed successfully."))
    if (r$prep$problem == "Classification") {
      tagList(
        panel_card("Model Comparison", div(class = "scroll-x", tableOutput("ml_comparison")), dl = "ml_comparison"),
        panel_card("Confusion Matrix", selectInput("ml_eval_model", "Model:", choices = vapply(r$results, `[[`, character(1), "model")), tableOutput("ml_confusion"), dl = "ml_confusion"),
        panel_card("Classification Performance", plotOutput("ml_metric_plot", height = "380px"), dl = "ml_metric_plot")
      )
    } else {
      tagList(
        panel_card("Model Comparison", div(class = "scroll-x", tableOutput("ml_comparison")), dl = "ml_comparison"),
        panel_card("Actual vs Predicted", plotOutput("ml_reg_plot", height = "420px"), dl = "ml_reg_plot"),
        panel_card("Residuals", plotOutput("ml_resid_plot", height = "420px"), dl = "ml_resid_plot")
      )
    }
  })

  ml_results_for_table <- reactive({
    r <- req(ml_result())
    do.call(rbind, lapply(r$results, function(x) data.frame(Model = x$model, t(round(x$metrics, 4)), check.names = FALSE)))
  })
  register_table(output, "ml_comparison", ml_results_for_table, digits = 4, group = "ml", registry = dl_registry)

  ml_confusion_fun <- function() {
    r <- req(ml_result()); validate(need(r$prep$problem == "Classification", "Classification results are required."))
    x <- r$results[[match(input$ml_eval_model, vapply(r$results, `[[`, character(1), "model"))]]
    cm <- as.data.frame.matrix(x$confusion); cbind(Actual = rownames(cm), cm, row.names = NULL)
  }
  register_table(output, "ml_confusion", ml_confusion_fun, group = "ml", registry = dl_registry)

  ml_metric_plot_fun <- function() {
    d <- ml_results_for_table()
    m <- do.call(rbind, lapply(setdiff(names(d), "Model"), function(v) data.frame(Model = d$Model, Metric = v, Score = d[[v]])))
    ggplot(m, aes(Model, Score, fill = Metric)) + geom_col(position = "dodge") +
      coord_cartesian(ylim = c(0, 1)) + labs(x = NULL, y = "Score", title = "Classification performance") +
      theme_app + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  }
  register_plot(output, "ml_metric_plot", ml_metric_plot_fun, height = "380px", w = 9, h = 5, group = "ml", registry = dl_registry)

  ml_reg_plot_fun <- function() {
    r <- req(ml_result()); x <- r$results[[1]]
    ggplot(data.frame(Actual = r$prep$y_test, Predicted = x$prediction), aes(Actual, Predicted)) +
      geom_point(alpha = .7) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = paste(x$model, "\u2014 Actual vs Predicted")) + theme_app
  }
  register_plot(output, "ml_reg_plot", ml_reg_plot_fun, height = "420px", group = "ml", registry = dl_registry)

  ml_resid_plot_fun <- function() {
    r <- req(ml_result()); x <- r$results[[1]]
    ggplot(data.frame(Fitted = x$prediction, Residual = r$prep$y_test - x$prediction), aes(Fitted, Residual)) +
      geom_point(alpha = .7) + geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste(x$model, "\u2014 Residual Plot")) + theme_app
  }
  register_plot(output, "ml_resid_plot", ml_resid_plot_fun, height = "420px", group = "ml", registry = dl_registry)

}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
