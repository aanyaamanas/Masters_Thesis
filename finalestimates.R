# =============================================================================
# FINAL TABLES 1-4 — Aanyaa Manas | UPenn MS Criminology | May 2026
# Supervisor: Aaron Chalfin
#
# SPECIFICATION (per Aaron's final instructions):
#   - No clustering, no HC2
#   - Dependent variable: log(hits) where no zeros present
#   - Dependent variable: Poisson GLM where zeros are present
#   - Standard OLS standard errors throughout
#   - RDiT on aggregated group×week series (one series per group)
#   - G3 uses weekly data
#   - Table 4 includes POST1+POST2+POST3 main effects + interactions
# =============================================================================

rm(list = ls()); gc()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(flextable, officer, dplyr, lubridate, lmtest, sandwich)

# ---------------------------------------------------------------------------
# EVENT DATES
# ---------------------------------------------------------------------------
date_imagine <- as.Date("2024-07-29")
date_spicy   <- as.Date("2025-07-27")
date_policy  <- as.Date("2026-01-15")

SIG_LINE <- "* p < 0.05, ** p < 0.01, *** p < 0.001"


# =============================================================================
# =============================================================================
# =============================================================================
# =============================================================================
# IRR TABLES 
# Converts Poisson coefficients to Incidence Rate Ratios (exp(coef))
# IRR interpretation: IRR of 2.0 means the rate doubled; 0.5 means halved
# OLS log coefficients also exponentiated for consistency
# Adjusted R² row removed where not applicable (Poisson models)
# Table 2 footnote made clearer
# =============================================================================
# =============================================================================
# =============================================================================
# =============================================================================

# =============================================================================
# HELPERS
# =============================================================================

fmt_est <- function(est, pval, digits = 3) {
  sig <- ifelse(pval < 0.001, "***",
                ifelse(pval < 0.01,  "**",
                       ifelse(pval < 0.05,  "*", "")))
  paste0(round(est, digits), sig)
}
fmt_se <- function(se, digits = 3) paste0("(", round(se, digits), ")")

# Works for both lm and glm objects
extract_coef <- function(mod, term, digits = 3) {
  cf <- summary(mod)$coefficients
  if (!term %in% rownames(cf))
    return(list(est = "—", se = "", raw_est = NA, raw_p = NA))
  est  <- cf[term, "Estimate"]
  se   <- cf[term, "Std. Error"]
  # glm uses z, lm uses t — both stored in column 4
  pval <- cf[term, 4]
  list(est     = fmt_est(est, pval, digits),
       se      = fmt_se(se, digits),
       raw_est = est,
       raw_p   = pval)
}

get_n <- function(mod) {
  tryCatch(as.character(nobs(mod)),
           error = function(e) as.character(nrow(model.frame(mod))))
}

get_r2 <- function(mod) {
  s <- summary(mod)
  if (!is.null(s$r.squared))
    return(list(r2    = as.character(round(s$r.squared,     3)),
                adjr2 = as.character(round(s$adj.r.squared, 3))))
  # For GLM use McFadden pseudo-R2
  pr2 <- round(1 - s$deviance / s$null.deviance, 3)
  list(r2 = paste0(pr2, " (McF)"), adjr2 = "—")
}

build_df <- function(row_specs, col_names) {
  rows <- list()
  for (r in row_specs) {
    rows[[length(rows)+1]] <- c(r$label, r$values)
    if (!is.null(r$se) && any(nchar(trimws(r$se)) > 0))
      rows[[length(rows)+1]] <- c("", r$se)
  }
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- col_names
  df
}

se_rows <- function(df) which(df[[1]] == "")

apa_ft <- function(ft, col_labels, se_idx, gof_row, title, note,
                   w1 = 3.5, wn = 1.9) {
  nc <- ncol_keys(ft)
  ft <- do.call(set_header_labels, c(list(ft), col_labels))
  ft |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 11, part = "all") |>
    bold(part = "header") |>
    italic(i = se_idx, part = "body") |>
    align(align = "left",   j = 1,         part = "all") |>
    align(align = "center", j = seq(2, nc), part = "all") |>
    border_remove() |>
    hline_top(part = "header",
              border = fp_border(color = "black", width = 1.5)) |>
    hline_bottom(part = "header",
                 border = fp_border(color = "black", width = 0.75)) |>
    hline_bottom(part = "body",
                 border = fp_border(color = "black", width = 1.5)) |>
    hline(i = gof_row - 1,
          border = fp_border(color = "#888888", width = 0.5),
          part = "body") |>
    padding(padding.top = 3, padding.bottom = 3,
            padding.left = 5, padding.right = 5, part = "all") |>
    add_footer_lines(SIG_LINE) |>
    add_footer_lines(paste0("Note. ", note)) |>
    merge_at(i = 1, j = seq_len(ncol_keys(ft)), part = "footer") |>
    merge_at(i = 2, j = seq_len(ncol_keys(ft)), part = "footer") |>
    font(fontname = "Times New Roman", part = "footer") |>
    fontsize(size = 10, part = "footer") |>
    italic(part = "footer") |>
    align(align = "left", part = "footer") |>
    set_caption(
      caption = as_paragraph(
        as_chunk(title,
                 props = fp_text(bold = TRUE, italic = TRUE,
                                 font.family = "Times New Roman",
                                 font.size = 11))
      ),
      align_with_table = FALSE,
      fp_p = fp_par(text.align = "center")
    ) |>
    width(j = 1,         width = w1) |>
    width(j = seq(2, nc), width = wn)
}

# =============================================================================
# SECTION 1: LOAD DATA
# =============================================================================

cat("=== LOADING DATA ===\n")

# G1: raw file with both Grok + General AI keywords
g1 <- read.csv("main_comp_g1/Analysis/grok_ncii_google_trends_raw.csv",
               stringsAsFactors = FALSE) |>
  mutate(
    date        = as.Date(date),
    hits        = as.numeric(ifelse(hits == "<1", 0.5, hits)),
    treatment   = as.integer(platform == "Grok"),
    post_event1 = as.integer(date >= date_imagine),
    post_event2 = as.integer(date >= date_spicy),
    post_event3 = as.integer(date >= date_policy)
  )

# G3: weekly file
g3_raw <- read.csv("comp_g3/comparison3_weekly_data_updated.csv",
                   stringsAsFactors = FALSE) |>
  mutate(
    date        = as.Date(date),
    hits        = as.numeric(ifelse(hits == "<1", 0.5, hits)),
    post_event1 = as.integer(date >= date_imagine)
  )

# Detect treatment in G3
if ("platform_binary" %in% names(g3_raw)) {
  g3_raw <- g3_raw |> mutate(treatment = as.integer(platform_binary == 1))
} else {
  g3_raw <- g3_raw |>
    mutate(treatment = as.integer(
      grepl("NCII|ncii|nudify|deepfake|undress|remov|clothes",
            platform, ignore.case = TRUE)))
}

# G2: monthly
g2_raw <- read.csv("comp_g2/Analysis2/dataset/G2_keyword_monthly_clean.csv",
                   stringsAsFactors = FALSE)
if (!"group" %in% names(g2_raw)) g2_raw$group <- g2_raw$platform
g2_raw <- g2_raw |>
  mutate(
    date   = as.Date(date),
    hits   = as.numeric(ifelse(hits == "<1", 0.5, hits)),
    time_c = as.numeric(date - date_spicy) / 7,
    post   = as.integer(date >= date_spicy)
  )
priv_label  <- unique(g2_raw$group)[grepl("Privacy|privacy",  unique(g2_raw$group), ignore.case=TRUE)][1]
adult_label <- unique(g2_raw$group)[grepl("Adult|adult|Main", unique(g2_raw$group), ignore.case=TRUE)][1]

# Check zeros
cat(sprintf("G1  zeros in hits: %d / %d (%.1f%%)\n",
            sum(g1$hits == 0), nrow(g1), 100*mean(g1$hits == 0)))
cat(sprintf("G3  zeros in hits: %d / %d (%.1f%%)\n",
            sum(g3_raw$hits == 0), nrow(g3_raw), 100*mean(g3_raw$hits == 0)))
cat(sprintf("G2  zeros in hits: %d / %d (%.1f%%)\n",
            sum(g2_raw$hits == 0), nrow(g2_raw), 100*mean(g2_raw$hits == 0)))

# =============================================================================
# SECTION 2: AGGREGATE G3 + G2 FOR RDIT (one series per group×week)
# =============================================================================

cat("\n=== AGGREGATING FOR RDIT ===\n")

g3_agg <- g3_raw |>
  group_by(date, treatment) |>
  summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop") |>
  mutate(
    time_c = as.numeric(date - date_imagine) / 7,
    post   = as.integer(date >= date_imagine)
  )

g2_agg <- g2_raw |>
  group_by(date, group) |>
  summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop") |>
  mutate(
    time_c = as.numeric(date - date_spicy) / 7,
    post   = as.integer(date >= date_spicy)
  )

# Check zeros in aggregated series
cat(sprintf("G3 agg zeros: %d / %d\n", sum(g3_agg$hits == 0), nrow(g3_agg)))
cat(sprintf("G2 agg zeros: %d / %d\n", sum(g2_agg$hits == 0), nrow(g2_agg)))

# =============================================================================
# SECTION 3: FIT MODELS
# Decision rule:
#   - If zeros present → Poisson GLM (log link, handles count/zero data)
#   - If no zeros      → OLS on log(hits) (natural log, not log1p)
# =============================================================================

cat("\n=== FITTING MODELS ===\n")

fit_model <- function(formula, data, label) {
  zeros <- sum(data[[as.character(formula[[2]])[1]]] == 0, na.rm = TRUE)
  if (zeros > 0) {
    cat(sprintf("  %s: %d zeros → POISSON\n", label, zeros))
    mod <- glm(formula, data = data, family = poisson(link = "log"))
    attr(mod, "model_type") <- "poisson"
  } else {
    cat(sprintf("  %s: no zeros → log OLS\n", label))
    # Add log outcome on the fly
    outcome_var <- as.character(formula[[2]])
    data[[paste0("log_", outcome_var)]] <- log(data[[outcome_var]])
    new_formula <- update(formula,
                          as.formula(paste0("log_", outcome_var, " ~ .")))
    mod <- lm(new_formula, data = data)
    attr(mod, "model_type") <- "log_ols"
  }
  mod
}

# --- TABLE 1 ---
m1_g1 <- fit_model(hits ~ treatment + post_event2 + treatment:post_event2,
                   g1, "T1 G1")
m1_g3 <- fit_model(hits ~ treatment + post_event1 + treatment:post_event1,
                   g3_raw, "T1 G3")

# --- TABLE 2 --- quadratic RDiT 
# Spec: Y ~ time_c + time_c^2 + post + time_c*post + time_c^2*post
# Allows curvature in both pre- and post-event trajectories
g3_ncii  <- filter(g3_agg, treatment == 1)
g3_main  <- filter(g3_agg, treatment == 0)
m2_ncii  <- fit_model(hits ~ time_c + I(time_c^2) + post +
                        time_c:post + I(time_c^2):post,
                      g3_ncii, "T2 NCII")
m2_main  <- fit_model(hits ~ time_c + I(time_c^2) + post +
                        time_c:post + I(time_c^2):post,
                      g3_main, "T2 Main")

# --- TABLE 3 --- quadratic RDiT per Aaron's suggestion
g2_priv  <- filter(g2_agg, group == priv_label)
g2_adult <- filter(g2_agg, group == adult_label)
m3_priv  <- fit_model(hits ~ time_c + I(time_c^2) + post +
                        time_c:post + I(time_c^2):post,
                      g2_priv,  "T3 Priv")
m3_adult <- fit_model(hits ~ time_c + I(time_c^2) + post +
                        time_c:post + I(time_c^2):post,
                      g2_adult, "T3 Adult")

# --- TABLE 4 ---
m4 <- fit_model(
  hits ~ treatment + post_event1 + post_event2 + post_event3
  + treatment:post_event1 + treatment:post_event2 + treatment:post_event3,
  g1, "T4"
)

cat("\nAll models fitted.\n")

# Model type labels for footnotes
type_label <- function(mod) {
  if (attr(mod, "model_type") == "poisson") "Poisson GLM (log link)"
  else "OLS on log(hits)"
}

cat(sprintf("\nModel types:\n"))
cat(sprintf("  T1 G1: %s\n", type_label(m1_g1)))
cat(sprintf("  T1 G3: %s\n", type_label(m1_g3)))
cat(sprintf("  T2 NCII: %s\n", type_label(m2_ncii)))
cat(sprintf("  T2 Main: %s\n", type_label(m2_main)))
cat(sprintf("  T3 Priv: %s\n", type_label(m3_priv)))
cat(sprintf("  T3 Adult: %s\n", type_label(m3_adult)))
cat(sprintf("  T4: %s\n", type_label(m4)))

# Key T4 diagnostics
cat("\n=== TABLE 4 KEY COEFFICIENTS ===\n")
t4_terms <- c("post_event1","post_event2","post_event3",
              "treatment:post_event1","treatment:post_event2","treatment:post_event3")
for (tm in t4_terms) {
  x <- extract_coef(m4, tm)
  cat(sprintf("  %-35s  %s  %s\n", tm, x$est, x$se))
}

# =============================================================================
# SECTION 4: BUILD TABLES
# =============================================================================

cat("\n=== BUILDING TABLES ===\n")

# helper — outcome label for footnote
outcome_note <- function(mod) {
  if (attr(mod, "model_type") == "poisson")
    "Poisson GLM with log link. Coefficients are log incidence-rate ratios; exp(coef) \u2212 1 gives the proportional change."
  else
    "OLS estimated on log(hits). Coefficients approximate proportional changes; exp(coef) \u2212 1 gives the exact proportional effect."
}

# ── TABLE 1 ────────────────────────────────────────────────────────────────

t1_df <- build_df(list(
  list(label  = "Constant (\u03b2\u2080)",
       values = c(extract_coef(m1_g1,"(Intercept)")$est,
                  extract_coef(m1_g3,"(Intercept)")$est),
       se     = c(extract_coef(m1_g1,"(Intercept)")$se,
                  extract_coef(m1_g3,"(Intercept)")$se)),
  list(label  = "Treatment \u2014 TREAT (\u03b2\u2081)",
       values = c(extract_coef(m1_g1,"treatment")$est,
                  extract_coef(m1_g3,"treatment")$est),
       se     = c(extract_coef(m1_g1,"treatment")$se,
                  extract_coef(m1_g3,"treatment")$se)),
  list(label  = "Post-Launch \u2014 POST (\u03b2\u2082)",
       values = c(extract_coef(m1_g1,"post_event2")$est,
                  extract_coef(m1_g3,"post_event1")$est),
       se     = c(extract_coef(m1_g1,"post_event2")$se,
                  extract_coef(m1_g3,"post_event1")$se)),
  list(label  = "Treatment \u00d7 Post \u2014 DiD Estimator (\u03b2\u2083)",
       values = c(extract_coef(m1_g1,"treatment:post_event2")$est,
                  extract_coef(m1_g3,"treatment:post_event1")$est),
       se     = c(extract_coef(m1_g1,"treatment:post_event2")$se,
                  extract_coef(m1_g3,"treatment:post_event1")$se)),
  list(label  = "Observations",
       values = c(get_n(m1_g1), get_n(m1_g3)), se = NULL),
  list(label  = "R\u00b2 / Pseudo-R\u00b2",
       values = c(get_r2(m1_g1)$r2, get_r2(m1_g3)$r2), se = NULL),
  list(label  = "Adjusted R\u00b2",
       values = c(get_r2(m1_g1)$adjr2, get_r2(m1_g3)$adjr2), se = NULL)
), c("Variable","G1","G3"))

ft1 <- flextable(t1_df) |>
  apa_ft(
    col_labels = list(Variable = "",
                      G1 = "Group 1: Grok vs.\nGeneral AI",
                      G3 = "Group 3: NCII Tools vs.\nMainstream AI"),
    se_idx  = se_rows(t1_df),
    gof_row = which(t1_df[[1]] == "Observations"),
    title   = "Table 1. Difference-in-Differences Analysis of Grok-Attributable Search Behaviour",
    note    = paste0(
      "Dependent variable: Google Trends Search Volume Index, logged. ",
      "Group 1 (N=5420): 20 keywords (10 Grok + 10 General AI), weekly; ",
      "intervention = Spicy Mode (July 27, 2025). ",
      "Group 3 (N=2959): 11 keywords, weekly; ",
      "intervention = Grok Imagine (July 29, 2024). ",
      "Where zeros are present in hits, Poisson GLM is used (log link); ",
      "otherwise OLS on log(hits). ",
      "Standard errors are conventional (no clustering). ",
      "\u03b2\u2083 in Group 1 is positive (+2.524***): on a multiplicative scale, Grok-specific ",
      "searches grew proportionally more than General AI after Spicy Mode, ",
      "consistent with Grok emerging from a near-zero base. ",
      "See Table 4 for event-by-event Grok-specific effects."
    ),
    w1 = 3.6, wn = 2.0
  )
cat("Table 1 done.\n")

# ── TABLE 2 ────────────────────────────────────────────────────────────────

t2_df <- build_df(list(
  list(label  = "Intercept (\u03b2\u2080) \u2014 log search volume at intervention",
       values = c(extract_coef(m2_ncii,"(Intercept)")$est,
                  extract_coef(m2_main,"(Intercept)")$est),
       se     = c(extract_coef(m2_ncii,"(Intercept)")$se,
                  extract_coef(m2_main,"(Intercept)")$se)),
  list(label  = "Time \u2014 Pre-trend (\u03b2\u2081, per week)",
       values = c(extract_coef(m2_ncii,"time_c")$est,
                  extract_coef(m2_main,"time_c")$est),
       se     = c(extract_coef(m2_ncii,"time_c")$se,
                  extract_coef(m2_main,"time_c")$se)),
  list(label  = "Post-Launch \u2014 Level shift (\u03b2\u2082)",
       values = c(extract_coef(m2_ncii,"post")$est,
                  extract_coef(m2_main,"post")$est),
       se     = c(extract_coef(m2_ncii,"post")$se,
                  extract_coef(m2_main,"post")$se)),
  list(label  = "Time \u00d7 Post \u2014 Slope change (\u03b2\u2083)",
       values = c(extract_coef(m2_ncii,"time_c:post")$est,
                  extract_coef(m2_main,"time_c:post")$est),
       se     = c(extract_coef(m2_ncii,"time_c:post")$se,
                  extract_coef(m2_main,"time_c:post")$se)),
  list(label  = "Time\u00b2 \u2014 Pre-trend curvature (\u03b2\u2084)",
       values = c(extract_coef(m2_ncii,"I(time_c^2)")$est,
                  extract_coef(m2_main,"I(time_c^2)")$est),
       se     = c(extract_coef(m2_ncii,"I(time_c^2)")$se,
                  extract_coef(m2_main,"I(time_c^2)")$se)),
  list(label  = "Time\u00b2 \u00d7 Post \u2014 Post-trend curvature (\u03b2\u2085)",
       values = c(extract_coef(m2_ncii,"I(time_c^2):post")$est,
                  extract_coef(m2_main,"I(time_c^2):post")$est),
       se     = c(extract_coef(m2_ncii,"I(time_c^2):post")$se,
                  extract_coef(m2_main,"I(time_c^2):post")$se)),
  list(label  = "Observations",
       values = c(get_n(m2_ncii), get_n(m2_main)), se = NULL),
  list(label  = "R\u00b2 / Pseudo-R\u00b2",
       values = c(get_r2(m2_ncii)$r2, get_r2(m2_main)$r2), se = NULL),
  list(label  = "Adjusted R\u00b2",
       values = c(get_r2(m2_ncii)$adjr2, get_r2(m2_main)$adjr2), se = NULL),
  list(label  = "Estimator",
       values = c(type_label(m2_ncii), type_label(m2_main)), se = NULL)
), c("Variable","NCII","Mainstream"))

ft2 <- flextable(t2_df) |>
  apa_ft(
    col_labels = list(Variable   = "",
                      NCII       = "NCII-Specific Tools",
                      Mainstream = "Mainstream AI Platforms"),
    se_idx  = se_rows(t2_df),
    gof_row = which(t2_df[[1]] == "Observations"),
    title   = "Table 2. Regression Discontinuity in Time: NCII-Specific Tools vs. Mainstream AI Platforms",
    note    = paste0(
      "Dependent variable: Google Trends Search Volume Index, logged. ",
      "RDiT estimated on group\u00d7week aggregated means (keywords averaged within ",
      "group per week), producing one series per group as the method requires. ",
      "Time is centred at zero at Grok Imagine (July 29, 2024). ",
      "\u03b2\u2080 = log search volume at exact intervention date; ",
      "\u03b2\u2081 = pre-existing weekly log trend; ",
      "\u03b2\u2082 = immediate log level shift at intervention; ",
      "\u03b2\u2083 = change in weekly log growth rate post-intervention. ",
      "Quadratic RDiT specification (per reviewer suggestion): Y = \u03b2\u2080 + \u03b2\u2081(Time) + \u03b2\u2082(POST) + \u03b2\u2083(Time\u00d7POST) + \u03b2\u2084(Time\u00b2) + \u03b2\u2085(Time\u00b2\u00d7POST). The quadratic terms allow curvature in both pre- and post-event trajectories. Standard errors are conventional (no clustering)."
    ),
    w1 = 3.9, wn = 1.9
  )
cat("Table 2 done.\n")

# ── TABLE 3 ────────────────────────────────────────────────────────────────

t3_df <- build_df(list(
  list(label  = "Intercept (\u03b2\u2080) \u2014 log search volume at intervention",
       values = c(extract_coef(m3_priv,"(Intercept)")$est,
                  extract_coef(m3_adult,"(Intercept)")$est),
       se     = c(extract_coef(m3_priv,"(Intercept)")$se,
                  extract_coef(m3_adult,"(Intercept)")$se)),
  list(label  = "Time \u2014 Pre-trend (\u03b2\u2081, per week)",
       values = c(extract_coef(m3_priv,"time_c")$est,
                  extract_coef(m3_adult,"time_c")$est),
       se     = c(extract_coef(m3_priv,"time_c")$se,
                  extract_coef(m3_adult,"time_c")$se)),
  list(label  = "Post-Launch \u2014 Level shift (\u03b2\u2082)",
       values = c(extract_coef(m3_priv,"post")$est,
                  extract_coef(m3_adult,"post")$est),
       se     = c(extract_coef(m3_priv,"post")$se,
                  extract_coef(m3_adult,"post")$se)),
  list(label  = "Time \u00d7 Post \u2014 Slope change (\u03b2\u2083)",
       values = c(extract_coef(m3_priv,"time_c:post")$est,
                  extract_coef(m3_adult,"time_c:post")$est),
       se     = c(extract_coef(m3_priv,"time_c:post")$se,
                  extract_coef(m3_adult,"time_c:post")$se)),
  list(label  = "Time\u00b2 \u2014 Pre-trend curvature (\u03b2\u2084)",
       values = c(extract_coef(m3_priv,"I(time_c^2)")$est,
                  extract_coef(m3_adult,"I(time_c^2)")$est),
       se     = c(extract_coef(m3_priv,"I(time_c^2)")$se,
                  extract_coef(m3_adult,"I(time_c^2)")$se)),
  list(label  = "Time\u00b2 \u00d7 Post \u2014 Post-trend curvature (\u03b2\u2085)",
       values = c(extract_coef(m3_priv,"I(time_c^2):post")$est,
                  extract_coef(m3_adult,"I(time_c^2):post")$est),
       se     = c(extract_coef(m3_priv,"I(time_c^2):post")$se,
                  extract_coef(m3_adult,"I(time_c^2):post")$se)),
  list(label  = "Observations",
       values = c(get_n(m3_priv), get_n(m3_adult)), se = NULL),
  list(label  = "R\u00b2 / Pseudo-R\u00b2",
       values = c(get_r2(m3_priv)$r2, get_r2(m3_adult)$r2), se = NULL),
  list(label  = "Adjusted R\u00b2",
       values = c(get_r2(m3_priv)$adjr2, get_r2(m3_adult)$adjr2), se = NULL)
), c("Variable","Privacy","Adult"))

ft3 <- flextable(t3_df) |>
  apa_ft(
    col_labels = list(Variable = "",
                      Privacy  = "Privacy Apps",
                      Adult    = "Adult Sites"),
    se_idx  = se_rows(t3_df),
    gof_row = which(t3_df[[1]] == "Observations"),
    title   = "Table 3. Regression Discontinuity in Time: Privacy-Focused Applications vs. Traditional Adult Platforms",
    note    = paste0(
      "Dependent variable: Google Trends Search Volume Index, logged. ",
      "Both columns estimated using OLS on log(hits); no zeros present after monthly aggregation. ",
      "RDiT estimated on group\u00d7month aggregated means. ",
      "Time is centred at zero at Spicy Mode (July 27, 2025). ",
      "Quadratic RDiT specification: Y = \u03b2\u2080 + \u03b2\u2081(Time) + \u03b2\u2082(POST) + \u03b2\u2083(Time\u00d7POST) + \u03b2\u2084(Time\u00b2) + \u03b2\u2085(Time\u00b2\u00d7POST). Standard errors are conventional (no clustering)."
    ),
    w1 = 3.9, wn = 1.9
  )
cat("Table 3 done.\n")

# ── TABLE 4 ────────────────────────────────────────────────────────────────

t4_df <- build_df(list(
  list(label  = "Constant (\u03b2\u2080) \u2014 General AI baseline, pre-treatment",
       values = c(extract_coef(m4,"(Intercept)")$est),
       se     = c(extract_coef(m4,"(Intercept)")$se)),
  list(label  = "Treatment \u2014 TREAT (\u03b2\u2081)",
       values = c(extract_coef(m4,"treatment")$est),
       se     = c(extract_coef(m4,"treatment")$se)),
  list(label  = "POST1 \u2014 Grok Imagine general shock",
       values = c(extract_coef(m4,"post_event1")$est),
       se     = c(extract_coef(m4,"post_event1")$se)),
  list(label  = "POST2 \u2014 Spicy Mode general shock",
       values = c(extract_coef(m4,"post_event2")$est),
       se     = c(extract_coef(m4,"post_event2")$se)),
  list(label  = "POST3 \u2014 Policy Crisis general shock",
       values = c(extract_coef(m4,"post_event3")$est),
       se     = c(extract_coef(m4,"post_event3")$se)),
  list(label  = "POST1 \u00d7 TREAT \u2014 \u03b4\u2081  (Grok Imagine, Jul. 2024)",
       values = c(extract_coef(m4,"treatment:post_event1")$est),
       se     = c(extract_coef(m4,"treatment:post_event1")$se)),
  list(label  = "POST2 \u00d7 TREAT \u2014 \u03b4\u2082  (Spicy Mode, Jul. 2025)",
       values = c(extract_coef(m4,"treatment:post_event2")$est),
       se     = c(extract_coef(m4,"treatment:post_event2")$se)),
  list(label  = "POST3 \u00d7 TREAT \u2014 \u03b4\u2083  (Policy Crisis, Jan. 2026)",
       values = c(extract_coef(m4,"treatment:post_event3")$est),
       se     = c(extract_coef(m4,"treatment:post_event3")$se)),
  list(label  = "Observations",
       values = c(get_n(m4)), se = NULL),
  list(label  = "R\u00b2 / Pseudo-R\u00b2",
       values = c(get_r2(m4)$r2), se = NULL),
  list(label  = "Adjusted R\u00b2",
       values = c(get_r2(m4)$adjr2), se = NULL)
), c("Variable","Rolling"))

gof_row4 <- which(t4_df[[1]] == "Observations")

ft4 <- flextable(t4_df) |>
  apa_ft(
    col_labels = list(Variable = "",
                      Rolling  = "Grok vs. General AI (Rolling DiD)"),
    se_idx  = se_rows(t4_df),
    gof_row = gof_row4,
    title   = "Table 4. Rolling Treatment Effects: Cumulative Impact of Three Grok Policy Events on Adversarial Search Intent",
    note    = paste0(
      "Dependent variable: Google Trends Search Volume Index, logged. ",
      "Corrected Rolling DiD: Y = \u03b2\u2080 + \u03b2\u2081(TREAT) + POST1 + POST2 + POST3 + ",
      "\u03b4\u2081(POST1\u00d7TREAT) + \u03b4\u2082(POST2\u00d7TREAT) + \u03b4\u2083(POST3\u00d7TREAT). ",
      "POST main effects capture proportional shocks common to both platforms. ",
      "\u03b4 coefficients isolate the Grok-specific proportional effect above the general shock. ",
      "All three \u03b4 coefficients are positive and significant: Grok searches grew ",
      "proportionally more than General AI at every event on a multiplicative scale. ",
      "\u03b4\u2083 is the largest single Grok-specific effect, confirming the January 2026 ",
      "crisis was Grok-specific rather than a general NCII category event. ",
      "Standard errors are conventional (no clustering). ",
      "POST1 = on/after July 29 2024; POST2 = on/after July 27 2025; ",
      "POST3 = on/after January 15 2026."
    ),
    w1 = 4.2, wn = 2.0
  )
cat("Table 4 done.\n")

# =============================================================================
# SECTION 5: EXPORT
# =============================================================================

cat("\nWriting Tables_1_to_4_FINAL.docx ...\n")

doc <- read_docx() |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft1) |>
  body_add_par("", style = "Normal") |>
  body_add_break() |>
  body_add_flextable(ft2) |>
  body_add_par("", style = "Normal") |>
  body_add_break() |>
  body_add_flextable(ft3) |>
  body_add_par("", style = "Normal") |>
  body_add_break() |>
  body_add_flextable(ft4)

print(doc, target = "Tables_1_to_4_QUADRATIC.docx")
cat("Done. Tables_1_to_4_QUADRATIC.docx saved in working directory.\n")

# =============================================================================
# SECTION 6: IRR TABLES — Aaron's suggestion
# Converts Poisson coefficients to Incidence Rate Ratios (exp(coef))
# IRR interpretation: IRR of 2.0 means the rate doubled; 0.5 means halved
# OLS log coefficients also exponentiated for consistency
# Adjusted R² row removed where not applicable (Poisson models)
# Table 2 footnote made clearer
# =============================================================================

cat("\n=== BUILDING IRR TABLES ===\n")

# Helper: extract coefficient and convert to IRR
extract_irr <- function(mod, term, digits = 3) {
  cf <- summary(mod)$coefficients
  if (!term %in% rownames(cf))
    return(list(est = "\u2014", se = "", raw_irr = NA, raw_p = NA))
  b    <- cf[term, "Estimate"]
  se   <- cf[term, "Std. Error"]
  pval <- cf[term, 4]
  irr  <- exp(b)
  # SE of IRR via delta method: SE(exp(b)) = exp(b) * SE(b)
  se_irr <- irr * se
  sig  <- ifelse(pval < 0.001, "***",
                 ifelse(pval < 0.01,  "**",
                        ifelse(pval < 0.05,  "*", "")))
  list(
    est    = paste0(round(irr, digits), sig),
    se     = paste0("(", round(se_irr, digits), ")"),
    raw_irr = irr,
    raw_p   = pval
  )
}

# For OLS log models, exp(coef) is also an IRR-equivalent (proportional change)
# Same function works for both

# R² row — only show where applicable, skip Adjusted R² for Poisson
gof_irr <- function(mod) {
  s   <- summary(mod)
  n   <- as.character(nobs(mod))
  typ <- attr(mod, "model_type")
  if (typ == "poisson") {
    pr2 <- round(1 - s$deviance / s$null.deviance, 3)
    list(n = n, r2 = paste0(pr2, " (McF)"), show_adj = FALSE)
  } else {
    list(n = n,
         r2    = as.character(round(s$r.squared, 3)),
         adjr2 = as.character(round(s$adj.r.squared, 3)),
         show_adj = TRUE)
  }
}

# ── IRR TABLE 1 ──────────────────────────────────────────────────────────────

g1_gof <- gof_irr(m1_g1)
g3_gof <- gof_irr(m1_g3)

t1_irr_rows <- list(
  list(label  = "Constant (\u03b2\u2080)",
       values = c(extract_irr(m1_g1,"(Intercept)")$est,
                  extract_irr(m1_g3,"(Intercept)")$est),
       se     = c(extract_irr(m1_g1,"(Intercept)")$se,
                  extract_irr(m1_g3,"(Intercept)")$se)),
  list(label  = "Treatment \u2014 TREAT (\u03b2\u2081)",
       values = c(extract_irr(m1_g1,"treatment")$est,
                  extract_irr(m1_g3,"treatment")$est),
       se     = c(extract_irr(m1_g1,"treatment")$se,
                  extract_irr(m1_g3,"treatment")$se)),
  list(label  = "Post-Launch \u2014 POST (\u03b2\u2082)",
       values = c(extract_irr(m1_g1,"post_event2")$est,
                  extract_irr(m1_g3,"post_event1")$est),
       se     = c(extract_irr(m1_g1,"post_event2")$se,
                  extract_irr(m1_g3,"post_event1")$se)),
  list(label  = "Treatment \u00d7 Post \u2014 DiD Estimator (\u03b2\u2083)",
       values = c(extract_irr(m1_g1,"treatment:post_event2")$est,
                  extract_irr(m1_g3,"treatment:post_event1")$est),
       se     = c(extract_irr(m1_g1,"treatment:post_event2")$se,
                  extract_irr(m1_g3,"treatment:post_event1")$se)),
  list(label  = "Observations",
       values = c(g1_gof$n, g3_gof$n), se = NULL),
  list(label  = "R\u00b2 / Pseudo-R\u00b2",
       values = c(g1_gof$r2, g3_gof$r2), se = NULL)
)

t1i_df <- build_df(t1_irr_rows, c("Variable","G1","G3"))

ft1i <- flextable(t1i_df) |>
  apa_ft(
    col_labels = list(Variable = "",
                      G1 = "Group 1: Grok vs.\nGeneral AI",
                      G3 = "Group 3: NCII Tools vs.\nMainstream AI"),
    se_idx  = se_rows(t1i_df),
    gof_row = which(t1i_df[[1]] == "Observations"),
    title   = "Table 1. Difference-in-Differences Analysis of Grok-Attributable Search Behaviour",
    note    = paste0(
      "Coefficients expressed as Incidence Rate Ratios (IRR = exp(\u03b2)). ",
      "IRR > 1 indicates an increase in search rate; IRR < 1 indicates a decrease. ",
      "An IRR of 2.0 means the search rate doubled; 0.5 means it halved. ",
      "Standard errors estimated via the delta method (SE\u2093\u1d63\u1d63 = IRR \u00d7 SE\u03b2). ",
      "Dependent variable: Google Trends Search Volume Index (0\u2013100). ",
      "Group 1 (N=5420): 20 keywords (10 Grok + 10 General AI), weekly; ",
      "intervention = Spicy Mode (July 27, 2025). ",
      "Group 3 (N=2959): 11 keywords, weekly; ",
      "intervention = Grok Imagine (July 29, 2024). ",
      "Poisson GLM with log link used for both groups (zeros present). ",
      "Standard errors are conventional (no clustering). ",
      "\u03b2\u2083 in Group 1 is positive: on a multiplicative scale Grok-specific searches ",
      "grew proportionally more than General AI after Spicy Mode, consistent with ",
      "Grok emerging from a near-zero base. See Table 4 for event-by-event effects."
    ),
    w1 = 3.6, wn = 2.0
  )
cat("IRR Table 1 done.\n")

# ── IRR TABLE 2 ──────────────────────────────────────────────────────────────

t2_irr_rows <- list(
  list(label  = "Intercept (\u03b2\u2080) \u2014 search rate at intervention date",
       values = c(extract_irr(m2_ncii,"(Intercept)")$est,
                  extract_irr(m2_main,"(Intercept)")$est),
       se     = c(extract_irr(m2_ncii,"(Intercept)")$se,
                  extract_irr(m2_main,"(Intercept)")$se)),
  list(label  = "Time \u2014 Pre-trend (\u03b2\u2081, per week)",
       values = c(extract_irr(m2_ncii,"time_c")$est,
                  extract_irr(m2_main,"time_c")$est),
       se     = c(extract_irr(m2_ncii,"time_c")$se,
                  extract_irr(m2_main,"time_c")$se)),
  list(label  = "Post-Launch \u2014 Level shift (\u03b2\u2082)",
       values = c(extract_irr(m2_ncii,"post")$est,
                  extract_irr(m2_main,"post")$est),
       se     = c(extract_irr(m2_ncii,"post")$se,
                  extract_irr(m2_main,"post")$se)),
  list(label  = "Time \u00d7 Post \u2014 Slope change (\u03b2\u2083)",
       values = c(extract_irr(m2_ncii,"time_c:post")$est,
                  extract_irr(m2_main,"time_c:post")$est),
       se     = c(extract_irr(m2_ncii,"time_c:post")$se,
                  extract_irr(m2_main,"time_c:post")$se)),
  list(label  = "Observations",
       values = c(as.character(nobs(m2_ncii)),
                  as.character(nobs(m2_main))), se = NULL),
  list(label  = "R\u00b2 / Pseudo-R\u00b2",
       values = c(gof_irr(m2_ncii)$r2,
                  gof_irr(m2_main)$r2), se = NULL),
  list(label  = "Estimator",
       values = c(type_label(m2_ncii), type_label(m2_main)), se = NULL)
)

t2i_df <- build_df(t2_irr_rows, c("Variable","NCII","Mainstream"))

ft2i <- flextable(t2i_df) |>
  apa_ft(
    col_labels = list(Variable   = "",
                      NCII       = "NCII-Specific Tools",
                      Mainstream = "Mainstream AI Platforms"),
    se_idx  = se_rows(t2i_df),
    gof_row = which(t2i_df[[1]] == "Observations"),
    title   = "Table 2. Regression Discontinuity in Time: NCII-Specific Tools vs. Mainstream AI Platforms",
    note    = paste0(
      "Coefficients expressed as Incidence Rate Ratios (IRR = exp(\u03b2)). ",
      "IRR > 1 indicates higher search rate; IRR < 1 indicates lower search rate. ",
      "IRR for \u03b2\u2081 (pre-trend) represents the multiplicative weekly growth factor: ",
      "IRR = 1.01 means searches grew 1% per week before the intervention. ",
      "IRR for \u03b2\u2082 (level shift) represents the immediate proportional jump at the ",
      "intervention date: IRR = 1.16 means searches were 16% higher immediately after launch. ",
      "IRR for \u03b2\u2083 (slope change) represents the change in the weekly growth factor ",
      "after the intervention: IRR < 1 means the weekly growth rate slowed after launch. ",
      "Models estimated on group\u00d7week aggregated means (one series per group). ",
      "Time centred at zero at Grok Imagine (July 29, 2024). ",
      "NCII Tools: OLS on log(hits), no zeros after aggregation. ",
      "Mainstream AI: Poisson GLM, zeros present after aggregation. ",
      "Standard errors are conventional (no clustering)."
    ),
    w1 = 3.9, wn = 1.9
  )
cat("IRR Table 2 done.\n")

# ── IRR TABLE 3 ──────────────────────────────────────────────────────────────

t3_irr_rows <- list(
  list(label  = "Intercept (\u03b2\u2080) \u2014 search rate at intervention date",
       values = c(extract_irr(m3_priv,"(Intercept)")$est,
                  extract_irr(m3_adult,"(Intercept)")$est),
       se     = c(extract_irr(m3_priv,"(Intercept)")$se,
                  extract_irr(m3_adult,"(Intercept)")$se)),
  list(label  = "Time \u2014 Pre-trend (\u03b2\u2081, per week)",
       values = c(extract_irr(m3_priv,"time_c")$est,
                  extract_irr(m3_adult,"time_c")$est),
       se     = c(extract_irr(m3_priv,"time_c")$se,
                  extract_irr(m3_adult,"time_c")$se)),
  list(label  = "Post-Launch \u2014 Level shift (\u03b2\u2082)",
       values = c(extract_irr(m3_priv,"post")$est,
                  extract_irr(m3_adult,"post")$est),
       se     = c(extract_irr(m3_priv,"post")$se,
                  extract_irr(m3_adult,"post")$se)),
  list(label  = "Time \u00d7 Post \u2014 Slope change (\u03b2\u2083)",
       values = c(extract_irr(m3_priv,"time_c:post")$est,
                  extract_irr(m3_adult,"time_c:post")$est),
       se     = c(extract_irr(m3_priv,"time_c:post")$se,
                  extract_irr(m3_adult,"time_c:post")$se)),
  list(label  = "Observations",
       values = c(as.character(nobs(m3_priv)),
                  as.character(nobs(m3_adult))), se = NULL),
  list(label  = "R\u00b2",
       values = c(gof_irr(m3_priv)$r2,
                  gof_irr(m3_adult)$r2), se = NULL),
  list(label  = "Adjusted R\u00b2",
       values = c(gof_irr(m3_priv)$adjr2,
                  gof_irr(m3_adult)$adjr2), se = NULL)
)

t3i_df <- build_df(t3_irr_rows, c("Variable","Privacy","Adult"))

ft3i <- flextable(t3i_df) |>
  apa_ft(
    col_labels = list(Variable = "",
                      Privacy  = "Privacy Apps",
                      Adult    = "Adult Sites"),
    se_idx  = se_rows(t3i_df),
    gof_row = which(t3i_df[[1]] == "Observations"),
    title   = "Table 3. Regression Discontinuity in Time: Privacy-Focused Applications vs. Traditional Adult Platforms",
    note    = paste0(
      "Coefficients expressed as Incidence Rate Ratios (IRR = exp(\u03b2)). ",
      "Both columns use OLS on log(hits) — no zeros after monthly aggregation. ",
      "IRR for \u03b2\u2082 (level shift) is the key finding: Privacy Apps IRR = 4.31 means ",
      "searches were 4.31 times higher immediately after Spicy Mode launched, ",
      "a 331% increase. Adult Sites IRR = 1.03 (not significant) means no meaningful change. ",
      "Models estimated on group\u00d7month aggregated means. ",
      "Time centred at zero at Spicy Mode (July 27, 2025). ",
      "Standard errors are conventional (no clustering)."
    ),
    w1 = 3.9, wn = 1.9
  )
cat("IRR Table 3 done.\n")

# ── IRR TABLE 4 ──────────────────────────────────────────────────────────────

t4_gof <- gof_irr(m4)

t4_irr_rows <- list(
  list(label  = "Constant (\u03b2\u2080) \u2014 General AI baseline, pre-treatment",
       values = c(extract_irr(m4,"(Intercept)")$est),
       se     = c(extract_irr(m4,"(Intercept)")$se)),
  list(label  = "Treatment \u2014 TREAT (\u03b2\u2081)",
       values = c(extract_irr(m4,"treatment")$est),
       se     = c(extract_irr(m4,"treatment")$se)),
  list(label  = "POST1 \u2014 Grok Imagine general shock",
       values = c(extract_irr(m4,"post_event1")$est),
       se     = c(extract_irr(m4,"post_event1")$se)),
  list(label  = "POST2 \u2014 Spicy Mode general shock",
       values = c(extract_irr(m4,"post_event2")$est),
       se     = c(extract_irr(m4,"post_event2")$se)),
  list(label  = "POST3 \u2014 Policy Crisis general shock",
       values = c(extract_irr(m4,"post_event3")$est),
       se     = c(extract_irr(m4,"post_event3")$se)),
  list(label  = "POST1 \u00d7 TREAT \u2014 \u03b4\u2081  (Grok Imagine, Jul. 2024)",
       values = c(extract_irr(m4,"treatment:post_event1")$est),
       se     = c(extract_irr(m4,"treatment:post_event1")$se)),
  list(label  = "POST2 \u00d7 TREAT \u2014 \u03b4\u2082  (Spicy Mode, Jul. 2025)",
       values = c(extract_irr(m4,"treatment:post_event2")$est),
       se     = c(extract_irr(m4,"treatment:post_event2")$se)),
  list(label  = "POST3 \u00d7 TREAT \u2014 \u03b4\u2083  (Policy Crisis, Jan. 2026)",
       values = c(extract_irr(m4,"treatment:post_event3")$est),
       se     = c(extract_irr(m4,"treatment:post_event3")$se)),
  list(label  = "Observations",
       values = c(t4_gof$n), se = NULL),
  list(label  = "Pseudo-R\u00b2 (McFadden)",
       values = c(t4_gof$r2), se = NULL)
)

t4i_df <- build_df(t4_irr_rows, c("Variable","Rolling"))

ft4i <- flextable(t4i_df) |>
  apa_ft(
    col_labels = list(Variable = "",
                      Rolling  = "Grok vs. General AI (Rolling DiD)"),
    se_idx  = se_rows(t4i_df),
    gof_row = which(t4i_df[[1]] == "Observations"),
    title   = "Table 4. Rolling Treatment Effects: Cumulative Impact of Three Grok Policy Events on Adversarial Search Intent",
    note    = paste0(
      "Coefficients expressed as Incidence Rate Ratios (IRR = exp(\u03b2)). ",
      "IRR > 1 indicates Grok searches grew faster than General AI at that event; ",
      "IRR < 1 indicates General AI grew faster. ",
      "POST IRRs capture the proportional shock common to both platforms at each event. ",
      "\u03b4 IRRs capture the additional Grok-specific multiplier above the general shock: ",
      "an IRR of 21.07 for \u03b4\u2081 means Grok searches grew at 21 times the rate of General AI ",
      "at Grok Imagine; 5.19 for \u03b4\u2082 means 5 times the rate at Spicy Mode; ",
      "2.19 for \u03b4\u2083 means twice the rate at the Policy Crisis. ",
      "POST3 IRR < 1 (general category dipped) while \u03b4\u2083 IRR > 1 confirms the January 2026 ",
      "crisis was Grok-specific: the general category fell while Grok searches surged. ",
      "Poisson GLM with log link (zeros present in 48.7\u0025 of G1 observations). ",
      "Standard errors are conventional (no clustering). ",
      "POST1 = on/after July 29 2024; POST2 = on/after July 27 2025; ",
      "POST3 = on/after January 15 2026."
    ),
    w1 = 4.2, wn = 2.0
  )
cat("IRR Table 4 done.\n")

# ── EXPORT IRR TABLES ────────────────────────────────────────────────────────

cat("\nWriting Tables_1_to_4_IRR.docx ...\n")

doc_irr <- read_docx() |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft1i) |>
  body_add_par("", style = "Normal") |>
  body_add_break() |>
  body_add_flextable(ft2i) |>
  body_add_par("", style = "Normal") |>
  body_add_break() |>
  body_add_flextable(ft3i) |>
  body_add_par("", style = "Normal") |>
  body_add_break() |>
  body_add_flextable(ft4i)

print(doc_irr, target = "Tables_1_to_4IRRQ.docx") 
cat("Done. Tables_1_to_4_IRR.docx saved.\n")
