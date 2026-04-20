# =============================================================================
# GROK NCII SEARCH INTEREST ANALYSIS
# Aanyaa Manas | MS Criminology | University of Pennsylvania | May 2026
#
# Three events:
#   Event 1: August 2024     — Grok 2 launches with image generation (Aurora/Flux)
#   Event 2: July 28, 2025  — Grok 4 / Spicy Mode launches (NSFW generation)
#   Event 3: January 1, 2026 — Public crisis + regulatory spotlight
#
# Designs:
#   (1) Difference-in-Differences (DiD): Grok-specific NCII terms vs general AI terms
#   (2) Regression Discontinuity in Time (RDiT): threshold effects at each event
#   Both with Newey-West HAC standard errors for autocorrelation
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gtrendsR,      # Google Trends API
  dplyr,         # data manipulation
  tidyr,         # reshaping
  tibble,        # tibbles
  lubridate,     # dates
  zoo,           # rolling means
  lmtest,        # coeftest
  sandwich,      # vcovHAC
  broom,         # tidy model output
  ggplot2,       # plots (for diagnostics — not final figures)
  stringr,       # string cleaning
  purrr          # map/reduce
)

# -----------------------------------------------------------------------------
# 1. DEFINE TERMS AND EVENT DATES
# -----------------------------------------------------------------------------

# Treatment group: Grok-specific NCII search terms
grok_terms <- c(
  "grok deepfake",
  "grok face swap",
  "grok undress",
  "grok remove clothes",
  "grok porn",
  "grok nude",
  "grok boobs",
  "grok ass",
  "grok bikini",
  "grok girl"
)

# Control group: General AI NCII search terms (same harm category, no Grok)
ai_terms <- c(
  "ai deepfake",
  "ai face swap",
  "ai undress",
  "ai remove clothes",
  "ai porn",
  "ai nude",
  "ai boobs",
  "ai ass",
  "ai bikini",
  "ai girl"
)

# NOTE: control terms are parallel in structure to treatment terms.
# This is important — the DiD assumption is that general AI NCII trends
# are a valid counterfactual for Grok-specific trends absent the events.

# Event dates — CORRECTED from original script
DATE_EVENT1 <- as.Date("2024-08-01")  # Grok 2: first image generation capability
DATE_EVENT2 <- as.Date("2025-07-01")  # Grok 4 / Spicy Mode (released July 28 — 
# using July 1 as the monthly boundary 
# since Google Trends data is monthly)
DATE_EVENT3 <- as.Date("2026-01-01")  # January 2026 public crisis and regulatory spotlight

# Study window: 5 years ending at data pull
TIME_RANGE <- "2021-01-01 2026-03-01"

# -----------------------------------------------------------------------------
# 2. FETCH DATA
# -----------------------------------------------------------------------------

# Google Trends fetches up to 5 keywords at a time — split into batches
# Fetching in two batches of 5 per group to stay within API limits

cat("Fetching Google Trends data...\n")

# Grok terms — batch 1 and 2
grok_batch1 <- gtrends(keyword = grok_terms[1:5],  time = TIME_RANGE, geo = "")
grok_batch2 <- gtrends(keyword = grok_terms[6:10], time = TIME_RANGE, geo = "")

# AI terms — batch 1 and 2
ai_batch1 <- gtrends(keyword = ai_terms[1:5],  time = TIME_RANGE, geo = "")
ai_batch2 <- gtrends(keyword = ai_terms[6:10], time = TIME_RANGE, geo = "")

cat("Data fetched.\n")

# -----------------------------------------------------------------------------
# 3. CLEAN AND COMBINE
# -----------------------------------------------------------------------------

clean_trends <- function(batch1, batch2, platform_label) {
  bind_rows(
    batch1$interest_over_time,
    batch2$interest_over_time
  ) |>
    dplyr::select(date, keyword, hits) |>
    mutate(
      date     = as.Date(date),
      hits     = as.numeric(ifelse(hits == "<1", "0.5", as.character(hits))),
      # NOTE: "<1" is coded as 0.5 rather than 0 — it represents a non-zero
      # but very low signal. Coding it as 0 would understate the baseline.
      platform = platform_label
    )
}

grok_long <- clean_trends(grok_batch1, grok_batch2, "Grok")
ai_long   <- clean_trends(ai_batch1,   ai_batch2,   "AI")

# Combined long format — the master dataframe
all_long <- bind_rows(grok_long, ai_long)

cat("Rows in combined data:", nrow(all_long), "\n")
cat("Date range:", as.character(min(all_long$date)), "to", as.character(max(all_long$date)), "\n")
cat("Keywords:", paste(unique(all_long$keyword), collapse = ", "), "\n")

# Quick sanity check on the data we actually have
all_long |>
  filter(platform == "Grok") |>
  group_by(date) |>
  summarise(mean_hits = mean(hits)) |>
  filter(mean_hits > 0) |>
  print(n = 30)

# -----------------------------------------------------------------------------
# 4. BUILD ANALYSIS DATAFRAME
# -----------------------------------------------------------------------------
# Unit of analysis: platform x month
# For DiD: we need one row per platform per month, with
#   - mean search volume across all keywords in that platform
#   - treatment indicator (Grok = 1, AI = 0)
#   - three event dummies
#   - three treatment x event interactions (the DiD estimators)
#   - a time trend
#   - time-since-event variables for RDiT

platform_monthly <- all_long |>
  group_by(platform, date) |>
  summarise(
    search_volume = mean(hits, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Treatment indicator
    treatment = if_else(platform == "Grok", 1L, 0L),
    
    # Event dummies — 1 from event date onwards
    post_event1 = if_else(date >= DATE_EVENT1, 1L, 0L),  # Aug 2024 image gen
    post_event2 = if_else(date >= DATE_EVENT2, 1L, 0L),  # Jul 2025 Spicy Mode
    post_event3 = if_else(date >= DATE_EVENT3, 1L, 0L),  # Jan 2026 crisis
    
    # Time trend (months from start of series)
    time = as.integer(difftime(date, min(date), units = "weeks") / 4.33)
  ) |>
  arrange(platform, date)

# Check event windows
cat("\nObservations per event window:\n")
platform_monthly |>
  summarise(
    pre_event1  = sum(post_event1 == 0),
    post_event1_only = sum(post_event1 == 1 & post_event2 == 0),
    post_event2_only = sum(post_event2 == 1 & post_event3 == 0),
    post_event3 = sum(post_event3 == 1)
  ) |>
  print()

# -----------------------------------------------------------------------------
# 5. KEYWORD-LEVEL LONG FORMAT FOR ITS
# -----------------------------------------------------------------------------
# For the Interrupted Time Series, we work at keyword level within Grok terms
# so we can see which specific terms drove effects at each event

keyword_monthly <- grok_long |>
  mutate(
    post_event1 = if_else(date >= DATE_EVENT1, 1L, 0L),
    post_event2 = if_else(date >= DATE_EVENT2, 1L, 0L),
    post_event3 = if_else(date >= DATE_EVENT3, 1L, 0L),
    time        = as.integer(difftime(date, min(date), units = "weeks") / 4.33)
  )

# -----------------------------------------------------------------------------
# 6. DESCRIPTIVE STATISTICS
# -----------------------------------------------------------------------------

cat("\n DESCRIPTIVE STATISTICS \n")

# Mean search volume by platform and period
desc_stats <- platform_monthly |>
  mutate(
    period = case_when(
      date < DATE_EVENT1 ~ "Pre-Event 1 (before Aug 2024)",
      date >= DATE_EVENT1 & date < DATE_EVENT2 ~ "Post-Event 1 (Aug 2024 – Jun 2025)",
      date >= DATE_EVENT2 & date < DATE_EVENT3 ~ "Post-Event 2 (Jul–Dec 2025)",
      date >= DATE_EVENT3 ~ "Post-Event 3 (Jan 2026+)"
    )
  ) |>
  group_by(platform, period) |>
  summarise(
    n_months       = n(),
    mean_volume    = round(mean(search_volume), 3),
    median_volume  = round(median(search_volume), 3),
    sd_volume      = round(sd(search_volume), 3),
    max_volume     = round(max(search_volume), 3),
    .groups = "drop"
  )

print(desc_stats)

# Keyword-level peak dates (when did each Grok term spike?)
cat("\nPeak dates by Grok keyword:\n")
grok_long |>
  group_by(keyword) |>
  filter(hits == max(hits)) |>
  dplyr::select(keyword, date, hits) |>
  arrange(desc(hits)) |>
  print()

# -----------------------------------------------------------------------------
# 7. DIFFERENCE-IN-DIFFERENCES MODEL
# -----------------------------------------------------------------------------
# Model: search_volume ~ time + treatment 
#                      + post_event1 + treatment:post_event1  <- Event 1 DiD
#                      + post_event2 + treatment:post_event2  <- Event 2 DiD  
#                      + post_event3 + treatment:post_event3  <- Event 3 DiD
#
# The treatment:event interaction terms ARE the DiD estimators.
# They answer: "Did the Grok-specific search volume shift at this event
#               BEYOND what the general AI trend was doing?"
#
# Standard errors: Newey-West HAC to correct for autocorrelation in monthly
# time series data (standard in this literature).

cat("\n DIFFERENCE-IN-DIFFERENCES MODEL \n")

did_model <- lm(
  search_volume ~ time
  + treatment
  + post_event1 + treatment:post_event1
  + post_event2 + treatment:post_event2
  + post_event3 + treatment:post_event3,
  data = platform_monthly
)

# HAC standard errors — Newey-West with automatic bandwidth selection
did_hac <- coeftest(did_model, vcov = vcovHAC(did_model))

cat("\nDiD Model — HAC Standard Errors:\n")
print(did_hac)

# Extract and label cleanly
did_results <- tidy(did_hac) |>
  mutate(
    label = dplyr::recode(term,
                          "(Intercept)"           = "Intercept",
                          "time"                  = "Time Trend",
                          "treatment"             = "Grok vs AI (baseline difference)",
                          "post_event1"           = "Post Aug 2024 (Event 1 level shift)",
                          "post_event2"           = "Post Jul 2025 (Event 2 level shift)",
                          "post_event3"           = "Post Jan 2026 (Event 3 level shift)",
                          "treatment:post_event1" = "DiD Event 1: Grok x Aug 2024 image gen",
                          "treatment:post_event2" = "DiD Event 2: Grok x Jul 2025 Spicy Mode",
                          "treatment:post_event3" = "DiD Event 3: Grok x Jan 2026 crisis"
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) |>
  dplyr::select(label, estimate, std.error, statistic, p.value, sig)

cat("\nLabelled DiD Results:\n")
print(did_results, n = Inf)

# -----------------------------------------------------------------------------
# 8. ITS (REGRESSION DISCONTINUITY IN TIME) — KEYWORD LEVEL
# -----------------------------------------------------------------------------
# For each Grok keyword individually, estimate:
#   hits ~ time + post_event1 + post_event2 + post_event3
#
# This tells us which specific terms drove the aggregate effects,
# and whether the three events had heterogeneous effects across keywords.
# This is the RDiT component — testing for level shifts at each threshold.

cat("\n ITS / RDiT MODEL (KEYWORD LEVEL) \n")

run_its <- function(kw, df) {
  dat <- df |> filter(keyword == kw)
  
  if (nrow(dat) < 20 || var(dat$hits) == 0) {
    return(NULL)  # skip keywords with no variance
  }
  
  model <- lm(
    hits ~ time + post_event1 + post_event2 + post_event3,
    data = dat
  )
  
  # HAC SEs
  hac <- coeftest(model, vcov = vcovHAC(model))
  
  tidy(hac) |>
    mutate(
      keyword = kw,
      r_squared = summary(model)$r.squared,
      n_obs = nrow(dat)
    )
}

its_results <- map_dfr(unique(keyword_monthly$keyword), ~run_its(.x, keyword_monthly))

# Clean up labels
its_results_clean <- its_results |>
  filter(term %in% c("post_event1", "post_event2", "post_event3")) |>
  mutate(
    event = dplyr::recode(term,
                          "post_event1" = "Event 1: Aug 2024 (Image Gen)",
                          "post_event2" = "Event 2: Jul 2025 (Spicy Mode)",
                          "post_event3" = "Event 3: Jan 2026 (Crisis)"
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) |>
  dplyr::select(keyword, event, estimate, std.error, statistic, p.value, sig, r_squared)

cat("\nITS Results by Keyword and Event:\n")
print(its_results_clean, n = Inf)

# Summary: which events were significant across keywords?
cat("\nSignificance summary by event:\n")
its_results_clean |>
  group_by(event) |>
  summarise(
    n_significant_p05 = sum(p.value < 0.05),
    n_significant_p01 = sum(p.value < 0.01),
    mean_estimate      = round(mean(estimate), 3),
    median_estimate    = round(median(estimate), 3),
    .groups = "drop"
  ) |>
  print()

# -----------------------------------------------------------------------------
# 9. PRE-TREND CHECK (PARALLEL TRENDS ASSUMPTION)
# -----------------------------------------------------------------------------
# For DiD validity, we need to verify that Grok and AI search trends
# were moving in parallel BEFORE the events. We do this by:
# (a) Visual inspection of pre-period trends
# (b) Testing for differential pre-trends using a placebo event date

cat("\n PARALLEL TRENDS CHECK \n")

# Restrict to pre-Event 1 period only
pre_period <- platform_monthly |>
  filter(date < DATE_EVENT1)

# If trends are parallel, the interaction of treatment x time should be ~0
pre_trend_model <- lm(
  search_volume ~ time * treatment,
  data = pre_period
)

pre_trend_hac <- coeftest(pre_trend_model, vcov = vcovHAC(pre_trend_model))

cat("\nPre-trend test (treatment x time interaction should be non-significant):\n")
print(pre_trend_hac)

# A non-significant treatment:time term = parallel trends assumption holds

# -----------------------------------------------------------------------------
# 10. PLACEBO TEST
# -----------------------------------------------------------------------------
# If our model is picking up real event effects and not spurious patterns,
# a fake event date in the pre-period should produce a null result.
# Using March 2023 as a placebo — no Grok image capability existed then.

cat("\n PLACEBO TEST \n")

PLACEBO_DATE <- as.Date("2023-03-01")

placebo_df <- platform_monthly |>
  filter(date < DATE_EVENT1) |>  # pre-period only
  mutate(
    post_placebo = if_else(date >= PLACEBO_DATE, 1L, 0L)
  )

placebo_model <- lm(
  search_volume ~ time + treatment + post_placebo + treatment:post_placebo,
  data = placebo_df
)

placebo_hac <- coeftest(placebo_model, vcov = vcovHAC(placebo_model))

cat("\nPlacebo test — interaction term should be non-significant:\n")
print(placebo_hac)

# If treatment:post_placebo is NOT significant, our model is not
# just picking up noise — it responds to real events.

# -----------------------------------------------------------------------------
# 11. SAVE CLEAN OUTPUTS FOR TABLES
# -----------------------------------------------------------------------------

# These dataframes are what you pass to your table-making script
# No formatting here — just clean data

write.csv(did_results,       "did_results_clean.csv",       row.names = FALSE)
write.csv(its_results_clean, "its_results_clean.csv",       row.names = FALSE)
write.csv(desc_stats,        "descriptive_stats_clean.csv", row.names = FALSE)
write.csv(platform_monthly,  "platform_monthly_clean.csv",  row.names = FALSE)
write.csv(keyword_monthly,   "keyword_monthly_clean.csv",   row.names = FALSE)

cat("\n ANALYSIS COMPLETE \n")
cat("Output files written:\n")
cat("  did_results_clean.csv       — DiD model results\n")
cat("  its_results_clean.csv       — ITS/RDiT keyword-level results\n")
cat("  descriptive_stats_clean.csv — Descriptive statistics by period\n")
cat("  platform_monthly_clean.csv  — Platform-level monthly data\n")
cat("  keyword_monthly_clean.csv   — Keyword-level monthly data\n")

# -----------------------------------------------------------------------------
# 12. QUICK DIAGNOSTIC PLOTS (not final figures — just to check the model)
# -----------------------------------------------------------------------------

# Plot 1: Raw search volume by platform over time with event lines
ggplot(platform_monthly, aes(x = date, y = search_volume, color = platform)) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = DATE_EVENT1, linetype = "dashed", color = "black") +
  geom_vline(xintercept = DATE_EVENT2, linetype = "dashed", color = "steelblue") +
  geom_vline(xintercept = DATE_EVENT3, linetype = "dashed", color = "red") +
  annotate("text", x = DATE_EVENT1, y = Inf, label = "Event 1\nAug 2024", 
           vjust = 1.5, hjust = -0.1, size = 3) +
  annotate("text", x = DATE_EVENT2, y = Inf, label = "Event 2\nJul 2025", 
           vjust = 1.5, hjust = -0.1, size = 3) +
  annotate("text", x = DATE_EVENT3, y = Inf, label = "Event 3\nJan 2026", 
           vjust = 1.5, hjust = -0.1, size = 3) +
  labs(
    title = "Mean Search Volume: Grok NCII Terms vs General AI NCII Terms",
    subtitle = "Vertical lines = three Grok product events",
    x = "Date", y = "Mean Google Trends Index (0–100)",
    color = "Platform"
  ) +
  theme_minimal()

ggsave("diagnostic_platform_trends.png", width = 10, height = 5, dpi = 150)

# Plot 2: DiD coefficient plot — the three interaction terms
did_results |>
  filter(str_detect(label, "DiD")) |>
  ggplot(aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(
    xmin = estimate - 1.96 * std.error,
    xmax = estimate + 1.96 * std.error
  ), height = 0.2) +
  labs(
    title = "DiD Estimates: Grok-Specific vs General AI NCII Search Interest",
    subtitle = "95% confidence intervals. Positive = Grok searches rose more than AI baseline",
    x = "DiD Estimate", y = NULL
  ) +
  theme_minimal()

ggsave("diagnostic_did_coefficients.png", width = 9, height = 4, dpi = 150)

# Plot 3: ITS estimates by keyword and event
its_results_clean |>
  ggplot(aes(x = estimate, y = keyword, color = event)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbarh(aes(
    xmin = estimate - 1.96 * std.error,
    xmax = estimate + 1.96 * std.error
  ), height = 0.2, position = position_dodge(width = 0.6)) +
  labs(
    title = "ITS Level-Shift Estimates by Grok Keyword and Event",
    subtitle = "95% CI. Positive = search interest increased after event",
    x = "Estimated Level Shift", y = NULL, color = "Event"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("diagnostic_its_by_keyword.png", width = 10, height = 6, dpi = 150)

cat("\nDiagnostic plots saved.\n")

# =============================================================================
# FIGURES AND TABLES 
# =============================================================================

# DiD is a limitation now 







# NOW STARTING 


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, dplyr, tidyr, lubridate, stringr,
  scales, patchwork, ggtext,        # plot layout and text
  flextable, officer,               # publication tables
  RColorBrewer, viridis             # colour palettes
)

# ── Colour palette (used across ALL figures for consistency) ──────────────────
# Muted, academic — works in greyscale too
COL_GROK   <- "#2C7BB6"   # blue  — Grok NCII terms
COL_AI     <- "#D7191C"   # red   — General AI terms
COL_E1     <- "#4D4D4D"   # dark grey — Event 1 line
COL_E2     <- "#1A9641"   # green — Event 2 line  
COL_E3     <- "#D7191C"   # red   — Event 3 line
COL_LIGHT  <- "#F7F7F7"   # background

# ── Global ggplot theme ───────────────────────────────────────────────────────
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text                = element_text(family = "serif", color = "#1a1a1a"),
      plot.title          = element_text(face = "bold", size = base_size + 2,
                                         margin = margin(b = 4)),
      plot.subtitle       = element_text(size = base_size - 1, color = "#444444",
                                         margin = margin(b = 8)),
      plot.caption        = element_text(size = base_size - 2, color = "#666666",
                                         hjust = 0, margin = margin(t = 8)),
      axis.title          = element_text(size = base_size, face = "plain"),
      axis.text           = element_text(size = base_size - 1, color = "#333333"),
      axis.line           = element_line(color = "#cccccc", linewidth = 0.4),
      panel.grid.major    = element_line(color = "#eeeeee", linewidth = 0.3),
      panel.grid.minor    = element_blank(),
      panel.background    = element_rect(fill = "white", color = NA),
      plot.background     = element_rect(fill = "white", color = NA),
      legend.position     = "bottom",
      legend.title        = element_text(face = "bold", size = base_size - 1),
      legend.text         = element_text(size = base_size - 1),
      legend.key.size     = unit(0.8, "lines"),
      strip.text          = element_text(face = "bold", size = base_size - 1),
      plot.margin         = margin(12, 12, 12, 12)
    )
}

# Event annotation helper — adds 3 vertical lines + labels consistently
add_events <- function(p, y_label_pos = Inf) {
  p +
    geom_vline(xintercept = as.Date("2024-08-01"),
               linetype = "dashed", color = COL_E1, linewidth = 0.5) +
    geom_vline(xintercept = as.Date("2025-07-01"),
               linetype = "dashed", color = COL_E2, linewidth = 0.5) +
    geom_vline(xintercept = as.Date("2026-01-01"),
               linetype = "dashed", color = COL_E3, linewidth = 0.5) +
    annotate("text", x = as.Date("2024-08-01"), y = y_label_pos,
             label = "E1: Image\nGen", vjust = 1.4, hjust = -0.08,
             size = 2.6, color = COL_E1, fontface = "italic") +
    annotate("text", x = as.Date("2025-07-01"), y = y_label_pos,
             label = "E2: Spicy\nMode", vjust = 1.4, hjust = -0.08,
             size = 2.6, color = COL_E2, fontface = "italic") +
    annotate("text", x = as.Date("2026-01-01"), y = y_label_pos,
             label = "E3: Crisis", vjust = 1.4, hjust = -0.08,
             size = 2.6, color = COL_E3, fontface = "italic")
}

# =============================================================================
# FIGURE 1 — Time Series: Grok vs AI NCII Search Interest
# Two-panel: top = raw monthly means, bottom = zoom on Grok terms
# =============================================================================

fig1_data <- platform_monthly |>
  mutate(
    Platform = if_else(platform == "Grok",
                       "Grok-Specific NCII Terms",
                       "General AI NCII Terms (Control)")
  )

# Top panel: both series
p1_top <- ggplot(fig1_data, aes(x = date, y = search_volume,
                                color = Platform, group = Platform)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  scale_color_manual(values = c(
    "Grok-Specific NCII Terms"       = COL_GROK,
    "General AI NCII Terms (Control)"= COL_AI
  )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               limits = c(as.Date("2021-01-01"), as.Date("2026-03-01"))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = NULL, y = "Mean Google Trends Index (0–100)",
    color = NULL
  ) +
  theme_pub()

p1_top <- add_events(p1_top, y_label_pos = Inf)

# Bottom panel: Grok only, zoomed in
p1_bottom <- fig1_data |>
  filter(platform == "Grok") |>
  ggplot(aes(x = date, y = search_volume)) +
  geom_area(fill = COL_GROK, alpha = 0.15) +
  geom_line(color = COL_GROK, linewidth = 0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               limits = c(as.Date("2021-01-01"), as.Date("2026-03-01"))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "Date", y = "Mean Google Trends Index (0–100)"
  ) +
  theme_pub()

p1_bottom <- add_events(p1_bottom, y_label_pos = Inf)

fig1 <- p1_top / p1_bottom +
  plot_annotation(
    title    = "Figure 1. Search Interest in NCII-Related Terms: Grok vs. General AI Baseline",
    subtitle = paste0(
      "Monthly Google Trends index (0–100, worldwide) for ten Grok-specific NCII search terms (blue) ",
      "and ten parallel\ngeneral AI NCII terms (red). ",
      "Upper panel shows both series; lower panel isolates Grok-specific terms.\n",
      "Dashed vertical lines mark three xAI product events: ",
      "E1 = Grok 2 image generation (Aug 2024), ",
      "E2 = Spicy Mode (Jul 2025), E3 = Public crisis (Jan 2026)."
    ),
    caption  = paste0(
      "Source: Google Trends (worldwide, monthly, March 2021–March 2026). ",
      "Index values are normalised relative to peak search volume within the series.\n",
      "N = 10 treatment keywords (Grok-specific NCII terms); ",
      "N = 10 control keywords (general AI NCII terms)."
    ),
    theme = theme(
      plot.title    = element_text(family = "serif", face = "bold", size = 12),
      plot.subtitle = element_text(family = "serif", size = 9, color = "#444444"),
      plot.caption  = element_text(family = "serif", size = 8, color = "#666666",
                                   hjust = 0)
    )
  )

ggsave("Figure1_TimeSeries.pdf",  fig1, width = 8, height = 7, dpi = 300)
ggsave("Figure1_TimeSeries.png",  fig1, width = 8, height = 7, dpi = 300)

# =============================================================================
# FIGURE 2 — ITS Forest Plot: Level-Shift Estimates by Keyword and Event
# =============================================================================

# Clean keyword labels
its_plot_data <- its_results_clean |>
  mutate(
    keyword_label = str_to_title(str_remove(keyword, "grok ")),
    event_label   = factor(event,
                           levels = c(
                             "Event 1: Aug 2024 (Image Gen)",
                             "Event 2: Jul 2025 (Spicy Mode)",
                             "Event 3: Jan 2026 (Crisis)"
                           ),
                           labels = c("E1: Aug 2024\n(Image Gen)",
                                      "E2: Jul 2025\n(Spicy Mode)",
                                      "E3: Jan 2026\n(Crisis)")
    ),
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error,
    significant = p.value < 0.05
  ) |>
  # Order keywords by E3 estimate (most dramatic first at top)
  mutate(keyword_label = reorder(keyword_label,
                                 ifelse(event == "Event 3: Jan 2026 (Crisis)",
                                        estimate, NA),
                                 FUN = function(x) max(x, na.rm = TRUE)))

event_colours <- c(
  "E1: Aug 2024\n(Image Gen)"  = COL_E1,
  "E2: Jul 2025\n(Spicy Mode)" = COL_E2,
  "E3: Jan 2026\n(Crisis)"     = COL_E3
)

fig2 <- ggplot(its_plot_data,
               aes(x = estimate, y = keyword_label,
                   color = event_label, shape = significant)) +
  geom_vline(xintercept = 0, linetype = "solid",
             color = "#888888", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25,
                 position = position_dodge(width = 0.7),
                 linewidth = 0.5) +
  geom_point(size = 2.8,
             position = position_dodge(width = 0.7)) +
  scale_color_manual(values = event_colours, name = "Event") +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                     labels = c("FALSE" = "p ≥ 0.05 (n.s.)",
                                "TRUE"  = "p < 0.05"),
                     name = "Significance") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x)) +
  labs(
    x = "Estimated Level Shift in Search Index (Google Trends units)",
    y = NULL,
    title = "Figure 2. ITS Level-Shift Estimates by Grok Search Term and Event",
    subtitle = paste0(
      "Regression Discontinuity in Time (RDiT) estimates with 95% confidence intervals. ",
      "Each point is the estimated\nstep-change in monthly search interest ",
      "for a Grok-specific NCII term at each of three product events.\n",
      "Filled circles = statistically significant (p < 0.05); ",
      "open circles = non-significant. HAC standard errors."
    ),
    caption = paste0(
      "Note: Estimates derive from OLS models of the form: ",
      "hits ~ time + post_E1 + post_E2 + post_E3, ",
      "estimated separately for each keyword.\n",
      "Newey-West heteroskedasticity and autocorrelation consistent (HAC) standard errors. ",
      "N = 62 monthly observations per keyword."
    )
  ) +
  theme_pub() +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    panel.grid.major.y = element_line(color = "#f0f0f0")
  ) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  )

ggsave("Figure2_ITS_Forest.pdf", fig2, width = 8.5, height = 6, dpi = 300)
ggsave("Figure2_ITS_Forest.png", fig2, width = 8.5, height = 6, dpi = 300)

# =============================================================================
# FIGURE 3 — DiD Coefficient Plot: Three Interaction Terms
# =============================================================================

did_plot_data <- did_results |>
  filter(str_detect(label, "DiD")) |>
  mutate(
    event_label = factor(
      c("E1: Aug 2024\n(Image Gen)",
        "E2: Jul 2025\n(Spicy Mode)",
        "E3: Jan 2026\n(Crisis)"),
      levels = c("E1: Aug 2024\n(Image Gen)",
                 "E2: Jul 2025\n(Spicy Mode)",
                 "E3: Jan 2026\n(Crisis)")
    ),
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error,
    direction = if_else(estimate > 0, "Grok > AI baseline", "Grok < AI baseline")
  )

fig3 <- ggplot(did_plot_data,
               aes(x = estimate, y = event_label, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "#888888", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.18, linewidth = 0.7) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(ifelse(estimate > 0, "+", ""),
                               round(estimate, 2), sig)),
            hjust = -0.25, size = 3.2,
            family = "serif", fontface = "bold") +
  scale_color_manual(
    values = c("Grok > AI baseline" = COL_GROK,
               "Grok < AI baseline" = COL_AI),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(-18, 26),
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x)
  ) +
  labs(
    x = "DiD Estimate (Grok NCII terms relative to AI baseline)",
    y = NULL,
    title = "Figure 3. Difference-in-Differences Estimates: Grok-Specific NCII Search Interest",
    subtitle = paste0(
      "DiD interaction coefficients (treatment × post-event) from OLS model with HAC standard errors.\n",
      "Positive values indicate Grok NCII searches increased more than the general AI NCII baseline.\n",
      "Negative values indicate Grok NCII searches lagged the AI baseline — ",
      "reflecting AI-sector growth exceeding nascent Grok-specific interest."
    ),
    caption = paste0(
      "Note: Model specification: search_volume ~ time + treatment + Σ(post_Ek + treatment×post_Ek) for k ∈ {1,2,3}.\n",
      "Newey-West HAC standard errors. Treatment = Grok NCII terms (N=10); ",
      "Control = general AI NCII terms (N=10). *** p<0.001.\n",
      "Negative DiD for E1 and E2 reflects the dominance of the AI sector trend over nascent Grok-specific searches ",
      "and should be\ninterpreted alongside the ITS estimates, which show absolute increases in Grok searches after each event."
    )
  ) +
  theme_pub() +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_line(color = "#eeeeee"),
    panel.grid.major.y = element_blank()
  )

ggsave("Figure3_DiD.pdf", fig3, width = 8.5, height = 4.5, dpi = 300)
ggsave("Figure3_DiD.png", fig3, width = 8.5, height = 4.5, dpi = 300)
cat("Figure 3 saved.\n")

# =============================================================================
# FIGURE 4 — Heatmap: Keyword × Month Search Interest
# Shows the exact moment each keyword activates
# =============================================================================

heatmap_data <- keyword_monthly |>
  mutate(
    keyword_label = str_to_title(str_remove(keyword, "grok ")),
    # Cap at 50 so the Jan 2026 spike doesn't bleach everything else
    hits_capped = pmin(hits, 50),
    year_month = floor_date(date, "month")
  ) |>
  filter(date >= as.Date("2023-06-01"))  # trim to relevant window

# Order rows by first month of sustained activity
kw_order <- heatmap_data |>
  group_by(keyword_label) |>
  summarise(first_active = min(date[hits > 0.5], na.rm = TRUE)) |>
  arrange(first_active) |>
  pull(keyword_label)

heatmap_data <- heatmap_data |>
  mutate(keyword_label = factor(keyword_label, levels = rev(kw_order)))

fig4 <- ggplot(heatmap_data,
               aes(x = year_month, y = keyword_label, fill = hits_capped)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Event lines
  geom_vline(xintercept = as.numeric(as.Date("2024-08-01")),
             color = COL_E1, linewidth = 0.8, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2025-07-01")),
             color = COL_E2, linewidth = 0.8, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2026-01-01")),
             color = COL_E3, linewidth = 0.8, linetype = "solid") +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    name    = "Search Index\n(capped at 50)",
    limits  = c(0, 50),
    breaks  = c(0, 10, 25, 50),
    labels  = c("0", "10", "25", "50+")
  ) +
  scale_x_date(
    date_breaks = "3 months", date_labels = "%b\n%Y",
    expand = expansion(mult = 0)
  ) +
  annotate("text", x = as.Date("2024-08-01"), y = 10.6,
           label = "E1", color = COL_E1, size = 3, fontface = "bold", vjust = -0.3) +
  annotate("text", x = as.Date("2025-07-01"), y = 10.6,
           label = "E2", color = COL_E2, size = 3, fontface = "bold", vjust = -0.3) +
  annotate("text", x = as.Date("2026-01-01"), y = 10.6,
           label = "E3", color = COL_E3, size = 3, fontface = "bold", vjust = -0.3) +
  labs(
    x = "Month",
    y = NULL,
    title = "Figure 4. Temporal Activation of Grok-Specific NCII Search Terms",
    subtitle = paste0(
      "Monthly Google Trends index for ten Grok NCII keywords, June 2023–March 2026. ",
      "Darker cells = higher search volume.\n",
      "Vertical lines mark the three Grok product events (E1–E3). ",
      "Values capped at 50 for visual clarity; Jan 2026 peaks reach 100."
    ),
    caption = paste0(
      "Source: Google Trends (worldwide). Index rescaled relative to peak within each query batch.\n",
      "E1 = Grok 2 image generation (Aug 2024); ",
      "E2 = Spicy Mode launch (Jul 2025); ",
      "E3 = January 2026 public crisis and regulatory spotlight."
    )
  ) +
  theme_pub() +
  theme(
    axis.text.y      = element_text(size = 9),
    legend.key.width = unit(1.2, "cm"),
    legend.position  = "right"
  )

ggsave("Figure4_Heatmap.pdf", fig4, width = 9, height = 5.5, dpi = 300)
ggsave("Figure4_Heatmap.png", fig4, width = 9, height = 5.5, dpi = 300)


# =============================================================================
# FIGURE 5 — Stacked Area: Cumulative Search Interest by Keyword Over Time
# Shows which terms carry the total volume and when
# =============================================================================

area_data <- keyword_monthly |>
  mutate(
    keyword_label = str_to_title(str_remove(keyword, "grok "))
  ) |>
  filter(date >= as.Date("2024-01-01"))

# Order stacking by total volume (largest at bottom)
kw_vol_order <- area_data |>
  group_by(keyword_label) |>
  summarise(total = sum(hits)) |>
  arrange(desc(total)) |>
  pull(keyword_label)

area_data <- area_data |>
  mutate(keyword_label = factor(keyword_label, levels = rev(kw_vol_order)))

# Palette: 10 distinct colours, muted
area_palette <- c(
  "#2C7BB6","#D7191C","#1A9641","#756BB1","#E6AB02",
  "#A6761D","#E08214","#80B1D3","#FB8072","#8DD3C7"
)
names(area_palette) <- kw_vol_order

fig5 <- ggplot(area_data,
               aes(x = date, y = hits, fill = keyword_label)) +
  geom_area(position = "stack", alpha = 0.85, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = as.Date("2024-08-01"),
             linetype = "dashed", color = "#333333", linewidth = 0.6) +
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype = "dashed", color = "#333333", linewidth = 0.6) +
  geom_vline(xintercept = as.Date("2026-01-01"),
             linetype = "dashed", color = "#333333", linewidth = 0.6) +
  annotate("label", x = as.Date("2024-08-01"), y = Inf,
           label = "E1: Image Gen\n(Aug 2024)",
           vjust = 1.5, hjust = -0.05, size = 2.5,
           fill = "white", color = "#333333", label.size = 0.2) +
  annotate("label", x = as.Date("2025-07-01"), y = Inf,
           label = "E2: Spicy Mode\n(Jul 2025)",
           vjust = 1.5, hjust = -0.05, size = 2.5,
           fill = "white", color = "#1A9641", label.size = 0.2) +
  annotate("label", x = as.Date("2026-01-01"), y = Inf,
           label = "E3: Crisis\n(Jan 2026)",
           vjust = 1.5, hjust = -0.05, size = 2.5,
           fill = "white", color = "#D7191C", label.size = 0.2) +
  scale_fill_manual(values = area_palette, name = "Search Term") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = "Date",
    y = "Stacked Google Trends Index",
    title = "Figure 5. Cumulative Search Volume by Grok NCII Keyword, 2024–2026",
    subtitle = paste0(
      "Stacked area chart showing the composition and growth of aggregate Grok NCII search interest.\n",
      "Each colour represents one search term; total height = aggregate index across all ten terms.\n",
      "Dashed lines mark the three product events."
    ),
    caption = paste0(
      "Source: Google Trends (worldwide, monthly). Stacked values represent the sum of ",
      "ten individual Google Trends indices;\n",
      "note that because each query batch is independently normalised, ",
      "absolute stacked values should be interpreted comparatively rather than as literal totals.\n",
      "E1 = Aug 2024 (image gen); E2 = Jul 2025 (Spicy Mode); E3 = Jan 2026 (crisis)."
    )
  ) +
  theme_pub() +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

ggsave("Figure5_StackedArea.pdf", fig5, width = 9.5, height = 6, dpi = 300)
ggsave("Figure5_StackedArea.png", fig5, width = 9.5, height = 6, dpi = 300)
cat("Figure 5 saved.\n")

# =============================================================================
# TABLE 1 — Descriptive Statistics by Platform and Period
# =============================================================================

flextable_theme <- function(ft) {
  ft |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 10, part = "body") |>
    fontsize(size = 10, part = "header") |>
    bold(part = "header") |>
    align(align = "center", part = "header") |>
    align(j = 1:2, align = "left", part = "body") |>
    align(j = 3:7, align = "center", part = "body") |>
    border_remove() |>
    hline_top(part = "header",
              border = fp_border(color = "black", width = 1.5)) |>
    hline(i = 1, part = "header",
          border = fp_border(color = "black", width = 0.5)) |>
    hline_bottom(part = "body",
                 border = fp_border(color = "black", width = 1.5)) |>
    set_table_properties(layout = "autofit") |>
    padding(padding = 3, part = "all")
}

table1_data <- desc_stats |>
  mutate(
    period = factor(period, levels = c(
      "Pre-Event 1 (before Aug 2024)",
      "Post-Event 1 (Aug 2024 – Jun 2025)",
      "Post-Event 2 (Jul–Dec 2025)",
      "Post-Event 3 (Jan 2026+)"
    )),
    platform = if_else(platform == "Grok",
                       "Grok NCII Terms",
                       "General AI NCII Terms")
  ) |>
  arrange(platform, period) |>
  dplyr::select(
    Platform = platform,
    Period = period,
    N = n_months,
    Mean = mean_volume,
    Median = median_volume,
    SD = sd_volume,
    Max = max_volume
  ) |>
  mutate(across(c(Mean, Median, SD, Max), ~round(., 2)))

table1_ft <- flextable(table1_data) |>
  flextable_theme() |>
  # Shade Grok rows lightly
  bg(i = which(table1_data$Platform == "Grok NCII Terms"),
     bg = "#f2f7fc", part = "body") |>
  merge_v(j = "Platform") |>
  valign(j = "Platform", valign = "top") |>
  set_header_labels(
    Platform = "Platform / Term Group",
    Period   = "Analytical Period",
    N        = "N (Months)",
    Mean     = "Mean",
    Median   = "Median",
    SD       = "SD",
    Max      = "Max"
  ) |>
  add_header_row(
    values = c("", "", "Google Trends Index (0–100)"),
    colwidths = c(1, 2, 4)
  ) |>
  align(i = 1, align = "center", part = "header") |>
  set_caption(
    caption = as_paragraph(
      as_chunk("Table 1. ",
               props = fp_text(bold = TRUE, font.family = "Times New Roman",
                               font.size = 10)),
      as_chunk(
        paste0("Descriptive Statistics of Google Trends Search Volume by Platform and Analytical Period"),
        props = fp_text(bold = FALSE, font.family = "Times New Roman",
                        font.size = 10)
      )
    )
  ) |>
  add_footer_lines(
    values = paste0(
      "Note. Google Trends index values are normalised to a 0–100 scale, ",
      "where 100 represents peak search volume within the query batch. ",
      "Treatment group comprises ten Grok-specific NCII search terms ",
      "(e.g., 'grok porn', 'grok undress', 'grok deepfake'). ",
      "Control group comprises ten parallel general AI NCII terms ",
      "(e.g., 'ai porn', 'ai undress', 'ai deepfake'). ",
      "N reflects monthly observations per platform per period. ",
      "E1 = Grok 2 image generation (Aug 2024); ",
      "E2 = Spicy Mode (Jul 2025); E3 = January 2026 public crisis. ",
      "Source: Google Trends (worldwide web search, Mar 2021–Mar 2026)."
    )
  ) |>
  font(fontname = "Times New Roman", part = "footer") |>
  fontsize(size = 9, part = "footer") |>
  italic(part = "footer") |>
  color(color = "#444444", part = "footer")

# =============================================================================
# TABLE 2 — ITS and DiD Results Combined
# =============================================================================

# ITS summary: for each event, how many keywords significant, mean estimate
its_summary <- its_results_clean |>
  mutate(
    event_short = dplyr::recode(event,
                                "Event 1: Aug 2024 (Image Gen)"  = "E1: Aug 2024 (Image Gen)",
                                "Event 2: Jul 2025 (Spicy Mode)" = "E2: Jul 2025 (Spicy Mode)",
                                "Event 3: Jan 2026 (Crisis)"     = "E3: Jan 2026 (Crisis)"
    )
  ) |>
  group_by(event_short) |>
  summarise(
    n_sig_p05    = sum(p.value < 0.05),
    n_sig_p001   = sum(p.value < 0.001),
    mean_est     = round(mean(estimate), 2),
    median_est   = round(median(estimate), 2),
    range_est    = paste0("[", round(min(estimate), 2), ", ",
                          round(max(estimate), 2), "]"),
    .groups = "drop"
  )

# DiD summary: just the three interaction terms
did_summary <- did_results |>
  filter(str_detect(label, "DiD")) |>
  mutate(
    event_short = c("E1: Aug 2024 (Image Gen)",
                    "E2: Jul 2025 (Spicy Mode)",
                    "E3: Jan 2026 (Crisis)"),
    did_est = round(estimate, 3),
    did_se  = round(std.error, 3),
    did_sig = sig
  ) |>
  dplyr::select(event_short, did_est, did_se, did_sig)

# Merge
table2_data <- its_summary |>
  left_join(did_summary, by = "event_short") |>
  mutate(
    did_cell = paste0(did_est, did_sig, "\n(", did_se, ")")
  ) |>
  dplyr::select(
    `Event`                  = event_short,
    `N Sig.\n(p < .05)`      = n_sig_p05,
    `N Sig.\n(p < .001)`     = n_sig_p001,
    `Mean\nEstimate`         = mean_est,
    `Median\nEstimate`       = median_est,
    `Range`                  = range_est,
    `DiD Estimate\n(SE)`     = did_cell
  )

table2_ft <- flextable(table2_data) |>
  flextable_theme() |>
  bg(i = 3, bg = "#fff5f5", part = "body") |>   # highlight crisis row
  bg(i = 2, bg = "#f5fff7", part = "body") |>   # highlight spicy row
  add_header_row(
    values = c("", "ITS / RDiT Results (N = 10 Keywords)", "DiD"),
    colwidths = c(1, 5, 1)
  ) |>
  align(i = 1, align = "center", part = "header") |>
  set_caption(
    caption = as_paragraph(
      as_chunk("Table 2. ",
               props = fp_text(bold = TRUE, font.family = "Times New Roman",
                               font.size = 10)),
      as_chunk(
        "Interrupted Time Series and Difference-in-Differences Results by Event",
        props = fp_text(bold = FALSE, font.family = "Times New Roman",
                        font.size = 10)
      )
    )
  ) |>
  add_footer_lines(
    values = paste0(
      "Note. ITS columns report results from ten separate OLS models ",
      "(one per Grok NCII keyword), each of the form: ",
      "hits ~ time + post_E1 + post_E2 + post_E3. ",
      "Estimates represent the level shift in the Google Trends index ",
      "at the event threshold. ",
      "N Sig. = number of keywords (out of 10) ",
      "for which the event dummy reached significance. ",
      "DiD Estimate is the treatment × post-event interaction coefficient ",
      "from a pooled OLS model comparing Grok NCII terms (treatment) ",
      "to general AI NCII terms (control); standard error in parentheses. ",
      "All models use Newey-West HAC standard errors. ",
      "E1 = Grok 2 image generation (Aug 2024); ",
      "E2 = Spicy Mode (Jul 2025); ",
      "E3 = January 2026 public crisis and regulatory spotlight. ",
      "*** p < .001, ** p < .01, * p < .05, . p < .10."
    )
  ) |>
  font(fontname = "Times New Roman", part = "footer") |>
  fontsize(size = 9, part = "footer") |>
  italic(part = "footer") |>
  color(color = "#444444", part = "footer")

# =============================================================================
# SAVE TABLES TO WORD DOCUMENT
# =============================================================================

doc <- read_docx() |>
  body_add_par("Table 1", style = "heading 1") |>
  body_add_flextable(table1_ft) |>
  body_add_par("") |>
  body_add_par("") |>
  body_add_par("Table 2", style = "heading 1") |>
  body_add_flextable(table2_ft)

print(doc, target = "Tables_1_and_2.docx")
cat("Tables saved to Tables_1_and_2.docx\n")


# 2 / 3 more

# =============================================================================
# APA TABLE THEME  — mirrors the example exactly:
#   top rule (1.5pt), sub-header rule (0.5pt), bottom rule (1.5pt)
#   no vertical lines, no shading, Times New Roman 11pt
# =============================================================================

apa_theme <- function(ft) {
  ft |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 11, part = "all") |>
    bold(part = "header") |>
    italic(part = "footer") |>
    fontsize(size = 10, part = "footer") |>
    align(align = "left",   part = "header") |>
    align(align = "left",   j = 1, part = "body") |>
    align(align = "center", j = 2:ncol_keys(ft), part = "body") |>
    border_remove() |>
    # top rule — thick
    hline_top(part = "header",
              border = fp_border(color = "black", width = 1.5)) |>
    # rule under column names — thin
    hline_bottom(part = "header",
                 border = fp_border(color = "black", width = 0.75)) |>
    # bottom rule — thick
    hline_bottom(part = "body",
                 border = fp_border(color = "black", width = 1.5)) |>
    padding(padding.top = 3, padding.bottom = 3,
            padding.left = 4, padding.right = 4, part = "all") |>
    set_table_properties(layout = "autofit")
}

# Helper: format p-value the APA way
fmt_p <- function(p) {
  case_when(
    p < .001 ~ "< .001",
    p < .01  ~ paste0(sprintf("%.3f", p), ""),
    p < .05  ~ paste0(sprintf("%.3f", p), ""),
    TRUE     ~ sprintf("%.3f", p)
  )
}

# Helper: significance stars only
stars <- function(p) {
  case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
}

# =============================================================================
# TABLE 1 — Descriptive Statistics
# =============================================================================

t1 <- desc_stats |>
  mutate(
    Group = if_else(platform == "Grok",
                    "Grok NCII Terms (Treatment)",
                    "General AI NCII Terms (Control)"),
    Period = factor(period, levels = c(
      "Pre-Event 1 (before Aug 2024)",
      "Post-Event 1 (Aug 2024 – Jun 2025)",
      "Post-Event 2 (Jul–Dec 2025)",
      "Post-Event 3 (Jan 2026+)"
    ))
  ) |>
  arrange(Group, Period) |>
  transmute(
    `Term Group`   = Group,
    `Period`       = as.character(Period),
    `n`            = n_months,
    `M`            = sprintf("%.2f", mean_volume),
    `Mdn`          = sprintf("%.2f", median_volume),
    `SD`           = sprintf("%.2f", sd_volume),
    `Max`          = sprintf("%.2f", max_volume)
  )

ft1 <- flextable(t1) |>
  apa_theme() |>
  merge_v(j = "Term Group") |>
  valign(j = "Term Group", valign = "top", part = "body") |>
  # thin rule between the two groups
  hline(i = 4, border = fp_border(color = "black", width = 0.5),
        part = "body") |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 1. Descriptive Statistics of Google Trends Search Volume by Term Group and Period",
        props = fp_text(bold = TRUE, font.family = "Times New Roman",
                        font.size = 11)
      )
    )
  ) |>
  add_footer_lines(paste0(
    "Note. Google Trends index values are normalised to a 0–100 scale ",
    "relative to peak search volume within each query batch. ",
    "Treatment group: ten Grok-specific NCII search terms ",
    "(e.g., grok porn, grok undress, grok deepfake). ",
    "Control group: ten parallel general AI NCII terms ",
    "(e.g., ai porn, ai undress, ai deepfake). ",
    "n = monthly observations. ",
    "E1 = Grok 2 image generation (August 2024); ",
    "E2 = Spicy Mode launch (July 2025); ",
    "E3 = January 2026 public crisis and regulatory spotlight."
  ))

# =============================================================================
# TABLE 2 — ITS and DiD Results
# =============================================================================

# ── ITS panel: one row per event, summarised across keywords ─────────────────
its_panel <- its_results_clean |>
  mutate(event_short = dplyr::recode(event,
                                     "Event 1: Aug 2024 (Image Gen)"  = "E1: August 2024 — Grok 2 image generation",
                                     "Event 2: Jul 2025 (Spicy Mode)" = "E2: July 2025 — Spicy Mode launch",
                                     "Event 3: Jan 2026 (Crisis)"     = "E3: January 2026 — Public crisis"
  )) |>
  group_by(event_short) |>
  summarise(
    n_kw_sig  = paste0(sum(p.value < .05), "/10"),
    m_est     = sprintf("%.2f", mean(estimate)),
    mdn_est   = sprintf("%.2f", median(estimate)),
    rng       = paste0("[", sprintf("%.2f", min(estimate)), ", ",
                       sprintf("%.2f", max(estimate)), "]"),
    .groups   = "drop"
  ) |>
  rename(
    `Event`                     = event_short,
    `Keywords Significant`      = n_kw_sig,
    `M Estimate`                = m_est,
    `Mdn Estimate`              = mdn_est,
    `Range`                     = rng
  )

# ── DiD panel: one row per event, the interaction coefficient ─────────────────
did_panel <- did_results |>
  filter(str_detect(label, "DiD")) |>
  transmute(
    `Event` = c(
      "E1: August 2024 — Grok 2 image generation",
      "E2: July 2025 — Spicy Mode launch",
      "E3: January 2026 — Public crisis"
    ),
    `β`   = sprintf("%.3f%s", estimate, sig),
    `SE`  = sprintf("(%.3f)", std.error),
    `p`   = fmt_p(p.value)
  )

# ── Merge ─────────────────────────────────────────────────────────────────────
t2 <- its_panel |>
  left_join(did_panel, by = "Event")

ft2 <- flextable(t2) |>
  apa_theme() |>
  # thin rules between events
  hline(i = 1:2, border = fp_border(color = "black", width = 0.4),
        part = "body") |>
  # spanning header rows
  add_header_row(
    values     = c("", "ITS / RDiT (N = 10 keywords per event)",
                   "DiD"),
    colwidths  = c(1, 4, 3)
  ) |>
  bold(i = 1, part = "header") |>
  align(i = 1, align = "center", part = "header") |>
  # rule under spanning row
  hline(i = 1, part = "header",
        border = fp_border(color = "black", width = 0.5)) |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2. Interrupted Time Series and Difference-in-Differences Results by Event",
        props = fp_text(bold = TRUE, font.family = "Times New Roman",
                        font.size = 11)
      )
    )
  ) |>
  add_footer_lines(paste0(
    "Note. ITS columns report results from ten separate OLS models ",
    "(one per Grok NCII keyword) of the form: ",
    "hits ~ time + post_E1 + post_E2 + post_E3. ",
    "Keywords Significant = number of keywords (out of 10) reaching p < .05 ",
    "at the event threshold. M Estimate and Mdn Estimate are the mean and ",
    "median level-shift coefficients across all ten keywords. ",
    "DiD β is the treatment × post-event interaction from a pooled OLS model ",
    "comparing Grok NCII terms (treatment) to general AI NCII terms (control); ",
    "standard error in parentheses. ",
    "Negative DiD estimates for E1 and E2 reflect faster growth in general AI ",
    "NCII searches relative to nascent Grok-specific interest during those periods ",
    "and should be read alongside the ITS estimates, which show absolute increases. ",
    "All models use Newey-West HAC standard errors. ",
    "*** p < .001, ** p < .01, * p < .05."
  ))

# =============================================================================
# SAVE TABLES
# =============================================================================

doc <- read_docx() |>
  body_add_par("Table 1", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft1) |>
  body_add_par("", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_par("Table 2", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft2)

print(doc, target = "Tables_Clean.docx")
cat("Tables saved.\n")

# =============================================================================
# FIGURE — Three-panel annotated time series
# Panel A: Both series full window
# Panel B: Grok only, 2024-2026, zoomed, with period shading
# Panel C: Per-keyword lines, 2025-2026 only (the dramatic window)
# =============================================================================

# Colour palette — accessible, prints well in greyscale
C_GROK   <- "#1f78b4"
C_AI     <- "#e31a1c"
C_SHADE1 <- "#f0f7ff"
C_SHADE2 <- "#f0fff4"
C_SHADE3 <- "#fff5f5"

# Event label data for annotations
events <- data.frame(
  date  = as.Date(c("2024-08-01", "2025-07-01", "2026-01-01")),
  label = c("E1: Grok 2\nImage Gen", "E2: Spicy\nMode", "E3: Public\nCrisis"),
  col   = c("#333333", "#1A9641", "#e31a1c")
)

theme_fig <- theme_minimal(base_size = 10) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, color = "#555555"),
    plot.caption     = element_text(size = 8, color = "#666666", hjust = 0),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(size = 8, color = "#333333"),
    panel.grid.major = element_line(color = "#eeeeee", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.text      = element_text(size = 9),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    plot.margin      = margin(8, 10, 8, 10)
  )

# ── Panel A: Full window, both series ────────────────────────────────────────
pA_data <- platform_monthly |>
  mutate(label = if_else(platform == "Grok",
                         "Grok NCII Terms",
                         "General AI NCII Terms"))

pA <- ggplot(pA_data, aes(x = date, y = search_volume,
                          color = label, group = label)) +
  # period shading
  annotate("rect",
           xmin = as.Date("2024-08-01"), xmax = as.Date("2025-07-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE1, alpha = 0.6) +
  annotate("rect",
           xmin = as.Date("2025-07-01"), xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE2, alpha = 0.6) +
  annotate("rect",
           xmin = as.Date("2026-01-01"), xmax = as.Date("2026-04-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE3, alpha = 0.6) +
  geom_line(linewidth = 0.7) +
  # event lines
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", linewidth = 0.45,
             color = events$col) +
  # event labels at top
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            inherit.aes = FALSE, vjust = 1.4, hjust = -0.07,
            size = 2.4, color = events$col, fontface = "italic",
            family = "serif") +
  scale_color_manual(values = c("Grok NCII Terms"         = C_GROK,
                                "General AI NCII Terms"   = C_AI)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2021-01-01"), as.Date("2026-03-15"))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = NULL, y = "Mean Search Index (0–100)",
    subtitle = "A. Full study window (2021–2026): both series"
  ) +
  theme_fig

# ── Panel B: Grok only, zoomed, period ribbons ────────────────────────────────
pB_data <- platform_monthly |>
  filter(platform == "Grok", date >= as.Date("2023-06-01")) |>
  mutate(
    period = case_when(
      date < as.Date("2024-08-01") ~ "Pre-events",
      date < as.Date("2025-07-01") ~ "Post-E1",
      date < as.Date("2026-01-01") ~ "Post-E2",
      TRUE                         ~ "Post-E3"
    ),
    period = factor(period, levels = c("Pre-events","Post-E1","Post-E2","Post-E3"))
  )

pB <- ggplot(pB_data, aes(x = date, y = search_volume)) +
  annotate("rect",
           xmin = as.Date("2024-08-01"), xmax = as.Date("2025-07-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE1, alpha = 0.7) +
  annotate("rect",
           xmin = as.Date("2025-07-01"), xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE2, alpha = 0.7) +
  annotate("rect",
           xmin = as.Date("2026-01-01"), xmax = as.Date("2026-04-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE3, alpha = 0.7) +
  geom_area(fill = C_GROK, alpha = 0.12) +
  geom_line(color = C_GROK, linewidth = 0.9) +
  geom_point(data = pB_data |> filter(date %in% events$date),
             color = C_GROK, size = 2.5) +
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", linewidth = 0.45,
             color = events$col) +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            inherit.aes = FALSE, vjust = 1.4, hjust = -0.07,
            size = 2.4, color = events$col, fontface = "italic",
            family = "serif") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2023-06-01"), as.Date("2026-03-15"))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = NULL, y = "Mean Search Index (0–100)",
    subtitle = "B. Grok NCII terms only (Jun 2023–Mar 2026)"
  ) +
  theme_fig +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ── Panel C: Per-keyword lines, crisis window ─────────────────────────────────
kw_palette <- c(
  "#1f78b4","#e31a1c","#33a02c","#ff7f00",
  "#6a3d9a","#b15928","#a6cee3","#fb9a99",
  "#b2df8a","#fdbf6f"
)

pC_data <- keyword_monthly |>
  filter(date >= as.Date("2025-01-01")) |>
  mutate(kw_label = str_to_title(str_remove(keyword, "grok ")))

pC <- ggplot(pC_data, aes(x = date, y = hits,
                          color = kw_label, group = kw_label)) +
  annotate("rect",
           xmin = as.Date("2025-07-01"), xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE2, alpha = 0.6) +
  annotate("rect",
           xmin = as.Date("2026-01-01"), xmax = as.Date("2026-04-01"),
           ymin = -Inf, ymax = Inf, fill = C_SHADE3, alpha = 0.6) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype = "dashed", color = "#1A9641", linewidth = 0.45) +
  geom_vline(xintercept = as.Date("2026-01-01"),
             linetype = "dashed", color = C_AI, linewidth = 0.45) +
  annotate("text", x = as.Date("2025-07-01"), y = Inf,
           label = "E2: Spicy Mode", vjust = 1.5, hjust = -0.07,
           size = 2.4, color = "#1A9641", fontface = "italic", family = "serif") +
  annotate("text", x = as.Date("2026-01-01"), y = Inf,
           label = "E3: Crisis", vjust = 1.5, hjust = -0.07,
           size = 2.4, color = C_AI, fontface = "italic", family = "serif") +
  scale_color_manual(values = setNames(kw_palette,
                                       unique(pC_data$kw_label))) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = "Date", y = "Search Index (0–100)",
    subtitle = "C. Individual keyword trajectories (Jan 2025–Mar 2026)"
  ) +
  theme_fig +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1),
    legend.position = "right",
    legend.text     = element_text(size = 7.5)
  ) +
  guides(color = guide_legend(ncol = 1, keyheight = unit(0.6, "lines")))

# ── Assemble ──────────────────────────────────────────────────────────────────
fig <- (pA / pB / pC) +
  plot_annotation(
    title = paste0(
      "Figure 1. Search Interest in Grok-Specific NCII Terms Relative to General AI Baseline, ",
      "2021–2026"
    ),
    subtitle = paste0(
      "Google Trends monthly search index (0–100, worldwide). ",
      "Shaded regions: blue = post-E1 window, green = post-E2 window, red = post-E3 window.\n",
      "E1 = Grok 2 image generation (August 2024); ",
      "E2 = Spicy Mode launch (July 2025); ",
      "E3 = January 2026 public crisis and regulatory spotlight."
    ),
    caption = paste0(
      "Source: Google Trends (worldwide web search, March 2021–March 2026). ",
      "Index rescaled to 0–100 relative to peak volume within each query batch.\n",
      "Treatment: N = 10 Grok-specific NCII search terms. ",
      "Control: N = 10 parallel general AI NCII terms."
    ),
    theme = theme(
      plot.title    = element_text(family = "serif", face = "bold", size = 12),
      plot.subtitle = element_text(family = "serif", size = 9, color = "#444444"),
      plot.caption  = element_text(family = "serif", size = 8,
                                   color = "#666666", hjust = 0)
    )
  )



write.csv(all_long, "grok_ncii_google_trends_raw.csv", row.names = FALSE)




