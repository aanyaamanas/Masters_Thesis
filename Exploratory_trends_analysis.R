# COMPARISON MAIN
# Search Interest for GROK TERMS VS AI TERMS 
getwd()
pacman::p_load(gtrendsR, dplyr, tibble, tidyr, skimr, lubridate, texreg, zoo, tseries, lmtest, sandwich, car, gridExtra, dendextend, forecast, vars, ggbeeswarm, kableExtra, broom, officer, flextable, patchwork)
# Defining your search terms
ai_terms <- c( "ai porn", "deepfake", "face swap", "remove clothes", "ai undress")
grok_terms <- c("grok deepfake",	"grok face swap",	"grok remove clothes",	"grok undress",	"grok porn")

ai_terms2 <- c("ai boobs", "ai nude", "ai ass", "ai bikini", "ai girl")
grok_terms2 <- c("grok boobs",	"grok nude",	"grok ass",	"grok bikini",	"grok girl")

# Time range (5 years)
time_range <- "2021-01-01 2026-01-01"

# Fetching Data 
ai_trends <- gtrends(keyword = ai_terms, time = time_range, geo = "")
ai_trends2 <-gtrends(keyword = ai_terms2, time = time_range, geo = "")

grok_trends <- gtrends(keyword = grok_terms, time = time_range, geo = "")
grok_trends2 <-gtrends(keyword = grok_terms2, time = time_range, geo = "")

#combining trends
ai_trends_df <- bind_rows(ai_trends$interest_over_time, ai_trends2$interest_over_time)
grok_trends_df <- bind_rows(grok_trends$interest_over_time, grok_trends2$interest_over_time)

# inspect
skimr::skim(ai_trends_df)
skimr::skim(grok_trends_df)

# AI terms wide 

ai_wide <- ai_trends_df |>
  dplyr::select(date, keyword, hits) |>
  mutate(hits = as.numeric(ifelse(hits == "<1", "0", as.character(hits)))) |>
  pivot_wider(names_from = keyword, values_from = hits) |>
  arrange(date)

# Grok terms wide

grok_wide <- grok_trends_df |>
  dplyr::select(date, keyword, hits) |>
  mutate(hits = as.numeric(ifelse(hits == "<1", "0", as.character(hits)))) |>
  pivot_wider(names_from = keyword, values_from = hits) |>
  arrange(date)

#Inspect

head(ai_wide)
head(grok_wide)

# Function to summarise a dataset

summarise_trends <- function(df) {
  df |>
    dplyr::select(-date) |>  # Explicitly call dplyr here
    summarise(across(everything(), list(
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      total = ~sum(., na.rm = TRUE)
    )))
}

ai_summary_labeled <- summarise_trends(ai_wide) %>%
  mutate(platform = "General AI")

grok_summary_labeled <- summarise_trends(grok_wide) %>%
  mutate(platform = "Grok")

# 1. Tidy the AI Summary
ai_summary_tidy <- ai_summary_labeled |>
  # Pivot everything except 'platform' into a long format
  pivot_longer(cols = -platform, names_to = "temp", values_to = "value") |>
  # Split "ai porn_mean" into "keyword" and "stat"
  # This regex handles keywords with spaces (like 'ai porn') correctly
  separate(temp, into = c("keyword", "stat"), sep = "_(?=[^_]+$)") |>
  # Pivot the stats (mean, median, etc.) back into their own columns
  pivot_wider(names_from = stat, values_from = value)

# 2. Tidy the Grok Summary (assuming it has the same structure)
grok_summary_tidy <- grok_summary_labeled |>
  pivot_longer(cols = -platform, names_to = "temp", values_to = "value") |>
  separate(temp, into = c("keyword", "stat"), sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = stat, values_from = value)

# Inspect
ai_summary_tidy
grok_summary_tidy

#TIME STAMPS

timestamps <- as.Date(c("2021-06-01","2023-03-01","2025-11-01"))

# AI 
ai_snapshots <- ai_wide |>
  filter(date %in% timestamps)

#grok
grok_snapshots <- grok_wide |>
  filter(date %in% timestamps)

# Computing percent changes between time stamps 
percent_change <- function(x) {
  round((x/lag(x)-1) * 100, 1)
}

# AI percent change table 
ai_pct <- ai_wide |>
  mutate(across(-date, percent_change)) |>
  filter(date %in% timestamps)

# Grok percent change table 
grok_pct <- grok_wide |>
  mutate(across(-date, percent_change)) |>
  filter(date %in% timestamps)

# Compare AI vs Grok — Total Search Interest & Growth
# Making a combined table showing for each keyword:
# Total hits
# Mean hits
# Median hits
# Percent change (from first to last date)


# Combining into one table 
combined_summary <- bind_rows(ai_summary_tidy, grok_summary_tidy) |>
  dplyr::select(platform, keyword, mean, median, total) |>
  arrange(platform, desc(mean))

colnames(ai_summary_labeled)

print(combined_summary)

# Peaks and Spikes 
# Function to create peak table for a dataset 
get_peaks <- function(df, platform_name) {
  df |>
    dplyr::select(date, keyword, hits) |>
    group_by(keyword) |>
    summarise(
      date_max = date[which.max(hits)],
      max_hits = max(hits, na.rm = TRUE),
      date_min = date[which.min(hits)],
      min_hits = min(hits, na.rm = TRUE),
      median_hits = median(hits, na.rm = TRUE)
    ) |>
    mutate(platform = platform_name) |>
    arrange(desc(max_hits))
}

# Applying to AI and Grok 
ai_long <- ai_wide |> 
  tidyr::pivot_longer(-date, names_to = "keyword", values_to = "hits") |>
  mutate(platform = "AI")

grok_long <- grok_wide |> 
  tidyr::pivot_longer(-date, names_to = "keyword", values_to = "hits") |>
  mutate(platform = "Grok")

# 2. Combine them into one master long frame
all_data_long1 <- bind_rows(ai_long, grok_long)

# 3. Identify the 95th percentile peaks and summarize in one go
peaks_table <- all_data_long1 |>
  group_by(platform, keyword) |>
  # We filter for peaks WITHIN the pipe to keep 'hits' visible
  filter(hits >= quantile(hits, 0.95, na.rm = TRUE)) |> 
  summarise(
    date_of_peak = date[which.max(hits)][1], # [1] handles ties safely
    max_hits     = max(hits, na.rm = TRUE),
    avg_hits_in_peak = mean(hits, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::select(platform, keyword, date_of_peak, max_hits, avg_hits_in_peak) |>
  arrange(platform, desc(max_hits))

# View the final table
print(peaks_table)


# Percent Change over Time
# Function to calculate % change 

calc_pct_change1 <- function(df_long1) {
  df_long1 |>
    arrange(keyword, date) |>
    group_by(keyword) |>
    mutate(
      pct_change = (hits - lag(hits)) / lag(hits) * 100 
    ) |>
    replace_na(list(pct_change = 0)) # since first value is NA, I am replacing it with 0
}

# Pivot long and calculating % change 
ai_pct1 <- ai_wide |>
  tidyr::pivot_longer(-date, names_to = "keyword", values_to = "hits") |>
  calc_pct_change1() |>
  mutate(platform = "AI")

grok_pct1 <- grok_wide |>
  tidyr::pivot_longer(-date, names_to = "keyword", values_to = "hits") |>
  calc_pct_change1() |>
  mutate(platform = "Grok")

# Combining into one table 
pct_change_table1 <- bind_rows(ai_pct1, grok_pct1) |>
  dplyr::select(platform, keyword, date, hits, pct_change)

# Inspect
head(pct_change_table1, 10)
view(pct_change_table1)

# AI vs Grok Comparison Table 

# Summarising means and total hits per platform and keyword
platform_summary1 <- pct_change_table1 |>
  group_by(platform, keyword) |>
  summarise(
    mean_hits = mean(hits),
    total_hits = sum(hits),
    mean_pct_change = mean(pct_change),
    .groups = "drop"
  )

# Pivot wider to compare AI and Grok side-by-side
comparison_table1 <- platform_summary1 |>
  tidyr::pivot_wider(
    names_from = platform,
    values_from = c(mean_hits, total_hits, mean_pct_change),
    names_glue = "{platform}_{.value}"
  )


# Trend Timing Analysis 
trend_timing1 <- pct_change_table1 |>
  group_by(platform, keyword) |>
  summarise(
    first_date = min(date),
    peak_date = date[which.max(hits)],
    peak_hits = max(hits),
    time_to_peak_days = as.numeric(difftime(peak_date, first_date, units = "days")),
    median_hits = median(hits),
    .groups = "drop"
  ) |>
  arrange(platform, desc(peak_hits))

# Combine AI and Grok data in long format 
all_trends_long1 <- bind_rows(
  ai_wide |> pivot_longer(-date, names_to = "keyword", values_to = "hits") |> mutate(platform = "AI"),
  grok_wide |> pivot_longer(-date, names_to = "keyword", values_to = "hits") |>  mutate(platform = "Grok")
)


# Function to calculate volatility metrics
volatility_metrics1 <- all_trends_long1 |>
  group_by(platform, keyword) |>
  summarise(
    mean_hits = mean(hits),
    sd_hits = sd(hits),
    cv_hits = ifelse(mean(hits) == 0, NA, sd(hits)/mean(hits)),
    zero_weeks = sum(hits == 0),
    longest_nonzero_streak = max(rle(hits != 0)$lengths[rle(hits != 0)$values == TRUE]),
    .groups = "drop"
  )

# Comparative Growth Dynamics

# Calculating weekly % growth for each keyword and platform, and 
# summarize it with:
# Mean weekly growth
# Median weekly growth
# Maximum weekly growth
# Minimum weekly growth

# This gives us insight into which trends spike quickly and which grow steadily

# Weekly % change 
growth_metrics1 <- all_trends_long1 |>
  arrange(platform, keyword, date) |>
  group_by(platform, keyword) |>
  mutate(
    pct_change = (hits - lag(hits)) / lag(hits) * 100,
    pct_change = ifelse(is.infinite(pct_change) | is.nan(pct_change), 0, pct_change)
  ) |>
  summarise(
    mean_growth = mean(pct_change, na.rm = TRUE),
    median_growth = median(pct_change, na.rm = TRUE),
    max_growth = max(pct_change, na.rm = TRUE),
    min_growth = min(pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# 
library(stats)

# Reshape wide for correlation
ai_wide2 <- all_trends_long1 |>
  filter(platform == "AI") |>
  dplyr::select(date, keyword, hits) |>
  pivot_wider(names_from = keyword, values_from = hits)

grok_wide2 <- all_trends_long1 |>
  filter(platform == "Grok") |>
  dplyr::select(date, keyword, hits) |>
  pivot_wider(names_from = keyword, values_from = hits)

# Aligning dates 
common_dates <- intersect(ai_wide2$date, grok_wide2$date)

ai_aligned <- ai_wide2 |>
  dplyr::filter(date %in% common_dates) |>
  dplyr::select(-date)

grok_aligned <- grok_wide2 |>
  dplyr::filter(date %in% common_dates) |>
  dplyr::select(-date)

# Pair-wise correlations 
cor_matrix1 <- cor(ai_aligned, grok_aligned, use = "pairwise.complete.obs")


# Detecting Trend Shifts and Peak Alignments 
# Function to compute cross-correlation with lag
compute_lag1 <- function(ai_series, grok_series, max_lag = 12) {
  ccf_res <- ccf(ai_series, grok_series, lag.max = max_lag, plot = FALSE)
  lag_at_max1 <- ccf_res$lag[which.max(ccf_res$acf)]
  max_corr1 <- max(ccf_res$acf)
  tibble(lag_weeks = lag_at_max1, max_corr = max_corr1)
}

# Example for AI "ai porn" vs Grok "grok porn"
lag_ai_porn <- compute_lag1(ai_aligned$`ai porn`, grok_aligned$`grok porn`)
lag_ai_porn

# Automate for all AI-Grok keyword pairs
ai_keywords <- colnames(ai_aligned)
grok_keywords <- colnames(grok_aligned)

lag_matrix1 <- expand.grid(ai = ai_keywords, grok = grok_keywords) |>
  rowwise() |>
  mutate(
    lag_info = list(compute_lag1(ai_aligned[[ai]], grok_aligned[[grok]]))
  ) |>
  unnest(lag_info)

# Clustering and Pattern Detection 
library(cluster)
install.packages("factoextra")
library(factoextra)

# Combining key metrics 
clustering_df1 <- volatility_metrics1 |>
  dplyr::select(platform, keyword, mean_hits, sd_hits, cv_hits, longest_nonzero_streak) |>
  left_join(growth_metrics1 |>
              dplyr::select(platform, keyword, mean_growth, max_growth), 
            by = c("platform","keyword")) |>
  left_join(lag_matrix1 |>
              mutate(keyword = ai) |>  
              group_by(keyword) |>
              summarise(
                max_corr_val = max(max_corr, na.rm = TRUE), 
                lag_at_max = lag_weeks[which.max(max_corr)], 
                .groups = "drop"
              ) |>
              dplyr::select(keyword, max_corr_val, lag_at_max),
            by = "keyword") |>
  replace_na(list(max_corr_val = 0, lag_at_max = 0))

head(clustering_df1)

# Scaling numeric variables 
clustering_scaled1 <- clustering_df1 |>
  dplyr::select(mean_hits, sd_hits, cv_hits, longest_nonzero_streak, mean_growth, max_growth, max_corr_val, lag_at_max) |>
  scale()

# Computing distance matrix and performing hierarchical clustering 
dist_matrix1 <- dist(clustering_scaled1, method = "euclidean")
hc1 <- hclust(dist_matrix1, method = "ward.D2")

# Visualizing dendrogram 
fviz_dend(hc1, k = 4,
          cex = 0.8, 
          k_colors = c("#1b9e77", "steelblue", "#7570b3", "pink"),
          main = "Cluster Dendrogram of AI & Grok Keywords")

#Cutting tree and getting cluster assignments
clusters1 <- cutree(hc1, k = 4)
clustering_df1$cluster <- clusters1

# Check cluster membership
clustering_df1 |> arrange(cluster)

# Time Series Descriptives and Trend Smoothing 

# Capturing weekly and cumulative dynamics for each keyword on AI and Grok
# Identifying spikes, steady growth, and volatility visually
# Sub-steps:
#   Pivot data into long format for easier plotting

# Then calculating rolling averages:
# 4-week rolling = short-term trend
# 12-week rolling = seasonal/long-term trend

# Calculating cumulative hits to show total reach over time
# Flag peak events: any week that exceeds 90th percentile of hits for that keyword.
# Visualizing:
# 1. Line plots faceted by keyword showing raw hits + rolling averages
# 2. Color by platform for direct AI vs Grok comparison

# Why: Provides baseline understanding of timing, spikes, and long-term trends before modeling

time_series_df1 <- all_trends_long1 |>  
  dplyr::select(date, platform, keyword, hits) |>
  arrange(platform, keyword, date) |>
  group_by(platform, keyword) |>
  mutate(
    # Rolling averages
    roll4 = zoo::rollmean(hits, k = 4, fill = NA, align = "right"),   # short-term trend
    roll12 = zoo::rollmean(hits, k = 12, fill = NA, align = "right"), # long-term trend
    # Cumulative hits
    cum_hits = cumsum(hits),
    # Flagging peaks (top 10% per keyword)
    peak_flag = if_else(hits >= quantile(hits, 0.9, na.rm = TRUE), TRUE, FALSE)
  ) |>
  ungroup()

# Plotting raw hits and rolling averages 
ggplot(time_series_df1, aes(x = date, y = hits, color = platform)) +
  geom_line(alpha = 0.6) +
  geom_line(aes(y = roll4), linetype = "dashed", size = 0.8) +
  geom_line(aes(y = roll12), linetype = "dotdash", size = 0.8) +
  geom_point(data = subset(time_series_df1, peak_flag == TRUE),
             aes(y = hits), shape = 21, fill = "red", size = 2, stroke = 1) +
  facet_wrap(~keyword, scales = "free_y") +
  labs(
    title = "AI vs Grok Keyword Trends",
    subtitle = "Raw hits, 4-week (dashed) and 12-week (dotdash) rolling averages\nRed dots = top 10% peaks",
    x = "Date",
    y = "Hits"
    ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

write.csv(time_series_df1, "time_series_keyword_data.csv", row.names = FALSE)

# Faceted line plots for trends 
library(scales)
time_series_df1 <- time_series_df1 |>
  mutate(date = as.Date(date))

ggplot(time_series_df1, aes(x = date, y = hits, color = platform)) +
  geom_line(alpha = 0.4) + # Raw hits, slightly transparent
  geom_line(aes(y = roll4), size = 0.8) +   # Short-term trend
  geom_line(aes(y = roll12), size = 1.1, linetype = "dashed") + # Long-term trend
  geom_point(data = time_series_df1 |>
               filter(peak_flag),
             aes(x = date, y = hits),
             color = "red",
             size = 2) + # Highlight peaks
  facet_wrap(~ keyword, scales = "free_y", ncol = 3) + # Separating panel per keyword
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(
    title = "AI vs Grok Keyword Trends: Raw Hits, Rolling Averages & Peaks",
    subtitle = "4-week rolling (solid), 12-week rolling (dashed), red points = top 10% hits",
    x = "Date",
    y = "Hits",
    color = "Platform"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Cumulative Hits and Peak Event Analysis
ggplot(time_series_df1, aes(x = date, y = cum_hits, color = platform)) +
  geom_line(size = 1) +
  facet_wrap(~ keyword, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(
    title = "Cumulative Hits Over Time: AI vs Grok Keywords",
    subtitle = "Total reach accumulation per keyword",
    x = "Date",
    y = "Cumulative Hits",
    color = "Platform"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# OR STACKING PLOTS 
time_series_agg1 <- time_series_df1 |>
  mutate(month = floor_date(date, "month")) |>
  group_by(platform, keyword, month) |>
  summarise(
    hits = sum(hits),
    roll4 = mean(roll4, na.rm = TRUE),
    roll12 = mean(roll12, na.rm = TRUE),
    peak_flag = any(peak_flag),
    .groups = "drop"
  ) |>
  rename(date = month)

# AI plot 
#Separate filtered data for peaks
# 1. Standardize the main data
time_series_agg1$date <- lubridate::as_date(time_series_agg1$date)

ai_peaks <- ai_long |>
  group_by(keyword) |>
  summarise(
    date = date[which.max(hits)][1],
    hits = max(hits, na.rm = TRUE),
    date_min = date[which.min(hits)][1],
    min_hits = min(hits, na.rm = TRUE),
    median_hits = median(hits, na.rm = TRUE),
    platform = first(platform),
    .groups = "drop"
  )

grok_peaks1 <- grok_long |>
  group_by(keyword) |>
  summarise(
    date = date[which.max(hits)][1],
    hits = max(hits, na.rm = TRUE),
    date_min = date[which.min(hits)][1],
    min_hits = min(hits, na.rm = TRUE),
    median_hits = median(hits, na.rm = TRUE),
    platform = first(platform),
    .groups = "drop"
  )

# 2. Fix the AI peaks with a "Smart" date conversion
ai_peaks_final <- ai_peaks |>
  as.data.frame() |>
  # Set names by position to avoid the 'not found' ghost
  setNames(c("keyword", "date", "hits", "date_min", "min_hits", "median_hits", "platform")) |>
  mutate(date = lubridate::as_date(date)) |> # SMART DATE CONVERSION
  dplyr::select(keyword, date, hits) |>
  filter(!is.na(date)) # Remove any rows that failed to convert

# 3. Fix the Grok peaks
grok_peaks_final <- grok_peaks1 |>
  as.data.frame() |>
  mutate(date = lubridate::as_date(date)) |>
  dplyr::select(keyword, date, hits) |>
  filter(!is.na(date))

# 4. Final Plot Attempt
p_ai1 <- time_series_agg1 |>
  dplyr::filter(platform == "AI") |>
  ggplot(aes(x = date, y = hits, color = keyword)) +
  geom_line(alpha = 0.3) +
  geom_line(aes(y = roll4), linewidth = 0.8) +
  geom_line(aes(y = roll12), linewidth = 1.1, linetype = "dashed") +
  geom_point(data = ai_peaks_final, 
             aes(x = date, y = hits), 
             color = "red", size = 2, inherit.aes = FALSE) + 
  facet_wrap(~ keyword, scales = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%b %Y") +
  theme_minimal() +
  theme(legend.position = "none")

p_grok1 <- time_series_agg1 |>
  dplyr::filter(platform == "Grok") |>
  ggplot(aes(x = date, y = hits, color = keyword)) +
  geom_line(alpha = 0.3) +
  geom_line(aes(y = roll4), linewidth = 0.8) +
  geom_line(aes(y = roll12), linewidth = 1.1, linetype = "dashed") +
  geom_point(data = grok_peaks_final, 
             aes(x = date, y = hits), 
             color = "red", size = 2, inherit.aes = FALSE) +
  facet_wrap(~ keyword, scales = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%b %Y") +
  theme_minimal() +
  theme(legend.position = "none")

library(patchwork)
p_ai1 / p_grok1

colnames(ai_peaks)
colnames(grok_peaks1)

# Comparative Interrupted Time Series (ITS) / Regression Analysis
# Objective: To quantify whether spikes or trends for "AI" 
# lead, lag, or correlate with "Grok",  while accounting for 
# baseline trend, seasonality, and volatility. 
# Essentially, moving into causal or quasi-causal analysis


# Sub-steps:
# Data Preparation for ITS/Regression
# Using time_series_df or pct_change_table aggregated weekly
# Creating platform-specific time indicators (e.g., AI_hits, Grok_hits) for each keyword
# Adding lagged variables based on your lag_matrix to test delayed effects (e.g., AI_hits_lag4 → effect on Grok_hits)
# Including control variables for volatility (sd_hits, cv_hits) or peaks (peak_flag) to adjust for irregular bursts

#Model Selection
# 1 - ITS model: 
# For each keyword pair, regress Grok hits on AI hits (or vice versa), 
# including pre- and post-spike periods, and optional lagged terms

# FORMULA TEMPLATE
# Grok_hits ~ time + AI_hits + AI_hits_lag + peak_flag + offset

# 2- Panel regression / mixed-effects model
# Using keyword as a random effect to account for repeated measures and platform differences
# This is useful for analysing all keywords simultaneously

# EXAMPLE
# lme4::lmer(hits ~ platform*lag_hits + (1|keyword), data = time_series_df)

# Testing Lead-Lag Relationships

# Utilising the lag_matrix to implement cross-lagged regression:
# Testing if spikes in AI precede Grok spikes (or vice versa)
# Computing Granger causality tests for each AI-Grok keyword pair to assess 
# whether one platform’s past hits predict the other’s future hits

# Incorporating Volatility / Growth Metrics
# Including cv_hits, mean_growth, max_growth as covariates to see if the type of
# trend (steady vs spiky) affects cross-platform influence
# This makes the model more nuanced, controlling for trend quality, not just magnitude

# Visualization of Model Effects
# For ITS: plot predicted vs observed hits, showing counterfactual baseline trend (no AI spike) vs actual Grok hits
# For cross-lagged regression: heatmap of max_corr values with lag direction (positive/negative weeks) to visually summarize temporal influence

library(broom)
library(stringr)
#
its_df_wide1 <- all_data_long1 |>
dplyr::select(date, keyword, hits) |>
  pivot_wider(names_from = keyword, values_from = hits) |>
  rename_with(~str_replace_all(., " ", "_"))

# 3. Quick check to make sure it exists now
colnames(its_df_wide1)

# Ensuring no spaces in keyword names
its_df_wide1 <- its_df_wide1 |>
  rename_with(~str_replace_all(., " ", "_"))

#
lag_list_clean1 <- expand.grid(ai = colnames(ai_aligned), 
                               grok = colnames(grok_aligned)) |>
  rowwise() |>
  mutate(
    lag_info = list(compute_lag1(ai_aligned[[ai]], grok_aligned[[grok]])),
    ai = str_replace_all(as.character(ai), " ", "_"),
    grok = str_replace_all(as.character(grok), " ", "_")
  ) |>
  unnest(lag_info) |>
  mutate(
    lag_weeks = if_else(lag_weeks <= 0, 1, as.numeric(lag_weeks))
  )

# Inspect
head(lag_list_clean1)

lag_list_clean1 <- lag_list_clean1 |>
  mutate(
    ai = str_replace_all(ai, " ", "_"),
    grok = str_replace_all(grok, " ", "_"),
    # Ensure lag is at least 1 week
    lag_weeks = if_else(lag_weeks <= 0, 1, lag_weeks)
  )

# Define lagged column create function
create_lagged_column1 <- function(ai_kw, grok_kw, lag_val, df) {
  ai_col <- paste0("hits_", ai_kw)
  new_col <- paste0("hits_", ai_kw, "_lag_for_", grok_kw)
  
  if (!ai_col %in% colnames(df)) {
    warning("Column not found: ", ai_col)
    return(df)
  }
  
  df |>
    mutate(!!new_col := dplyr::lag(.data[[ai_col]], n = lag_val))
}

library(purrr)

# Pivot wide 
its_df_wide1 <- all_trends_long1 |>
  filter(platform == "AI") |>      # only AI keywords, since lags are AI -> Grok
  dplyr::select(-platform) |>
  mutate(keyword = str_replace_all(keyword, " ", "_")) |> 
  pivot_wider(
    names_from = keyword,
    values_from = hits,
    names_prefix = "hits_"
  ) |>
  # Adding time variables
  mutate(
    date = as.Date(date),
    post_launch = if_else(date >= as.Date("2023-11-03"), 1, 0),
    post_spicy  = if_else(date >= as.Date("2025-07-28"), 1, 0),
    post_tweet  = if_else(date >= as.Date("2026-01-03"), 1, 0),
    week_index = 1:n()
  )

# Applying lag for all AI → Grok pairs
# Using the lag_list_clean1 and create_lagged_column1 we defined previously
its_df_lagged <- reduce(
  1:nrow(lag_list_clean1),
  function(df, i) {
    create_lagged_column1(
      ai_kw = lag_list_clean1$ai[i],
      grok_kw = lag_list_clean1$grok[i],
      lag_val = lag_list_clean1$lag_weeks[i],
      df = df
    )
  },
  .init = its_df_wide1
)

# Inspect the result
colnames(its_df_lagged)
head(its_df_lagged[, 1:15])


library(stringr)

# Re-build the wide dataframe and force numeric values

its_df_wide1 <- all_trends_long1 |>
  dplyr::select(date, keyword, hits) |>
  mutate(
    keyword = str_replace_all(keyword, " ", "_"),
    hits = as.numeric(hits) # Ensure hits are numeric before pivoting
  ) |>
  pivot_wider(
    names_from = keyword,
    values_from = hits,
    names_prefix = "hits_",
    values_fn = ~mean(.x, na.rm = TRUE) # This prevents the 'list' error
  ) |>
  # 2. Add time indicators
  mutate(
    date = as.Date(date),
    post_launch = if_else(date >= as.Date("2023-11-03"), 1, 0),
    post_spicy  = if_else(date >= as.Date("2025-07-28"), 1, 0),
    post_tweet  = if_else(date >= as.Date("2026-01-03"), 1, 0),
    week_index = 1:n()
  )

# 3. Re-run your lag creation code (the 'reduce' block you used before)
# Corrected: Using the '1' suffix for objects and functions defined in your main block
its_df_lagged1 <- reduce(
  1:nrow(lag_list_clean1),
  function(df, i) {
    create_lagged_column1( # Corrected function name
      ai_kw = lag_list_clean1$ai[i],
      grok_kw = lag_list_clean1$grok[i],
      lag_val = lag_list_clean1$lag_weeks[i],
      df = df
    )
  },
  .init = its_df_wide1 # Corrected starting object
)

# Function to run ITS safely for each keyword
run_its_safe1 <- function(grok_kw, df) {
  grok_clean <- str_replace_all(grok_kw, " ", "_")
  grok_col <- paste0("hits_", grok_clean)
  lag_col <- grep(paste0("_lag_for_", grok_clean), colnames(df), value = TRUE)
  
  # Building formula dynamically
  formula_str <- paste(grok_col, "~ week_index + post_launch + post_spicy + post_tweet")
  if(length(lag_col) > 0) formula_str <- paste(formula_str, "+", lag_col[1])
  
  tryCatch({
    model <- lm(as.formula(formula_str), data = df)
    tidy(model) |> mutate(grok_keyword = grok_kw)
  }, error = function(e) return(NULL))
}

# Run models
grok_keywords_clean <- unique(grok_trends_df$keyword)
its_results <- map_dfr(grok_keywords_clean, ~run_its_safe1(.x, its_df_lagged))
head(its_results, 20)

###

grok_wide_for_its <- grok_wide |>
  mutate(across(-date, ~as.numeric(ifelse(. == "<1", "0", as.character(.))))) |>
  rename_with(~paste0("hits_", str_replace_all(., " ", "_")), -date)

# 2. Merge Grok targets into the lagged AI dataframe
its_df_final <- its_df_lagged |>
  left_join(grok_wide_for_its, by = "date")

# 3. Run the models again using the combined dataframe
grok_keywords_clean <- unique(grok_trends_df$keyword)
its_results <- map_dfr(grok_keywords_clean, ~run_its_safe1(.x, its_df_final))

# 4. Verify results before cleaning
if(nrow(its_results) > 0) {
  cat("✓ Successfully estimated models for Grok keywords.\n")
  
  # Now run your cleaning and table generation
  its_results_clean <- its_results |>
    filter(term %in% c("week_index", "post_launch", "post_spicy", "post_tweet")) |>
    mutate(term = dplyr::recode(term,
                                "week_index" = "Time Trend",
                                "post_launch" = "Post Launch",
                                "post_spicy" = "Post Spicy Event",
                                "post_tweet" = "Post Tweet"))
} else {
  cat("⚠️ Error: its_results is still empty. Check if run_its_safe1 is matching column names correctly.")
}


# CLEANING 
its_results_clean <- its_results |>
  filter(term %in% c("week_index", "post_launch", "post_spicy", "post_tweet")) |>
  mutate(term = dplyr::recode(term,
                              "week_index" = "Time Trend",
                              "post_launch" = "Post Launch",
                              "post_spicy" = "Post Spicy Event",
                              "post_tweet" = "Post Tweet"))

# Visual ITS effect
ggplot(its_results_clean, aes(x = estimate, y = grok_keyword, color = term)) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = estimate - 1.96*std.error, 
                    xmax = estimate + 1.96*std.error), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  labs(x = "Effect Estimate", y = "Grok Keyword", color = "Term") +
  theme_minimal()

# Table formatting
important_terms <- c("(Intercept)", "Time Trend", "Post Launch", "Post Spicy Event", "Post Tweet")

its_table1 <- its_results_clean |> # Corrected: using the cleaned results
  mutate(
    estimate_fmt = case_when(
      p.value < 0.001 ~ paste0(round(estimate, 3), "***"),
      p.value < 0.01  ~ paste0(round(estimate, 3), "**"),
      p.value < 0.05  ~ paste0(round(estimate, 3), "*"),
      TRUE ~ as.character(round(estimate, 3))
    ),
    std.error_fmt = paste0("(", round(std.error, 3), ")")
  ) |>
  dplyr::select(grok_keyword, term, estimate_fmt, std.error_fmt)

# Creating a treatment index 
# Corrected: Generating its_platform_df from main long data
its_platform_df1 <- all_trends_long1 |>
  group_by(platform, date) |>
  summarise(search_volume = mean(hits, na.rm = TRUE), .groups = "drop") |>
  mutate(
    date = as.Date(date),
    post_launch = if_else(date >= as.Date("2023-11-03"), 1, 0),
    treatment = if_else(platform == "Grok", 1, 0)
  ) |>
  group_by(platform) |>
  arrange(date) |>
  mutate(time = row_number()) |>
  ungroup()

# ITS Interaction Model
its_model1 <- lm(
  search_volume ~ time + treatment + post_launch + treatment:post_launch,
  data = its_platform_df1
)

summary(its_model1)

# TABLES GROUP 1

theme_group1 <- function(x) {
  x <- theme_booktabs(x)
  x <- font(x, fontname = "Times New Roman", part = "all")
  x <- fontsize(x, size = 11, part = "all")
  x <- align(x, align = "center", part = "header")
  x <- bold(x, part = "header")
  x <- set_table_properties(x, layout = "autofit")
  return(x)
}

# TABLE 1:Descriptive Statistics by Platform

#Prepare Data

table1_data_g1 <- volatility_metrics1 |>
  group_by(platform) |>
  summarise(
    `Avg Hits` = sprintf("%.2f", mean(mean_hits, na.rm = TRUE)),
    `Volatility (CV)` = sprintf("%.2f", mean(cv_hits, na.rm = TRUE)),
    `Zero Search Weeks` = sprintf("%.1f", mean(zero_weeks, na.rm = TRUE)),
    `Max Streak` = sprintf("%.1f", mean(longest_nonzero_streak, na.rm = TRUE))
  ) |>
  rename(Platform = platform)

# Create Flextable

table1_g1 <- flextable(table1_data_g1) |>
  theme_group1() |>
  set_caption(caption = "Table 1. Descriptive Statistics of Search Volume Intensity and Persistence") |>
  add_footer_lines("Note: CV = Coefficient of Variation (SD/Mean). Max Streak refers to consecutive weeks with non-zero hits.")

table1_g1

# TABLE 2: Cross-Platform Lag & Correlation Analysis

# Prepare Data 

table2_data_g1 <- lag_list_clean1 |>
  arrange(desc(max_corr)) |>
  head(10) |> # Showing top 10 strongest relationships
  dplyr::select(ai, grok, max_corr, lag_weeks) |>
  mutate(
    max_corr = sprintf("%.3f", max_corr),
    lag_weeks = paste(lag_weeks, "Weeks")
  ) |>
  rename(`AI Term` = ai, `Grok Term` = grok, `Peak Corr` = max_corr, `Lead/Lag` = lag_weeks)

# Create Flextable
table2_g1 <- flextable(table2_data_g1) |>
  theme_group1() |>
  set_caption(caption = "Table 2. Cross-Correlation Analysis: Mainstream AI Leading Indicators for Grok Interest") |>
  add_footer_lines("Note: Positive lags indicate Mainstream AI interest precedes Grok interest.")

table2_g1

# TABLE 3: Interrupted Time Series (ITS) Model Results

# Prepare Data 
table3_data_g1 <- its_results_clean |>
  mutate(
    Estimate = sprintf("%.3f", estimate),
    `Std. Error` = sprintf("(%.3f)", std.error),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) |>
  mutate(Estimate = paste0(Estimate, Sig)) |>
  dplyr::select(grok_keyword, term, Estimate, `Std. Error`) |>
  pivot_wider(names_from = term, values_from = c(Estimate, `Std. Error`))

# Create Flextable
table3_g1 <- flextable(table3_data_g1) |>
  theme_group1() |>
  set_caption(caption = "Table 3. Interrupted Time Series (ITS) Estimates: Level Shifts Following Platform Events") |>
  add_footer_lines("Note: Standard errors in parentheses. *** p<0.001, ** p<0.01, * p<0.05.")

table3_g1

# TABLE 4: Difference-in-Differences Summary

# Prepare Data 
table4_data_g1 <- broom::tidy(its_model1) |>
  mutate(
    term = dplyr::recode(term,
                         "(Intercept)" = "Intercept",
                         "time" = "Temporal Trend",
                         "treatment" = "Grok Treatment",
                         "post_launch" = "Launch Level Shift",
                         "treatment:post_launch" = "Interaction (DiD)"),
    estimate = sprintf("%.4f", estimate),
    std.error = sprintf("(%.4f)", std.error),
    p.value = scales::pvalue(p.value)
  ) |>
  rename(Variable = term, Estimate = estimate, SE = std.error, `P-Value` = p.value) |>
  dplyr::select(Variable, Estimate, SE, `P-Value`)

# Create Flextable
table4_g1 <- flextable(table4_data_g1) |>
  theme_group1() |>
  set_caption(caption = "Table 4. Platform Comparison: Double Interrupted Time Series Regression Results")

table4_g1

# 
cat("GENERATING TABLES \n\n")

doc <- read_docx()

# 2. Add Table 1: Descriptive Statistics
cat("Adding Table 1: Descriptive Statistics\n")
doc <- doc |>
  body_add_flextable(table1_g1) |>
  body_add_par("") |> body_add_par("") # Adding spacers

# 3. Add Table 2: Lag & Correlation
cat("Adding Table 2: Cross-Platform Lag Analysis\n")
doc <- doc |>
  body_add_flextable(table2_g1) |>
  body_add_par("") |> body_add_par("")

# 4. Add Table 3: ITS Model Results
cat("Adding Table 3: ITS Model Estimates\n")
doc <- doc |>
  body_add_flextable(table3_g1) |>
  body_add_par("") |> body_add_par("")

# 5. Add Table 4: Double ITS / DiD
cat("Adding Table 4: Platform Comparison (DiD)\n")
doc <- doc |>
  body_add_flextable(table4_g1)

# 6. Save the Word Document
print(doc, target = "Aanyaa_Thesis_Regression_Tables.docx")

# SAVING INDIVIDUAL IMAGES

# Note: requires the 'webshot' or 'webshot2' package
save_as_image(table1_g1, "table1_descriptives.png", zoom = 2)
save_as_image(table2_g1, "table2_lag_analysis.png", zoom = 2)
save_as_image(table3_g1, "table3_its_results.png", zoom = 2)
save_as_image(table4_g1, "table4_did_comparison.png", zoom = 2)


# --- CONSOLE PREVIEW ---
print(table1_g1)

print(table4_g1)


###
#COMPLETING SCRIPT

#  CROSS-CORRELATION (LEAD-LAG) ANALYSIS
# Testing if Mainstream AI interest precedes Grok-specific adversarial intent
platform_agg_s1 <- all_data_long1 |>
  group_by(date, platform) |>
  summarise(total_hits = sum(hits, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = platform, values_from = total_hits)

ccf_res1 <- ccf(platform_agg_s1$AI, platform_agg_s1$Grok, lag.max = 20, plot = FALSE)

ccf_df_s1 <- data.frame(
  lag = ccf_res1$lag,
  correlation = ccf_res1$acf,
  significant = abs(ccf_res1$acf) > 2/sqrt(nrow(platform_agg_s1))
)

# Plot CCF
ggplot(ccf_df_s1, aes(x = lag, y = correlation)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2/sqrt(nrow(platform_agg_s1)), 2/sqrt(nrow(platform_agg_s1))), 
             linetype = "dashed", color = "red") +
  geom_segment(aes(xend = lag, yend = 0, color = significant), size = 1.2) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "gray60")) +
  labs(title = "Cross-Correlation: Mainstream AI (AI) vs. GrokAI (Grok)",
       subtitle = "Negative lags = AI leads Grok | Red lines = 95% significance",
       x = "Lag (Weeks)", y = "Correlation") +
  theme_minimal()

# ROBUST REGRESSION (NEWEY-WEST HAC)
# Re-running the DiD model from this script with Robust Standard Errors
m_did_robust <- lm(search_volume ~ time + treatment + post_launch + treatment:post_launch, 
                   data = its_platform_df1)

# Generate coeftest with Newey-West SEs to fix autocorrelation
did_hac_results <- coeftest(m_did_robust, vcov = vcovHAC(m_did_robust))
print(did_hac_results)

################################################################################
# DISTRIBUTIONAL VISUALIZATIONS
################################################################################

# Density Overlay: Comparing Frequency of Interest
p_density_s1 <- all_data_long1 |>
  ggplot(aes(x = hits, fill = platform, color = platform)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("AI" = "#2c7bb6", "Grok" = "#d7191c")) +
  scale_color_manual(values = c("AI" = "#2c7bb6", "Grok" = "#d7191c")) +
  labs(title = "Search Volume Density Profile: AI vs. Grok",
       x = "Search Hits (Normalized)", y = "Density") +
  theme_minimal()

# Boxplot with Jitter 
p_box_s1 <- all_data_long1 |>
  ggplot(aes(x = platform, y = hits, fill = platform)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  ggbeeswarm::geom_quasirandom(alpha = 0.2, size = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Distribution of Search Interest: Comparison of Intensity",
       x = "Category", y = "Search Index") +
  theme_bw()

# View Plots
grid.arrange(p_density_s1, p_box_s1, ncol = 2)

################################################################################
# FLEXTABLES
################################################################################

# Format the HAC results for the final Word document
hac_tidy <- tidy(did_hac_results) |>
  mutate(
    term = dplyr::recode(term, 
                         "(Intercept)" = "Constant",
                         "time" = "Temporal Trend",
                         "treatment" = "Grok Treatment (Binary)",
                         "post_launch" = "Post-Spicy Mode Shift",
                         "treatment:post_launch" = "Interaction (The Grok Effect)"),
    estimate = sprintf("%.4f", estimate),
    std.error = sprintf("(%.4f)", std.error),
    p.value = scales::pvalue(p.value)
  )

# Footnote
footnote_text <- paste0(
  "Note: Dependent variable is the monthly Google Trends Search Volume Index (0–100). ",
  "The 'Grok Treatment' group includes adversarial keywords specifically referencing xAI’s Grok, ",
  "while the 'Constant' reflects the baseline interest in general generative AI (Control). ",
  "The 'Interaction (The Grok Effect)' represents the Difference-in-Differences (DiD) estimator, ",
  "quantifying the marginal increase in adversarial search intent directly attributable to the ",
  "launch of 'Spicy Mode' (July 2025) beyond general industry trends. ",
  "Standard errors in parentheses are Newey-West heteroskedasticity and autocorrelation consistent (HAC) ",
  "estimates with a 4-period lag. *** p < 0.001, ** p < 0.01, * p < 0.05."
)

# Append to Flextable
final_table_s1 <- final_table_s1 |>
  add_footer_lines(values = footnote_text) |>
  font(fontname = "Times New Roman", part = "footer") |>
  fontsize(size = 9, part = "footer") |>
  italic(part = "footer") |>
  align(align = "left", part = "footer")

# Save to the Master Word Document
doc <- read_docx()
doc <- body_add_flextable(doc, value = final_table_s1)
print(doc, target = "Script1_Final_Analysis_Identical.docx")

# COMPLETE 