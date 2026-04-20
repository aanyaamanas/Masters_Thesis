# =============================================================================
# GROUP 2 ANALYSIS: PRIVACY-FOCUSED APPS vs MAINSTREAM ADULT PLATFORMS
# Aanyaa Manas | MS Criminology | University of Pennsylvania | May 2026
#
# Research question 2:
#   Do privacy-seeking adult content behaviours show different temporal dynamics
#   from mainstream platform use? Does the Grok NCII crisis (Jan 2026) shift
#   relative search interest toward privacy-seeking alternatives?
#
# Design: Interrupted Time Series (ITS) + Descriptive trend analysis
# Unit of analysis: keyword × month (consistent throughout — no mixing of
#   weekly and monthly, which was the source of unequal N in the original script)
#
# Two groups:
#   Group A — Privacy-Focused Apps:  terms reflecting encrypted/private access
#   Group B — Mainstream Adult Sites: terms reflecting open platform use
#
# Three anchor events (same as main Grok analysis for cross-script comparability):
#   E1: August 2024     — Grok 2 image generation launch
#   E2: July 2025       — Grok Spicy Mode launch
#   E3: January 2026    — Grok public crisis + regulatory spotlight
# =============================================================================


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gtrendsR,       # Google Trends API
  dplyr,          # data manipulation
  tidyr,          # reshaping
  tibble,         # tibbles
  lubridate,      # dates
  stringr,        # string ops
  purrr,          # functional tools
  zoo,            # rolling means
  lmtest,         # coeftest
  sandwich,       # vcovHAC
  broom,          # tidy()
  ggplot2,        # base plotting
  ggridges,       # ridge/joy plots
  patchwork,      # multi-panel layout
  ggtext,         # rich text in ggplot
  scales,         # axis formatting
  flextable,      # publication tables
  officer         # Word export
)

# =============================================================================
# 1. DEFINE TERMS AND DATES
# =============================================================================

# Group A: Privacy-focused adult content terms
# Reflect users seeking encrypted, offline, or anonymous access
privacy_terms <- c(
  "private porn app",
  "adult VPN",
  "encrypted adult content",
  "offline porn download",
  "anonymous adult browser"
)

# Group B: Mainstream adult platform terms
# Reflect open, high-volume public platform use
adult_terms <- c(
  "pornhub",
  "xvideos",
  "xhamster",
  "redtube",
  "tube8"
)

# Anchor event dates (matching main script exactly)
DATE_E1 <- as.Date("2024-08-01")   # Grok 2 image generation
DATE_E2 <- as.Date("2025-07-01")   # Grok Spicy Mode
DATE_E3 <- as.Date("2026-01-01")   # Grok public crisis

# Study window — monthly, 5 years
TIME_RANGE <- "2021-01-01 2026-03-01"

# =============================================================================
# 2. FETCH DATA
# =============================================================================
# All fetches use the same geo = "" (worldwide) and same time window.
# Monthly resolution ensures equal N across all models and tables.

cat("Fetching Google Trends data (monthly, worldwide)...\n")

privacy_raw <- gtrends(keyword = privacy_terms, time = TIME_RANGE, geo = "")
adult_raw   <- gtrends(keyword = adult_terms,   time = TIME_RANGE, geo = "")

cat("Data fetched.\n")

# =============================================================================
# 3. CLEAN AND COMBINE
# =============================================================================
# Critical fix from original script: use MONTHLY data exclusively.
# Original mixed weekly and monthly, causing unequal N across tables.
# Here we extract $interest_over_time which Google returns as monthly
# when the time window is > 3 months.

clean_gtrends <- function(raw, group_label) {
  raw$interest_over_time |>
    dplyr::select(date, keyword, hits) |>
    mutate(
      date  = floor_date(as.Date(date), "month"),   # force to month start
      hits  = as.numeric(ifelse(hits == "<1", 0.5, as.character(hits))),
      group = group_label
    ) |>
    # deduplicate — gtrendsR occasionally returns duplicate rows
    distinct(date, keyword, .keep_all = TRUE) |>
    arrange(keyword, date)
}

privacy_long <- clean_gtrends(privacy_raw, "Privacy Apps")
adult_long   <- clean_gtrends(adult_raw,   "Mainstream Adult Sites")

# Master long-format dataframe
all_long_g2 <- bind_rows(privacy_long, adult_long)

# Sanity checks — confirm equal N per keyword
cat("\n N check: rows per keyword \n")
print(as.data.frame(all_long_g2 |> count(keyword, group)))

cat("\nDate range:", format(min(all_long_g2$date)), "to", format(max(all_long_g2$date)), "\n")
cat("Total rows:", nrow(all_long_g2), "\n")

# Save raw data
write.csv(all_long_g2, "group2_google_trends_raw.csv", row.names = FALSE)
cat("Raw data saved to group2_google_trends_raw.csv\n")

# =============================================================================
# 4. BUILD ANALYSIS DATAFRAMES
# =============================================================================

# 4a. Keyword-level monthly: for ITS and bump chart
kw_monthly_g2 <- all_long_g2 |>
  mutate(
    post_e1   = if_else(date >= DATE_E1, 1L, 0L),
    post_e2   = if_else(date >= DATE_E2, 1L, 0L),
    post_e3   = if_else(date >= DATE_E3, 1L, 0L),
    treatment = if_else(group == "Privacy Apps", 1L, 0L),
    time      = as.integer(interval(min(date), date) %/% months(1))
  )

# 4b. Group-level monthly: mean hits per group per month (for ITS pooled)
group_monthly_g2 <- all_long_g2 |>
  group_by(group, date) |>
  summarise(mean_hits = mean(hits, na.rm = TRUE), .groups = "drop") |>
  mutate(
    post_e1   = if_else(date >= DATE_E1, 1L, 0L),
    post_e2   = if_else(date >= DATE_E2, 1L, 0L),
    post_e3   = if_else(date >= DATE_E3, 1L, 0L),
    treatment = if_else(group == "Privacy Apps", 1L, 0L),
    time      = as.integer(interval(min(date), date) %/% months(1))
  )

# Confirm N per group — must be equal
cat("\n N check: group monthly rows (must be equal) \n")
print(as.data.frame(group_monthly_g2 |> count(group)))

# =============================================================================
# 5. DESCRIPTIVE STATISTICS
# =============================================================================

desc_g2 <- all_long_g2 |>
  mutate(
    period = case_when(
      date < DATE_E1                    ~ "Pre-Event 1 (before Aug 2024)",
      date >= DATE_E1 & date < DATE_E2  ~ "Post-E1 (Aug 2024 – Jun 2025)",
      date >= DATE_E2 & date < DATE_E3  ~ "Post-E2 (Jul – Dec 2025)",
      date >= DATE_E3                   ~ "Post-E3 (Jan 2026+)"
    ),
    period = factor(period, levels = c(
      "Pre-Event 1 (before Aug 2024)",
      "Post-E1 (Aug 2024 – Jun 2025)",
      "Post-E2 (Jul – Dec 2025)",
      "Post-E3 (Jan 2026+)"
    ))
  ) |>
  group_by(group, period) |>
  summarise(
    n_obs        = n(),
    mean_hits    = round(mean(hits, na.rm = TRUE), 2),
    median_hits  = round(median(hits, na.rm = TRUE), 2),
    sd_hits      = round(sd(hits, na.rm = TRUE), 2),
    max_hits     = round(max(hits, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n Descriptive Statistics \n")
print(as.data.frame(desc_g2))

# =============================================================================
# 6. ITS MODELS
# =============================================================================
# Model: mean_hits ~ time + post_e1 + post_e2 + post_e3
# Estimated separately for each group (not pooled DiD — appropriate here
# because our goal is to describe each group's response to the Grok events,
# not to attribute causality to Grok itself for these groups)

run_its_g2 <- function(grp, df) {
  dat <- df |> filter(group == grp)
  mod <- lm(mean_hits ~ time + post_e1 + post_e2 + post_e3, data = dat)
  hac <- coeftest(mod, vcov = vcovHAC(mod))
  tidy(hac) |>
    mutate(
      group     = grp,
      r_squared = round(summary(mod)$r.squared, 3),
      n_obs     = nrow(dat),
      term_label = dplyr::recode(term,
                                 "(Intercept)" = "Intercept",
                                 "time"        = "Temporal Trend (months)",
                                 "post_e1"     = "Level Shift: E1 (Aug 2024 image gen)",
                                 "post_e2"     = "Level Shift: E2 (Jul 2025 Spicy Mode)",
                                 "post_e3"     = "Level Shift: E3 (Jan 2026 crisis)"
      ),
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01  ~ "**",
        p.value < .05  ~ "*",
        p.value < .10  ~ ".",
        TRUE           ~ ""
      )
    )
}

its_privacy <- run_its_g2("Privacy Apps",           group_monthly_g2)
its_adult   <- run_its_g2("Mainstream Adult Sites",  group_monthly_g2)

its_results_g2 <- bind_rows(its_privacy, its_adult)

cat("\n ITS Results \n")
print(as.data.frame(its_results_g2 |> dplyr::select(group, term_label, estimate, std.error, p.value, sig)))

# Keyword-level ITS (same structure as main script)
run_its_keyword_g2 <- function(kw, df) {
  dat <- df |> filter(keyword == kw)
  if (nrow(dat) < 20 || var(dat$hits) < 0.01) return(NULL)
  mod <- lm(hits ~ time + post_e1 + post_e2 + post_e3, data = dat)
  hac <- coeftest(mod, vcov = vcovHAC(mod))
  tidy(hac) |>
    mutate(keyword = kw, group = unique(dat$group), r_squared = summary(mod)$r.squared)
}

its_kw_g2 <- map_dfr(unique(kw_monthly_g2$keyword), ~run_its_keyword_g2(.x, kw_monthly_g2)) |>
  filter(term %in% c("post_e1", "post_e2", "post_e3")) |>
  mutate(
    event = dplyr::recode(term,
                          "post_e1" = "E1: Aug 2024",
                          "post_e2" = "E2: Jul 2025",
                          "post_e3" = "E3: Jan 2026"
    ),
    sig = case_when(
      p.value < .001 ~ "***", p.value < .01 ~ "**",
      p.value < .05  ~ "*",   p.value < .10 ~ ".",
      TRUE ~ ""
    )
  )

# =============================================================================
# 7. KEYWORD RANKING OVER TIME (for bump chart)
# =============================================================================

bump_data <- all_long_g2 |>
  # quarterly averages for cleaner bump chart
  mutate(quarter = floor_date(date, "quarter")) |>
  group_by(quarter, keyword, group) |>
  summarise(mean_hits = mean(hits, na.rm = TRUE), .groups = "drop") |>
  group_by(quarter) |>
  # rank within each quarter: 1 = highest hits
  mutate(rank = rank(-mean_hits, ties.method = "first")) |>
  ungroup() |>
  # clean keyword labels
  mutate(kw_label = str_to_title(keyword))

# =============================================================================
# 8. COLOUR PALETTE AND GLOBAL THEME
# =============================================================================

# 10 keywords total — 5 per group
# Group colours: privacy = teal family, adult = warm family
kw_palette <- c(
  # Privacy Apps (5)
  "Private Porn App"          = "#2C7BB6",
  "Adult Vpn"                 = "#4EB3D3",
  "Encrypted Adult Content"   = "#0868AC",
  "Offline Porn Download"     = "#7FCDBB",
  "Anonymous Adult Browser"   = "#1D91C0",
  # Mainstream Adult Sites (5)
  "Pornhub"                   = "#D7191C",
  "Xvideos"                   = "#E66101",
  "Xhamster"                  = "#D01C8B",
  "Redtube"                   = "#F1B6DA",
  "Tube8"                     = "#B2182B"
)

group_colours <- c(
  "Privacy Apps"              = "#2C7BB6",
  "Mainstream Adult Sites"    = "#D7191C"
)

group_fills <- c(
  "Privacy Apps"              = "#a6cee3",
  "Mainstream Adult Sites"    = "#fb9a99"
)

# Event reference lines — reusable
event_lines <- list(
  geom_vline(xintercept = as.Date("2024-08-01"),
             linetype = "dashed", linewidth = 0.45, color = "#555555"),
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype = "dashed", linewidth = 0.45, color = "#1A9641"),
  geom_vline(xintercept = as.Date("2026-01-01"),
             linetype = "dashed", linewidth = 0.45, color = "#D7191C")
)

# Publication theme
theme_pub2 <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(family = "serif", color = "#1a1a1a"),
      plot.title        = element_text(face = "bold", size = base_size + 2),
      plot.subtitle     = element_text(size = base_size - 1, color = "#555555",
                                       margin = margin(b = 6)),
      plot.caption      = element_text(size = base_size - 2, color = "#777777",
                                       hjust = 0, margin = margin(t = 6)),
      axis.title        = element_text(size = base_size),
      axis.text         = element_text(size = base_size - 1, color = "#333333"),
      panel.grid.major  = element_line(color = "#eeeeee", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold", size = base_size - 1),
      legend.text       = element_text(size = base_size - 1),
      strip.text        = element_text(face = "bold", size = base_size - 1),
      plot.margin       = margin(10, 12, 10, 12)
    )
}

# =============================================================================
# FIGURE 1 — Three-panel time series
# Panel A: Group means, full window
# Panel B: Group means, 2024–2026 zoomed with period shading
# Panel C: Ridge plot — distribution of monthly hits by group and period
# =============================================================================

# Panel A
pA_data <- group_monthly_g2 |>
  mutate(Group = group)

pA <- ggplot(pA_data, aes(x = date, y = mean_hits,
                          color = Group, group = Group)) +
  annotate("rect", xmin = DATE_E1, xmax = DATE_E2,
           ymin = -Inf, ymax = Inf, fill = "#f0f7ff", alpha = 0.6) +
  annotate("rect", xmin = DATE_E2, xmax = DATE_E3,
           ymin = -Inf, ymax = Inf, fill = "#f0fff4", alpha = 0.6) +
  annotate("rect", xmin = DATE_E3, xmax = as.Date("2026-04-01"),
           ymin = -Inf, ymax = Inf, fill = "#fff5f5", alpha = 0.7) +
  event_lines +
  geom_line(linewidth = 0.75) +
  annotate("text", x = DATE_E1, y = Inf,
           label = "E1", vjust = 1.5, hjust = -0.3,
           size = 2.6, color = "#555555", fontface = "italic", family = "serif") +
  annotate("text", x = DATE_E2, y = Inf,
           label = "E2", vjust = 1.5, hjust = -0.3,
           size = 2.6, color = "#1A9641", fontface = "italic", family = "serif") +
  annotate("text", x = DATE_E3, y = Inf,
           label = "E3", vjust = 1.5, hjust = -0.3,
           size = 2.6, color = "#D7191C", fontface = "italic", family = "serif") +
  scale_color_manual(values = group_colours, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Mean Search Index (0–100)",
       subtitle = "A. Full study window (2021–2026): group means") +
  theme_pub2()

# Panel B — zoomed 2024+
pB_data <- group_monthly_g2 |>
  filter(date >= as.Date("2023-06-01"))

pB <- ggplot(pB_data, aes(x = date, y = mean_hits,
                          color = group, group = group)) +
  annotate("rect", xmin = DATE_E1, xmax = DATE_E2,
           ymin = -Inf, ymax = Inf, fill = "#f0f7ff", alpha = 0.7) +
  annotate("rect", xmin = DATE_E2, xmax = DATE_E3,
           ymin = -Inf, ymax = Inf, fill = "#f0fff4", alpha = 0.7) +
  annotate("rect", xmin = DATE_E3, xmax = as.Date("2026-04-01"),
           ymin = -Inf, ymax = Inf, fill = "#fff5f5", alpha = 0.8) +
  event_lines +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.5, alpha = 0.6) +
  annotate("text", x = DATE_E1, y = Inf,
           label = "E1: Grok 2\nImage Gen", vjust = 1.5, hjust = -0.07,
           size = 2.4, color = "#555555", fontface = "italic", family = "serif") +
  annotate("text", x = DATE_E2, y = Inf,
           label = "E2: Spicy\nMode", vjust = 1.5, hjust = -0.07,
           size = 2.4, color = "#1A9641", fontface = "italic", family = "serif") +
  annotate("text", x = DATE_E3, y = Inf,
           label = "E3: Crisis", vjust = 1.5, hjust = -0.07,
           size = 2.4, color = "#D7191C", fontface = "italic", family = "serif") +
  scale_color_manual(values = group_colours, name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = NULL, y = "Mean Search Index (0–100)",
       subtitle = "B. Zoomed view (Jun 2023–Mar 2026)") +
  theme_pub2() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Panel C — ridge plot: distribution of monthly hits by period
ridge_data <- all_long_g2 |>
  mutate(
    period = case_when(
      date < DATE_E1                    ~ "Pre-E1",
      date >= DATE_E1 & date < DATE_E2  ~ "Post-E1",
      date >= DATE_E2 & date < DATE_E3  ~ "Post-E2",
      date >= DATE_E3                   ~ "Post-E3"
    ),
    period = factor(period,
                    levels = c("Post-E3","Post-E2","Post-E1","Pre-E1"))
  )

pC <- ggplot(ridge_data,
             aes(x = hits, y = period, fill = group, color = group)) +
  geom_density_ridges(alpha = 0.55, scale = 1.1,
                      bandwidth = 3, rel_min_height = 0.01) +
  facet_wrap(~group, ncol = 2) +
  scale_fill_manual(values  = group_fills,  guide = "none") +
  scale_color_manual(values = group_colours, guide = "none") +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  labs(x = "Monthly Search Index (0–100)", y = NULL,
       subtitle = "C. Distribution of monthly hits by period and group") +
  theme_pub2() +
  theme(strip.text = element_text(face = "bold"))

fig1_g2 <- (pA / pB / pC) +
  plot_annotation(
    title = paste0(
      "Figure 1. Search Interest Trends: Privacy-Focused Apps vs. ",
      "Mainstream Adult Platforms, 2021–2026"
    ),
    subtitle = paste0(
      "Google Trends monthly search index (worldwide, 0–100). ",
      "Shaded regions: blue = post-E1, green = post-E2, red = post-E3.\n",
      "E1 = Grok 2 image generation (Aug 2024); ",
      "E2 = Spicy Mode (Jul 2025); ",
      "E3 = January 2026 public crisis."
    ),
    caption = paste0(
      "Source: Google Trends (worldwide, monthly, Jan 2021–Mar 2026). ",
      "N = 5 keywords per group × 62 monthly observations = 310 observations per group.\n",
      "Privacy Apps: private porn app, adult VPN, encrypted adult content, ",
      "offline porn download, anonymous adult browser.\n",
      "Mainstream Adult Sites: pornhub, xvideos, xhamster, redtube, tube8."
    ),
    theme = theme(
      plot.title    = element_text(family = "serif", face = "bold", size = 12),
      plot.subtitle = element_text(family = "serif", size = 9, color = "#444444"),
      plot.caption  = element_text(family = "serif", size = 8,
                                   color = "#666666", hjust = 0)
    )
  )

ggsave("G2_Figure1_TimeSeries.pdf", fig1_g2, width = 8.5, height = 11, dpi = 300)
ggsave("G2_Figure1_TimeSeries.png", fig1_g2, width = 8.5, height = 11, dpi = 300)


# =============================================================================
# FIGURE 2 — Bump Chart: Keyword Rankings by Quarter
# Shows how relative popularity of keywords shifted across study periods
# =============================================================================

# Highlight keywords for labels at last time point
last_quarter <- max(bump_data$quarter)

bump_labels <- bump_data |>
  filter(quarter == last_quarter) |>
  arrange(rank)

# Colour each keyword by group membership
bump_group <- all_long_g2 |>
  distinct(keyword, group) |>
  mutate(kw_label = str_to_title(keyword))

bump_plot_data <- bump_data |>
  left_join(bump_group, by = c("kw_label" = "kw_label", "group"))

fig2_g2 <- ggplot(bump_plot_data,
                  aes(x = quarter, y = rank,
                      group = kw_label, color = group)) +
  # event shading
  annotate("rect", xmin = DATE_E1, xmax = DATE_E2,
           ymin = 0.4, ymax = 10.6, fill = "#f0f7ff", alpha = 0.5) +
  annotate("rect", xmin = DATE_E2, xmax = DATE_E3,
           ymin = 0.4, ymax = 10.6, fill = "#f0fff4", alpha = 0.5) +
  annotate("rect", xmin = DATE_E3, xmax = as.Date("2026-04-01"),
           ymin = 0.4, ymax = 10.6, fill = "#fff5f5", alpha = 0.6) +
  # event lines
  geom_vline(xintercept = DATE_E1, linetype = "dashed",
             linewidth = 0.4, color = "#555555") +
  geom_vline(xintercept = DATE_E2, linetype = "dashed",
             linewidth = 0.4, color = "#1A9641") +
  geom_vline(xintercept = DATE_E3, linetype = "dashed",
             linewidth = 0.4, color = "#D7191C") +
  # bump lines and points
  geom_line(linewidth = 0.65, alpha = 0.8) +
  geom_point(size = 2.2, alpha = 0.9) +
  # right-hand keyword labels
  geom_text(data = bump_labels |>
              left_join(bump_group, by = c("kw_label","group")),
            aes(x = last_quarter, y = rank, label = kw_label,
                color = group),
            hjust = -0.08, size = 2.5, fontface = "italic",
            family = "serif") +
  # event annotations at top
  annotate("text", x = DATE_E1, y = 0.2,
           label = "E1", size = 2.6, color = "#555555",
           fontface = "bold", family = "serif") +
  annotate("text", x = DATE_E2, y = 0.2,
           label = "E2", size = 2.6, color = "#1A9641",
           fontface = "bold", family = "serif") +
  annotate("text", x = DATE_E3, y = 0.2,
           label = "E3", size = 2.6, color = "#D7191C",
           fontface = "bold", family = "serif") +
  scale_color_manual(values = group_colours, name = "Group") +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL) +
  scale_x_date(
    date_breaks  = "6 months", date_labels = "%b %Y",
    expand       = expansion(mult = c(0.02, 0.22))
  ) +
  labs(
    x = "Quarter",
    y = "Rank (1 = highest search volume)",
    title = "Figure 2. Keyword Ranking Dynamics: Privacy Apps vs. Mainstream Adult Platforms",
    subtitle = paste0(
      "Bump chart showing quarterly rank of each keyword by search volume (rank 1 = most searched).\n",
      "Shaded regions mark post-event windows. ",
      "Rank inversions reveal relative shifts in search interest after each Grok event."
    ),
    caption = paste0(
      "Rankings based on mean monthly Google Trends index within each quarter. ",
      "N = 10 keywords ranked jointly across both groups.\n",
      "Blue lines = Privacy App terms; Red lines = Mainstream Adult Site terms."
    )
  ) +
  theme_pub2() +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  )

ggsave("G2_Figure2_BumpChart.pdf", fig2_g2, width = 10, height = 6.5, dpi = 300)
ggsave("G2_Figure2_BumpChart.png", fig2_g2, width = 10, height = 6.5, dpi = 300)
cat("Figure 2 saved.\n")

# =============================================================================
# FIGURE 3 — Heatmap: Every keyword × month
# =============================================================================

heat_g2 <- all_long_g2 |>
  mutate(
    kw_label   = str_to_title(keyword),
    hits_cap   = pmin(hits, 60)
  ) |>
  filter(date >= as.Date("2023-01-01")) |>
  left_join(
    all_long_g2 |>
      group_by(keyword) |>
      summarise(total = sum(hits)) |>
      mutate(kw_label = str_to_title(keyword)),
    by = "kw_label"
  ) |>
  mutate(kw_label = reorder(kw_label, -total))

# group strip label positions
group_strip <- heat_g2 |>
  distinct(kw_label, group) |>
  mutate(x_pos = as.Date("2023-01-01"))

fig3_g2 <- ggplot(heat_g2,
                  aes(x = date, y = kw_label, fill = hits_cap)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_vline(xintercept = as.numeric(DATE_E1),
             color = "#555555", linewidth = 0.7, linetype = "solid") +
  geom_vline(xintercept = as.numeric(DATE_E2),
             color = "#1A9641", linewidth = 0.7, linetype = "solid") +
  geom_vline(xintercept = as.numeric(DATE_E3),
             color = "#D7191C", linewidth = 0.7, linetype = "solid") +
  # group annotation brackets
  annotate("text", x = as.Date("2023-03-01"), y = 3,
           label = "Mainstream Adult", color = "#D7191C",
           size = 2.6, fontface = "bold.italic", family = "serif", hjust = 0) +
  annotate("text", x = as.Date("2023-03-01"), y = 8,
           label = "Privacy Apps", color = "#2C7BB6",
           size = 2.6, fontface = "bold.italic", family = "serif", hjust = 0) +
  scale_fill_gradientn(
    colours = c("#f7fbff","#c6dbef","#6baed6","#2171b5","#08306b"),
    name    = "Search Index\n(capped at 60)",
    limits  = c(0, 60),
    breaks  = c(0, 15, 30, 45, 60),
    labels  = c("0","15","30","45","60+")
  ) +
  scale_x_date(
    date_breaks = "3 months", date_labels = "%b\n%Y",
    expand = expansion(mult = 0)
  ) +
  annotate("text", x = DATE_E1, y = 10.7,
           label = "E1", color = "#555555", size = 2.8,
           fontface = "bold", vjust = -0.2, family = "serif") +
  annotate("text", x = DATE_E2, y = 10.7,
           label = "E2", color = "#1A9641", size = 2.8,
           fontface = "bold", vjust = -0.2, family = "serif") +
  annotate("text", x = DATE_E3, y = 10.7,
           label = "E3", color = "#D7191C", size = 2.8,
           fontface = "bold", vjust = -0.2, family = "serif") +
  labs(
    x = "Month",
    y = NULL,
    title = "Figure 3. Temporal Search Activity Heatmap: All Keywords, Jan 2023–Mar 2026",
    subtitle = paste0(
      "Each row = one keyword; each column = one month. ",
      "Darker blue = higher search volume.\n",
      "Vertical lines mark Grok events E1–E3. ",
      "Values capped at 60 for visual clarity."
    ),
    caption = paste0(
      "Source: Google Trends (worldwide, monthly). ",
      "Keywords ordered by total cumulative search volume.\n",
      "E1 = Aug 2024; E2 = Jul 2025; E3 = Jan 2026."
    )
  ) +
  theme_pub2() +
  theme(
    axis.text.y      = element_text(size = 8.5),
    legend.key.width = unit(1.2, "cm"),
    legend.position  = "right"
  )

ggsave("G2_Figure3_Heatmap.pdf", fig3_g2, width = 10, height = 5, dpi = 300)
ggsave("G2_Figure3_Heatmap.png", fig3_g2, width = 10, height = 5, dpi = 300)
cat("Figure 3 saved.\n")

# =============================================================================
# FIGURE 4 — ITS Forest Plot: Level shifts by keyword and event
# =============================================================================

its_forest_data <- its_kw_g2 |>
  mutate(
    kw_label  = str_to_title(keyword),
    event     = factor(event,
                       levels = c("E1: Aug 2024","E2: Jul 2025","E3: Jan 2026")),
    ci_lo     = estimate - 1.96 * std.error,
    ci_hi     = estimate + 1.96 * std.error,
    sig_flag  = p.value < 0.05
  ) |>
  # sort by E3 estimate
  mutate(kw_label = reorder(kw_label,
                            ifelse(event == "E3: Jan 2026", estimate, NA),
                            FUN = function(x) max(x, na.rm = TRUE)))

event_colours_g2 <- c(
  "E1: Aug 2024" = "#555555",
  "E2: Jul 2025" = "#1A9641",
  "E3: Jan 2026" = "#D7191C"
)

fig4_g2 <- ggplot(its_forest_data,
                  aes(x = estimate, y = kw_label,
                      color = event, shape = sig_flag)) +
  geom_vline(xintercept = 0, color = "#aaaaaa", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.22,
                 position = position_dodge(width = 0.7),
                 linewidth = 0.5) +
  geom_point(size = 2.8,
             position = position_dodge(width = 0.7)) +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  scale_color_manual(values = event_colours_g2, name = "Event") +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("FALSE" = "p ≥ .05", "TRUE" = "p < .05"),
    name = "Significance"
  ) +
  scale_x_continuous(labels = function(x) ifelse(x > 0, paste0("+", x), x)) +
  labs(
    x = "Estimated Level Shift in Search Index (Google Trends units)",
    y = NULL,
    title = "Figure 4. ITS Level-Shift Estimates by Keyword, Group, and Event",
    subtitle = paste0(
      "OLS estimates with 95% confidence intervals (HAC standard errors). ",
      "Each point is the step-change in search\nindex at the event threshold. ",
      "Filled circles = significant at p < .05; open circles = non-significant."
    ),
    caption = paste0(
      "Model: hits ~ time + post_E1 + post_E2 + post_E3, ",
      "estimated separately per keyword. ",
      "Newey-West HAC standard errors.\n",
      "N = 62 monthly observations per keyword."
    )
  ) +
  theme_pub2() +
  theme(
    legend.position    = "bottom",
    legend.box         = "horizontal",
    strip.background   = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major.y = element_blank()
  ) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  )

ggsave("G2_Figure4_ITS_Forest.pdf", fig4_g2, width = 8.5, height = 8, dpi = 300)
ggsave("G2_Figure4_ITS_Forest.png", fig4_g2, width = 8.5, height = 8, dpi = 300)
cat("Figure 4 saved.\n")

# =============================================================================
# TABLE 1 — Descriptive Statistics (APA, no colour, no shading)
# =============================================================================

apa_ft <- function(ft) {
  ft |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 11, part = "all") |>
    fontsize(size = 10, part = "footer") |>
    bold(part = "header") |>
    italic(part = "footer") |>
    align(align = "left",   j = 1:2, part = "all") |>
    align(align = "center", j = 3:ncol_keys(ft), part = "body") |>
    align(align = "center", j = 3:ncol_keys(ft), part = "header") |>
    border_remove() |>
    hline_top(part = "header",
              border = fp_border(color = "black", width = 1.5)) |>
    hline_bottom(part = "header",
                 border = fp_border(color = "black", width = 0.75)) |>
    hline_bottom(part = "body",
                 border = fp_border(color = "black", width = 1.5)) |>
    padding(padding.top = 3, padding.bottom = 3,
            padding.left = 4, padding.right = 4, part = "all") |>
    set_table_properties(layout = "autofit")
}

t1_data <- desc_g2 |>
  mutate(
    Group  = group,
    Period = as.character(period),
    n      = n_obs,
    M      = sprintf("%.2f", mean_hits),
    Mdn    = sprintf("%.2f", median_hits),
    SD     = sprintf("%.2f", sd_hits),
    Max    = sprintf("%.2f", max_hits)
  ) |>
  arrange(Group, Period) |>
  dplyr::select(Group, Period, n, M, Mdn, SD, Max)

ft1 <- flextable(t1_data) |>
  apa_ft() |>
  merge_v(j = "Group") |>
  valign(j = "Group", valign = "top", part = "body") |>
  hline(i = nrow(t1_data) / 2,
        border = fp_border(color = "black", width = 0.5),
        part = "body") |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        paste0("Table 1. Descriptive Statistics of Google Trends Search Volume by ",
               "Group and Analytical Period"),
        props = fp_text(bold = TRUE, font.family = "Times New Roman", font.size = 11)
      )
    )
  ) |>
  add_footer_lines(paste0(
    "Note. Google Trends index values are normalised to a 0–100 scale relative to ",
    "peak search volume within each query batch (worldwide). ",
    "All observations are at monthly resolution; N per group = 5 keywords × number of months per period. ",
    "Privacy Apps group: private porn app, adult VPN, encrypted adult content, ",
    "offline porn download, anonymous adult browser. ",
    "Mainstream Adult Sites group: pornhub, xvideos, xhamster, redtube, tube8. ",
    "E1 = Grok 2 image generation (August 2024); ",
    "E2 = Spicy Mode launch (July 2025); ",
    "E3 = January 2026 public crisis and regulatory spotlight. ",
    "Source: Google Trends (Jan 2021–Mar 2026)."
  ))

# =============================================================================
# TABLE 2 — ITS Results: Level-Shift Estimates by Group and Event
# =============================================================================

fmt_p <- function(p) {
  case_when(p < .001 ~ "< .001", TRUE ~ sprintf("%.3f", p))
}

t2_data <- its_results_g2 |>
  filter(term %in% c("post_e1", "post_e2", "post_e3")) |>
  transmute(
    Group    = group,
    Event    = dplyr::recode(term,
                             "post_e1" = "E1: Aug 2024 — Grok 2 image generation",
                             "post_e2" = "E2: Jul 2025 — Spicy Mode launch",
                             "post_e3" = "E3: Jan 2026 — Public crisis"
    ),
    `β`      = sprintf("%.3f%s", estimate, sig),
    SE       = sprintf("(%.3f)", std.error),
    `t`      = sprintf("%.3f", statistic),
    p        = fmt_p(p.value),
    `R²`     = sprintf("%.3f", r_squared),
    `N`      = as.character(n_obs)
  ) |>
  arrange(Group, Event)

ft2 <- flextable(t2_data) |>
  apa_ft() |>
  merge_v(j = c("Group", "R²", "N")) |>
  valign(j = c("Group", "R²", "N"), valign = "top", part = "body") |>
  hline(i = 3,
        border = fp_border(color = "black", width = 0.5),
        part = "body") |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        paste0("Table 2. Interrupted Time Series Estimates: Level Shifts at ",
               "Grok Product Events by Group"),
        props = fp_text(bold = TRUE, font.family = "Times New Roman", font.size = 11)
      )
    )
  ) |>
  add_footer_lines(paste0(
    "Note. ITS models estimated separately for each group using OLS of the form: ",
    "mean_hits ~ time + post_E1 + post_E2 + post_E3, ",
    "where mean_hits is the monthly average of Google Trends index values across ",
    "the five keywords in each group. ",
    "β = estimated level shift at each event threshold; ",
    "SE = standard error in parentheses; ",
    "R² is reported for the full model. ",
    "Newey-West HAC standard errors throughout. ",
    "N = monthly observations per group. ",
    "E1 = Grok 2 image generation (August 2024); ",
    "E2 = Spicy Mode launch (July 2025); ",
    "E3 = January 2026 public crisis and regulatory spotlight. ",
    "*** p < .001, ** p < .01, * p < .05, . p < .10."
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
  body_add_par("Table 2", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft2)

print(doc, target = "G2_Tables_Clean.docx")
cat("Tables saved to G2_Tables_Clean.docx\n")

# =============================================================================
# SAVE CLEAN CSVs
# =============================================================================

write.csv(all_long_g2,      "G2_google_trends_raw.csv",      row.names = FALSE)
write.csv(group_monthly_g2, "G2_group_monthly_clean.csv",    row.names = FALSE)
write.csv(kw_monthly_g2,    "G2_keyword_monthly_clean.csv",  row.names = FALSE)
write.csv(desc_g2,          "G2_descriptive_stats.csv",      row.names = FALSE)
write.csv(its_results_g2,   "G2_its_results.csv",            row.names = FALSE)
write.csv(its_kw_g2,        "G2_its_keyword_results.csv",    row.names = FALSE)


# =============================================================================

# ── Shared colours ────────────────────────────────────────────────────────────
C_PRIV  <- "#2C7BB6"   # blue  — Privacy Apps
C_MAIN  <- "#D7191C"   # red   — Mainstream Adult Sites
C_E1    <- "#555555"
C_E2    <- "#1A9641"
C_E3    <- "#D7191C"

DATE_E1 <- as.Date("2024-08-01")
DATE_E2 <- as.Date("2025-07-01")
DATE_E3 <- as.Date("2026-01-01")

theme_pub2 <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(family = "serif", color = "#1a1a1a"),
      plot.title        = element_text(face = "bold", size = base_size + 2),
      plot.subtitle     = element_text(size = base_size - 1, color = "#555555",
                                       margin = margin(b = 6)),
      plot.caption      = element_text(size = base_size - 2, color = "#777777",
                                       hjust = 0, margin = margin(t = 6)),
      axis.title        = element_text(size = base_size),
      axis.text         = element_text(size = base_size - 1, color = "#333333"),
      panel.grid.major  = element_line(color = "#eeeeee", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold", size = base_size - 1),
      legend.text       = element_text(size = base_size - 1),
      strip.text        = element_text(face = "bold", size = base_size - 1),
      plot.margin       = margin(10, 12, 10, 12)
    )
}

# =============================================================================
# FIGURE A — Mirrored Diverging Bar Chart
# ITS event estimates for both groups, mirrored around zero
# Privacy Apps bars point RIGHT (+), Mainstream bars point LEFT (−)
# Shows at a glance that the two groups respond to DIFFERENT events
# =============================================================================

its_events <- its_results_g2 |>
  filter(str_detect(term_label, "Level Shift")) |>
  mutate(
    event = dplyr::recode(term,
                          "post_e1" = "E1: Aug 2024\n(Image Gen)",
                          "post_e2" = "E2: Jul 2025\n(Spicy Mode)",
                          "post_e3" = "E3: Jan 2026\n(Crisis)"
    ),
    event = factor(event, levels = c(
      "E1: Aug 2024\n(Image Gen)",
      "E2: Jul 2025\n(Spicy Mode)",
      "E3: Jan 2026\n(Crisis)"
    )),
    # Mirror: privacy goes positive, mainstream goes negative
    bar_val = if_else(group == "Privacy Apps", estimate, -estimate),
    ci_lo   = if_else(group == "Privacy Apps",
                      estimate - 1.96 * std.error,
                      -(estimate + 1.96 * std.error)),
    ci_hi   = if_else(group == "Privacy Apps",
                      estimate + 1.96 * std.error,
                      -(estimate - 1.96 * std.error)),
    sig_label = paste0(sprintf("%.2f", estimate), sig),
    sig_flag  = p.value < 0.05
  )

figA <- ggplot(its_events,
               aes(x = bar_val, y = event,
                   fill = group, alpha = sig_flag)) +
  geom_col(width = 0.55, position = "identity") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.18, linewidth = 0.5,
                 color = "#333333") +
  geom_vline(xintercept = 0, color = "#333333", linewidth = 0.6) +
  # value labels
  geom_text(data = its_events |> filter(group == "Privacy Apps"),
            aes(x = ci_hi + 0.3, label = sig_label),
            hjust = 0, size = 2.8, family = "serif", color = C_PRIV) +
  geom_text(data = its_events |> filter(group == "Mainstream Adult Sites"),
            aes(x = ci_lo - 0.3, label = sig_label),
            hjust = 1, size = 2.8, family = "serif", color = C_MAIN) +
  # group labels at top
  annotate("text", x =  6, y = 3.55,
           label = "← Mainstream Adult Sites",
           size = 3, family = "serif", fontface = "bold",
           color = C_MAIN, hjust = 1) +
  annotate("text", x = -6, y = 3.55,
           label = "Privacy Apps →",
           size = 3, family = "serif", fontface = "bold",
           color = C_PRIV, hjust = 0) +
  scale_fill_manual(values = c("Privacy Apps" = C_PRIV,
                               "Mainstream Adult Sites" = C_MAIN),
                    guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.35),
                     guide = "none") +
  scale_x_continuous(
    limits = c(-14, 14),
    breaks = seq(-12, 12, 4),
    labels = function(x) as.character(abs(x))
  ) +
  labs(
    x = "Estimated Level Shift (absolute value, Google Trends units)",
    y = NULL,
    title = "Figure A. Mirrored ITS Estimates: Contrasting Group Responses to Grok Events",
    subtitle = paste0(
      "Bars show the level shift in mean monthly search index at each event threshold. ",
      "Privacy Apps (blue) plotted rightward;\n",
      "Mainstream Adult Sites (red) plotted leftward. ",
      "Faded bars = non-significant (p ≥ .05). ",
      "Error bars = 95% CI (HAC SEs)."
    ),
    caption = paste0(
      "Note. The asymmetric response pattern shows that the two groups reacted to ",
      "different Grok events:\n",
      "Mainstream Sites spiked at E1 (image generation launch); ",
      "Privacy Apps spiked at E2 (Spicy Mode — accessible NSFW generation).\n",
      "Neither group showed a significant additional shift at E3 (January 2026 crisis)."
    )
  ) +
  theme_pub2() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#eeeeee")
  )

ggsave("G2_FigureA_MirroredBar.pdf", figA, width = 8.5, height = 4.5, dpi = 300)
ggsave("G2_FigureA_MirroredBar.png", figA, width = 8.5, height = 4.5, dpi = 300)
cat("Figure A saved.\n")

# =============================================================================
# FIGURE B — Slopegraph
# Mean search volume at four anchor dates — one dot per group per anchor,
# connected by slopes. Shows trajectory and crossings.
# Anchor dates: pre-E1 mean, post-E1 mean, post-E2 mean, post-E3 mean
# =============================================================================

slope_data <- desc_g2 |>
  mutate(
    anchor = dplyr::recode(as.character(period),
                           "Pre-Event 1 (before Aug 2024)"  = "Pre-E1\n(baseline)",
                           "Post-E1 (Aug 2024 – Jun 2025)"  = "Post-E1\n(Aug 2024)",
                           "Post-E2 (Jul – Dec 2025)"       = "Post-E2\n(Jul 2025)",
                           "Post-E3 (Jan 2026+)"            = "Post-E3\n(Jan 2026)"
    ),
    anchor = factor(anchor, levels = c(
      "Pre-E1\n(baseline)",
      "Post-E1\n(Aug 2024)",
      "Post-E2\n(Jul 2025)",
      "Post-E3\n(Jan 2026)"
    )),
    group_col = if_else(group == "Privacy Apps", C_PRIV, C_MAIN)
  )

# Right-hand labels with % change from baseline
slope_labels_r <- slope_data |>
  filter(anchor == "Post-E3\n(Jan 2026)") |>
  left_join(
    slope_data |>
      filter(anchor == "Pre-E1\n(baseline)") |>
      dplyr::select(group, baseline = mean_hits),
    by = "group"
  ) |>
  mutate(
    pct_chg = round((mean_hits / baseline - 1) * 100),
    label_r = paste0(group, "\n",
                     sprintf("%.1f", mean_hits),
                     " (", ifelse(pct_chg > 0, "+", ""), pct_chg, "%)")
  )

figB <- ggplot(slope_data,
               aes(x = anchor, y = mean_hits,
                   group = group, color = group)) +
  # connect dots
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 4, alpha = 0.95) +
  # value labels on each dot
  geom_text(aes(label = sprintf("%.1f", mean_hits)),
            vjust = -1.1, size = 2.6, family = "serif",
            fontface = "bold") +
  # right-hand group labels
  geom_text(data = slope_labels_r,
            aes(x = anchor, y = mean_hits,
                label = group,
                color = group),
            hjust = -0.12, size = 2.8, family = "serif",
            fontface = "italic") +
  scale_color_manual(values = c("Privacy Apps"          = C_PRIV,
                                "Mainstream Adult Sites" = C_MAIN),
                     guide = "none") +
  scale_y_continuous(
    limits = c(0, 50),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.3))) +
  labs(
    x = NULL,
    y = "Mean Search Index (0–100)",
    title = "Figure B. Slopegraph: Mean Search Volume at Four Analytical Anchors",
    subtitle = paste0(
      "Each point = group mean across all keywords and months within that period. ",
      "Line slope shows the direction and\n",
      "magnitude of change between anchor periods. ",
      "Privacy Apps (blue) converge toward Mainstream (red) after E2."
    ),
    caption = paste0(
      "Anchor values are period means from descriptive statistics (Table 1). ",
      "Pre-E1 = Jan 2021–Jul 2024; Post-E1 = Aug 2024–Jun 2025;\n",
      "Post-E2 = Jul–Dec 2025; Post-E3 = Jan–Mar 2026. ",
      "N = 5 keywords × months per period per group."
    )
  ) +
  theme_pub2() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    axis.text.x        = element_text(size = 9, face = "bold")
  )

ggsave("G2_FigureB_Slopegraph.pdf", figB, width = 8, height = 5, dpi = 300)
ggsave("G2_FigureB_Slopegraph.png", figB, width = 8, height = 5, dpi = 300)
cat("Figure B saved.\n")

# =============================================================================
# FIGURE C — Small Multiple Line Charts (one panel per keyword)
# Individual trajectories — shows heterogeneity within groups
# Completely different from heatmap: shows curve shape, not just intensity
# =============================================================================

sm_data <- all_long_g2 |>
  mutate(
    kw_label  = str_to_title(keyword),
    grp_color = if_else(group == "Privacy Apps", C_PRIV, C_MAIN)
  )

# Period-mean reference lines per keyword
sm_means <- sm_data |>
  mutate(period = case_when(
    date < DATE_E1                   ~ "pre",
    date >= DATE_E1 & date < DATE_E2 ~ "e1",
    date >= DATE_E2 & date < DATE_E3 ~ "e2",
    date >= DATE_E3                  ~ "e3"
  )) |>
  group_by(kw_label, group, period) |>
  summarise(pmean = mean(hits), .groups = "drop") |>
  mutate(
    xmin = case_when(
      period == "pre" ~ as.Date("2021-01-01"),
      period == "e1"  ~ DATE_E1,
      period == "e2"  ~ DATE_E2,
      period == "e3"  ~ DATE_E3
    ),
    xmax = case_when(
      period == "pre" ~ DATE_E1,
      period == "e1"  ~ DATE_E2,
      period == "e2"  ~ DATE_E3,
      period == "e3"  ~ as.Date("2026-04-01")
    )
  )

figC <- ggplot(sm_data, aes(x = date, y = hits)) +
  # shaded period means as horizontal segments
  geom_segment(data = sm_means,
               aes(x = xmin, xend = xmax, y = pmean, yend = pmean,
                   color = group),
               linewidth = 0.9, alpha = 0.4, linetype = "solid") +
  # raw monthly line
  geom_line(aes(color = group), linewidth = 0.45, alpha = 0.8) +
  # event lines
  geom_vline(xintercept = DATE_E1, linetype = "dashed",
             linewidth = 0.3, color = C_E1) +
  geom_vline(xintercept = DATE_E2, linetype = "dashed",
             linewidth = 0.3, color = C_E2) +
  geom_vline(xintercept = DATE_E3, linetype = "dashed",
             linewidth = 0.3, color = C_E3) +
  facet_wrap(~kw_label, ncol = 5, scales = "free_y") +
  scale_color_manual(values = c("Privacy Apps"          = C_PRIV,
                                "Mainstream Adult Sites" = C_MAIN),
                     name = "Group") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "Date",
    y = "Search Index (0–100)",
    title = "Figure C. Individual Keyword Trajectories with Period-Mean Reference Lines",
    subtitle = paste0(
      "One panel per keyword. Thin line = raw monthly series; ",
      "thick horizontal segment = within-period mean.\n",
      "Dashed verticals = E1 (Aug 2024), E2 (Jul 2025), E3 (Jan 2026). ",
      "Free y-axes to show shape; note scale differences across panels."
    ),
    caption = paste0(
      "Privacy Apps (blue): private porn app, adult VPN, encrypted adult content, ",
      "offline porn download, anonymous adult browser.\n",
      "Mainstream Adult Sites (red): pornhub, xvideos, xhamster, redtube, tube8. ",
      "Source: Google Trends, worldwide monthly, Jan 2021–Mar 2026."
    )
  ) +
  theme_pub2() +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y    = element_text(size = 7),
    legend.position = "bottom",
    panel.spacing  = unit(0.8, "lines"),
    strip.text     = element_text(size = 7.5, face = "bold")
  )

ggsave("G2_FigureC_SmallMultiple.pdf", figC, width = 11, height = 5.5, dpi = 300)
ggsave("G2_FigureC_SmallMultiple.png", figC, width = 11, height = 5.5, dpi = 300)
cat("Figure C saved.\n")

# =============================================================================
# FIGURE D — Lollipop: % Change from Pre-E1 Baseline
# Compares relative (not absolute) change — important because the two groups
# start at very different baselines (2.77 vs 34.26)
# One lollipop per keyword × period combination
# =============================================================================

# Baseline: Pre-E1 mean per keyword
baseline_kw <- all_long_g2 |>
  filter(date < DATE_E1) |>
  group_by(keyword, group) |>
  summarise(baseline = mean(hits, na.rm = TRUE), .groups = "drop")

pct_change_data <- all_long_g2 |>
  mutate(period = case_when(
    date >= DATE_E1 & date < DATE_E2 ~ "Post-E1",
    date >= DATE_E2 & date < DATE_E3 ~ "Post-E2",
    date >= DATE_E3                  ~ "Post-E3",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(period)) |>
  group_by(keyword, group, period) |>
  summarise(period_mean = mean(hits, na.rm = TRUE), .groups = "drop") |>
  left_join(baseline_kw, by = c("keyword", "group")) |>
  mutate(
    pct_chg   = (period_mean - baseline) / (baseline + 0.5) * 100,
    kw_label  = str_to_title(str_remove(keyword,
                                        "pornhub|xvideos|xhamster|redtube|tube8")),
    kw_label  = str_to_title(keyword),
    period    = factor(period,
                       levels = c("Post-E1","Post-E2","Post-E3")),
    direction = if_else(pct_chg >= 0, "Increase", "Decrease")
  ) |>
  # sort within each group by Post-E2 change
  group_by(group) |>
  mutate(sort_val = if_else(period == "Post-E2", pct_chg, NA_real_)) |>
  group_by(group, kw_label) |>
  mutate(max_sort = max(sort_val, na.rm = TRUE)) |>
  ungroup() |>
  mutate(kw_label = reorder(kw_label, max_sort))

figD <- ggplot(pct_change_data,
               aes(x = pct_chg, y = kw_label, color = group)) +
  geom_vline(xintercept = 0, color = "#888888",
             linewidth = 0.5, linetype = "solid") +
  geom_vline(xintercept = c(-50, 50, 100, 200, 400),
             color = "#f0f0f0", linewidth = 0.3) +
  # lollipop stems
  geom_segment(aes(x = 0, xend = pct_chg, y = kw_label, yend = kw_label),
               position = position_dodge(width = 0.7),
               linewidth = 0.5, alpha = 0.7) +
  # lollipop heads
  geom_point(size = 3,
             position = position_dodge(width = 0.7)) +
  # percentage labels
  geom_text(aes(label = paste0(ifelse(pct_chg > 0, "+", ""),
                               round(pct_chg, 0), "%")),
            position = position_dodge(width = 0.7),
            hjust = ifelse(pct_change_data$pct_chg >= 0, -0.2, 1.2),
            size = 2.4, family = "serif") +
  facet_grid(group ~ period, scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("Privacy Apps"          = C_PRIV,
                                "Mainstream Adult Sites" = C_MAIN),
                     guide = "none") +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.15, 0.3))
  ) +
  labs(
    x = "Percentage Change from Pre-E1 Baseline (%)",
    y = NULL,
    title = "Figure D. Percentage Change from Baseline by Keyword, Group, and Post-Event Period",
    subtitle = paste0(
      "Lollipop chart showing relative change from the pre-E1 mean (Jan 2021–Jul 2024) ",
      "for each keyword.\n",
      "Relative change is shown because the two groups start at very different ",
      "baselines (Privacy Apps: M = 2.77; Mainstream: M = 34.26).\n",
      "Post-E2 shows the largest divergence: Privacy Apps rise sharply; ",
      "Mainstream Sites are largely stable or declining."
    ),
    caption = paste0(
      "Note. Percentage change = (post-period mean − pre-E1 mean) / (pre-E1 mean + 0.5) × 100. ",
      "A small constant (0.5) is added to the\n",
      "denominator to avoid division by near-zero values for low-baseline Privacy App terms.\n",
      "E1 = Aug 2024; E2 = Jul 2025; E3 = Jan 2026."
    )
  ) +
  theme_pub2() +
  theme(
    strip.text         = element_text(face = "bold", size = 9),
    strip.background   = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#eeeeee"),
    axis.text.y        = element_text(size = 8.5)
  )



#################################

C_PRIV  <- "#2C7BB6"
C_MAIN  <- "#D7191C"
DATE_E1 <- as.Date("2024-08-01")
DATE_E2 <- as.Date("2025-07-01")
DATE_E3 <- as.Date("2026-01-01")

theme_pub2 <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(family = "serif", color = "#1a1a1a"),
      plot.title        = element_text(face = "bold", size = base_size + 2),
      plot.subtitle     = element_text(size = base_size - 1, color = "#555555",
                                       margin = margin(b = 6)),
      plot.caption      = element_text(size = base_size - 2, color = "#777777",
                                       hjust = 0, margin = margin(t = 6)),
      axis.title        = element_text(size = base_size),
      axis.text         = element_text(size = base_size - 1, color = "#333333"),
      panel.grid.major  = element_line(color = "#eeeeee", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold", size = base_size - 1),
      legend.text       = element_text(size = base_size - 1),
      strip.text        = element_text(face = "bold", size = base_size - 1),
      plot.margin       = margin(10, 12, 10, 12)
    )
}

# =============================================================================
# FIGURE 1 — ggRepel Scatterplot
# x = Privacy Apps monthly mean hits
# y = Mainstream Adult Sites monthly mean hits
# Each dot = one month; labelled by month-year if it's an outlier or event month
# Regression fit overlaid — do the two groups move together or diverge?
# Colour coded by period (pre / post E1 / post E2 / post E3)
# =============================================================================

# Pivot wide so each month has both group values as columns
scatter_data <- group_monthly_g2 |>
  dplyr::select(group, date, mean_hits) |>
  pivot_wider(names_from = group, values_from = mean_hits) |>
  rename(privacy = `Privacy Apps`, mainstream = `Mainstream Adult Sites`) |>
  mutate(
    period = case_when(
      date < DATE_E1                   ~ "Pre-E1 (baseline)",
      date >= DATE_E1 & date < DATE_E2 ~ "Post-E1: Image Gen",
      date >= DATE_E2 & date < DATE_E3 ~ "Post-E2: Spicy Mode",
      date >= DATE_E3                  ~ "Post-E3: Crisis"
    ),
    period = factor(period, levels = c(
      "Pre-E1 (baseline)",
      "Post-E1: Image Gen",
      "Post-E2: Spicy Mode",
      "Post-E3: Crisis"
    )),
    month_label = format(date, "%b %Y"),
    # label event months + statistical outliers (high privacy or extreme mainstream)
    is_event_month = date %in% c(DATE_E1, DATE_E2, DATE_E3),
    # label months where privacy > 10 OR mainstream < 20 OR mainstream > 70
    is_notable = privacy > 8 | mainstream > 70 | mainstream < 15 | is_event_month,
    label = if_else(is_notable, month_label, "")
  )

period_colours <- c(
  "Pre-E1 (baseline)"   = "#888888",
  "Post-E1: Image Gen"  = "#4D9DE0",
  "Post-E2: Spicy Mode" = "#1A9641",
  "Post-E3: Crisis"     = "#D7191C"
)

#####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, dplyr, tidyr, lubridate, stringr,
  patchwork, scales, ggrepel, ggstatsplot
)
######

fig1 <- ggplot(scatter_data,
               aes(x = privacy, y = mainstream, color = period)) +
  # regression fit — overall trend across all months
  geom_smooth(aes(group = 1),
              method = "lm", se = TRUE,
              color = "#333333", fill = "#dddddd",
              linewidth = 0.7, linetype = "solid", alpha = 0.3) +
  # per-period smoother (loess, no SE) to show divergence
  geom_smooth(aes(group = period, color = period),
              method = "loess", se = FALSE,
              linewidth = 0.45, linetype = "dashed", alpha = 0.6) +
  # raw monthly dots
  geom_point(size = 2.6, alpha = 0.85) +
  # ggRepel labels — no overlap
  geom_text_repel(
    aes(label = label),
    size          = 2.6,
    family        = "serif",
    fontface      = "italic",
    box.padding   = 0.35,
    point.padding = 0.3,
    segment.size  = 0.3,
    segment.color = "#aaaaaa",
    max.overlaps  = 20,
    min.segment.length = 0.2
  ) +
  scale_color_manual(values = period_colours, name = "Period") +
  scale_x_continuous(
    name   = "Privacy Apps — Mean Monthly Search Index (0–100)",
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_y_continuous(
    name   = "Mainstream Adult Sites — Mean Monthly Search Index (0–100)",
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = paste0(
      "Figure 1. Privacy Apps vs. Mainstream Adult Sites: ",
      "Monthly Search Volume Scatterplot, 2021–2026"
    ),
    subtitle = paste0(
      "Each point = one calendar month (N = 62). ",
      "Colour = analytical period. ",
      "Solid grey line = OLS regression fit (overall);\n",
      "dashed lines = per-period LOESS. ",
      "Labels show notable months (event months and statistical outliers). ",
      "Points in the upper-right\n",
      "quadrant indicate months where both privacy-seeking and mainstream use were elevated."
    ),
    caption = paste0(
      "Note. A positive overall slope would suggest co-movement (shared drivers); ",
      "a flat or negative slope suggests substitution or divergence.\n",
      "Labels produced with ggrepel (non-overlapping automatic placement). ",
      "Source: Google Trends, worldwide monthly, Jan 2021–Mar 2026."
    )
  ) +
  theme_pub2() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.7, "lines")
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 3)))


# =============================================================================
# FIGURE 2 — ggStatsPlot Violin + Boxplot
# Distribution of monthly search hits across the four periods
# One panel per group — side by side using patchwork
# Shows full distributional shape, not just means
# =============================================================================

# Long format with period labels — one row per keyword-month observation
violin_data <- all_long_g2 |>
  mutate(
    period = case_when(
      date < DATE_E1                   ~ "Pre-E1",
      date >= DATE_E1 & date < DATE_E2 ~ "Post-E1",
      date >= DATE_E2 & date < DATE_E3 ~ "Post-E2",
      date >= DATE_E3                  ~ "Post-E3"
    ),
    period = factor(period,
                    levels = c("Pre-E1", "Post-E1", "Post-E2", "Post-E3"))
  )

period_fill_colours <- c(
  "Pre-E1"  = "#cccccc",
  "Post-E1" = "#4D9DE0",
  "Post-E2" = "#1A9641",
  "Post-E3" = "#D7191C"
)

# Panel A: Privacy Apps
pA_violin <- ggbetweenstats(
  data             = violin_data |> filter(group == "Privacy Apps"),
  x                = period,
  y                = hits,
  type             = "nonparametric",      # Kruskal-Wallis (appropriate for non-normal)
  pairwise.display = "significant",        # only significant pairwise comparisons shown
  p.adjust.method  = "BH",                # Benjamini-Hochberg correction
  centrality.plotting = TRUE,
  centrality.type  = "median",
  point.args       = list(alpha = 0.25, size = 1.2,
                          position = ggplot2::position_jitter(width = 0.1)),
  violin.args      = list(alpha = 0.35, linewidth = 0.5),
  boxplot.args     = list(width = 0.15, alpha = 0.6),
  ggplot.component = list(
    scale_color_manual(values = period_fill_colours),
    scale_fill_manual(values  = period_fill_colours),
    labs(
      x        = NULL,
      y        = "Monthly Search Index (0–100)",
      subtitle = "A. Privacy Apps"
    ),
    theme_pub2(),
    theme(
      plot.subtitle = element_text(face = "bold", size = 11, color = C_PRIV),
      legend.position = "none"
    )
  )
)

# Panel B: Mainstream Adult Sites
pB_violin <- ggbetweenstats(
  data             = violin_data |> filter(group == "Mainstream Adult Sites"),
  x                = period,
  y                = hits,
  type             = "nonparametric",
  pairwise.display = "significant",
  p.adjust.method  = "BH",
  centrality.plotting = TRUE,
  centrality.type  = "median",
  point.args       = list(alpha = 0.25, size = 1.2,
                          position = ggplot2::position_jitter(width = 0.1)),
  violin.args      = list(alpha = 0.35, linewidth = 0.5),
  boxplot.args     = list(width = 0.15, alpha = 0.6),
  ggplot.component = list(
    scale_color_manual(values = period_fill_colours),
    scale_fill_manual(values  = period_fill_colours),
    labs(
      x        = "Analytical Period",
      y        = "Monthly Search Index (0–100)",
      subtitle = "B. Mainstream Adult Sites"
    ),
    theme_pub2(),
    theme(
      plot.subtitle = element_text(face = "bold", size = 11, color = C_MAIN),
      legend.position = "none"
    )
  )
)

fig2 <- (pA_violin / pB_violin) +
  plot_annotation(
    title = paste0(
      "Figure 2. Distribution of Monthly Search Volume by ",
      "Analytical Period: Privacy Apps vs. Mainstream Adult Sites"
    ),
    subtitle = paste0(
      "Violin plots (distributional shape) with embedded boxplots (median, IQR) ",
      "and jittered raw observations.\n",
      "Statistical test: Kruskal-Wallis with Dunn pairwise comparisons ",
      "(Benjamini-Hochberg correction). ",
      "Median marked with diamond.\n",
      "Pre-E1 = Jan 2021–Jul 2024; Post-E1 = Aug 2024–Jun 2025; ",
      "Post-E2 = Jul–Dec 2025; Post-E3 = Jan–Mar 2026."
    ),
    caption = paste0(
      "Note. Each observation = one keyword × one month (N = 5 keywords per group). ",
      "Pairwise comparisons shown only for significant pairs (p < .05, BH-corrected).\n",
      "Kruskal-Wallis is appropriate given non-normal distributions ",
      "(Google Trends data is bounded 0–100 with floor effects).\n",
      "Source: Google Trends, worldwide monthly, Jan 2021–Mar 2026."
    ),
    theme = theme(
      plot.title    = element_text(family = "serif", face = "bold", size = 12),
      plot.subtitle = element_text(family = "serif", size = 9, color = "#444444"),
      plot.caption  = element_text(family = "serif", size = 8,
                                   color = "#666666", hjust = 0)
    )
  )

#####



