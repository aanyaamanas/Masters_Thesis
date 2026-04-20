# =============================================================================
# GROUP 3 ANALYSIS: MAINSTREAM AI PLATFORMS vs NCII-SPECIFIC TOOLS
# Aanyaa Manas | MS Criminology | University of Pennsylvania | May 2026
#
# Research question 3:
#   Do NCII-specific tool searches respond differently to Grok product events
#   than mainstream AI image generation platform searches?
#   Does Grok's normalisation of NSFW generation (E2) accelerate search
#   interest in purpose-built NCII tools relative to general AI platforms?
#
# Design: Interrupted Time Series (ITS)
# Unit: keyword × month — MONTHLY throughout, no weekly mixing
#
# Group A — Mainstream AI Platforms:
#   DALL-E, Midjourney, Stable Diffusion, Adobe Firefly, ChatGPT image
#
# Group B — NCII-Specific Tools:
#   deepfake app, nudify AI, undress AI, clothes remover AI, face swap porn
#
# Three anchor events (same as Groups 1 and 2):
#   E1: August 2024     — Grok 2 image generation launch
#   E2: July 2025       — Grok Spicy Mode launch
#   E3: January 2026    — Grok public crisis + regulatory spotlight
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gtrendsR,      # Google Trends API
  dplyr,         # data wrangling
  tidyr,         # reshaping
  tibble,        # tibbles
  lubridate,     # dates
  stringr,       # string ops
  purrr,         # map/reduce
  zoo,           # rolling means
  lmtest,        # coeftest
  sandwich,      # vcovHAC
  broom,         # tidy()
  ggplot2,       # base plotting
  patchwork,     # multi-panel
  ggrepel,       # non-overlapping labels
  scales,        # axis formatting
  flextable,     # tables
  officer        # Word export
)

# =============================================================================
# 1. DEFINE TERMS AND DATES
# =============================================================================

mainstream_terms <- c(
  "DALL-E",
  "Midjourney",
  "Stable Diffusion",
  "Adobe Firefly",
  "ChatGPT image"
)

ncii_terms <- c(
  "deepfake app",
  "nudify AI",
  "undress AI",
  "clothes remover AI",
  "face swap porn"
)

DATE_E1    <- as.Date("2024-08-01")
DATE_E2    <- as.Date("2025-07-01")
DATE_E3    <- as.Date("2026-01-01")
TIME_RANGE <- "2021-01-01 2026-03-01"

# =============================================================================
# 2. FETCH DATA
# =============================================================================

cat("Fetching Google Trends data (monthly, worldwide)...\n")

mainstream_raw <- gtrends(keyword = mainstream_terms, time = TIME_RANGE, geo = "")
ncii_raw       <- gtrends(keyword = ncii_terms,       time = TIME_RANGE, geo = "")

cat("Data fetched.\n")

# =============================================================================
# 3. CLEAN AND COMBINE
# =============================================================================

clean_g3 <- function(raw, group_label) {
  raw$interest_over_time |>
    dplyr::select(date, keyword, hits) |>
    mutate(
      date  = floor_date(as.Date(date), "month"),
      hits  = as.numeric(ifelse(hits == "<1", 0.5, as.character(hits))),
      group = group_label
    ) |>
    distinct(date, keyword, .keep_all = TRUE) |>
    arrange(keyword, date)
}

mainstream_long <- clean_g3(mainstream_raw, "Mainstream AI")
ncii_long       <- clean_g3(ncii_raw,       "NCII Tools")

all_long_g3 <- bind_rows(mainstream_long, ncii_long)

# N check — must be equal per keyword
cat("\n N check: rows per keyword \n")
print(as.data.frame(all_long_g3 |> count(keyword, group)))
cat("Date range:", format(min(all_long_g3$date)), "to",
    format(max(all_long_g3$date)), "\n")
cat("Total rows:", nrow(all_long_g3), "\n")

write.csv(all_long_g3, "G3_google_trends_raw.csv", row.names = FALSE)

# =============================================================================
# 4. BUILD ANALYSIS DATAFRAMES
# =============================================================================

# Keyword-level monthly (for ITS and figures)
kw_monthly_g3 <- all_long_g3 |>
  mutate(
    post_e1   = if_else(date >= DATE_E1, 1L, 0L),
    post_e2   = if_else(date >= DATE_E2, 1L, 0L),
    post_e3   = if_else(date >= DATE_E3, 1L, 0L),
    treatment = if_else(group == "NCII Tools", 1L, 0L),
    time      = as.integer(interval(min(date), date) %/% months(1))
  )

# Group-level monthly (mean across keywords per group per month)
group_monthly_g3 <- all_long_g3 |>
  group_by(group, date) |>
  summarise(mean_hits = mean(hits, na.rm = TRUE), .groups = "drop") |>
  mutate(
    post_e1   = if_else(date >= DATE_E1, 1L, 0L),
    post_e2   = if_else(date >= DATE_E2, 1L, 0L),
    post_e3   = if_else(date >= DATE_E3, 1L, 0L),
    treatment = if_else(group == "NCII Tools", 1L, 0L),
    time      = as.integer(interval(min(date), date) %/% months(1))
  )

cat("\n N check: group monthly rows (must be equal) \n")
print(as.data.frame(group_monthly_g3 |> count(group)))

# =============================================================================
# 5. DESCRIPTIVE STATISTICS
# =============================================================================

desc_g3 <- all_long_g3 |>
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
    n_obs       = n(),
    mean_hits   = round(mean(hits,   na.rm = TRUE), 2),
    median_hits = round(median(hits, na.rm = TRUE), 2),
    sd_hits     = round(sd(hits,     na.rm = TRUE), 2),
    max_hits    = round(max(hits,    na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n Descriptive Statistics \n")
print(as.data.frame(desc_g3))

# =============================================================================
# 6. ITS MODELS
# =============================================================================

run_its_g3 <- function(grp, df) {
  dat <- df |> filter(group == grp)
  mod <- lm(mean_hits ~ time + post_e1 + post_e2 + post_e3, data = dat)
  hac <- coeftest(mod, vcov = vcovHAC(mod))
  tidy(hac) |>
    mutate(
      group      = grp,
      r_squared  = round(summary(mod)$r.squared, 3),
      n_obs      = nrow(dat),
      term_label = dplyr::recode(term,
                                 "(Intercept)" = "Intercept",
                                 "time"        = "Temporal Trend (months)",
                                 "post_e1"     = "Level Shift: E1 (Aug 2024)",
                                 "post_e2"     = "Level Shift: E2 (Jul 2025)",
                                 "post_e3"     = "Level Shift: E3 (Jan 2026)"
      ),
      sig = case_when(
        p.value < .001 ~ "***", p.value < .01 ~ "**",
        p.value < .05  ~ "*",   p.value < .10 ~ ".",
        TRUE ~ ""
      )
    )
}

its_mainstream <- run_its_g3("Mainstream AI", group_monthly_g3)
its_ncii       <- run_its_g3("NCII Tools",    group_monthly_g3)
its_results_g3 <- bind_rows(its_mainstream, its_ncii)

cat("\n ITS Results \n")
print(as.data.frame(
  its_results_g3 |>
    dplyr::select(group, term_label, estimate, std.error, p.value, sig)
))

# Keyword-level ITS
run_kw_g3 <- function(kw, df) {
  dat <- df |> filter(keyword == kw)
  if (nrow(dat) < 20 || var(dat$hits) < 0.01) return(NULL)
  mod <- lm(hits ~ time + post_e1 + post_e2 + post_e3, data = dat)
  hac <- coeftest(mod, vcov = vcovHAC(mod))
  tidy(hac) |>
    mutate(keyword = kw, group = unique(dat$group),
           r_squared = summary(mod)$r.squared)
}

its_kw_g3 <- map_dfr(unique(kw_monthly_g3$keyword), ~run_kw_g3(.x, kw_monthly_g3)) |>
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
# 7. COLOURS AND THEME
# =============================================================================

C_MAIN  <- "#2C7BB6"   # blue — Mainstream AI
C_NCII  <- "#D7191C"   # red  — NCII Tools
C_E1    <- "#555555"
C_E2    <- "#1A9641"
C_E3    <- "#D7191C"

# 10 keyword colours — warm reds for NCII, cool blues for Mainstream
kw_cols <- c(
  "DALL-E"              = "#4292C6",
  "Midjourney"          = "#2171B5",
  "Stable Diffusion"    = "#6BAED6",
  "Adobe Firefly"       = "#084594",
  "ChatGPT Image"       = "#9ECAE1",
  "Deepfake App"        = "#CB181D",
  "Nudify AI"           = "#EF3B2C",
  "Undress AI"          = "#FB6A4A",
  "Clothes Remover AI"  = "#FC9272",
  "Face Swap Porn"      = "#FCBBA1"
)

theme_g3 <- function(base_size = 10) {
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
# FIGURE 1 — Stacked Area Chart with Inline Labels + Vertical Annotations
# Shows composition and growth of EACH group's total search volume over time
# Two panels stacked: Mainstream AI (top), NCII Tools (bottom)
# =============================================================================

area_data <- kw_monthly_g3 |>
  filter(date >= as.Date("2022-01-01")) |>
  mutate(kw_label = str_to_title(keyword))

# For inline labels: find the last date where each keyword is big enough to label
# Place label at the rightmost point of the series
label_positions <- area_data |>
  filter(date == max(date)) |>
  group_by(group) |>
  arrange(group, desc(hits)) |>
  mutate(
    cum_hits  = cumsum(hits),
    label_y   = cum_hits - hits / 2   # midpoint of each stack segment
  ) |>
  ungroup()

# Build stacked area for each group separately
make_area_panel <- function(grp, col_pal, subtitle_text) {
  dat <- area_data |> filter(group == grp)
  
  # Compute stacking order: largest total at bottom
  kw_order <- dat |>
    group_by(kw_label) |>
    summarise(tot = sum(hits)) |>
    arrange(tot) |>
    pull(kw_label)
  
  dat <- dat |>
    mutate(kw_label = factor(kw_label, levels = kw_order))
  
  # Inline label data: last 3 months mean to stabilise position
  lab_dat <- dat |>
    filter(date >= max(date) - months(2)) |>
    group_by(kw_label) |>
    summarise(hits = mean(hits), .groups = "drop") |>
    arrange(factor(kw_label, levels = kw_order)) |>
    mutate(
      cum_top = cumsum(hits),
      cum_bot = lag(cum_top, default = 0),
      label_y = (cum_top + cum_bot) / 2,
      label_x = max(dat$date) + days(20)
    )
  
  ggplot(dat, aes(x = date, y = hits, fill = kw_label)) +
    # Period shading
    annotate("rect", xmin = DATE_E1, xmax = DATE_E2,
             ymin = -Inf, ymax = Inf, fill = "#f0f7ff", alpha = 0.5) +
    annotate("rect", xmin = DATE_E2, xmax = DATE_E3,
             ymin = -Inf, ymax = Inf, fill = "#f0fff4", alpha = 0.5) +
    annotate("rect", xmin = DATE_E3, xmax = as.Date("2026-04-01"),
             ymin = -Inf, ymax = Inf, fill = "#fff5f5", alpha = 0.6) +
    # Stacked area
    geom_area(position = "stack", alpha = 0.88,
              color = "white", linewidth = 0.25) +
    # Event vertical lines
    geom_vline(xintercept = DATE_E1,
               linetype = "dashed", linewidth = 0.45, color = C_E1) +
    geom_vline(xintercept = DATE_E2,
               linetype = "dashed", linewidth = 0.45, color = C_E2) +
    geom_vline(xintercept = DATE_E3,
               linetype = "dashed", linewidth = 0.45, color = C_E3) +
    # Event labels at top
    annotate("label", x = DATE_E1, y = Inf,
             label = "E1: Grok 2\nImage Gen",
             vjust = 1.4, hjust = -0.05, size = 2.4,
             fill = "white", color = C_E1,
             label.size = 0.15, label.padding = unit(0.15, "lines"),
             family = "serif", fontface = "italic") +
    annotate("label", x = DATE_E2, y = Inf,
             label = "E2: Spicy\nMode",
             vjust = 1.4, hjust = -0.05, size = 2.4,
             fill = "white", color = C_E2,
             label.size = 0.15, label.padding = unit(0.15, "lines"),
             family = "serif", fontface = "italic") +
    annotate("label", x = DATE_E3, y = Inf,
             label = "E3: Crisis",
             vjust = 1.4, hjust = -0.05, size = 2.4,
             fill = "white", color = C_E3,
             label.size = 0.15, label.padding = unit(0.15, "lines"),
             family = "serif", fontface = "italic") +
    # Inline keyword labels at right edge
    geom_text(data = lab_dat,
              aes(x = label_x, y = label_y,
                  label = kw_label, color = kw_label),
              inherit.aes = FALSE,
              hjust = 0, size = 2.5, fontface = "italic",
              family = "serif") +
    scale_fill_manual(values  = col_pal, guide = "none") +
    scale_color_manual(values = col_pal, guide = "none") +
    scale_x_date(
      date_breaks = "6 months", date_labels = "%b %Y",
      expand = expansion(mult = c(0.01, 0.28))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = NULL, y = "Stacked Search Index",
         subtitle = subtitle_text) +
    theme_g3() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

mainstream_pal <- kw_cols[c("DALL-E","Midjourney","Stable Diffusion",
                            "Adobe Firefly","ChatGPT Image")]
ncii_pal       <- kw_cols[c("Deepfake App","Nudify AI","Undress AI",
                            "Clothes Remover AI","Face Swap Porn")]

pA_area <- make_area_panel(
  "Mainstream AI", mainstream_pal,
  "A. Mainstream AI Platforms (DALL-E, Midjourney, Stable Diffusion, Adobe Firefly, ChatGPT image)"
)
pB_area <- make_area_panel(
  "NCII Tools", ncii_pal,
  "B. NCII-Specific Tools (deepfake app, nudify AI, undress AI, clothes remover AI, face swap porn)"
)

fig1_g3 <- (pA_area / pB_area) +
  plot_annotation(
    title = paste0(
      "Figure 1. Stacked Search Volume: Mainstream AI Platforms vs. ",
      "NCII-Specific Tools, 2022–2026"
    ),
    subtitle = paste0(
      "Monthly Google Trends index stacked across keywords within each group. ",
      "Shaded regions: blue = post-E1, green = post-E2, red = post-E3.\n",
      "Inline labels show keyword composition at the right edge of the series. ",
      "Note different y-axis scales between panels."
    ),
    caption = paste0(
      "Source: Google Trends (worldwide, monthly, Jan 2021–Mar 2026). ",
      "Stacked values are the sum of normalised indices within each group.\n",
      "Because each query batch is independently normalised, ",
      "within-group stacking is internally consistent but cross-group totals ",
      "are not directly comparable in absolute terms.\n",
      "E1 = Grok 2 image generation (Aug 2024); ",
      "E2 = Spicy Mode launch (Jul 2025); ",
      "E3 = January 2026 public crisis."
    ),
    theme = theme(
      plot.title    = element_text(family = "serif", face = "bold", size = 12),
      plot.subtitle = element_text(family = "serif", size = 9, color = "#444444"),
      plot.caption  = element_text(family = "serif", size = 8,
                                   color = "#666666", hjust = 0)
    )
  )


# =============================================================================
# FIGURE 2 — Bertin-Style Valued Points / Bubble Scatterplot with ggRepel
# x = mean search volume (signal strength)
# y = volatility (SD) — how erratic the series is
# size = max observed hits (peak salience)
# colour = group
# label = keyword (ggRepel, no overlap)
# One panel per period — 2x2 facet showing how the keyword landscape shifted
# =============================================================================

bubble_data <- kw_monthly_g3 |>
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
    ))
  ) |>
  group_by(keyword, group, period) |>
  summarise(
    mean_hits = mean(hits,   na.rm = TRUE),
    sd_hits   = sd(hits,     na.rm = TRUE),
    max_hits  = max(hits,    na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    kw_label = str_to_title(keyword),
    # Only label the most informative points (high mean or high SD)
    show_label = mean_hits > 5 | sd_hits > 8 | max_hits > 40
  )

fig2_g3 <- ggplot(bubble_data,
                  aes(x = mean_hits, y = sd_hits,
                      size = max_hits, color = group)) +
  # zero reference lines
  geom_hline(yintercept = 0, color = "#cccccc", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "#cccccc", linewidth = 0.3) +
  # bubbles
  geom_point(alpha = 0.72, stroke = 0.3) +
  # ggRepel labels — only notable points
  geom_text_repel(
    data = bubble_data |> filter(show_label),
    aes(label = kw_label),
    size            = 2.6,
    family          = "serif",
    fontface        = "italic",
    box.padding     = 0.4,
    point.padding   = 0.3,
    segment.size    = 0.3,
    segment.color   = "#bbbbbb",
    segment.curvature = -0.1,
    max.overlaps    = 20,
    min.segment.length = 0.15,
    show.legend     = FALSE
  ) +
  facet_wrap(~period, ncol = 2) +
  scale_color_manual(
    values = c("Mainstream AI" = C_MAIN, "NCII Tools" = C_NCII),
    name   = "Group"
  ) +
  scale_size_continuous(
    name   = "Peak Search Index",
    range  = c(2, 12),
    breaks = c(10, 30, 60, 100)
  ) +
  scale_x_continuous(
    name   = "Mean Monthly Search Index (0–100)",
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  scale_y_continuous(
    name   = "Volatility — SD of Monthly Search Index",
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  labs(
    title = paste0(
      "Figure 2. Keyword Signal Strength vs. Volatility by Period: ",
      "Mainstream AI vs. NCII Tools"
    ),
    subtitle = paste0(
      "Each bubble = one keyword in one analytical period. ",
      "x-axis = average monthly search interest; ",
      "y-axis = standard deviation (volatility).\n",
      "Bubble size = peak observed search index. ",
      "Labels shown for keywords with mean > 5 or peak > 40 ",
      "(ggrepel, non-overlapping).\n",
      "Keywords moving rightward and upward over periods show ",
      "both growing interest and increasing instability."
    ),
    caption = paste0(
      "Note. Inspired by Bertin's valued points representation: ",
      "position encodes two statistical dimensions simultaneously, ",
      "while size adds a third.\n",
      "Pre-E1 = Jan 2021–Jul 2024; Post-E1 = Aug 2024–Jun 2025; ",
      "Post-E2 = Jul–Dec 2025; Post-E3 = Jan–Mar 2026.\n",
      "Source: Google Trends (worldwide, monthly). ",
      "N = 5 keywords per group × months per period."
    )
  ) +
  theme_g3() +
  theme(
    strip.text      = element_text(face = "bold", size = 9.5),
    strip.background = element_rect(fill = "#f5f5f5", color = NA),
    legend.box      = "horizontal",
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 4)),
    size  = guide_legend(order = 2)
  )


# =============================================================================
# TABLES — APA style, no colour, no shading
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

# ── TABLE 1: Descriptive Statistics ──────────────────────────────────────────

t1_data <- desc_g3 |>
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
        paste0("Table 1. Descriptive Statistics of Google Trends Search Volume ",
               "by Group and Analytical Period"),
        props = fp_text(bold = TRUE, font.family = "Times New Roman",
                        font.size = 11)
      )
    )
  ) |>
  add_footer_lines(paste0(
    "Note. Google Trends index values are normalised to a 0–100 scale relative to ",
    "peak search volume within each query batch (worldwide). ",
    "All observations are at monthly resolution; ",
    "N per group = 5 keywords × number of months per period. ",
    "Mainstream AI group: DALL-E, Midjourney, Stable Diffusion, ",
    "Adobe Firefly, ChatGPT image. ",
    "NCII Tools group: deepfake app, nudify AI, undress AI, ",
    "clothes remover AI, face swap porn. ",
    "E1 = Grok 2 image generation (August 2024); ",
    "E2 = Spicy Mode launch (July 2025); ",
    "E3 = January 2026 public crisis and regulatory spotlight. ",
    "Source: Google Trends (Jan 2021–Mar 2026)."
  ))

# ── TABLE 2: ITS Results ─────────────────────────────────────────────────────

fmt_p_g3 <- function(p) {
  case_when(p < .001 ~ "< .001", TRUE ~ sprintf("%.3f", p))
}

t2_data <- its_results_g3 |>
  filter(term %in% c("post_e1", "post_e2", "post_e3")) |>
  transmute(
    Group  = group,
    Event  = dplyr::recode(term,
                           "post_e1" = "E1: Aug 2024 — Grok 2 image generation",
                           "post_e2" = "E2: Jul 2025 — Spicy Mode launch",
                           "post_e3" = "E3: Jan 2026 — Public crisis"
    ),
    `β`    = sprintf("%.3f%s", estimate, sig),
    SE     = sprintf("(%.3f)", std.error),
    `t`    = sprintf("%.3f", statistic),
    p      = fmt_p_g3(p.value),
    `R²`   = sprintf("%.3f", r_squared),
    N      = as.character(n_obs)
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
        paste0("Table 2. Interrupted Time Series Estimates: Level Shifts ",
               "at Grok Product Events by Group"),
        props = fp_text(bold = TRUE, font.family = "Times New Roman",
                        font.size = 11)
      )
    )
  ) |>
  add_footer_lines(paste0(
    "Note. ITS models estimated separately for each group using OLS of the form: ",
    "mean_hits ~ time + post_E1 + post_E2 + post_E3, ",
    "where mean_hits is the monthly mean Google Trends index across the five ",
    "keywords in each group. ",
    "β = estimated level shift at each event threshold; ",
    "SE = standard error in parentheses; ",
    "R² is the full-model R². ",
    "All standard errors are Newey-West HAC. ",
    "N = monthly observations per group. ",
    "E1 = Grok 2 image generation (August 2024); ",
    "E2 = Spicy Mode launch (July 2025); ",
    "E3 = January 2026 public crisis and regulatory spotlight. ",
    "*** p < .001, ** p < .01, * p < .05, . p < .10."
  ))

# ── Save Word doc ─────────────────────────────────────────────────────────────

doc <- read_docx() |>
  body_add_par("Table 1", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft1) |>
  body_add_par("", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_par("Table 2", style = "Normal") |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft2)

print(doc, target = "G3_Tables_Clean.docx")
cat("Tables saved to G3_Tables_Clean.docx\n")

# =============================================================================
# SAVE CLEAN CSVs
# =============================================================================

write.csv(all_long_g3,      "G3_google_trends_raw.csv",     row.names = FALSE)
write.csv(group_monthly_g3, "G3_group_monthly_clean.csv",   row.names = FALSE)
write.csv(kw_monthly_g3,    "G3_keyword_monthly_clean.csv", row.names = FALSE)
write.csv(desc_g3,          "G3_descriptive_stats.csv",     row.names = FALSE)
write.csv(its_results_g3,   "G3_its_results.csv",           row.names = FALSE)
write.csv(its_kw_g3,        "G3_its_keyword_results.csv",   row.names = FALSE)


# =============================================================================
# More Plots
# =============================================================================



if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, dplyr, tidyr, lubridate, stringr,
  patchwork, scales, ggrepel
)

# ── Palette — muted academic, 4 period colours + 2 group colours ─────────────
# Groups: slate-teal for Mainstream AI, burnt sienna for NCII Tools
C_MAIN  <- "#4E7E8B"   # slate teal
C_NCII  <- "#B85C38"   # burnt sienna
C_E1    <- "#5A5A5A"   # charcoal
C_E2    <- "#3A7D44"   # forest green
C_E3    <- "#9B2335"   # deep crimson

# Period fill colours (for shading)
C_P1 <- "#EFF6F8"   # very pale teal  — Pre-E1
C_P2 <- "#EAF4EC"   # very pale green — Post-E1
C_P3 <- "#FFF3E8"   # very pale amber — Post-E2
C_P4 <- "#F9EBEB"   # very pale rose  — Post-E3

# 10-keyword palette — analogous, not random
kw_cols <- c(
  "DALL-E"             = "#1D6A96",
  "Midjourney"         = "#2B9EB3",
  "Stable Diffusion"   = "#5BBCD6",
  "Adobe Firefly"      = "#8ED4E3",
  "Chatgpt Image"      = "#BFE9F0",
  "Deepfake App"       = "#7B2D2D",
  "Nudify Ai"          = "#A84848",
  "Undress Ai"         = "#C87B5A",
  "Clothes Remover Ai" = "#D9A07A",
  "Face Swap Porn"     = "#ECC9A8"
)

DATE_E1 <- as.Date("2024-08-01")
DATE_E2 <- as.Date("2025-07-01")
DATE_E3 <- as.Date("2026-01-01")

theme_compact <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(family = "serif", color = "#1a1a1a"),
      plot.title        = element_text(face = "bold", size = base_size + 1.5,
                                       margin = margin(b = 3)),
      plot.subtitle     = element_text(size = base_size - 1, color = "#555555",
                                       lineheight = 1.3, margin = margin(b = 5)),
      plot.caption      = element_text(size = base_size - 2, color = "#888888",
                                       hjust = 0, lineheight = 1.2,
                                       margin = margin(t = 5)),
      axis.title        = element_text(size = base_size - 0.5),
      axis.text         = element_text(size = base_size - 1.5, color = "#444444"),
      panel.grid.major  = element_line(color = "#f0f0f0", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold", size = base_size - 1),
      legend.text       = element_text(size = base_size - 1.5),
      legend.key.size   = unit(0.6, "lines"),
      strip.text        = element_text(face = "bold", size = base_size - 0.5),
      plot.margin       = margin(8, 10, 6, 8)
    )
}

# =============================================================================
# FIGURE 1 — Annotated dual-line chart + ITS coefficient strip
# Panel A: both group mean series with shaded periods (compact, half-page width)
# Panel B: dot-and-CI strip of only the 6 ITS event coefficients
# Together they form one self-contained figure
# =============================================================================

# ── Panel A: time series ─────────────────────────────────────────────────────

ts_data <- group_monthly_g3 |>
  mutate(Group = if_else(group == "Mainstream AI",
                         "Mainstream AI", "NCII Tools"))

# Period-mean segments to show the ITS step structure visually
period_means <- ts_data |>
  mutate(period = case_when(
    date < DATE_E1                   ~ 1L,
    date >= DATE_E1 & date < DATE_E2 ~ 2L,
    date >= DATE_E2 & date < DATE_E3 ~ 3L,
    date >= DATE_E3                  ~ 4L
  )) |>
  group_by(Group, period) |>
  summarise(
    xmin  = min(date),
    xmax  = max(date),
    pmean = mean(mean_hits),
    .groups = "drop"
  )

pA <- ggplot(ts_data, aes(x = date, y = mean_hits, color = Group)) +
  # period background shading
  annotate("rect", xmin = as.Date("2021-01-01"), xmax = DATE_E1,
           ymin = -Inf, ymax = Inf, fill = C_P1, alpha = 1) +
  annotate("rect", xmin = DATE_E1, xmax = DATE_E2,
           ymin = -Inf, ymax = Inf, fill = C_P2, alpha = 1) +
  annotate("rect", xmin = DATE_E2, xmax = DATE_E3,
           ymin = -Inf, ymax = Inf, fill = C_P3, alpha = 1) +
  annotate("rect", xmin = DATE_E3, xmax = as.Date("2026-04-01"),
           ymin = -Inf, ymax = Inf, fill = C_P4, alpha = 1) +
  # period mean lines (ITS step function) — thin, same colour, dashed
  geom_segment(data = period_means,
               aes(x = xmin, xend = xmax,
                   y = pmean, yend = pmean,
                   color = Group),
               linewidth = 0.9, linetype = "dashed",
               alpha = 0.5, inherit.aes = FALSE) +
  # raw monthly series
  geom_line(linewidth = 0.65, alpha = 0.9) +
  # event lines
  geom_vline(xintercept = DATE_E1,
             color = C_E1, linewidth = 0.5, linetype = "solid") +
  geom_vline(xintercept = DATE_E2,
             color = C_E2, linewidth = 0.5, linetype = "solid") +
  geom_vline(xintercept = DATE_E3,
             color = C_E3, linewidth = 0.5, linetype = "solid") +
  # event labels
  annotate("text", x = DATE_E1, y = Inf,
           label = "E1", vjust = 1.6, hjust = -0.3,
           size = 2.4, color = C_E1, fontface = "bold", family = "serif") +
  annotate("text", x = DATE_E2, y = Inf,
           label = "E2", vjust = 1.6, hjust = -0.3,
           size = 2.4, color = C_E2, fontface = "bold", family = "serif") +
  annotate("text", x = DATE_E3, y = Inf,
           label = "E3", vjust = 1.6, hjust = -0.3,
           size = 2.4, color = C_E3, fontface = "bold", family = "serif") +
  scale_color_manual(values = c("Mainstream AI" = C_MAIN,
                                "NCII Tools"    = C_NCII),
                     name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    x = NULL,
    y = "Mean Search Index (0–100)",
    subtitle = "A. Monthly group means with period-mean reference (dashed) and ITS event lines"
  ) +
  theme_compact() +
  theme(legend.position = "bottom")

# ── Panel B: ITS event coefficient strip ─────────────────────────────────────

coef_data <- its_results_g3 |>
  filter(term %in% c("post_e1", "post_e2", "post_e3")) |>
  mutate(
    event  = dplyr::recode(term,
                           "post_e1" = "E1: Aug 2024\n(Image Gen)",
                           "post_e2" = "E2: Jul 2025\n(Spicy Mode)",
                           "post_e3" = "E3: Jan 2026\n(Crisis)"
    ),
    event  = factor(event, levels = c(
      "E1: Aug 2024\n(Image Gen)",
      "E2: Jul 2025\n(Spicy Mode)",
      "E3: Jan 2026\n(Crisis)"
    )),
    Group  = group,
    ci_lo  = estimate - 1.96 * std.error,
    ci_hi  = estimate + 1.96 * std.error,
    sig_flag = p.value < 0.05,
    # nudge groups slightly so they don't overlap
    y_dodge = if_else(Group == "Mainstream AI", 0.15, -0.15)
  )

pB <- ggplot(coef_data,
             aes(x = estimate,
                 y = as.numeric(event) + y_dodge,
                 color = Group)) +
  geom_vline(xintercept = 0, color = "#aaaaaa",
             linewidth = 0.5, linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.12, linewidth = 0.55) +
  geom_point(aes(shape = sig_flag), size = 2.8) +
  # significance annotation
  geom_text(
    data = coef_data |> filter(sig_flag),
    aes(x = ci_hi, label = sig),
    hjust = -0.2, vjust = 0.4, size = 2.8,
    family = "serif", fontface = "bold"
  ) +
  scale_color_manual(values = c("Mainstream AI" = C_MAIN,
                                "NCII Tools"    = C_NCII),
                     name = NULL) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < .05", "FALSE" = "n.s."),
                     name = NULL) +
  scale_x_continuous(
    labels = function(x) ifelse(x > 0, paste0("+", x), as.character(x))
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = levels(coef_data$event),
    expand = expansion(mult = 0.25)
  ) +
  labs(
    x = "Estimated Level Shift (Google Trends units)",
    y = NULL,
    subtitle = "B. ITS level-shift estimates at each event (95% CI, HAC SEs)"
  ) +
  theme_compact() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 7.5, lineheight = 1.1),
    legend.position    = "bottom"
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 3)),
    shape = guide_legend(order = 2)
  )

fig1 <- (pA | pB) +
  plot_layout(widths = c(1.7, 1)) +
  plot_annotation(
    title = paste0(
      "Figure 1. Search Trends and ITS Estimates: ",
      "Mainstream AI vs. NCII-Specific Tools, 2021–2026"
    ),
    subtitle = paste0(
      "Left: monthly Google Trends index (mean across 5 keywords per group). ",
      "Shaded periods: pale teal = Pre-E1, pale green = Post-E1, ",
      "pale amber = Post-E2, pale rose = Post-E3.\n",
      "Dashed lines show within-period group means. ",
      "Right: OLS level-shift coefficients from ITS models with HAC SEs. ",
      "Filled circles = p < .05; open circles = n.s."
    ),
    caption = paste0(
      "Note. Both groups show strong positive secular trends ",
      "(Mainstream AI: +0.44/month***; NCII Tools: +0.41/month***), ",
      "indicating growth independent of Grok product events. ",
      "Mainstream AI shows significant negative shifts at E1 (−9.77*) and E3 (−5.79***),\n",
      "while NCII Tools are unresponsive to all three events as discrete shocks. ",
      "The descriptive 3.5× increase in NCII Tool mean hits from pre- to post-E1 ",
      "reflects secular trend acceleration, not an event-driven level shift.\n",
      "Source: Google Trends (worldwide, monthly, Jan 2021–Mar 2026). ",
      "N = 62 monthly observations per group."
    ),
    theme = theme(
      plot.title    = element_text(family = "serif", face = "bold", size = 11),
      plot.subtitle = element_text(family = "serif", size = 8.5,
                                   color = "#444444", lineheight = 1.3),
      plot.caption  = element_text(family = "serif", size = 7.5,
                                   color = "#666666", hjust = 0,
                                   lineheight = 1.3)
    )
  )

fig1


# =============================================================================
# FIGURE 2 — ggRepel scatterplot: keyword-level signal × volatility
# x = mean hits across full study period
# y = SD across full study period (volatility)
# size = max observed hits
# colour = group
# label every keyword (ggRepel keeps them separate)
# Compact, single panel, half-page
# =============================================================================

scatter_kw <- all_long_g3 |>
  group_by(keyword, group) |>
  summarise(
    mean_hits = mean(hits,   na.rm = TRUE),
    sd_hits   = sd(hits,     na.rm = TRUE),
    max_hits  = max(hits,    na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    kw_label  = str_to_title(keyword),
    Group     = group
  )

# Overall regression line per group
fig2 <- ggplot(scatter_kw,
               aes(x = mean_hits, y = sd_hits, color = Group)) +
  # per-group regression bands
  geom_smooth(aes(group = Group),
              method = "lm", se = TRUE,
              linewidth = 0.6, alpha = 0.12,
              fill = NA, linetype = "solid") +
  # reference lines at medians
  geom_hline(yintercept = median(scatter_kw$sd_hits),
             color = "#cccccc", linewidth = 0.3, linetype = "dotted") +
  geom_vline(xintercept = median(scatter_kw$mean_hits),
             color = "#cccccc", linewidth = 0.3, linetype = "dotted") +
  # bubbles
  geom_point(aes(size = max_hits), alpha = 0.80, stroke = 0.25) +
  # ggRepel labels — ALL 10 keywords labelled, no overlap
  geom_text_repel(
    aes(label = kw_label),
    size              = 2.5,
    family            = "serif",
    fontface          = "italic",
    box.padding       = 0.45,
    point.padding     = 0.3,
    segment.size      = 0.28,
    segment.color     = "#bbbbbb",
    segment.curvature = -0.08,
    max.overlaps      = Inf,
    min.segment.length = 0.1,
    show.legend       = FALSE,
    seed              = 42
  ) +
  # quadrant labels
  annotate("text", x = Inf, y = Inf,
           label = "High signal\nHigh volatility",
           hjust = 1.1, vjust = 1.4, size = 2.3,
           color = "#999999", fontface = "italic", family = "serif") +
  annotate("text", x = -Inf, y = Inf,
           label = "Low signal\nHigh volatility",
           hjust = -0.1, vjust = 1.4, size = 2.3,
           color = "#999999", fontface = "italic", family = "serif") +
  annotate("text", x = Inf, y = -Inf,
           label = "High signal\nLow volatility",
           hjust = 1.1, vjust = -0.5, size = 2.3,
           color = "#999999", fontface = "italic", family = "serif") +
  annotate("text", x = -Inf, y = -Inf,
           label = "Low signal\nLow volatility",
           hjust = -0.1, vjust = -0.5, size = 2.3,
           color = "#999999", fontface = "italic", family = "serif") +
  scale_color_manual(
    values = c("Mainstream AI" = C_MAIN, "NCII Tools" = C_NCII),
    name   = "Group"
  ) +
  scale_size_continuous(
    name   = "Peak Index",
    range  = c(3, 11),
    breaks = c(20, 50, 80)
  ) +
  scale_x_continuous(
    name   = "Mean Monthly Search Index (0–100), 2021–2026",
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_y_continuous(
    name   = "Volatility — SD of Monthly Search Index",
    expand = expansion(mult = c(0.1, 0.12))
  ) +
  labs(
    title = paste0(
      "Figure 2. Keyword Signal Strength vs. Volatility: ",
      "Mainstream AI vs. NCII Tools (Full Study Period)"
    ),
    subtitle = paste0(
      "Each point = one keyword. x = mean monthly search index; ",
      "y = standard deviation (volatility); size = peak index.\n",
      "Dotted lines mark cross-group medians. ",
      "Labels placed automatically with ggrepel (non-overlapping)."
    ),
    caption = paste0(
      "Note. NCII Tools (sienna) cluster in the upper-right quadrant — ",
      "high mean AND high volatility — ",
      "driven by episodic spikes rather than stable interest.\n",
      "Mainstream AI (teal) spans a wider range of stability, ",
      "with established platforms (Midjourney, Stable Diffusion) ",
      "showing higher mean but lower relative volatility.\n",
      "Source: Google Trends (worldwide, monthly, Jan 2021–Mar 2026). ",
      "N = 62 monthly observations per keyword."
    )
  ) +
  theme_compact() +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal"
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 4)),
    size  = guide_legend(order = 2)
  )

fig2
