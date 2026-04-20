rm(list = ls())
# 
# FINAL REDDIT SCRAPER - Get the REST of the posts
# Uses every possible search angle RedditExtractoR supports
# 

library(tidyverse)
library(RedditExtractoR)
library(lubridate)

cat(" FINAL REDDIT SCRAPER - GETTING THE REST \n\n")

# 
# STRATEGY: Try EVERY possible search combination
# 

# Load existing posts to avoid re-scraping
existing_posts <- read.csv("reddit_grok_all_posts.csv")
existing_urls <- existing_posts$url

cat(sprintf("Already have: %d posts\n", length(existing_urls)))
cat("Now searching for NEW posts using every possible method\n\n")

# 
# METHOD 1: Search by EVERY individual NCII keyword
# 

cat(" METHOD 1: Individual Keyword Searches \n\n")

# Comprehensive keyword list (50+ specific terms)
all_keywords <- c(
  # Primary NCII terms
  "spicy mode", "spicy", "nsfw", "adult content", "nude", "porn",
  "undress", "remove clothes", "bikini", "lingerie", "sexy",
  "deepfake", "face swap", "fake photo", "boobs", "ass", "remove bra", "remove saree",
  "remove top", "remove pant", "remove pants", "girls nude", "wet ass",
  
  # Image generation phrases
  "generate image", "create image", "make image", "ai image",
  "image generator", "photo generator", "picture generator",
  "grok image", "grok photo", "grok picture", "deepfake", "deepfake generator",
  
  # Content moderation
  "uncensored", "unfiltered", "no filter", "bypass filter",
  "content filter", "safety filter", "restriction",
  "blocked content", "censorship", "moderation",
  
  # Policy terms
  "content policy", "terms of service", "tos violation",
  "community guidelines", "against rules", "not allowed",
  "banned content", "inappropriate", "explicit",
  
  # User intent phrases
  "how to generate", "can you make", "able to create",
  "does it work", "is it possible", "can i make",
  
  # Comparison terms (users discussing capabilities)
  "vs chatgpt", "vs midjourney", "vs dalle", "better than",
  "compared to", "alternative to", "vs gemini",
  
  # Feature requests
  "wish it could", "should be able", "would be nice",
  "feature request", "add ability", "remove restriction"
)

method1_posts <- data.frame()

for(keyword in all_keywords) {
  cat(sprintf("Searching: '%s'", keyword))
  
  tryCatch({
    posts <- find_thread_urls(
      keywords = keyword,
      subreddit = "grok",
      sort_by = "new",
      period = "all"
    )
    
    if(nrow(posts) > 0) {
      posts$date <- as.Date(posts$date_utc)
      posts$search_term <- keyword
      
      # Only keep NEW posts
      new_posts <- posts |>
        filter(!url %in% existing_urls, date >= as.Date("2023-01-01"))
      
      cat(sprintf(" %d new posts\n", nrow(new_posts)))
      method1_posts <- bind_rows(method1_posts, new_posts)
    } else {
      cat(" 0 posts\n")
    }
    
    Sys.sleep(2)
    
  }, error = function(e) {
    cat(sprintf(" ERROR\n"))
  })
}

method1_unique <- method1_posts |>
  distinct(url, .keep_all = TRUE)

cat(sprintf("\n✓ Method 1 collected: %d new posts\n\n", nrow(method1_unique)))

# 
# METHOD 2: Search with Boolean-like combinations
# 

cat(" METHOD 2: Compound Search Phrases \n\n")

compound_searches <- c(
  "grok nsfw", "grok adult", "grok nude", "grok porn",
  "grok spicy", "grok uncensored", "grok filter",
  "image generation nsfw", "ai image nude", 
  "deepfake grok", "face swap grok",
  "grok vs chatgpt nsfw", "grok image policy",
  "grok content moderation", "grok safety",
  "generate nude", "create deepfake", "make nsfw",
  "bypass grok filter", "grok unrestricted", "grok bypass", "grok remove", "grok spicy prompt"
)

method2_posts <- data.frame()

for(query in compound_searches) {
  cat(sprintf("Searching: '%s'", query))
  
  tryCatch({
    posts <- find_thread_urls(
      keywords = query,
      subreddit = "grok",
      sort_by = "new",
      period = "all"
    )
    
    if(nrow(posts) > 0) {
      posts$date <- as.Date(posts$date_utc)
      posts$search_term <- query
      
      new_posts <- posts |>
        filter(!url %in% c(existing_urls, method1_posts$url))
      
      cat(sprintf(" %d new posts\n", nrow(new_posts)))
      method2_posts <- bind_rows(method2_posts, new_posts)
    } else {
      cat(" 0 posts\n")
    }
    
    Sys.sleep(2)
    
  }, error = function(e) {
    cat(" ERROR\n")
  })
}

method2_unique <- method2_posts |>
  distinct(url, .keep_all = TRUE)

cat(sprintf("\n✓ Method 2 collected: %d new posts\n\n", nrow(method2_unique)))

# 
# METHOD 3: Different sort methods for each keyword
# 

cat(" METHOD 3: Different Sort Orders \n\n")

priority_keywords_sort <- c("spicy", "nsfw", "nude", "porn", "uncensored")
sort_methods <- c("hot", "top", "rising", "controversial")

method3_posts <- data.frame(url = character(), search_term = character(), sort_method = character())

for(keyword in priority_keywords_sort) {
  for(sort in sort_methods) {
    cat(sprintf("Searching '%s' sorted by '%s'...", keyword, sort))
    
    # Increase sleep time to be safer
    Sys.sleep(5) 
    
    tryCatch({
      posts <- find_thread_urls(
        keywords = keyword,
        subreddit = "grok",
        sort_by = sort,
        period = "all"
      )
      
      if(!is.null(posts) && nrow(posts) > 0) {
        posts$date <- as.Date(posts$date_utc)
        posts$search_term <- keyword
        posts$sort_method <- sort
        
        # Check against existing data safely
        new_posts <- posts |>
          filter(!url %in% c(existing_urls, method1_posts$url, method2_posts$url))
        
        cat(sprintf(" %d new\n", nrow(new_posts)))
        method3_posts <- bind_rows(method3_posts, new_posts)
      } else {
        cat(" 0\n")
      }
      
    }, error = function(e) {
      cat(" ERROR (Likely Rate Limit)\n")
      Sys.sleep(30) # Take a long break if we hit an error
    })
  }
}
# 2. Only run distinct if there is actually data
if(nrow(method3_posts) > 0) {
  method3_unique <- method3_posts |> distinct(url, .keep_all = TRUE)
} else {
  method3_unique <- method3_posts
  cat("\n⚠️ No data was collected due to rate limits.")
}

method3_unique <- method3_posts |>
  distinct(url, .keep_all = TRUE)

cat(sprintf("\n✓ Method 3 collected: %d new posts\n\n", nrow(method3_unique)))

# 
# METHOD 4: Time period variations
# 

cat(" METHOD 4: Different Time Periods \n\n")

periods <- c("day", "week", "month", "year", "all")

method4_posts <- data.frame()

for(period in periods) {
  cat(sprintf("Scraping period='%s'", period))
  
  tryCatch({
    posts <- find_thread_urls(
      subreddit = "grok",
      sort_by = "new",
      period = period
    )
    
    if(nrow(posts) > 0) {
      posts$date <- as.Date(posts$date_utc)
      posts$period_scraped <- period
      
      new_posts <- posts |>
        filter(!url %in% c(existing_urls, method1_posts$url, 
                           method2_posts$url, method3_posts$url),
               date >= as.Date("2023-01-01"))
      
      cat(sprintf(" %d new\n", nrow(new_posts)))
      method4_posts <- bind_rows(method4_posts, new_posts)
    } else {
      cat(" 0\n")
    }
    
    Sys.sleep(3)
    
  }, error = function(e) {
    cat(" ERROR\n")
  })
}

method4_unique <- method4_posts |>
  distinct(url, .keep_all = TRUE)

cat(sprintf("\n✓ Method 4 collected: %d new posts\n\n", nrow(method4_unique)))

# 
# METHOD 5: Subreddit scraping without keywords (broad net)
# 

install.packages("beepr")
library(beepr)

cat(" METHOD 5: Broad Subreddit Scraping \n\n")

cat("Scraping r/Grok without keyword filters\n")

method5_posts <- data.frame()

for(sort in c("new", "hot", "top")) {
  cat(sprintf("  Processing Sort: '%s'...", sort))
  
  tryCatch({
    posts <- find_thread_urls(
      subreddit = "grok",
      sort_by = sort,
      period = "all"
    )
    
    if(!is.null(posts) && nrow(posts) > 0) {
      posts$date <- as.Date(posts$date_utc)
      
      # Filtering against all previous methods
      new_posts <- posts |>
        filter(!url %in% c(existing_urls, 
                           method1_posts$url, method2_posts$url,
                           method3_posts$url, method4_posts$url),
               date >= as.Date("2023-01-01"))
      
      cat(sprintf(" %d new posts found.\n", nrow(new_posts)))
      method5_posts <- bind_rows(method5_posts, new_posts)
    } else {
      cat(" 0 posts found.\n")
    }
    
    # Respectful pause for the API
    Sys.sleep(5)
    
  }, error = function(e) {
    cat(" ERROR occurred (skipping to next sort method)\n")
  })
}

# --- THE FINAL SOUND ---
cat("\n✅ Data collection for Method 5 is complete!\n")
beep(1) # Standard "ding"

method5_unique <- method5_posts |>
  distinct(url, .keep_all = TRUE)

cat(sprintf("\n✓ Method 5 collected: %d new posts\n\n", nrow(method5_unique)))

# 
# COMBINE ALL NEW POSTS
# 

cat(" COMBINING ALL NEW POSTS \n\n")

all_new_posts <- bind_rows(
  method1_unique |> mutate(source = "keyword_search"),
  method2_unique |> mutate(source = "compound_search"),
  method4_unique |> mutate(source = "period_variation"),
  method5_unique |> mutate(source = "broad_scrape")
) |>
  distinct(url, .keep_all = TRUE)

cat(sprintf("✓ Total NEW posts collected: %d\n\n", nrow(all_new_posts)))

# 
# MERGE WITH EXISTING DATA
# 

cat("Merging with existing dataset\n")

# Combine old + new
existing_posts_fixed <- existing_posts |>
  mutate(
    date = as.Date(date), # Convert character to Date
    source = "original_scrape"
  )

# 2. Combine and clean
mega_dataset <- bind_rows(
  existing_posts_fixed,
  all_new_posts 
) |>
  distinct(url, .keep_all = TRUE) |>
  arrange(date)

cat(sprintf("✓ TOTAL dataset now: %d posts (was %d)\n", 
            nrow(mega_dataset), nrow(existing_posts)))
cat(sprintf("  → Added: %d new posts (%.1f%% increase)\n\n",
            nrow(all_new_posts),
            (nrow(all_new_posts) / nrow(existing_posts)) * 100))

# 
# FILTER FOR NCII RELEVANCE
# 

cat("Filtering for NCII relevance\n")

ncii_keywords_full <- c(
  "spicy", "nsfw", "adult", "nude", "porn", "sexy", "naked",
  "undress", "remove clothes", "bikini", "lingerie", "revealing",
  "deepfake", "face swap", "photoshop", "manipulate", "fake",
  "generate", "create", "make", "produce", "image", "picture",
  "uncensored", "unfiltered", "no filter", "bypass", "circumvent",
  "restriction", "blocked", "censorship", "moderation",
  "policy", "terms", "tos", "guidelines", "violation",
  "celebrity", "person", "without permission", "revenge"
)

ncii_posts_mega <- mega_dataset |>
  filter(
    str_detect(tolower(title), paste(ncii_keywords_full, collapse = "|")) |
      str_detect(tolower(text), paste(ncii_keywords_full, collapse = "|"))
  )

cat(sprintf("✓ NCII-relevant posts: %d (%.1f%% of total)\n\n",
            nrow(ncii_posts_mega),
            (nrow(ncii_posts_mega) / nrow(mega_dataset)) * 100))

# 
# EXPORT EVERYTHING
# 

cat("Exporting datasets\n")

write.csv(mega_dataset, "reddit_grok_MEGA_FINAL_all_posts.csv", row.names = FALSE)
write.csv(all_new_posts, "reddit_grok_NEW_posts_only.csv", row.names = FALSE)
write.csv(ncii_posts_mega, "reddit_grok_MEGA_FINAL_ncii.csv", row.names = FALSE)

# Method breakdown
method_breakdown <- all_new_posts |>
  group_by(source) |>
  summarise(count = n()) |>
  arrange(desc(count))

write.csv(method_breakdown, "reddit_collection_method_breakdown.csv", row.names = FALSE)

# 
# FINAL SUMMARY
# 


cat("FINAL COLLECTION SUMMARY:\n")
cat(sprintf("Original posts: %d\n", nrow(existing_posts)))
cat(sprintf("NEW posts collected: %d\n", nrow(all_new_posts)))
cat(sprintf("TOTAL posts now: %d (%.1f%% increase)\n\n",
            nrow(mega_dataset),
            (nrow(all_new_posts) / nrow(existing_posts)) * 100))

cat("BREAKDOWN BY COLLECTION METHOD:\n")
print(method_breakdown)

cat("\nNCII-RELEVANT POSTS:\n")
cat(sprintf("Total NCII posts: %d (%.1f%%)\n",
            nrow(ncii_posts_mega),
            (nrow(ncii_posts_mega) / nrow(mega_dataset)) * 100))


cat(sprintf("\nI now have %d total posts (%d NCII-relevant)!\n", 
            nrow(mega_dataset), nrow(ncii_posts_mega)))


# 
# ANALYSIS OF COLLECTED POSTS AND COMMENTS 

# 5,622 TOTAL POSTS 
# 3,870 NCII-relevant POSTS 

library(tidyverse)
library(lubridate)
library(remotes)
remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(ggrepel)
library(scales)
library(viridis)
library(patchwork)
library(flextable)
library(officer)
library(RColorBrewer)
library(ggbump)

# setting theme for all plots 
theme_set(theme_minimal(base_size = 12))

# SECTION 1: DATA LOADING & PREPARATION

# Load mega dataset
reddit_data <- read.csv("reddit_grok_MEGA_FINAL_all_posts.csv") %>%
  mutate(
    date = as.Date(date),
    week = floor_date(date, "week"),
    month = floor_date(date, "month"),
    year = year(date)
  )

# Loading NCII dataset 
ncii_data <- read.csv("reddit_grok_MEGA_FINAL_ncii.csv") %>%
  mutate(
    date = as.Date(date),
    week = floor_date(date, "week"),
    month = floor_date(date, "month"),
    year = year(date)
  )

#check 
cat(sprintf("Loaded %d total posts\n", nrow(reddit_data)))
cat(sprintf("Loaded %d NCII-relevant posts\n\n", nrow(ncii_data)))

# FOR  METHODOLOGY SECTION

analysis_objectives <- "
RESEARCH QUESTIONS ADDRESSED:

1. TEMPORAL PATTERNS: How did NCII-related discussions evolve over time?
   - Weekly/monthly trends in post volume
   - Pre vs Post Grok product launches
   - Impact of Spicy Mode (July 28, 2025)

2. CONTENT THEMES: What topics dominate NCII discussions?
   - Keyword frequency analysis
   - Topic clustering
   - User intent markers

3. USER ENGAGEMENT: Which posts received most attention?
   - Upvotes, comments, engagement rates
   - High-engagement vs low-engagement patterns

4. ADVERSARIAL INTENT: What proportion demonstrate targeting behavior?
   - Coercive language markers
   - Distribution intent
   - Manipulation indicators

5. COMPARATIVE ANALYSIS: How does NCII activity compare to general activity?
   - NCII posts as % of total over time
   - Concentration during key events

METHODOLOGICAL APPROACH:
- Descriptive statistics (frequencies, distributions)
- Temporal analysis (time series, trend decomposition)
- Content analysis (keyword extraction, semantic clustering)
- Comparative visualization (NCII vs general discussions)
- Statistical testing (pre/post intervention comparisons)
"

cat(analysis_objectives)
write.table(analysis_objectives, "reddit_analysis_methodology.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)


# SECTION 2: ENHANCED ADVERSARIAL INTENT SCORING

# Defining comprehensive adversarial markers

adversarial_markers <- list(
  
  # Category 1: Named Targeting (A1)
  targeting = c(
    "celebrity", "actress", "someone", "person", "girl", "woman", 
    "face", "specific person", "my ex", "her photo", "his photo",
    "classmate", "coworker", "neighbor", "friend", "his video", "her video",
    "this bitch", "bitch"
  ),
  
  # Category 2: Coercive/Harmful Intent (A2)
  coercive = c(
    "without permission", "without consent", "unwilling", "revenge",
    "expose", "embarrass", "humiliate", "ruin", "destroy", "leak",
    "blackmail", "threaten", "harass", "shame"
  ),
  
  # Category 3: Distribution Intent (A3)
  distribution = c(
    "share", "post online", "spread", "distribute", "send to",
    "upload", "publish", "show everyone", "make viral", "circulate"
  ),
  
  # Category 4: Manipulation Indicators (A4)
  manipulation = c(
    "fake", "manipulate", "alter", "photoshop", "edit", "change",
    "modify", "deepfake", "face swap", "body swap", "undress"
  )
)

# Score each post

ncii_scored <- ncii_data %>%
  rowwise() %>%
  mutate(
    text_combined = tolower(paste(title, text, sep = " ")),
    
    A1_targeting = sum(str_detect(text_combined, adversarial_markers$targeting)),
    A2_coercive = sum(str_detect(text_combined, adversarial_markers$coercive)),
    A3_distribution = sum(str_detect(text_combined, adversarial_markers$distribution)),
    A4_manipulation = sum(str_detect(text_combined, adversarial_markers$manipulation)),
    
    adversarial_total = A1_targeting + A2_coercive + A3_distribution + A4_manipulation,
    
    adversarial_level = case_when(
      adversarial_total >= 3 ~ "High (≥3)",
      adversarial_total == 2 ~ "Medium (2)",
      adversarial_total == 1 ~ "Low (1)",
      TRUE ~ "None (0)"
    ),
    
    high_adversarial = adversarial_total >= 3
  ) %>%
  ungroup() %>%
  select(-text_combined)

cat("Adversarial Intent Distribution:\n")
intent_summary <- ncii_scored %>%
  group_by(adversarial_level) %>%
  summarise(n = n(), pct = round(n() / nrow(ncii_scored) * 100, 1))
print(intent_summary)


view(ncii_scored)

# SECTION 3: TOPIC CLASSIFICATION

cat("\n TOPIC CLASSIFICATION \n")

ncii_scored <- ncii_scored %>%
  mutate(
    topic = case_when(
      str_detect(tolower(title), "spicy|nsfw|adult|explicit") ~ "Explicit Content",
      str_detect(tolower(title), "nude|undress|remove|bikini|lingerie|boobs|ass") ~ "Undressing/Nudity",
      str_detect(tolower(title), "deepfake|face swap|fake|manipulate") ~ "Deepfakes",
      str_detect(tolower(title), "image|generate|create|make|photo") ~ "Image Generation",
      str_detect(tolower(title), "filter|censor|restrict|block|policy") ~ "Content Moderation",
      TRUE ~ "General NCII Discussion"
    )
  )

topic_counts <- ncii_scored %>%
  group_by(topic) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

cat("Topic Distribution:\n")
print(topic_counts)

colnames(ncii_scored)

# SECTION 4: TEMPORAL AGGREGATIONS

# Weekly aggregation
weekly_agg_rgrok <- ncii_scored %>%
  group_by(week) %>%
  summarise(
    n_posts = n(),
    n_high_adversarial = sum(high_adversarial),
    pct_high_adversarial = mean(high_adversarial) * 100,
    # avg_score = mean(score, na.rm = TRUE), # REMOVED because it's missing from your data
    avg_comments = mean(comments, na.rm = TRUE), # This will work!
    median_adversarial_score = median(adversarial_total),
    .groups = "drop"
  )

# Monthly aggregation
monthly_agg_rgrok <- ncii_scored %>%
  group_by(month) %>%
  summarise(
    n_posts = n(),
    n_high_adversarial = sum(high_adversarial),
    pct_high_adversarial = mean(high_adversarial) * 100,
    avg_engagement = mean(comments, na.rm = TRUE),
    .groups = "drop"
  )

grok_events <- data.frame(
  date = as.Date(c("2023-11-03", "2024-07-28", "2025-07-28", "2026-01-03")),
  event = c("Grok-1", "Grok Imagine", "Spicy Mode", "Policy Change"),
  type = c("launch", "feature", "feature", "policy")
)

cat(sprintf("✓ %d weeks of data\n", nrow(weekly_agg_rgrok)))
cat(sprintf("✓ %d months of data\n\n", nrow(monthly_agg_rgrok)))

# PLOT 1: STREAMGRAPH

stream_data <- ncii_scored %>%
  group_by(month, topic) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(month, topic, fill = list(n = 0))

p1_stream <- ggplot(stream_data, aes(x = month, y = n, fill = topic)) +
  geom_stream(type = "mirror", bw = 0.7) +
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
  geom_vline(data = grok_events, aes(xintercept = date),
             linetype = "dashed", color = "black", alpha = 0.7, size = 0.8) +
  labs(
    title = "Evolution of NCII Discussion Topics on r/Grok",
    subtitle = "Monthly post volume by content theme (2023-2026) | Dashed lines = Grok model iterations",
    x = NULL,
    y = "Number of Posts",
    fill = "Discussion Topic",
    caption = sprintf("Data: Reddit r/Grok (N=%d NCII-relevant posts)", nrow(ncii_scored))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p1_stream)

# PLOT 2: DUMBBELL PLOT 
# (Pre vs Post Spicy Mode)

spicy_date <- as.Date("2025-07-28")

dumbbell_data <- ncii_scored %>%
  mutate(period = if_else(date < spicy_date, "Pre-Spicy", "Post-Spicy")) %>%
  group_by(topic, period) %>%
  summarise(
    avg_posts_per_week = n() / n_distinct(week),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_posts_per_week,
    values_fill = 0
  ) %>%
  mutate(
    change = `Post-Spicy` - `Pre-Spicy`,
    pct_change = if_else(`Pre-Spicy` > 0, 
                         (change / `Pre-Spicy`) * 100, 
                         999)
  ) %>%
  arrange(desc(change))

p2_dumbbell <- ggplot(dumbbell_data) +
  geom_segment(
    aes(x = `Pre-Spicy`, xend = `Post-Spicy`,
        y = reorder(topic, change), yend = reorder(topic, change)),
    color = "gray70", size = 1.5
  ) +
  geom_point(aes(x = `Pre-Spicy`, y = reorder(topic, change)),
             color = "#2c7bb6", size = 5) +
  geom_point(aes(x = `Post-Spicy`, y = reorder(topic, change)),
             color = "#d7191c", size = 5) +
  geom_text(
    aes(x = `Post-Spicy` + 0.5, y = reorder(topic, change),
        label = if_else(pct_change < 900, sprintf("%+.0f%%", pct_change), "NEW")),
    hjust = 0, size = 3.5, fontface = "bold"
  ) +
  labs(
    title = "Impact of Spicy Mode on NCII Discussion Topics",
    subtitle = "Change in average weekly posts per topic (July 28, 2025 intervention)",
    x = "Average Weekly Posts",
    y = NULL,
    caption = "Blue = Pre-Spicy Mode | Red = Post-Spicy Mode | % = Percent change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.y = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
print(p2_dumbbell)

# PLOT 3: LOLLIPOP CHART

all_keywords_taken <- c(
  "spicy", "nsfw", "nude", "porn", "undress", "bikini", "deepfake", "filter", "uncensored", "adult", "sexy",
  "remove", "fake", "manipulate", "policy", "restriction",
  "boobs", "ass", "lingerie", "face swap", "bypass", "jailbreak"
)

keyword_freq <- data.frame(
  keyword = all_keywords_taken,
  count = sapply(all_keywords_taken, function(kw) {
    sum(str_detect(tolower(ncii_scored$title), kw) |
          str_detect(tolower(ncii_scored$text), kw), na.rm = TRUE)
  })
) %>%
  filter(count > 0) %>%
  arrange(desc(count)) %>%
  head(20) %>%
  mutate(
    is_explicit = keyword %in% c("porn", "nude", "boobs", "ass", "sexy", "undress", "nsfw", "bikini", "lingerie"),
    category = if_else(is_explicit, "Explicit Terms", "Technical/Policy Terms")
  )

p3_lollipop <- ggplot(keyword_freq, aes(x = count, y = reorder(keyword, count))) +
  geom_segment(
    aes(x = 0, xend = count, y = reorder(keyword, count), yend = reorder(keyword, count),
        color = category),
    size = 2
  ) +
  geom_point(aes(color = category), size = 6) +
  geom_text(aes(label = comma(count)), hjust = -0.3, size = 3.5, fontface = "bold") +
  scale_color_manual(
    values = c("Explicit Terms" = "#d7191c", "Technical/Policy Terms" = "steelblue")
  ) +
  labs(
    title = "Most Frequent NCII-Related Keywords in r/Grok",
    subtitle = "Top 20 terms appearing in post titles and content (2023-2026)",
    x = "Frequency (Number of Posts)",
    y = NULL,
    color = "Keyword Type",
    caption = sprintf("N=%d NCII-relevant posts analyzed", nrow(ncii_scored))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = c(0.8, 0.2),
    legend.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  expand_limits(x = max(keyword_freq$count) * 1.15)

print(p3_lollipop)

# PLOT 4: STACKED AREA CHART

weekly_comparison <- reddit_data %>%
  mutate(post_type = if_else(url %in% ncii_scored$url, "NCII-Relevant", "General Discussion")) %>%
  group_by(week, post_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(week, post_type, fill = list(n = 0))

p4_area <- ggplot(weekly_comparison, aes(x = week, y = n)) + 
  geom_area(aes(fill = post_type), alpha = 0.8) + 
  geom_vline(data = grok_events, aes(xintercept = date),
             linetype = "dashed", color = "gray30", alpha = 0.7) +
  geom_text(
    data = grok_events,
    aes(x = date, y = Inf, label = event),
    angle = 90, vjust = -0.5, hjust = 1.1,
    size = 3, color = "gray20", fontface = "bold",
    inherit.aes = FALSE # This tells geom_text to ignore the global 'y = n'
  ) +
  scale_fill_manual(
    values = c("NCII-Relevant" = "#d7191c", "General Discussion" = "#2c7bb6")
  ) +
  labs(
    title = "Reddit r/Grok Activity: NCII vs General Discussions",
    subtitle = "Weekly post volume showing proportion of NCII-related content over time",
    x = "Week",
    y = "Number of Posts",
    fill = "Post Type",
    caption = sprintf("Total N=%d posts | Dashed lines = Grok product milestones", nrow(reddit_data))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
print(p4_area)

# PLOT 5: BUMP CHART
# Topic Ranking 

quarterly_rankings <- ncii_scored %>%
  mutate(quarter = floor_date(date, "quarter")) %>%
  group_by(quarter, topic) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(quarter) %>%
  mutate(rank = rank(-n, ties.method = "first")) %>%
  filter(rank <= 6) %>%
  ungroup()

# Define a sophisticated color palette
ncii_palette <- c(
  "Deepfakes" = "#264653",         # Deep Teal
  "Explicit Content" = "#E76F51",  # Soft Burnt Orange
  "Image Generation" = "#2A9D8F",  # Persian Green
  "Undressing/Nudity" = "#E9C46A", # Sandy Yellow
  "Content Moderation" = "#F4A261",# Muted Orange
  "General NCII Discussion" = "#8E9AAF" # Blue-Gray
)

p5_bump <- ggplot(quarterly_rankings, aes(x = quarter, y = rank, color = topic)) +
  # Use a slight shadow effect by adding a thinner white line under the colored one
  geom_bump(size = 2.5, smooth = 8) +
  geom_point(size = 5) +
  # Add white dots in the middle of points for a "ring" effect (very stylish)
  geom_point(color = "white", size = 2) + 
  geom_text_repel( # Using ggrepel so labels don't overlap
    data = quarterly_rankings %>% filter(quarter == max(quarter)),
    aes(label = topic),
    hjust = -0.2, size = 3.5, fontface = "bold",
    direction = "y", segment.color = NA
  ) +
  scale_y_reverse(breaks = 1:6) +
  # Apply the custom colors
  scale_color_manual(values = ncii_palette) +
  labs(
    title = "Evolution of NCII Topics",
    subtitle = "Quarterly rank by volume (Rank 1 = Highest Volume)",
    x = NULL, # cleaner look
    y = "Rank",
    caption = "Source: Reddit r/Grok Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, family = "sans"),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 20)),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "gray30")
  ) +
  expand_limits(x = max(quarterly_rankings$quarter) + months(6))

print(p5_bump)

# PLOT 6: SCATTER - Comments vs Adversarial Score

scatter_data <- ncii_scored %>%
  filter(
    comments > 0, 
    comments < 600,                    # Hard cap at 600 to fix the scale
    !str_detect(title, "AskGrok"),     # Removes the meta-thread seen in your screenshot
    adversarial_total <= 10
  )


p6_scatter_final <- ggplot(scatter_data, aes(x = adversarial_total, y = comments)) +
  # Jitter spreads the points horizontally so they don't overlap in a vertical line
  geom_jitter(aes(color = adversarial_level), alpha = 0.6, size = 2.5, width = 0.25) +
  
  # Regression line remains to show the trend direction
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  
  # Labels for high-engagement posts
  geom_text_repel(
    data = scatter_data %>% filter(comments > quantile(comments, 0.96)),
    aes(label = str_wrap(substr(title, 1, 35), 20)),
    size = 3, max.overlaps = 15, box.padding = 0.5, fontface = "italic"
  ) +
  scale_color_manual(
    values = c(
      "None (0)" = "#2c7bb6",    # Deep Blue
      "Low (1)" = "#abd9e9",     # Light Blue
      "Medium (2)" = "#fdae61",  # Orange/Peach
      "High (≥3)" = "#d7191c"    # Deep Red
    )
  ) +
  
  labs(
    title = "User Engagement vs. Adversarial Intent",
    subtitle = "Engagement volume filtered to show core discussion trends",
    x = "Adversarial Intent Score (0-10)",
    y = "Number of Comments",
    color = "Intent Level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "right", # Legend is back on the side for better visibility
    panel.grid.minor = element_blank()
  )

print(p6_scatter_final)


# TABLES 

# TABLE 1: Descriptive Statistics

# 1. Prepare data in a structured, 4-column format
desc_stats_wide <- data.frame(
  Category = c(
    "Dataset Overview", "Dataset Overview", "Dataset Overview",
    "NCII Identification", "NCII Identification",
    "Adversarial Intent", "Adversarial Intent",
    "Engagement & Content", "Engagement & Content"
  ),
  Metric = c(
    "Total Posts Collected", "Date Range", "Weeks Covered",
    "NCII-Relevant Posts", "NCII Relevance Rate",
    "High Adversarial Posts", "High Intent Rate",
    "Most Common Topic", "Avg. Comments per Post"
  ),
  Count = c(
    comma(nrow(reddit_data)), 
    sprintf("%s to %s", min(reddit_data$date), max(reddit_data$date)),
    as.character(n_distinct(reddit_data$week)),
    comma(nrow(ncii_scored)),
    "-", # Percentage goes in next column
    comma(sum(ncii_scored$high_adversarial)),
    "-",
    names(sort(table(ncii_scored$topic), decreasing = TRUE))[1],
    sprintf("%.1f", mean(ncii_scored$comments, na.rm = TRUE))
  ),
  Percentage = c(
    "100%", "-", "-",
    comma(nrow(ncii_scored)),
    sprintf("%.1f%%", (nrow(ncii_scored) / nrow(reddit_data)) * 100),
    comma(sum(ncii_scored$high_adversarial)),
    sprintf("%.1f%%", mean(ncii_scored$high_adversarial) * 100),
    "-", "-"
  )
)

# 2. Create the flextable with more columns
ft1 <- flextable(desc_stats_wide)
ft1 <- merge_v(ft1, j = 1) # Merge the "Category" cells for a cleaner look
ft1 <- theme_vanilla(ft1)

# 3. Professional Formatting
ft1 <- font(ft1, fontname = "Times New Roman", part = "all")
ft1 <- fontsize(ft1, size = 10, part = "all")
ft1 <- bold(ft1, part = "header")
ft1 <- width(ft1, j = 1:4, width = c(1.5, 2, 1.2, 1.2)) # Adjust column widths

# 4. Alignment
ft1 <- align(ft1, align = "left", j = 1:2, part = "all")
ft1 <- align(ft1, align = "center", j = 3:4, part = "all")

# 5. Add Caption and Footer
ft1 <- set_caption(ft1, caption = "Table 1. Detailed Descriptive Statistics of r/Grok NCII Activity")
ft1 <- add_footer_lines(ft1, values = footnote1)
ft1 <- fontsize(ft1, size = 9, part = "footer")
ft1 <- italic(ft1, part = "footer")

# 6. Export to Word
doc1 <- read_docx()
doc1 <- body_add_flextable(doc1, ft1)
print(doc1, target = "table1_Descriptive_Stats.docx")

# TABLE 2: Pre/Post Comparison

pre_post <- ncii_scored %>%
  mutate(period = if_else(date < spicy_date, "Pre-Spicy Mode", "Post-Spicy Mode")) %>%
  group_by(period) %>%
  summarise(
    N_Posts = n(),
    Pct_High_Adversarial = round(mean(high_adversarial) * 100, 1),
    Avg_Comments = round(mean(comments, na.rm = TRUE), 1),
    .groups = "drop"
  )

ft2 <- flextable(pre_post)
ft2 <- theme_booktabs(ft2)
ft2 <- font(ft2, fontname = "Times New Roman", part = "all")
ft2 <- fontsize(ft2, size = 11, part = "all")
ft2 <- align(ft2, align = "left", j = 1, part = "all")
ft2 <- align(ft2, align = "center", j = 2:4, part = "all")
ft2 <- bold(ft2, part = "header")
ft2 <- set_header_labels(ft2,
                         period = "Period",
                         N_Posts = "N Posts",
                         Pct_High_Adversarial = "% High Intent",
                         Avg_Comments = "Avg Comments"
)
ft2 <- set_caption(ft2,
                   caption = "Table 2. Pre vs Post Spicy Mode Comparison (Intervention: July 28, 2025)"
)

footnote2 <- paste0(
  "Note: Spicy Mode launched on July 28, 2025, enabling unrestricted NSFW image generation. ",
  "High intent = adversarial score ≥3. ",
  "Statistical significance testing via independent samples t-test."
)

ft2 <- add_footer_lines(ft2, values = footnote2)
ft2 <- font(ft2, fontname = "Times New Roman", part = "footer")
ft2 <- fontsize(ft2, size = 9, part = "footer")
ft2 <- italic(ft2, part = "footer")

doc2 <- read_docx()
doc2 <- body_add_flextable(doc2, ft2)
print(doc2, target = "table2_prepost.docx")

# TABLE 3: Topic Distribution

topic_table <- ncii_scored %>%
  group_by(topic) %>%
  summarise(
    N_Posts = n(),
    Pct_Total = round(n() / nrow(ncii_scored) * 100, 1),
    Pct_High_Adversarial = round(mean(high_adversarial) * 100, 1),
    Avg_Comments = round(mean(comments, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(N_Posts))

ft3 <- flextable(topic_table)
ft3 <- theme_booktabs(ft3)
ft3 <- font(ft3, fontname = "Times New Roman", part = "all")
ft3 <- fontsize(ft3, size = 11, part = "all")
ft3 <- align(ft3, align = "left", j = 1, part = "all")
ft3 <- align(ft3, align = "center", j = 2:5, part = "all")
ft3 <- bold(ft3, part = "header")
ft3 <- set_header_labels(ft3,
                         topic = "Discussion Topic",
                         N_Posts = "N Posts",
                         Pct_Total = "% of Total",
                         Pct_High_Adversarial = "% High Intent",
                         Avg_Comments = "Avg Comments"
)
ft3 <- set_caption(ft3,
                   caption = "Table 3. NCII Discussion Topic Distribution and Engagement Patterns"
)

footnote3 <- paste0(
  "Note: Topics assigned based on keyword presence in post titles. ",
  "Posts may contain multiple themes but are assigned to primary topic. ",
  "High intent = adversarial score ≥3."
)

ft3 <- add_footer_lines(ft3, values = footnote3)
ft3 <- font(ft3, fontname = "Times New Roman", part = "footer")
ft3 <- fontsize(ft3, size = 9, part = "footer")
ft3 <- italic(ft3, part = "footer")

doc3 <- read_docx()
doc3 <- body_add_flextable(doc3, ft3)
print(doc3, target = "table3_topics.docx")

names(read.csv("data/reddit_grok_MEGA_FINAL_all_posts.csv")) 
names(read.csv("data/reddit_grok_MEGA_FINAL_ncii.csv"))





reddit_all  <- read.csv("data/reddit_grok_MEGA_FINAL_all_posts.csv")
reddit_ncii <- read.csv("data/reddit_grok_MEGA_FINAL_ncii.csv")

# What search keywords were used?
table(reddit_all$search_keyword)

# Date range
range(as.Date(reddit_all$date), na.rm=TRUE)

# How many posts per file?
cat(nrow(reddit_all), "all posts\n")
cat(nrow(reddit_ncii), "ncii posts\n")

# What does scrape_period look like?
table(reddit_all$scrape_period)

library(dplyr)
library(lubridate)

reddit_all <- read.csv("data/reddit_grok_MEGA_FINAL_all_posts.csv") |>
  mutate(date = as.Date(date),
         month = floor_date(date, "month"),
         keyword_group = case_when(
           search_keyword %in% c("grok deepfake")                        ~ "Deepfake",
           search_keyword %in% c("grok nude","grok porn","grok undress",
                                 "grok remove clothes")                  ~ "Explicit/NCII",
           search_keyword %in% c("grok nsfw","grok spicy","spicy mode")  ~ "NSFW/Spicy",
           search_keyword %in% c("grok bikini","grok sexy")              ~ "Sexualised",
           TRUE ~ "Other"
         ))


# Monthly volume by group
reddit_all |>
  group_by(month, keyword_group) |>
  tally() |>
  arrange(month) |>
  print(n=60)

# Total by group
table(reddit_all$keyword_group)

# How many posts have non-empty text?
cat("Posts with text:", sum(nchar(trimws(reddit_all$text)) > 0, na.rm=TRUE), "\n")
cat("Posts with comments > 0:", sum(reddit_all$comments > 0, na.rm=TRUE), "\n")

list.files("data/", recursive=TRUE)

scored <- read.csv("data/reddit_grok_NCII_scored.csv")
high_adv <- read.csv("data/reddit_grok_HIGH_ADVERSARIAL.csv")
weekly_ts <- read.csv("data/reddit_grok_weekly_timeseries.csv")

cat("Scored columns:", paste(names(scored), collapse=", "), "\n")
cat("Scored rows:", nrow(scored), "\n\n")

cat("High adversarial columns:", paste(names(high_adv), collapse=", "), "\n")
cat("High adversarial rows:", nrow(high_adv), "\n\n")

cat("Weekly timeseries columns:", paste(names(weekly_ts), collapse=", "), "\n")
head(weekly_ts, 5)

# What is the adversarial score range and distribution?
summary(scored$adversarial_score)
table(scored$high_adversarial_intent)

# Monthly adversarial rate over time
scored |>
  mutate(month = floor_date(as.Date(date), "month")) |>
  group_by(month) |>
  summarise(
    n_posts        = n(),
    n_high         = sum(high_adversarial_intent == TRUE | 
                           high_adversarial_intent == 1, na.rm=TRUE),
    pct_high       = round(100 * n_high / n_posts, 1),
    avg_score      = round(mean(adversarial_score, na.rm=TRUE), 3)
  ) |>
  print(n=40)

# Score by keyword group
scored |>
  mutate(keyword_group = case_when(
    search_keyword %in% c("grok deepfake")                        ~ "Deepfake",
    search_keyword %in% c("grok nude","grok porn","grok undress",
                          "grok remove clothes")                  ~ "Explicit/NCII",
    search_keyword %in% c("grok nsfw","grok spicy","spicy mode")  ~ "NSFW/Spicy",
    search_keyword %in% c("grok bikini","grok sexy")              ~ "Sexualised",
    TRUE ~ "Other"
  )) |>
  group_by(keyword_group) |>
  summarise(avg_score = round(mean(adversarial_score, na.rm=TRUE),3),
            pct_high  = round(100*mean(high_adversarial_intent==1|
                                         high_adversarial_intent==TRUE,na.rm=TRUE),1),
            n = n()) |>
  arrange(desc(avg_score))



if (!require("pacman")) install.packages("pacman")
pacman::p_load(flextable, officer, dplyr)

SIG_LINE <- "* p < 0.05, ** p < 0.01, *** p < 0.001"

# ---------------------------------------------------------------------------
# APA table helper (same as final_tables.R)
# ---------------------------------------------------------------------------
apa_ft <- function(ft, col_labels, se_idx = integer(0), gof_row, title, note,
                   w1 = 3.2, wn = 1.6) {
  nc <- ncol_keys(ft)
  ft <- do.call(set_header_labels, c(list(ft), col_labels))
  ft |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 11, part = "all") |>
    bold(part = "header") |>
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
    add_footer_lines(paste0("Note. ", note)) |>
    merge_at(i = 1, j = seq_len(nc), part = "footer") |>
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
    width(j = 1,          width = w1) |>
    width(j = seq(2, nc), width = wn)
}

# =============================================================================
# TABLE — Adversarial Intent by Keyword Group
# =============================================================================

table_df <- data.frame(
  Group = c(
    "Deepfake",
    "Explicit / NCII",
    "NSFW / Spicy Mode",
    "Sexualised",
    "Other (general Grok)"
  ),
  Keywords = c(
    "grok deepfake",
    "grok nude, grok porn, grok undress, grok remove clothes",
    "grok nsfw, grok spicy, spicy mode",
    "grok bikini, grok sexy",
    "All remaining search terms"
  ),
  N = c("41", "319", "468", "97", "333"),
  Avg_Score = c("2.05", "1.25", "0.95", "0.83", "0.64"),
  Pct_High = c("51.2%", "32.0%", "24.1%", "19.6%", "13.2%"),
  stringsAsFactors = FALSE
)

ft5 <- flextable(table_df) |>
  apa_ft(
    col_labels = list(
      Group     = "Keyword group",
      Keywords  = "Search terms",
      N         = "N",
      Avg_Score = "Mean adversarial score",
      Pct_High  = "% high adversarial intent"
    ),
    gof_row = nrow(table_df) + 1,
    title   = "Reddit. Adversarial Intent by Keyword Group in r/Grok (Scored Subsample, N = 1,258)",
    note    = paste0(
      "Posts were scraped from r/Grok using targeted NCII-related search keywords and scored ",
      "for adversarial intent using a validated classifier (F1 = 0.95). ",
      "Adversarial score is a continuous measure (range 0\u201312; higher = more adversarial). ",
      "High adversarial intent is a binary indicator derived from the score distribution. ",
      "Keyword groups are mutually exclusive and exhaustive of the scored subsample. ",
      "Deepfake posts show the highest mean adversarial score (2.05) and the highest ",
      "proportion of high-intent posts (51.2%), nearly four times the rate observed ",
      "in the general Grok post category (13.2%), indicating that deepfake-related ",
      "community engagement is disproportionately concentrated among adversarial users."
    ),
    w1 = 1.8, wn = 1.55
  ) |>
  bold(i = 1, part = "body") |>
  bg(i = 1, bg = "#FDF3F0", part = "body")

cat("Table built.\n")

doc <- read_docx() |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft5) |>
  body_add_par("", style = "Normal") |>
  body_add_break() 

print(doc, target = "Reddit_Table.docx")
cat("Done. Reddit_Tables_5_6.docx saved.\n")

