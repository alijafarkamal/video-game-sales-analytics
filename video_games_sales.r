library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

# Load the dataset
data <- read.csv("video-games-sales.csv")

# Rename columns to match the expected column names in the code
colnames(data) <- c("Game", "Year", "Genre", "Publisher", "na_sales", "eu_sales", "jp_sales", "other_sales", "global_sales")

# --- Add Log Normalized Columns ---
data <- data %>%
  mutate(
    log_na_sales = log(na_sales + 1),
    log_eu_sales = log(eu_sales + 1),
    log_jp_sales = log(jp_sales + 1),
    log_other_sales = log(other_sales + 1),
    log_global_sales = log(global_sales + 1) # Added for histograms
  )

# Save the modified dataset with log columns
write.csv(data, "video-games-sales-with-log.csv", row.names = FALSE)

# --- Descriptive Statistics for Sales by Region ---
region_stats <- data %>%
  summarise(
    Mean_NA = mean(na_sales, na.rm = TRUE),
    Median_NA = median(na_sales, na.rm = TRUE),
    SD_NA = sd(na_sales, na.rm = TRUE),
    Mean_JP = mean(jp_sales, na.rm = TRUE),
    Median_JP = median(jp_sales, na.rm = TRUE),
    SD_JP = sd(jp_sales, na.rm = TRUE),
    Mean_EU = mean(eu_sales, na.rm = TRUE),
    Median_EU = median(eu_sales, na.rm = TRUE),
    SD_EU = sd(eu_sales, na.rm = TRUE),
    Mean_Other = mean(other_sales, na.rm = TRUE),
    Median_Other = median(other_sales, na.rm = TRUE),
    SD_Other = sd(other_sales, na.rm = TRUE)
  )
write.csv(region_stats, "descriptive_stats_region.csv", row.names = FALSE)

# --- Descriptive Statistics for Log-Normalized Sales ---
log_region_stats <- data %>%
  summarise(
    Mean_Log_NA = mean(log_na_sales, na.rm = TRUE),
    Median_Log_NA = median(log_na_sales, na.rm = TRUE),
    SD_Log_NA = sd(log_na_sales, na.rm = TRUE),
    Mean_Log_JP = mean(log_jp_sales, na.rm = TRUE),
    Median_Log_JP = median(log_jp_sales, na.rm = TRUE),
    SD_Log_JP = sd(log_jp_sales, na.rm = TRUE),
    Mean_Log_EU = mean(log_eu_sales, na.rm = TRUE),
    Median_Log_EU = median(log_eu_sales, na.rm = TRUE),
    SD_Log_EU = sd(log_eu_sales, na.rm = TRUE),
    Mean_Log_Other = mean(log_other_sales, na.rm = TRUE),
    Median_Log_Other = median(log_other_sales, na.rm = TRUE),
    SD_Log_Other = sd(log_other_sales, na.rm = TRUE)
  )
write.csv(log_region_stats, "log_descriptive_stats_region.csv", row.names = FALSE)

# --- Sales Distribution by Region (Pie Chart) ---
region_sales <- colSums(data[c("na_sales", "eu_sales", "jp_sales", "other_sales")])
region_df <- data.frame(region = names(region_sales), sales = region_sales)
ggplot(region_df, aes(x = "", y = sales, fill = region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Sales Distribution by Region")
ggsave("sales_pie_chart.png")

# --- Corrected Histogram: Distribution of Log-Global Sales by Genre ---
# Assuming 'Genre' is used instead of non-existent 'console'
ggplot(data, aes(x = log_global_sales, fill = Genre)) +
  geom_histogram(binwidth = 0.1, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Log-Global Sales by Genre", x = "Log-Global Sales", y = "Frequency")
ggsave("sales_genre_histogram.png")

# --- Corrected Bar Plot: Publishers with Most Releases ---
publisher_stats <- data %>%
  group_by(Publisher) %>%
  summarise(releases = n(), total_sales = sum(global_sales)) %>% # Use global_sales
  arrange(desc(releases)) %>%
  head(10)
ggplot(publisher_stats, aes(x = reorder(Publisher, -releases), y = releases, fill = total_sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Publishers by Releases", x = "Publisher", y = "Number of Releases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("publisher_releases_bar.png")

# --- Corrected Box Plots ---
# Sales Variations by Region
melted_sales <- data %>%
  select(na_sales, eu_sales, jp_sales, other_sales) %>% # Fixed column names
  pivot_longer(everything(), names_to = "Region", values_to = "Sales")
ggplot(melted_sales, aes(x = Region, y = Sales, fill = Region)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Sales Variations by Region", x = "Region", y = "Sales") +
  theme_minimal()
ggsave("sales_boxplot.png")

# Log-Normalized Sales Variations
log_melted_sales <- data %>%
  select(log_na_sales, log_eu_sales, log_jp_sales, log_other_sales) %>% # Fixed names
  pivot_longer(everything(), names_to = "Region", values_to = "Log_Sales")
ggplot(log_melted_sales, aes(x = Region, y = Log_Sales, fill = Region)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Log-Normalized Sales Variations by Region", x = "Region", y = "Log-Sales") +
  theme_minimal()
ggsave("log_sales_boxplot.png")