# Clear all objects from the environment
rm(list = ls())

# Clear all plots
graphics.off()

# Clear memory
gc()

# Clear the console
cat("\014")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# Load the data
file_path <- "C:/Users/jaspr/Desktop/DESKTOP/AUEB_MscDataScience/Data visualization and communication/Teams_Project/Missing_Migrants_Global_Figures_allData.csv"
migrant_data <- read_csv(file_path)

library(janitor)
migrant_data <- migrant_data %>%
  clean_names()

# Filter for your regions
regions_of_interest <- c("Caribbean", "Central America", "North America", "South America")
filtered_data <- migrant_data %>%
  filter(`region_of_incident` %in% regions_of_interest)

filtered_data <- migrant_data %>%
  mutate(
    reported_year = as.numeric(incident_year),
    reported_month = match(tolower(month), tolower(month.name)), # Match month names to numbers
    total_dead_missing = as.numeric(total_number_of_dead_and_missing)
  ) %>%
  filter(!is.na(reported_year) & !is.na(total_dead_missing))

filtered_data <- migrant_data %>%
  mutate(
    reported_year = as.numeric(incident_year),
    reported_month = match(tolower(month), tolower(month.name)), # Convert month names to numeric
    total_dead_missing = as.numeric(total_number_of_dead_and_missing)
  ) %>%
  filter(!is.na(reported_year) & !is.na(total_dead_missing) & !is.na(reported_month))

# Filter and transform the data
filtered_data <- migrant_data %>%
  mutate(
    reported_year = as.numeric(incident_year),                          # Convert year to numeric
    reported_month = match(tolower(month), tolower(month.name)),        # Match month names to numbers
    total_dead_missing = as.numeric(total_number_of_dead_and_missing)   # Convert total dead/missing to numeric
  ) %>%
  filter(
    !is.na(reported_year) &             # Ensure year is not NA
      !is.na(total_dead_missing) &        # Ensure total dead/missing is not NA
      !is.na(reported_month)              # Ensure month conversion worked
  )

filtered_data <- filtered_data %>%
  filter(reported_year < 2024)  # Exclude rows with year 2024

head(filtered_data)

# 1. Temporal Trend of Total Deaths and Missing

ggplot(filtered_data, aes(x = reported_year, y = total_dead_missing)) +
  geom_line(stat = "summary", fun = "sum", color = "red") +
  ggtitle("Temporal Trend of Total Deaths and Missing") +
  xlab("Year") +
  ylab("Total Dead and Missing") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(filtered_data$reported_year), max(filtered_data$reported_year), by = 1))  # Ensure all years are visible


# 2. Breakdown by Cause of Death Over Time

ggplot(filtered_data, aes(x = reported_year, y = total_dead_missing, fill = cause_of_death)) +
  geom_area(stat = "summary", fun = "sum", alpha = 0.7) +
  ggtitle("Breakdown by Cause of Death Over Time") +
  xlab("Year") +
  ylab("Total Dead and Missing") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(breaks = seq(min(filtered_data$reported_year), max(filtered_data$reported_year), by = 1)) 

# Temporal Trend of Total Deaths and Missing

ggplot(filtered_data, aes(x = reported_year, y = total_dead_missing)) +
  geom_line(stat = "summary", fun = "sum", color = "red") +
  ggtitle("Temporal Trend of Total Deaths and Missing") +
  xlab("Year") +
  ylab("Total Dead and Missing") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(filtered_data$reported_year), max(filtered_data$reported_year), by = 1)) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "blue") +  # Example of a notable year
  annotate("text", x = 2016, y = max(filtered_data$total_dead_missing, na.rm = TRUE), 
           label = "High casualties", hjust = -0.2, color = "blue")

# Average Deaths per Incident Over Time

filtered_data <- filtered_data %>%
  group_by(reported_year) %>%
  mutate(deaths_per_incident = total_dead_missing / n()) %>%
  ungroup()

ggplot(filtered_data, aes(x = reported_year, y = deaths_per_incident)) +
  geom_line(stat = "summary", fun = "mean", color = "purple") +
  ggtitle("Average Deaths per Incident Over Time") +
  xlab("Year") +
  ylab("Average Deaths per Incident") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(filtered_data$reported_year), max(filtered_data$reported_year), by = 1))

# Total Deaths and Missing by Cause Over Time

ggplot(filtered_data, aes(x = reported_year, y = total_dead_missing)) +
  geom_area(stat = "summary", fun = "sum", fill = "steelblue", alpha = 0.7) +
  ggtitle("Total Deaths and Missing by Cause Over Time") +
  xlab("Year") +
  ylab("Total Dead and Missing") +
  theme_minimal() +
  facet_wrap(~ cause_of_death, scales = "free_y") +
  scale_x_continuous(breaks = seq(min(filtered_data$reported_year), max(filtered_data$reported_year), by = 1))

# Temporal Trend with Confidence Intervals

ggplot(filtered_data, aes(x = reported_year, y = total_dead_missing)) +
  stat_summary(fun = "mean", geom = "line", color = "darkgreen") +
  stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2, fill = "lightgreen") +
  ggtitle("Temporal Trend with Confidence Intervals") +
  xlab("Year") +
  ylab("Total Dead and Missing") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(filtered_data$reported_year), max(filtered_data$reported_year), by = 1))


# Calculate percentages for deaths and survivors over time
filtered_data_summary <- filtered_data %>%
  group_by(reported_year) %>%
  summarise(
    total_dead = sum(number_of_dead, na.rm = TRUE),
    total_survivors = sum(number_of_survivors, na.rm = TRUE),
    total_cases = total_dead + total_survivors
  ) %>%
  mutate(
    percent_dead = total_dead / total_cases * 100,
    percent_survivors = total_survivors / total_cases * 100
  )


# Create a graph comparing deaths vs survivors with percentages (fixed)
ggplot(filtered_data_summary, aes(x = reported_year)) +
  geom_area(aes(y = percent_survivors, fill = "Survivors"), alpha = 0.5) + # Survivors on top
  geom_area(aes(y = percent_dead, fill = "Deaths"), alpha = 0.8) + # Deaths below
  scale_fill_manual(
    values = c("Deaths" = "#FF4D4D", "Survivors" = "#4CAF50"), # Red for deaths, green for survivors
    name = "Category"
  ) +
  ggtitle("Comparison of Deaths vs Survivors Over Time (Percentages)") +
  xlab("Year") +
  ylab("Percentage (%)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(filtered_data_summary$reported_year), 
                                  max(filtered_data_summary$reported_year), by = 1))
