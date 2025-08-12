# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the dataset
census_data <- read.csv("C:\\Users\\Hriday\\Downloads\\acs2017_census_tract_data.csv", header = TRUE)

# Display dataset summary
summary(census_data)

# Count missing values per column
missing_counts <- colSums(is.na(census_data))
print(missing_counts)

# Histogram of total population across census tracts
hist(census_data$TotalPop, 
     main = "Total Population Distribution", 
     xlab = "Population", 
     
     col = "steelblue", 
     border = "white")

# Enhanced visualization with ggplot2
ggplot(census_data, aes(x = TotalPop)) +
  geom_histogram(binwidth = 5000, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Total Population Distribution", x = "Population", y = "Count")

# Compute average income per capita by state
avg_income_per_capita <- census_data %>%
  group_by(State) %>%
  summarise(avg_income = mean(IncomePerCap, na.rm = TRUE))
print(avg_income_per_capita)

# Identify state with the highest median income
median_income_state <- census_data %>%
  group_by(State) %>%
  summarise(median_income = median(IncomePerCap, na.rm = TRUE)) %>%
  arrange(desc(median_income))
print(median_income_state)

# Boxplot of unemployment rate by state
boxplot(Unemployment ~ State, data = census_data, las = 2,
        main = "State-wise Unemployment Rate", col = "tomato")

# Compute correlation between income and poverty rate
income_poverty_correlation <- cor(census_data$Income, census_data$Poverty, use = "complete.obs")
print(income_poverty_correlation)

# Identify top 5 counties with highest child poverty rate
top_child_poverty_counties <- census_data %>%
  arrange(desc(ChildPoverty)) %>%
  select(County, State, ChildPoverty) %>%
  head(5)
print(top_child_poverty_counties)

# Compute average percentage of commuters using public transport
avg_transit_use <- mean(census_data$Transit, na.rm = TRUE)
print(avg_transit_use)

# Calculate and rank states by average commuting time
avg_commute_time <- census_data %>%
  group_by(State) %>%
  summarise(avg_commute = mean(MeanCommute, na.rm = TRUE)) %>%
  arrange(desc(avg_commute))
print(avg_commute_time)

# Compute racial composition across census tracts
race_categories <- c("Hispanic", "White", "Black", "Native", "Asian", "Pacific")
racial_distribution <- colMeans(census_data[race_categories], na.rm = TRUE)
print(racial_distribution)

# Identify top 5 states with highest self-employment rates
top_self_employment_states <- census_data %>%
  group_by(State) %>%
  summarise(avg_self_employed = mean(SelfEmployed, na.rm = TRUE)) %>%
  arrange(desc(avg_self_employed)) %>%
  head(5)
print(top_self_employment_states)

# Bar plot of employment distribution by sector
sector_distribution <- colMeans(census_data[c("PrivateWork", "PublicWork", "SelfEmployed")], na.rm = TRUE)
barplot(sector_distribution,
        main = "Employment Sector Distribution",
        col = c("royalblue", "firebrick", "seagreen"))

# Compute gender ratio (men-to-women)
gender_ratio <- sum(census_data$Men, na.rm = TRUE) / sum(census_data$Women, na.rm = TRUE)
print(gender_ratio)

# Classify counties as Urban or Rural based on median population
census_data$UrbanRural <- ifelse(census_data$TotalPop > median(census_data$TotalPop, na.rm = TRUE), "Urban", "Rural")

# Compare average household income between urban and rural counties
urban_rural_income_comparison <- census_data %>%
  group_by(UrbanRural) %>%
  summarise(avg_income = mean(Income, na.rm = TRUE))
print(urban_rural_income_comparison)
