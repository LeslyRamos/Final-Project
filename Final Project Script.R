## Load libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(rstatix)
library(knitr)
library(gganimate)

## Load data
### Emissions from 1830 to 2023
emissions_data <- read_excel("USA Emissions.xlsx")
head(emissions_data)

## Fish data
fish_data <- read_csv("database (3).csv")
head(fish_data)
colnames(fish_data)

## Standardize Units, convert CH4 and N2O to CO2 equivalents
# Using global warming potentials (GWP): CH4 = 25, N2O = 298
### Standardize Units
# Convert greenhouse gases to CO2 equivalents
# GWP (Global Warming Potential): CH4 = 25, N2O = 298
emissions_data <- emissions_data %>%
  mutate(
    CH4_in_CO2eq = if_else(Gas == "CH[4]", Data * 25, 0), # Convert CH4 to CO2 equivalents
    N2O_in_CO2eq = if_else(Gas == "N[2]*O", Data * 298, 0), # Convert N2O to CO2 equivalents
    CO2_in_CO2eq = if_else(Gas == "CO[2]", Data * 1000, 0), # Convert CO2 from Pg to Tg
    Total_CO2eq = CH4_in_CO2eq + N2O_in_CO2eq + CO2_in_CO2eq # Sum of all gases in CO2eq
  ) %>%
  group_by(Year) %>%
  summarize(Total_CO2eq = sum(Total_CO2eq, na.rm = TRUE)) # Aggregate by year

head(emissions_data)

## Merge dataset
merged_data <- fish_data %>%
  mutate(Fish_Response = as.factor(Fish_Response)) %>%
  inner_join(emissions_data, by = "Year")

## Data exploration
# Summary for emissions
summary(emissions_data)

# Summary for fish response
fish_data %>% count(Fish_Response)


### Visualize Emissions Over Time
ggplot(emissions_data, aes(x = Year)) +
  geom_line(aes(y = Total_CO2eq, color = "Total CO2eq (Tg/year)")) +
  labs(
    title = "Historical USA Emissions in CO2 Equivalents",
    x = "Year",
    y = "Emissions (Tg CO2eq/year)",
    color = "Greenhouse gas"
  ) +
  theme_minimal()

### Animation: Emissions Over Time
ggplot(emissions_data, aes(x = Year, y = Total_CO2eq)) +
  geom_line(color = "blue") +
  labs(
    title = "Animated Emissions Over Time",
    x = "Year",
    y = "Total CO2eq (Tg/year)"
  ) +
  transition_reveal(Year) +
  theme_minimal()


## Statistical Analysis
### Model Relationship Between Emissions and Fish Response
# Logistic regression to analyze relationship
model <- glm(Fish_Response ~ Total_CO2eq, 
             data = merged_data, 
             family = "binomial")
summary(model)

### Visualization: Fish Response Distribution
ggplot(fish_data, aes(x = Fish_Response)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Fish Response Categories",
    x = "Fish Response Category",
    y = "Count"
  ) +
  theme_minimal()










