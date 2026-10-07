# Topic: Distribution of Party Identification
#Author: Kerri Rose Riley

# Import modules
# For data wrangling
library(tidyverse)

# To use read_dta() for Stata files
library(haven)

# For graphs and plots
library(ggplot2)


# Read the ANES data

anes_2024 <- read_dta("~/Desktop/Current Classes/R/Problem Set 7/Data/anes_2024.dta")

# Read the cumulative ANES data
anes_cum <- read_dta("~/Desktop/Current Classes/R/Problem Set 7/Data/anes_cum.dta")


# Recode partid

# Recode VCF0305 into four party identification categories
figure1_cum <- anes_cum |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id))


# Calculate proportions of each year
figure1_cum <- anes_cum |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id))


# Original Bartels figure: 1952-1996

figure1_cum <- anes_cum |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id))


figure1_cum <- anes_cum |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id))

# Extend analysis through 2020
# This includes midterm elections

figure1_cum <- anes_cum |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id))


# Add 2024 data
figure1_cum <- anes_cum |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id))


figure1_extended <- bind_rows(
  figure1_recent,
  figure1_2024
) |>
  mutate(
    panel = case_when(
      party_id %in% c(
        "Strong Identifiers",
        "Weak Identifiers"
      ) ~ "Party Identifiers",
      
      party_id %in% c(
        "Independent Leaners",
        "Pure Independents"
      ) ~ "Independents"
    )
  )

# Create figure showing extended figure 1

figure1_extended <- bind_rows(
  figure1_recent,
  figure1_2024
) |>
  mutate(
    panel = case_when(
      party_id %in% c(
        "Strong Identifiers",
        "Weak Identifiers"
      ) ~ "Party Identifiers",
      
      party_id %in% c(
        "Independent Leaners",
        "Pure Independents"
      ) ~ "Independents"
    )
  )

# Original Work -  Find two interesting variables and create compelling univariate graphs to illustrate their central tendency, distribution, and spread.

political_info <- anes_cum |>
  mutate(
    political_info = case_when(
      VCF0050b == 1 ~ "Very high",
      VCF0050b == 2 ~ "Fairly high",
      VCF0050b == 3 ~ "Average",
      VCF0050b == 4 ~ "Fairly low",
      VCF0050b == 5 ~ "Very low",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(political_info)) |>
  mutate(
    political_info = factor(
      political_info,
      levels = c(
        "Very high",
        "Fairly high",
        "Average",
        "Fairly low",
        "Very low"
      )
    )
  )
# Plot
ggplot(political_info, aes(x = political_info)) +
  geom_bar() +
  labs(
    x = "Level of Political Information",
    y = "Number of Respondents"
  ) +
  theme_classic()


# VCF0504 - the republicans party liberal to conservative scale
republican_ideology <- anes_cum |>
  mutate(
    republican_ideology = case_when(
      VCF0504 %in% c(1, 2, 3) ~ "Liberal",
      VCF0504 == 4 ~ "Moderate",
      VCF0504 %in% c(5, 6, 7) ~ "Conservative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(republican_ideology)) |>
  mutate(
    republican_ideology = factor(
      republican_ideology,
      levels = c(
        "Liberal",
        "Moderate",
        "Conservative"
      )
    )
  )

ggplot(republican_ideology, aes(x = republican_ideology)) +
  geom_bar() +
  labs(
    x = "Rating of Republican Party",
    y = "Number of Respondents"
  ) +
  theme_classic()


