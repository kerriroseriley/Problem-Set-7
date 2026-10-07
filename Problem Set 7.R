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

# Count respondents in each party identification category
# and calculate the proportion within each year
figure1_cum <- figure1_cum |>
  group_by(VCF0004, party_id) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |>
  group_by(VCF0004) |>
  mutate(
    proportion = n / sum(n)
  ) |>
  ungroup()


# Original Bartels figure: 1952-1996

# Keep observations from 1952 through 1996
figure1_original <- figure1_cum |>
  filter(
    VCF0004 >= 1952,
    VCF0004 <= 1996
  )

# Create two panels:
# Party Identifiers = Strong + Weak Identifiers
# Independents = Independent Leaners + Pure Independents
figure1_original <- figure1_original |>
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


# Create the original figure
ggplot(
  figure1_original,
  aes(
    x = VCF0004,
    y = proportion,
    group = party_id,
    linetype = party_id,
    shape = party_id
  )
) +
  geom_line() +
  geom_point() +
  facet_grid(panel ~ .) +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0.1, 0.5, 0.1)
  ) +
  scale_linetype_manual(
    values = c(
      "Strong Identifiers" = "solid",
      "Weak Identifiers" = "dashed",
      "Independent Leaners" = "dashed",
      "Pure Independents" = "solid"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Strong Identifiers" = 16,
      "Weak Identifiers" = 1,
      "Independent Leaners" = 1,
      "Pure Independents" = 16
    )
  ) +
  labs(
    x = "Year",
    y = "Proportions of National Election Study Sample"
  ) +
  theme_classic()


# Extend the analysis through 2020


# Keep all available observations from 1952 through 2020.
# This includes midterm years that are present in the
# cumulative ANES data.

figure1_recent <- figure1_cum |>
  filter(
    VCF0004 >= 1952,
    VCF0004 <= 2020
  )


# Rename the year variable so that it matches the 2024 data
figure1_recent <- figure1_recent |>
  rename(year = VCF0004)


# Add the 2024 ANES data

# Recode the 2024 party identification variable
figure1_2024 <- anes_2024 |>
  mutate(
    party_id = case_when(
      V241227x %in% c(1, 7) ~ "Strong Identifiers",
      V241227x %in% c(2, 6) ~ "Weak Identifiers",
      V241227x %in% c(3, 5) ~ "Independent Leaners",
      V241227x == 4 ~ "Pure Independents",
      TRUE ~ NA_character_
    ),
    
    # Add the survey year
    year = 2024
  ) |>
  filter(!is.na(party_id))


# Calculate the proportion for each party identification
# category in 2024
figure1_2024 <- figure1_2024 |>
  group_by(year, party_id) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |>
  group_by(year) |>
  mutate(
    proportion = n / sum(n)
  ) |>
  ungroup()


# Combine the historical data with the 2024 data
figure1_extended <- bind_rows(
  figure1_recent,
  figure1_2024
)


# Create the two panels
figure1_extended <- figure1_extended |>
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


# Create the extended figure through 2024

ggplot(
  figure1_extended,
  aes(
    x = year,
    y = proportion,
    group = party_id,
    linetype = party_id,
    shape = party_id
  )
) +
  geom_line() +
  geom_point() +
  facet_grid(panel ~ .) +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0.1, 0.5, 0.1)
  ) +
  scale_linetype_manual(
    values = c(
      "Strong Identifiers" = "solid",
      "Weak Identifiers" = "dashed",
      "Independent Leaners" = "dashed",
      "Pure Independents" = "solid"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Strong Identifiers" = 16,
      "Weak Identifiers" = 1,
      "Independent Leaners" = 1,
      "Pure Independents" = 16
    )
  ) +
  labs(
    x = "Year",
    y = "Proportions of National Election Study Sample"
  ) +
  theme_classic()


# Identify presidential and midterm election years


# Presidential election years are divisible by 4.
# Other election years in the ANES data are midterm years.
figure1_extended <- figure1_extended |>
  mutate(
    election_type = if_else(
      year %% 4 == 0,
      "Presidential",
      "Midterm"
    )
  )


#  Create figure including midterm years

# Use different shapes to distinguish presidential
# and midterm election years
ggplot(
  figure1_extended,
  aes(
    x = year,
    y = proportion,
    group = party_id,
    linetype = party_id,
    shape = election_type
  )
) +
  geom_line() +
  geom_point(size = 2) +
  facet_grid(panel ~ .) +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0.1, 0.5, 0.1)
  ) +
  scale_linetype_manual(
    values = c(
      "Strong Identifiers" = "solid",
      "Weak Identifiers" = "dashed",
      "Independent Leaners" = "dashed",
      "Pure Independents" = "solid"
    )
  ) +
  labs(
    x = "Year",
    y = "Proportions of National Election Study Sample",
    shape = "Election Type"
  ) +
  theme_classic()










