# Topic: Distribution of Party Identification
# Author: Kerri Rose Riley
# Purpose: Replicate figure 1 from Bartels(2000)-"Partisanship and Voting Behavior, 1952-1996"
# Requires: Cumulative American National Election Studies (ANES) file 
# Output: Figures as PDF (ggplot only)

# Import modules
library(tidyverse) # For data wrangling
library(haven) # To use read_dta() for Stata files
library(ggplot2) # For graphs and plots

# Read the ANES data
anes_2024 <- read_dta("~/Desktop/Current Classes/R/Problem Set 7/Data/anes_2024.dta")

# Read the cumulative ANES data
anes_cum <- read_dta("~/Desktop/Current Classes/R/Problem Set 7/Data/anes_cum.dta")


# Recode VCF0305 into four party identification categories

figure1 <- anes_cum |>
  # Filter to have same years
  filter(VCF0004 >= 1952 & VCF0004 <= 1996) |>
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

# Calculate proportions
figure1 <- figure1 |>
  group_by(VCF0004, party_id) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(VCF0004) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()


figure1 <- figure1 |>
  mutate(
    panel = case_when(
      party_id %in% c("Strong Identifiers", "Weak Identifiers") ~ "Identifiers",
      party_id %in% c("Independent Leaners", "Pure Independents") ~ "Independents"
    )
  )


# PLOTTING
ggplot(figure1, aes(
  x = VCF0004,
  y = proportion,
  group = party_id,
  linetype = party_id,
  shape = party_id
)) +
  # Lines connect all years
  geom_line() +
  
  # Points only at labeled years
  geom_point(
    data = filter(
      figure1,
      VCF0004 %% 4 == 0
    ),
    size = 2
  ) +
  
  facet_grid(panel ~ ., scales = "fixed") +
  
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0.1, 0.5, 0.1)
  ) +
  
  scale_x_continuous(
    breaks = c(1956, 1964, 1972, 1980, 1988, 1996)
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
    x = NULL,
    y = NULL,
    title = "Proportions of National Election Study Sample"
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA
    ),
    plot.title = element_text(
      hjust = 0.5
    )
  )


# Create the same four party identification categories
# for the 2024 ANES data

anes_2024 <- anes_2024 |>
  mutate(
    VCF0004 = 2024,
    party_id = case_when(
      
      # Pure Independents
      V241221 == 3 & V241223 == 2 ~ "Pure Independents",
      
      # Independent Leaners
      V241221 == 3 & V241223 %in% c(1, 3) ~ "Independent Leaners",
      
      # Strong Identifiers
      V241221 %in% c(1, 2) & V241222 == 1 ~ "Strong Identifiers",
      
      # Weak Identifiers
      V241221 %in% c(1, 2) & V241222 == 2 ~ "Weak Identifiers",
      
      TRUE ~ NA_character_
    )
  )

# Create the same party_id variable for the cumulative data
anes_cum_extended <- anes_cum |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  )

# Combine cumulative ANES and 2024 ANES
anes_all <- bind_rows(
  anes_cum_extended,
  anes_2024
)

# Calculate proportions
figure1_extended <- anes_all |>
  filter(VCF0004 >= 1952 & VCF0004 <= 2024) |>
  filter(!is.na(party_id)) |>
  group_by(VCF0004, party_id) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |>
  group_by(VCF0004) |>
  mutate(
    proportion = n / sum(n)
  ) |>
  ungroup() |>
  mutate(
    panel = case_when(
      party_id %in% c(
        "Strong Identifiers",
        "Weak Identifiers"
      ) ~ "Identifiers",
      
      party_id %in% c(
        "Independent Leaners",
        "Pure Independents"
      ) ~ "Independents"
    )
  )

ggplot(
  figure1_extended,
  aes(
    x = VCF0004,
    y = proportion,
    group = party_id,
    linetype = party_id,
    shape = party_id
  )
) +
  geom_line() +
  geom_point(
    data = filter(figure1_extended, VCF0004 %% 4 == 0),
    size = 2
  ) +
  facet_grid(panel ~ ., scales = "fixed") +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0.1, 0.5, 0.1)
  ) +
  scale_x_continuous(
    breaks = seq(1952, 2024, 8)
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
    x = NULL,
    y = NULL,
    title = "Proportions of National Election Study Sample"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA
    ),
    plot.title = element_text(hjust = 0.5)
  )



# Create figure including presidential and midterm election years
# Use the cumulative ANES data


figure1_midterms <- anes_cum |>
  filter(VCF0004 >= 1952 & VCF0004 <= 2020) |>
  mutate(
    party_id = case_when(
      VCF0305 == 1 ~ "Pure Independents",
      VCF0305 == 2 ~ "Independent Leaners",
      VCF0305 == 3 ~ "Weak Identifiers",
      VCF0305 == 4 ~ "Strong Identifiers",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id)) |>
  group_by(VCF0004, party_id) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(VCF0004) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  mutate(
    panel = case_when(
      party_id %in% c("Strong Identifiers", "Weak Identifiers") ~ "Identifiers",
      party_id %in% c("Independent Leaners", "Pure Independents") ~ "Independents"
    )
  )

# Plot
ggplot(
  figure1_midterms,
  aes(
    x = VCF0004,
    y = proportion,
    group = party_id,
    linetype = party_id,
    shape = party_id
  )
) +
  
  # Lines connect all available ANES years
  geom_line() +
  
  # Points show all available ANES years, including midterms
  geom_point(size = 2) +
  
  # Create two panels
  facet_grid(
    panel ~ .,
    scales = "fixed"
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0.1, 0.5, 0.1)
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = seq(1952, 2020, 4)
  ) +
  
  # Line types
  scale_linetype_manual(
    values = c(
      "Strong Identifiers" = "solid",
      "Weak Identifiers" = "dashed",
      "Independent Leaners" = "dashed",
      "Pure Independents" = "solid"
    )
  ) +
  
  # Point shapes
  scale_shape_manual(
    values = c(
      "Strong Identifiers" = 16,
      "Weak Identifiers" = 1,
      "Independent Leaners" = 1,
      "Pure Independents" = 16
    )
  ) +
  
  # Labels
  labs(
    x = NULL,
    y = NULL,
    title = "Proportions of National Election Study Sample"
  ) +
  
  # Theme
  theme_classic() +
  
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_blank(),
    
    # Border around each panel
    panel.border = element_rect(
      color = "black",
      fill = NA
    ),
    
    # Center title
    plot.title = element_text(
      hjust = 0.5
    )
  )

# Original Work
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



