

# Topic: Distribution of Party Identification

# Import modules

# For data wrangling
library(tidyverse)
# to use the read_dta()
library(haven)
# For graphs and plots
library(ggplot2)

# Read the GSS data
anes_2024 <- read_dta("~/Desktop/Current Classes/R/Problem Set 7/Data/anes_2024.dta")
anes_cum <- read_dta("~/Desktop/Current Classes/R/Problem Set 7/Data/anes_cum.dta")



# Recoding
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


figure1_cum <- figure1_cum |>
  group_by(VCF0004, party_id) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(VCF0004) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

figure1_original <- figure1_cum |>
  filter(VCF0004 >= 1952 & VCF0004 <= 1996)

figure1_original <- figure1_original |>
  mutate(
    panel = case_when(
      party_id %in% c("Strong Identifiers", "Weak Identifiers") ~
        "Party Identifiers",
      party_id %in% c("Independent Leaners", "Pure Independents") ~
        "Independents"
    )
  )


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

# Extend analysis 
figure1_recent <- figure1_cum |>
  filter(VCF0004 >= 1952 & VCF0004 <= 2020)



# Adding 2024
figure1_2024 <- anes_2024 |>
  mutate(
    party_id = case_when(
      V241227x %in% c(1, 7) ~ "Strong Identifiers",
      V241227x %in% c(2, 6) ~ "Weak Identifiers",
      V241227x %in% c(3, 5) ~ "Independent Leaners",
      V241227x == 4 ~ "Pure Independents",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party_id))

figure1_2024 <- figure1_2024 |>
  mutate(year = 2024)

figure1_2024 <- figure1_2024 |>
  group_by(year, party_id) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

figure1_recent <- figure1_recent |>
  rename(year = VCF0004)

figure1_extended <- bind_rows(
  figure1_recent,
  figure1_2024
)

figure1_extended <- figure1_extended |>
  mutate(
    panel = case_when(
      party_id %in% c("Strong Identifiers", "Weak Identifiers") ~
        "Party Identifiers",
      party_id %in% c("Independent Leaners", "Pure Independents") ~
        "Independents"
    )
  )

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



# 3: Extend to 2024 using anes_2024

figure1_2024 <- anes_2024 |>
  mutate(
    party_id = case_when(
      V241227x %in% c(1, 7) ~ "Strong Identifiers",
      V241227x %in% c(2, 6) ~ "Weak Identifiers",
      V241227x %in% c(3, 5) ~ "Independent Leaners",
      V241227x == 4 ~ "Pure Independents",
      TRUE ~ NA_character_
    ),
    year = 2024
  ) |>
  filter(!is.na(party_id)) |>
  group_by(year, party_id) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

figure1_extended <- bind_rows(
  figure1_recent |>
    rename(year = VCF0004),
  figure1_2024
) |>
  mutate(
    panel = case_when(
      party_id %in% c("Strong Identifiers", "Weak Identifiers") ~
        "Party Identifiers",
      party_id %in% c("Independent Leaners", "Pure Independents") ~
        "Independents"
    )
  )

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


# Include midterm years






