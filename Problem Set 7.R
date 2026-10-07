

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
figure1 <- anes_cum |>
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

ggplot(figure1, aes(
  x = VCF0004,
  y = proportion,
  group = party_id,
  linetype = party_id,
  shape = party_id
)) +
  geom_line() +
  geom_point(size = 2) +
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
    y = "Proportions of National Election Study Sample"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_blank()
  )





