# In scripts/03_data_patterns.R (new file)
# Ensure we are in the RStudio Project 'hackvis'. All file paths are relative.

# Load packages used today
library(tidyverse)   # readr, dplyr, tidyr, ggplot2, stringr, forcats, tibble
library(lubridate)   # date-time parsing and features (part of tidyverse family)
library(DT)          # used later in Shiny; fine to load now

# Ensure the sample CSV from Part 1 exists; if not, (re)generate quickly.
if (!file.exists("data/sample_projects.csv")) {
  set.seed(123)
  n <- 200
  categories <- c("Health", "FinTech", "Climate", "Education", "Open")
  projects <- tibble::tibble(
    project_id     = seq_len(n),
    team_name      = paste("Team", sample(100:999, n, replace = TRUE)),
    category       = sample(categories, n, replace = TRUE),
    score          = pmin(pmax(round(rnorm(n, 75, 12)), 0), 100),
    votes          = rpois(n, 25),
    lines_of_code  = round(rlnorm(n, 7, 0.6)),
    submitted_at   = as.POSIXct("2025-06-01", tz = "UTC") + runif(n, 0, 60*60*24*2)
  )
  readr::write_csv(projects, "data/sample_projects.csv")
}

# Read the CSV 
projects <- readr::read_csv("data/sample_projects.csv", show_col_types = FALSE)

# Always glance at structure immediately
dplyr::glimpse(projects)







# Suppose we have wide columns for votes by day; we want them long for plotting
toy_wide <- tibble::tibble(
  project_id = 1:3,
  votes_day1 = c(10, 12, 9),
  votes_day2 = c(14, 11, 15)
)

toy_wide

toy_long <- toy_wide |>
  pivot_longer(
    cols = starts_with("votes_day"),
    names_to = "day",
    values_to = "votes"
  ) |>
  mutate(day = readr::parse_number(day))  # extract day index (1,2,...)

toy_long

# And the reverse (long -> wide) if needed
toy_wide_again <- toy_long |>
  pivot_wider(names_from = day, values_from = votes, names_prefix = "votes_day")