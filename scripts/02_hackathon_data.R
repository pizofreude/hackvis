# Create a small, reproducible hackathon projects data set and save as CSV.


set.seed(123)  # for reproducibility

n <- 200
categories <- c("Health", "FinTech", "Climate", "Education", "Open")
projects <- tibble::tibble(
  project_id     = seq_len(n),
  team_name      = paste("Team", sample(100:999, n, replace = TRUE)),
  category       = sample(categories, n, replace = TRUE, prob = c(0.2,0.2,0.2,0.2,0.2)),
  score          = round(rnorm(n, mean = 75, sd = 12)),      # judging score 0–100 (approx)
  votes          = rpois(n, lambda = 25),                    # audience votes
  lines_of_code  = round(rlnorm(n, meanlog = 7, sdlog = 0.6)),  # skewed counts
  submitted_at   = as.POSIXct("2025-06-01", tz = "UTC") + runif(n, 0, 60*60*24*2)
)

?rnorm

# Clamp score into 0..100
projects$score[projects$score < 0]   <- 0
projects$score[projects$score > 100] <- 100

# Save to data/ so Shiny can use it later as a fallback
readr::write_csv(projects, "data/sample_projects.csv")