library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)

# 1. Assignments
x <- 42
greeting <- "hello world" # alt + - = <-
flag <- TRUE #CASE SENSITIVE
X <- 4.2 # NUMERIC  
X <- 42L # INTEGER

# 2. Vectors
nums <- c(10, 20, 30, 40)
names <- c("Emelia", "Hafeez", "Brautpaar")
bools <- c(FALSE, TRUE, TRUE, FALSE)

nums_plus_one <- nums + 1 # Vectorised maths, it operates on elementwise
nums_times_two <- nums * 2

# 3. Missing Values
has_na <- c(1, NA, 2)
mean(nums)
?mean
mean(has_na, na.rm = TRUE)
c(1/0, -1/0, 0/0)

# 4. Indexing
nums[1]
nums[1:3]
nums[bools]


# 5. Data frames
df <- data.frame(name = names, score = c(100, 100, 100))
df
tb <- tibble::tibble(name = names, score = c(100, 100, 100))
tb

df$name
df[["name"]]
tb |> dplyr::pull(score) # pipe


list(1, "Emelia", TRUE) # heterogeneous = can store multiple data types
matrix(c(1,2,3,4), nrow=2)


# 6. Control Flow & Functions
for (i in 0:2) {
  print(i)
}

f <- function(x) {
  x+1
}
  
f(2)

square <- function(x) {
  x*x
}

square(25)

?sqrt

# leftside|> rightside
result1 <- tb |>
  select(name, score) |>
  mutate(score2 = score * 2) |>
  arrange(desc(score2))
result1
