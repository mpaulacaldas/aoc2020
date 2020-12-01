library(purrr)

raw <- as.numeric(readLines("data-raw/day01.txt"))

xy_dont_sum_2020 <- function(x, y) x + y != 2020
cross2(raw, raw, .filter = xy_dont_sum_2020) %>%
  pluck(1) %>%
  reduce(`*`)

xyz_dont_sum_2020 <- function(x, y, z) x + y + z != 2020
cross3(raw, raw, raw, .filter = xyz_dont_sum_2020) %>%
  pluck(1) %>%
  reduce(`*`)
