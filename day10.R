input <- as.numeric(readLines("data-raw/day10.txt"))
sorted <- sort(input)

# question 1
diffs <- c(sorted - dplyr::lag(sorted, default = 0), 3)
Reduce(`*`, table(diffs))

# question 2
library(purrr)

pos <- sorted %>%
  set_names() %>%
  map(~ intersect(.x - 1:3, sorted)) %>%
  compact() # TOOD: think
