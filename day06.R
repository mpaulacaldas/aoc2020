library(stringr)
library(purrr)

str_split2 <- function(...) str_split(...)[[1]]

raw <- readr::read_file("data-raw/day06.txt") %>%
  str_trim(side = "right") %>% # remove new line at end of file
  str_split2("\\n\\n")

# question 1
raw %>%
  str_remove_all("\\n") %>%
  map(str_split2, "") %>%
  map(unique) %>%
  lengths() %>%
  sum()

# question 2
raw %>%
  map(str_split2, "\\n") %>%
  map_depth(2, str_split2, "") %>%
  map(reduce, intersect) %>%
  lengths() %>%
  sum()
