library(stringr)
library(tibble)
library(dplyr)

raw <- tibble(input = readLines("data-raw/day02.txt"))

clean <- raw %>%
  tidyr::extract(
    input,
    c("lower", "upper", "letter", "string"),
    "(.+)-(.+) (.)\\: (.+)",
    convert = TRUE
  )

# first answer
clean %>%
  rowwise() %>%
  mutate(n_matches = str_count(string, letter)) %>%
  ungroup() %>%
  summarise(sum(n_matches >= lower & n_matches <= upper))

# second one
clean %>%
  rowwise() %>%
  mutate(
    sub1 = str_sub(string, lower, lower),
    sub2 = str_sub(string, upper, upper)
    ) %>%
  ungroup() %>%
  filter(sub1 == letter | sub2 == letter) %>%
  filter(sub1 != sub2) %>%
  nrow()
