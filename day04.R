library(tidyverse)

raw <- read_file("data-raw/day04.txt") %>%
  str_split("\\n\\n") %>%
  pluck(1) %>%
  str_replace_all("\\n", " ")

req_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") # no cid


map_lgl(req_fields, ~ str_detect(raw[1], fixed(.x))) %>%
  sum()

str_count_all_ <- function(string, patterns = req_fields) {
  patterns %>%
    map_lgl(~ str_detect(string, fixed(.x))) %>%
    sum()
}
str_count_all <- function(string_vector) {
  map_int(string_vector, str_count_all_)
}

# question 1
raw %>%
  str_count_all() %>%
  table() %>%
  pluck("7")

# question 2
tibble(input = str_squish(raw), passport_n = seq_along(raw)) %>%
  separate_rows(input, sep = " ") %>%
  separate(input, c("field", "value"), sep = ":") %>%
  pivot_wider(names_from = field, values_from = value) %>%
  select(-cid) %>%
  mutate(
    across(c(byr, eyr, iyr), as.numeric),
    hgt_scale = str_extract(hgt, "cm|in"),
    hgt_value = parse_number(hgt)
    ) %>%
  drop_na() %>%
  filter(
    between(byr, 1920, 2002),
    between(iyr, 2010, 2020),
    between(eyr, 2020, 2030),
    (
      hgt_scale == "cm" & between(hgt_value, 150, 193) |
      hgt_scale == "in" & between(hgt_value, 59, 76)
    ),
    str_detect(hcl, "^#([0-9]|[a-f]){6}$"),
    ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    str_detect(pid, "^\\d{9}$")
  ) %>%
  nrow()
