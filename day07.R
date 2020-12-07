library(tidyverse)

# inputs ------------------------------------------------------------------

raw <- "data-raw/day07.txt"

parse_input <- function(input) {
  tibble(input = read_lines(input)) %>%
    separate(input, c("bag", "contains"), sep = " contain ") %>%
    separate_rows(contains, sep = ", ") %>%
    separate(
      contains,
      c("n_within", "type_within"),
      sep = " ", extra = "merge"
    ) %>%
    mutate(
      across(n_within, ~ .x %>% str_replace("no", "0") %>% as.numeric()),
      across(type_within, ~ .x %>% str_remove("[.]") %>% str_replace("bag$", "bags"))
    )
}


# question 1 --------------------------------------------------------------

tib <- parse_input(raw)

semi_bind_distinct_recursively <- function(x) {

  out_bags <- semi_join(tib, x, by = c("type_within" = "bag"))
  all_bags <- bind_rows(out_bags, x)
  dis_bags <- distinct(all_bags)

  if (nrow(dis_bags) == nrow(x)) return (dis_bags)
  semi_bind_distinct_recursively(dis_bags)
}

all_bags_gold <- tib %>%
  filter(type_within == "shiny gold bags") %>%
  semi_bind_distinct_recursively()

all_bags_gold %>%
  distinct(bag) %>%
  nrow()

# question 2 --------------------------------------------------------------

xib <- parse_input(raw)

inner_bind_distinct_recursively <- function(x) {

  out_bags <- xib %>%
    inner_join(x, by = c("bag" = "type_within"), suffix = c("", "_f")) %>%
    mutate(n_within = n_within * n_within_f) %>%
    select(-ends_with("_f"))

  all_bags <- bind_rows(out_bags, x)
  dis_bags <- distinct(all_bags)

  if (nrow(dis_bags) == nrow(x)) return (dis_bags)
  inner_bind_distinct_recursively(dis_bags)
}

xib %>%
  filter(bag == "shiny gold bags") %>%
  inner_bind_distinct_recursively() %>%
  pull(n_within) %>%
  sum()
