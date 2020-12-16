library(tidyverse)

input <- readr::read_lines("data-raw/day16.txt")

cond   <- input[1:(which(input == "")[1] - 1)]
mine   <- input[(str_which(input, "your") + 1):(which(input == "")[2] - 1)]
nearby <- input[(str_which(input, "nearby") + 1):(length(input) - 1)]

# part 1 ------------------------------------------------------------------

vals <- cond %>%
  str_extract_all("\\d+-\\d+") %>%
  map_depth(2, str_split, "-") %>%
  map_depth(3, as.numeric) %>%
  map_depth(3, reduce, `:`) %>%
  unlist() %>%
  unique()

nearby %>%
  str_split(",") %>%
  map(as.numeric) %>%
  map(~ setdiff(.x, vals)) %>%
  compact() %>%
  reduce(sum)


# part 2 ------------------------------------------------------------------

tib <- tibble(cond = cond) %>%
  extract(
    cond,
    into = c("field", "ll", "lr", "ul", "ur"),
    regex = "(.+)\\: (.+)-(.+) or (.+)-(.+)",
    convert = TRUE
    )

elements <- nearby %>%
  str_split(",") %>%
  map(as.numeric) %>%
  keep(~ is_empty(setdiff(.x, vals))) %>%
  transpose() %>%
  map(unlist)

is_valid <- function(x, ll, lr, ul, ur) {
  all(dplyr::between(x, ll, lr) | dplyr::between(x, ul, ur))
}
# is_valid(elements[[1]], 50, 692, 705, 969)

valid <- elements %>%
  map_dfr(
    ~ tib %>%
      rowwise() %>%
      mutate(all_valid = is_valid(.x, ll, lr, ul, ur)) %>%
      ungroup() %>%
      filter(all_valid) %>%
      select(field),
    .id = "order"
  ) %>%
  mutate(across(order, as.numeric))

leftover <- valid
ordered  <- NULL
while (nrow(leftover) > 0) {
  ordered <- leftover %>%
    group_by(field) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    bind_rows(ordered)
  leftover <- anti_join(leftover, ordered, by = "order")
}

idx <- ordered %>%
  filter(str_starts(field, "departure")) %>%
  pull(order)

mine %>%
  str_split(",") %>%
  { .[[1]][idx] } %>%
  as.numeric() %>%
  prod() %>%
  scales::number(big.mark = "")

