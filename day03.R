library(tidyverse)

raw <- readLines("data-raw/day03.txt")

# Part 1 ------------------------------------------------------------------

# number of trees, without increasing the pattern to the right
tibble(
  input = raw,
  x = 0:(length(input) - 1),
  y = list(0:(unique(str_length(input)) - 1)),
  sym = str_split(input, "")
  ) %>%
  unnest(c(y, sym)) %>%
  filter(y == 3 * x, sym == "#") %>%
  nrow()

# number of trees, after increasing the pattern x times
n_trees <- function(pattern_times = 1) {
  tibble(
    input = strrep(raw, pattern_times),
    x = 0:(length(input) - 1),
    y = list(0:(unique(str_length(input)) - 1)),
    sym = str_split(input, "")
  ) %>%
  unnest(c(y, sym)) %>%
  filter(y == 3 * x, sym == "#") %>%
  nrow()
}

n_trees(5)

# aprox number of rounds needed
length(raw) * 3 / (str_length(raw[1]))

n_trees(32)
n_trees(33)


# Part 2  -----------------------------------------------------------------

n_trees_slope <- function(slope) {

  n_rounds <- ceiling( length(raw) * slope / str_length(raw[1]) )
  message("n rounds: ", n_rounds)

  n_trees2 <- function(pattern_times = 1) {
    tibble(
      input = strrep(raw, pattern_times),
      x = 0:(length(input) - 1),
      y = list(0:(unique(str_length(input)) - 1)),
      sym = str_split(input, "")
    ) %>%
    unnest(c(y, sym)) %>%
    filter(y == slope * x, sym == "#") %>%
    nrow()
  }

  n1 <- n_trees2(n_rounds)
  n2 <- n_trees2(n_rounds + 1) # sanity check

  if (n1 == n2) return(n1) else stop("Wrong! ", n1, " ", n2)
}


list(
  n_trees_slope(1),
  n_trees_slope(3),
  n_trees_slope(5),
  n_trees_slope(7),
  n_trees_slope(0.5)
  ) %>%
  as.numeric() %>% # wohoo, integer overflow otherwise
  reduce(`*`)

