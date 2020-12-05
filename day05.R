library(purrr)
library(stringr)
# raw <- readLines("data-raw/day05.txt")

rows <- 0:127
cols <- 0:7


# Example -----------------------------------------------------------------

raw <- "FBFBBFFRLR"

get_rows <- function(input, f_or_b) {
  stopifnot(f_or_b %in% c("F", "B"))
  midpoint <- length(input) / 2
  if (f_or_b == "F") {
    input[1:midpoint]
  } else {
    input[(midpoint + 1):length(input)]
  }
}
rows %>%
  get_rows("F") %>%
  get_rows("B")

get_rows_string <- function(string) {
  vect <- stringr::str_split(string, "")[[1]]
  for (i in seq_along(vect)) {
    rows <- get_rows(rows, vect[i]) # rows comes from global envir for 1st it
  }
  rows
}
get_rows_string("FBFBBFF")

# lazy, copy paste
get_cols <- function(input, r_or_l) {
  stopifnot(r_or_l %in% c("R", "L"))
  midpoint <- length(input) / 2
  if (r_or_l == "L") {
    input[1:midpoint]
  } else {
    input[(midpoint + 1):length(input)]
  }
}
cols %>%
  get_cols("R") %>%
  get_cols("L") %>%
  get_cols("R")

get_cols_string <- function(string) {
  vect <- stringr::str_split(string, "")[[1]]
  for (i in seq_along(vect)) {
    cols <- get_cols(cols, vect[i])
  }
  cols
}
get_cols_string("RLR")

# Part 1  -----------------------------------------------------------------

raw <- readLines("data-raw/day05.txt")

input_rows <- stringr::str_sub(raw, 1, 7) %>%
  map_dbl(get_rows_string)
input_cols <- stringr::str_sub(raw, 8, -1) %>%
  map_dbl(get_cols_string)
input_ids <- (8 * input_rows) + input_cols

max(input_ids)

# Part 2 ------------------------------------------------------------------

ids_full <- min(input_ids):max(input_ids)
setdiff(ids_full, input_ids)
