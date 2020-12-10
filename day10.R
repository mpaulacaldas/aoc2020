library(purrr)

input <- as.numeric(readLines("data-raw/day10.txt"))

# question 1 --------------------------------------------------------------

lead <- c(sort(input), max(input) + 3)
lag  <- c(0, lead[-length(lead)])

diffs <- lead - lag
reduce(table(diffs), `*`)


# question 2 --------------------------------------------------------------

# if the input is valid, return the sorted input, if not, NULL
give_valid_arr <- function(x) {
  x <- sort(x)
  lead <- c(x, max(input) + 3) # max(input), NOT max(x)
  lag  <- c(0, lead[-length(lead)])
  diff <- lead - lag
  if (all(diff %in% 1:3)) x
}

# for a given input, return all combinations that drop one of the numbers, check
# and return the valid sequences
n_valid_arr_rem_1 <- function(x) {
  x %>%
    seq_along() %>%
    map(~ give_valid_arr(x[-.x])) %>%
    compact()
}

reduced <- n_valid_arr_rem_1(input)
count   <- 1 + length(reduced)

while (length(reduced) > 1) {
  reduced <- reduced %>%
    map(n_valid_arr_rem_1) %>%
    unlist(recursive = FALSE) %>%
    unique()
  count <- count + length(reduced)
}

count
