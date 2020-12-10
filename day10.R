library(purrr)

input <- as.numeric(readLines("data-raw/day10.txt"))

# question 1 --------------------------------------------------------------

lead <- c(sort(input), max(input) + 3)
lag  <- c(0, lead[-length(lead)])

diffs <- lead - lag
reduce(table(diffs), `*`)


# question 2, brute force, TERRIBLE IDEA ----------------------------------

# will take forever to run with the actual input
input <- c(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
           38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)

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

count == 19208


# question 2, using prob --------------------------------------------------
# https://twitter.com/antoine_fabri/status/1336958852491841537/photo/1

diffs

(rle_all <- rle(diffs))                         # length of runs
(rle_1s <- with(rle_all, lengths[values == 1])) # only care about the 1s

range(diffs) # there are no 2s, we only care about the permutations of 1s
max(rle_1s)  # max run is of 4

# one 1 can't be moved
# two 1s can be expressed two ways
# three 1s can be expressed 4 ways
# four 1s can be expressed 7 ways
multilplier <- c(1, 2, 4, 7)

scales::number(prod(multilplier[rle_1s]), big.mark = "")
