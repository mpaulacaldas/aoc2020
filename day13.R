library(tidyverse)

input <- readr::read_lines("data-raw/day13.txt")

# question 1 --------------------------------------------------------------

tibble(
  timestamp = as.numeric(input[1]),
  bus = str_split(input[2], ",")[[1]] %>% {.[. != "x"]} %>% as.numeric()
  ) %>%
  mutate(
    last_one = timestamp %% bus,
    next_one = bus - last_one
    ) %>%
  filter(next_one == min(next_one)) %>%
  summarise(bus * next_one)


# question 2 (ideas, not solved) ------------------------------------------

# my brute force approach, works for toy examples, not for the actual input
pat <- "67,7,x,59,61" %>% str_split(",") %>% .[[1]]
bus <- as.numeric(pat[pat != "x"])
off <- seq(0, by = 1, length.out = length(pat))[pat != "x"]

all_zero <- FALSE
start    <- ((8e14 %/% bus[1]) + 1 ) * bus[1]

while (!all_zero) {
  tms <- start + off
  all_zero <- all(tms %% bus == 0)
  if (all_zero) return(print( formatC(start, digits = 16) ))
  start <- start + bus[1] # jumps equal to first number
}


# smart thing is to solve using the chinese remainder theorem
# https://www.reddit.com/r/adventofcode/comments/kcb3bb/2020_day_13_part_2_can_anyone_tell_my_why_this/
#
# but can't be implemented with the numbers package, because of an approximation
# issue
# https://github.com/rundel/advent_of_code_2020/blob/master/day13/day13.R
pat <- input[2] %>% str_split(",") %>% .[[1]]
bus <- as.numeric(pat[pat != "x"])
off <- seq(0, by = 1, length.out = length(pat))[pat != "x"]

numbers::chinese(
  -off, # remainders
  bus   # primes
  ) %>%
  scales::number(big.mark = "") # too high

# can't believe this worked...
numbers::chinese(-off, bus) %>%
  { . - (. %% bus[1]) } %>% # remove the residual
  scales::number(big.mark = "")
