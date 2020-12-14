library(stringr)
library(purrr)

# input <- readr::read_lines(
# "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
# mem[8] = 11
# mem[7] = 101
# mem[8] = 0"
# )


input <- readr::read_lines("data-raw/day14.txt")
input <- input[-length(input)] # remove empty line

# question 1 --------------------------------------------------------------

parse_mask <- function(x) {
  s <- str_remove(x, "mask = ")
  v <- str_split(s, "")[[1]]
  v[v == "X"] <- NA
  as.numeric(v)
}
parse_bits <- function(x) {
  v <- str_extract_all(x, "\\d+")[[1]]

  b <- strtoi(rev(intToBits(v[2])), base = 2)
  r <- c(rep(0, times = 36 - length(b)), b) # make length 36

  list(address = v[1], bit = r)
}


memory <- list()
for (i in seq_along(input)) {
  if (str_starts(input[i], "mask")) {
    mask <- parse_mask(input[i])
    next
  } else {
    info <- parse_bits(input[i])
    memory[[info$address]] <- dplyr::coalesce(mask, info$bit)
  }
}

memory %>%
  map_chr(paste0, collapse = "") %>%
  Rmpfr::mpfr(base = 2) %>% # strtoi() won't work on 64-bits
  sum()


# question 2 --------------------------------------------------------------

# complete mess...

parse_bits2 <- function(x) {
  v <- str_extract_all(x, "\\d+")[[1]]
  b <- strtoi(rev(intToBits(v[1])), base = 2)
  r <- c(rep(0, times = 36 - length(b)), b) # make length 36
  list(address = r, val = v[2])
}

memory <- list()
for (i in seq_along(input)) {
  if (str_starts(input[i], "mask")) {
    mask <- parse_mask(input[i])
    next
  } else {
    info <- parse_bits2(input[i])
    new <- mask + info$address
    new[new == 2] <- 1
    memory[[i]] <- list(address = new, val = info$val)
  }
}

comb_01 <- function(n_na) {
  l01 <- rep(list(0:1), n_na)
  c01 <- purrr::cross(l01)
  purrr::map(c01, reduce, c) # collapse into vector
}

replace_na_ <- function(val, rep) {
  val[is.na(val)] <- rep
  val
}
replace_na <- function(rep, val) {
  purrr::map(rep, ~ replace_na_(val, .x))
}

memory <- compact(memory)

na_replacements <- memory %>%
  map("address") %>%
  map(~ sum(is.na(.x))) %>%
  map(~ rep(list(0:1), .x)) %>%
  map(cross) %>%
  map_depth(2, reduce, c)

addresses <- na_replacements %>%
  map2(map(memory, "address"), replace_na) %>%
  map_depth(2, paste0, collapse = "") %>%
  map_depth(2, Rmpfr::mpfr, base = 2) %>%
  map_depth(2, as.numeric) %>%
  map(reduce, c)

tibble::tibble(
  round   = seq_along(addresses),
  address = addresses,
  value = map_chr(memory, "val")
  ) %>%
  tidyr::unnest(address) %>%
  dplyr::group_by(address) %>%
  dplyr::filter(round == max(round)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(sum(as.numeric(value)))

