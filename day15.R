
raw <- readr::read_lines("data-raw/day15.txt", n_max = 1)
# raw <- "0,3,6"
input <- as.numeric(strsplit(raw, ",")[[1]])


# part 1 ------------------------------------------------------------------


hist <- vector("logical", length = 2020)
hist[-seq_along(input)] <- NA
hist[seq_along(input)]  <- input

turn <- length(input) + 1

while (any(is.na(hist))) {
  if (!hist[turn - 1] %in% hist[1:(turn - 2)]) {
    hist[turn] <- 0
  } else {
    hist[turn] <- (turn - 1) - max(which(hist[turn - 1] == hist[1:(turn - 2)]))
  }
  turn <- turn + 1
}

hist[length(hist)]


# part 2 ------------------------------------------------------------------

# brute force idea, doesn't work, need to hash it
prev <- hist

hist <- vector("logical", length = 30000000)
hist[-seq_along(prev)] <- NA
hist[seq_along(prev)]  <- prev

left <- (length(prev) + 1):length(hist)

for (turn in left) {
  prev <- hist[1:(turn - 2)]
  last <- hist[turn - 1]
  if (last %in% prev) {
    hist[turn] <- (turn - 1) - max(which(last == prev))
  } else {
    hist[turn] <- 0
  }
}
