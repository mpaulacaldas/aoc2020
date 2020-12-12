library(stringr)

# input <- readr::read_lines(
# "F10
# N3
# F7
# R90
# F11"
# )
input <- readr::read_lines("data-raw/day12.txt")

# question 1 --------------------------------------------------------------

s <- c(x = 0, y = 0)
d <- "E"

for (i in seq_along(input)) {

  instr <- input[i]
  action <- str_sub(instr, 1, 1)
  value  <- str_sub(instr, 2, -1) %>% as.numeric()

  if (action %in% c("L", "R")) {
    d <- switch(
      instr,
      "L90"  = , "R270" = switch(d, "E" = "N", "N" = "W", "W" = "S", "S" = "E"),
      "R90"  = , "L270" = switch(d, "E" = "S", "S" = "W", "W" = "N", "N" = "E"),
      "R180" = , "L180" = switch(d, "E" = "W", "W" = "E", "N" = "S", "S" = "N")
    )
  } else {
    action <- switch(action, "F" = d, action)
    s <- switch(
      action,
      "E" = s + (value * c(1, 0)),
      "W" = s + (value * c(-1, 0)),
      "N" = s + (value * c(0, 1)),
      "S" = s + (value * c(0, -1))
    )
  }
}

sum(abs(s))


# question 2 --------------------------------------------------------------

s <- c(x = 0, y = 0)
w <- c(x = 10, y = 1)

d <- "E"

for (i in seq_along(input)) {

  instr <- input[i]
  action <- str_sub(instr, 1, 1)
  value  <- str_sub(instr, 2, -1) %>% as.numeric()

  if (action %in% c("L", "R")) {
    w <- switch(
      instr,
      "R90"  = , "L270" = rev(w) * c(1, -1),
      "R180" = , "L180" = -w,
      "R270" = , "L90"  = rev(w) * c(-1, 1),
    )
  } else if (action == "F") {
    s <- s + (w * value)
  } else {
    w <- switch(
      action,
      "E" = w + (value * c(1, 0)),
      "W" = w + (value * c(-1, 0)),
      "N" = w + (value * c(0, 1)),
      "S" = w + (value * c(0, -1))
      )
  }
}

sum(abs(s))


# SMARTER -----------------------------------------------------------------

# Using cos() and pi to rotate, assignment within switch:
# https://twitter.com/Emil_Hvitfeldt/status/1337633974168875009/photo/2
