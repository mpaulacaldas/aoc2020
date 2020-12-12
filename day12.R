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

x <- 0
y <- 0
d <- "E"

for (i in seq_along(input)) {

  instr <- input[i]
  action <- str_sub(instr, 1, 1)
  value  <- str_sub(instr, 2, -1) %>% as.numeric()

  if (action %in% c("L", "R")) {
    d <- switch(
      instr,
      "L90"  = ,
      "R270" = switch(d, "E" = "N", "N" = "W", "W" = "S", "S" = "E"),
      "R90"  = ,
      "L270" = switch(d, "E" = "S", "S" = "W", "W" = "N", "N" = "E"),
      "R180" = ,
      "L180" = switch(d, "E" = "W", "W" = "E", "N" = "S", "S" = "N")
    )
  } else {
    action <- switch(action, "F" = d, action)
    x <- switch(action, "E" = x + value, "W" = x - value, x)
    y <- switch(action, "N" = y + value, "S" = y - value, y)
  }
}

sum(abs(c(x, y)))


# question 2 --------------------------------------------------------------

s_x <- 0
s_y <- 0
d <- "E"

w_x <- 10
w_y <- 1

for (i in seq_along(input)) {

  instr <- input[i]
  action <- str_sub(instr, 1, 1)
  value  <- str_sub(instr, 2, -1) %>% as.numeric()

  if (action %in% c("L", "R")) {
    # too convoluted
    o_x <- w_x
    o_y <- w_y
    w_x <- switch(
      instr,
      "R90"  = , "L270" =  o_y,
      "R180" = , "L180" = -o_x,
      "R270" = , "L90"  = -o_y,
    )
    w_y <- switch(
      instr,
      "R90"  = , "L270" = -o_x,
      "R180" = , "L180" = -o_y,
      "R270" = , "L90"  =  o_x,
    )
  } else if (action == "F") {
    s_x <- s_x + (w_x * value)
    s_y <- s_y + (w_y * value)
  } else {
    w_x <- switch(action, "E" = w_x + value, "W" = w_x - value, w_x)
    w_y <- switch(action, "N" = w_y + value, "S" = w_y - value, w_y)
  }
}

sum(abs(c(s_x, s_y)))
