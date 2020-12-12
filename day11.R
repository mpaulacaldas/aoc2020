input  <- readr::read_lines(
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
)
# input <- readr::read_lines("data-raw/day11.txt")


# question 1 --------------------------------------------------------------

nat <- mat <- stringr::str_split(input, "", simplify = TRUE)

nat[nat == "L"] <- "#" # to start off the while loop

while(!all(mat == nat)) {
  mat <- nat
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {

      seat <- mat[i, j]
      if (seat == ".") next

      ir <- intersect(i + (-1:1), 1:nrow(mat))
      jr <- intersect(j + (-1:1), 1:ncol(mat))

      adj  <- mat[ir, jr]

      if (seat == "L" && sum(adj == "#") == 0) nat[i, j] <- "#"
      if (seat == "#" && sum(adj == "#") >= 5) nat[i, j] <- "L"
    }
  }
}

sum(nat == "#")

# my hack with the little matrix doesn't work for problem 2, too tired to try
