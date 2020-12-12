aoc_edit <- function() {
  day <- format(Sys.Date(), "%d")
  fs  <- sprintf(c("day%s.R", "data-raw/day%s.txt"), day)
  file.edit(fs)
}
