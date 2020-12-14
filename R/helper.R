aoc_edit <- function() {
  day <- format(Sys.Date(), "%d")
  fs  <- sprintf(c("day%s.R", "data-raw/day%s.txt"), day)
  file.edit(fs)
}

aoc_fetch <- function() {
  # https://colin-fraser.net/post/a-quick-tutorial-on-importing-data-from-advent-of-code-into-r/
  day <- format(Sys.Date(), "%d")
  url <- sprintf("https://adventofcode.com/2020/day/%s/input", day)

  session_cookie <- keyring::key_get(
    "RStudio Keyring Secrets",
    "AOC 2020 session cookie"
    )
  cookie <- httr::set_cookies(session = session_cookie)

  res <- httr::GET(url, cookie)
  txt <- httr::content(res, as = "text", encoding = "UTF-8")

  dr  <- sprintf("data-raw/day%s.txt", day)
  writeChar(txt, dr)
}
