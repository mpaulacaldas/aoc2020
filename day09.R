library(purrr)

input <- as.numeric(readLines("data-raw/day09.txt"))

# question 1 --------------------------------------------------------------

for (i in 26:length(input)) {
  num <- input[i]

  pre <- input[(i-25):(i-1)]
  val <- pre[pre < num] # remove numbers that are bigger than the 26th

  if (length(val) < 2) {
    next
  } else if (length(val) == 2 & sum(val) == input[i]) {
    next
  } else {
    not_sum_to_num <- function(x, y) x + y != input[i]
    valid_combs <- cross2(val, val, .filter = not_sum_to_num)
    if (is_empty(valid_combs)) return(print(num))
    next
  }
}


# question 2 --------------------------------------------------------------


cumsums_ge <- seq_along(input) %>%
  set_names() %>%
  map( ~ accumulate(input[.x:length(input)], pass_num) )
cumsum_ok <- cumsums_ge %>%
  keep(~ max(.x) == num) %>%
  .[1]
id_first <- as.numeric(names(cumsum_ok))
id_last  <- id_first + lengths(cumsum_ok) - 1

sum(input[id_first:id_last]) == num
sum(range(input[id_first:id_last]))
