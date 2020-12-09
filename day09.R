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

num

# TODO: doesn't work, look later
pass_num <- function(x, y) {
  if (x + y > num) return(done())
  x + y
}

for (i in seq_along(input)) {
  acc <- accumulate(input[i:length(input)], pass_num)
  if (max(acc) < num) next
  if (max(acc) == num){
    return(input[i] + input[i + length(acc)])
  }
}
