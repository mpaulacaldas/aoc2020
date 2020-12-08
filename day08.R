input <- read.delim(
  "data-raw/day08.txt",
  sep = " ", col.names = c("operation", "argument")
  )

# LESSON: Don't use read.delim()!!!!! Reads the argument column as integer,
# which SOMEHOW gives 1462 as the answer. readr::read_delim() reads a numerical,
# that correctly returns 1475.
input <- readr::read_delim(
  "data-raw/day08.txt",
  delim = " ", col_names = c("operation", "argument")
  )

# question 1 --------------------------------------------------------------

input$executed <- FALSE

accu <- 0
iter <- 1

while (!input$executed[iter]) {

  input$executed[iter] <- TRUE

  step <- switch(input$operation[iter], "jmp" = input$argument[iter], 1)
  plus <- switch(input$operation[iter], "acc" = input$argument[iter], 0)

  accu <- accu + plus
  iter <- iter + step
}

accu

# question 2 --------------------------------------------------------------

input$executed <- FALSE

nop_or_jmp <- which(input$operation %in% c("nop", "jmp"))

for (i in nop_or_jmp) {

  input2 <- as.data.frame(input)
  input2[i, "operation"][[1]] <- switch(
    input2[i, "operation"][[1]],
    "jmp" = "nop",
    "nop" = "jmp"
    )

  accu <- 0
  iter <- 1

  while (!input2$executed[iter]) {

    input2$executed[iter] <- TRUE

    step <- switch(input2$operation[iter], "jmp" = input2$argument[iter], 1)
    plus <- switch(input2$operation[iter], "acc" = input2$argument[iter], 0)

    accu <- accu + plus

    if (iter == nrow(input)) return(print(accu))
    iter <- iter + step
  }

}
