test_that('calculate.alignment.step returns seq1.letter and seq2.letter when traceback.direction is "diag"', {
  # given
  seq1.letter <- 'S'
  seq2.letter <- 'A'
  traceback.direction <- "diag"

  # when
  result <- calculate.alignment.step(seq1.letter, seq2.letter, traceback.direction)

  # then
  expect_that(typeof(result), equals("list"))
  expect_that(result$alignment1, equals(seq1.letter))
  expect_that(result$alignment2, equals(seq2.letter))
})

test_that('calculate.alignment.step returns seq1.letter and "_" when traceback.direction is "left"', {
  # given
  seq1.letter <- 'T'
  seq2.letter <- 'C'
  traceback.direction <- "left"

  # when
  result <- calculate.alignment.step(seq1.letter, seq2.letter, traceback.direction)

  # then
  expect_that(typeof(result), equals("list"))
  expect_that(result$alignment1, equals(seq1.letter))
  expect_that(result$alignment2, equals("_"))
})

test_that('calculate.alignment.step returns "_" and seq2.letter when traceback.direction is "up"', {
  # given
  seq1.letter <- 'T'
  seq2.letter <- 'C'
  traceback.direction <- "up"

  # when
  result <- calculate.alignment.step(seq1.letter, seq2.letter, traceback.direction)

  # then
  expect_that(typeof(result), equals("list"))
  expect_that(result$alignment1, equals("_"))
  expect_that(result$alignment2, equals(seq2.letter))
})

test_that('calculate.alignment.step throws error when traceback.direction is not "up", "left" or "diag"', {
  # given
  seq1.letter <- 'T'
  seq2.letter <- 'C'
  traceback.direction <- "foobar"

  expect_error(calculate.alignment.step(seq1.letter, seq2.letter, traceback.direction))
})

test_that('calculate.alignment builds perfect alignment', {
  # given
  traceback.matrix <- matrix(data=c("done", "up", "up", "up", "left", "diag", "up", "up", "left", "left", "diag", "up", "left", "left", "diag", "up", "left", "left", "left", "diag"), nrow=4, ncol=5)
  dimnames(traceback.matrix)[[2]] <- c("*", "S", "E", "N", "D") # set column names
  dimnames(traceback.matrix)[[1]] <- c("*", "A", "N", "D") # set row names
  matrices <- list(value.matrix=matrix(), traceback.matrix=traceback.matrix)

  # when
  result <- calculate.alignment(matrices)
  expect_that(result, not(is_null()))
  expect_that(typeof(result), equals("list"))

  expect_that(result$alignment1, equals(c("S","E", "N", "D")))
  expect_that(result$alignment2, equals(c("A","_", "N", "D")))

})

test_that('calculate.maxscore yields the bottom-right value of the value.matrix from matrices', {
  # given
  value.matrix <- matrix(data=1:9, nrow=3, ncol=3)
  traceback.matrix <- matrix() # not needed
  matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)

  # when
  result <- calculate.maxscore(matrices)

  # then
  expect_that(is.integer(result), equals(TRUE))
  expect_that(result, equals(9)) # bottom right value
})
