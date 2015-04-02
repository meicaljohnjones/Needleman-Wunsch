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
