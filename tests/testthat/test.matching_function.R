test_that('mm will return mismatch.score when first.letter and second.letter do not match', {
  # given
  match.score = 0
  mismatch.score = 1
  mm <- create.mm.function(match.score=match.score, mismatch.score=mismatch.score)

  # when
  result <- mm('a','b')

  # then
  expect_that(result, equals(mismatch.score))
})


test_that('mm will return match.score when first.letter and second.letter match', {
  # given
  match.score = 0
  mismatch.score = 1
  mm <- create.mm.function(match.score=match.score, mismatch.score=mismatch.score)

  # when
  result <- mm('a','a')

  # then
  expect_that(result, equals(match.score))
})

test_that('mm throw error if first.letter is string longer than one', {
  # given
  match.score = 0
  mismatch.score = 1
  mm <- create.mm.function(match.score=match.score, mismatch.score=mismatch.score)

  # when
  expect_error(mm('aa','b'), 'first.letter arg wrong length - should be single character')
})

test_that('mm throw error if second.letter is string longer than one', {
  # given
  match.score = 0
  mismatch.score = 1
  mm <- create.mm.function(match.score=match.score, mismatch.score=mismatch.score)

  # when
  expect_error(mm('a','bb'), 'second.letter arg wrong length - should be single character')
})
