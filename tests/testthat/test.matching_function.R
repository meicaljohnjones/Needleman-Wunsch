test_that('mm will return 1 when first.letter and second.letter do not match', {
    expect_that(mm('a','b'), equals(1))
})


test_that('mm will return 0 when first.letter and second.letter match', {
    expect_that(mm('a','a'), equals(0))
})

test_that('mm throw error if first.letter is string longer than one', {
    expect_error(mm('aa','b'), 'first.letter arg wrong length - should be single character')
})

test_that('mm throw error if second.letter is string longer than one', {
    expect_error(mm('a','bb'), 'second.letter arg wrong length - should be single character')
})
