test_that('calculate.value.matrix generates matrix with length(first.sequence) columns and length(second.sequence) rows', {
    first.sequence <- c('a','a')
    second.sequence <- c('b','b')

    output.matrix <- calculate.value.matrix(first.sequence, second.sequence)

    expect_that(ncol(output.matrix), equals(length(first.sequence)))
    expect_that(nrow(output.matrix), equals(length(second.sequence)))
})

test_that('calculate.value.matrix labels col names using first.sequence and row names using second sequence', {
    first.sequence <- c('a','a')
    second.sequence <- c('b','b')

    output.matrix <- calculate.value.matrix(first.sequence, second.sequence)
    expect_that(first.sequence, equals(colnames(output.matrix)))
    expect_that(second.sequence, equals(rownames(output.matrix)))
})
