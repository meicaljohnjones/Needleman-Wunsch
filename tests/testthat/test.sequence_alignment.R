test_that('calculate.value.matrix generates matrix with length(first.sequence) + 1 columns and length(second.sequence) + 1 rows', {
    first.sequence <- c('a','a')
    second.sequence <- c('b','b')

    output.matrix <- calculate.value.matrix(first.sequence, second.sequence)

    expect_that(ncol(output.matrix), equals(length(first.sequence) + 1))
    expect_that(nrow(output.matrix), equals(length(second.sequence) + 1))
})

test_that('calculate.value.matrix labels col names using first.sequence and row names using second sequence, however, prepending an asterisk element row and column', {
    first.sequence <- c('a','a')
    second.sequence <- c('b','b')

    expected.cols <- c('*', first.sequence)
    expected.rows <- c('*', second.sequence)

    output.matrix <- calculate.value.matrix(first.sequence, second.sequence)
    expect_that(expected.cols, equals(colnames(output.matrix)))
    expect_that(expected.rows, equals(rownames(output.matrix)))
})

test_that('init.first.col.and.row fills matrix first row and column correctly', {
    # given
    nrow <- 5
    ncol <- 6
    in.matrix <- matrix(data = NA, nrow=nrow, ncol=ncol)

    in.matrix <- init.first.col.and.row(in.matrix)

    # test col values correct
    for (x in 1:nrow) {
        expect_that(in.matrix[1,x], equals(x - 1))
    }

    # test row values correct
    for (y in 1:nrow) {
        expect_that(in.matrix[y,1], equals(y - 1))
    }
})
