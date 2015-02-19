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

test_that('calculate.value throws exception when i or j are less than 2', {
    value.matrix <- matrix()
    expect_error(calculate.value(value.matrix, 1, 2),
                 'i must be greater than or equal to 2')
    expect_error(calculate.value(value.matrix, 2, 1),
                 'j must be greater than or equal to 2')
})

test_that('calculate value returns mistmatch value when is smallest', {
    value.matrix <- NULL

    with_mock(
        calculate.value.top = function(value.matrix, i, j) 20,
        calculate.value.left = function(value.matrix, i, j) 10,
        calculate.value.mismatch = function(value.matrix, i, j) 5,
        expect_that(calculate.value(value.matrix, 2, 2), equals(5))
    )
})

test_that('calculate.value.top returns value at coordinate [i,j-1]', {
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    i <- 3
    j <- 3
    expected.output <- value.matrix[j-1, i]

    expect_that(calculate.value.top(value.matrix, i, j), equals(expected.output))
})

test_that('calculate.value.left returns value at coordinate [i-1,j]', {
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    i <- 3
    j <- 3
    expected.output <- value.matrix[j, i-1]

    expect_that(calculate.value.left(value.matrix, i, j), equals(expected.output))
})


test_that('calculate.value.mismatch adds one to [i-1,j-1] when first.letter != second.letter', {
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    colnames(value.matrix) <- LETTERS[1:3]
    rownames(value.matrix) <- LETTERS[1:3]

    with_mock(mm = function(first.letter, second.letter) 1, {
        expect_that(calculate.value.mismatch(value.matrix,3,3), equals(5 + 1))
        expect_that(calculate.value.mismatch(value.matrix,3,2), equals(2 + 1))
    })
})


test_that('calculate.value.mismatch returns value of [i-1,j-1] when first.letter == second.letter', {
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    colnames(value.matrix) <- LETTERS[1:3]
    rownames(value.matrix) <- LETTERS[1:3]

    with_mock(mm = function(first.letter, second.letter) 0, {
        expect_that(calculate.value.mismatch(value.matrix,3,3), equals(5))
        expect_that(calculate.value.mismatch(value.matrix,3,2), equals(2))
    })
})
