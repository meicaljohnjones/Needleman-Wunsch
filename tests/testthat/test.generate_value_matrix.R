test_that('calculate.matrices generates value matrix with length(first.sequence) + 1 columns and length(second.sequence) + 1 rows and traceback matrix of same size both in a list', {
    first.sequence <- c('a','a')
    second.sequence <- c('b','b')
    mismatch.function <- function(first.letter, second.letter) 0

    output.matrices <- calculate.matrices(first.sequence, second.sequence, mismatch.function, gap=1)

    value.matrix <- output.matrices$value.matrix
    expect_that(value.matrix, not(is_null()))

    traceback.matrix <- output.matrices$traceback.matrix
    expect_that(traceback.matrix, not(is_null()))

    expect_that(ncol(value.matrix), equals(length(first.sequence) + 1))
    expect_that(nrow(value.matrix), equals(length(second.sequence) + 1))

    expect_that(ncol(traceback.matrix), equals(length(first.sequence) + 1))
    expect_that(nrow(traceback.matrix), equals(length(second.sequence) + 1))
})

test_that('calculate.matrices labels col names using first.sequence and row names using second sequence on value.matrix and traceback.matrix, however, prepending an asterisk element row and column', {
    first.sequence <- c('a','a')
    second.sequence <- c('b','b')
    mismatch.function <- function(first.letter, second.letter) 0

    expected.cols <- c('*', first.sequence)
    expected.rows <- c('*', second.sequence)

    output.matrices <- calculate.matrices(first.sequence, second.sequence, mismatch.function, gap=1)
    value.matrix <- output.matrices$value.matrix
    traceback.matrix <- output.matrices$traceback.matrix

    expect_that(expected.cols, equals(colnames(value.matrix)))
    expect_that(expected.rows, equals(rownames(value.matrix)))

    expect_that(expected.cols, equals(colnames(traceback.matrix)))
    expect_that(expected.rows, equals(rownames(traceback.matrix)))
})

test_that('init.first.col.and.row fills value matrix first row and column correctly where gap = 1', {
    # given
    nrow <- 5
    ncol <- 6
    gap <- 1
    value.matrix <- matrix(data = NA, nrow=nrow, ncol=ncol)
    traceback.matrix <- matrix(data = NA, nrow=nrow, ncol=ncol) # empty as we're not testing this
    matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)

    out.matrices <- init.first.col.and.row(matrices, gap)
    expect_that(typeof(out.matrices), equals("list"))
    out.matrices

    out.value.matrix <- out.matrices$value.matrix
    expect_that(out.value.matrix, not(is_null()))


    # test col values correct
    for (x in 1:ncol) { # 2 because first value is 'done'
        expect_that(out.value.matrix[1,x], equals(x * gap))
    }

    # test row values correct
    for (y in 1:nrow) {
        expect_that(out.value.matrix[y,1], equals(y * gap))
    }
})

test_that('init.first.col.and.row fills value matrix first row and column correctly where gap = 2', {
  # given
  nrow <- 5
  ncol <- 6
  gap <- 2
  value.matrix <- matrix(data = NA, nrow=nrow, ncol=ncol)
  traceback.matrix <- matrix(data = NA, nrow=nrow, ncol=ncol) # empty as we're not testing this
  matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)

  out.matrices <- init.first.col.and.row(matrices, gap)
  expect_that(typeof(out.matrices), equals("list"))
  out.matrices

  out.value.matrix <- out.matrices$value.matrix
  expect_that(out.value.matrix, not(is_null()))


  # test col values correct
  for (x in 1:ncol) { # 2 because first value is 'done'
    expect_that(out.value.matrix[1,x], equals(x * gap))
  }

  # test row values correct
  for (y in 1:nrow) {
    expect_that(out.value.matrix[y,1], equals(y * gap))
  }
})

test_that('init.first.col.and.row fills traceback matrix first row and column correctly', {
  # given
  nrow <- 5
  ncol <- 6
  traceback.matrix <- matrix(data = NA, nrow=nrow, ncol=ncol)
  value.matrix <- matrix(data = NA, nrow=nrow, ncol=ncol) # empty as we're not testing this
  matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)

  # when
  out.matrices <- init.first.col.and.row(matrices, gap=1)

  # then
  expect_that(typeof(out.matrices), equals("list"))
  out.matrices

  out.traceback.matrix <- out.matrices$traceback.matrix
  expect_that(out.traceback.matrix, not(is_null()))

  expect_that(out.traceback.matrix[1,1], equals("done"))

  # test col values correct
  for (x in 2:ncol) {
    expect_that(out.traceback.matrix[1,x], equals("left"))
  }

  # test row values correct
  for (y in 2:nrow) {
    expect_that(out.traceback.matrix[y,1], equals("up"))
  }
})

test_that('calculate.values should return vector(value.top, value.left, value.mismatch)', {
  my.matrix <- matrix()
  mm <- function(first.letter, second.letter) 0
  gap <- 1

  with_mock(
    calculate.value.top = function(value.matrix, i, j, gap) { 20 },
    calculate.value.left = function(value.matrix, i, j, gap) { 10 },
    calculate.value.mismatch = function(value.matrix, i, j, mismatch.function) { 5 },
    {
      values <- calculate.values(my.matrix, 2, 2, mm, gap)
      expect_that(values, equals(c(value.top=20,value.left=10,value.mismatch=5)))
    }
  )
})

test_that('calculate.values throws exception when i or j are less than 2', {
    value.matrix <- matrix()
    mm <- function(first.letter, second.letter) 0
    gap <- 1
    expect_error(calculate.values(value.matrix, 1, 2, mm, gap),
                 'i must be greater than or equal to 2')
    expect_error(calculate.values(value.matrix, 2, 1, mm, gap),
                 'j must be greater than or equal to 2')
})

test_that('calculate.value.top returns value at coordinate [i,j-1] + gap', {
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    i <- 3
    j <- 3
    gap <- 5
    expected.output <- value.matrix[j-1, i] + gap

    expect_that(calculate.value.top(value.matrix, i, j, gap), equals(expected.output))
})

test_that('calculate.value.left returns value at coordinate [i-1,j] + gap', {
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    i <- 3
    j <- 3
    gap <- 4
    expected.output <- value.matrix[j, i-1] + gap

    expect_that(calculate.value.left(value.matrix, i, j, gap), equals(expected.output))
})


test_that('calculate.value.mismatch adds one to [i-1,j-1] when first.letter != second.letter', {
    # given
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    colnames(value.matrix) <- LETTERS[1:3]
    rownames(value.matrix) <- LETTERS[1:3]
    mm <- function(first.letter, second.letter) 1

    # then
    expect_that(calculate.value.mismatch(value.matrix,3,3, mm), equals(5 + 1))
    expect_that(calculate.value.mismatch(value.matrix,3,2, mm), equals(2 + 1))
})


test_that('calculate.value.mismatch returns value of [i-1,j-1] when first.letter == second.letter', {
    # given
    value.matrix <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=T)
    colnames(value.matrix) <- LETTERS[1:3]
    rownames(value.matrix) <- LETTERS[1:3]
    mm <- function(first.letter, second.letter) 0

    # when
    expect_that(calculate.value.mismatch(value.matrix,3,3, mm), equals(5))
    expect_that(calculate.value.mismatch(value.matrix,3,2, mm), equals(2))
})

test_that('calculate.traceback.direction returns "up" string when value.top is largest in top.left.and.mismatch.values vector', {
    top.left.and.mismatch.values <- c(value.top=6,value.left=4,value.mismatch=5)
    result <- calculate.traceback.direction(top.left.and.mismatch.values)

    expect_that(result, equals('up'))
})

test_that('calculate.traceback.direction returns "left" when value.left is largest in top.left.and.mismatch.values vector', {
  top.left.and.mismatch.values <- c(value.top=5,value.left=6,value.mismatch=4)
  result <- calculate.traceback.direction(top.left.and.mismatch.values)

  expect_that(result, equals('left'))
})

test_that('calculate.traceback.direction returns "diag" when value.mismatch is largest in top.left.and.mismatch.values vector', {
  top.left.and.mismatch.values <- c(value.top=5,value.left=4,value.mismatch=6)
  result <- calculate.traceback.direction(top.left.and.mismatch.values)

  expect_that(result, equals('diag'))
})

test_that('init.non.edge.values calls calculate.value on non edge values in value.matrix', {
    value.matrix <- matrix(data=-1, nrow=3, ncol=3, byrow=T)
    traceback.matrix <- matrix(data=-1, nrow=3, ncol=3, byrow=T) # not used in this test
    matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)
    mismatch.function <- function() 0
    gap <- 1

    with_mock(calculate.values = function(value.matrix, i, j, mismatch.function, gap) { c(4, 1, 5) },
              calculate.traceback.direction = function(top.left.and.mismatch.values) { "test_passed" }, {
        out.matrices <- init.non.edge.values(matrices, mismatch.function, gap)
        expect_that(out.matrices, not(is_null()))
        out.value.matrix <- out.matrices$value.matrix

        expect_that(out.value.matrix[1,1], equals(-1))
        expect_that(out.value.matrix[1,2], equals(-1))
        expect_that(out.value.matrix[1,3], equals(-1))
        expect_that(out.value.matrix[2,1], equals(-1))
        expect_that(out.value.matrix[3,1], equals(-1))

        # expect that we're returning the largest value
        expect_that(out.value.matrix[2,2], equals(5))
        expect_that(out.value.matrix[2,3], equals(5))
        expect_that(out.value.matrix[3,2], equals(5))
        expect_that(out.value.matrix[3,3], equals(5))
    })
})

test_that('init.non.edge.values calls calculate.traceback.direction on non edge values in traceback.matrix', {
  value.matrix <- matrix(data=-1, nrow=3, ncol=3, byrow=T) # not used in this test
  traceback.matrix <- matrix(data="", nrow=3, ncol=3, byrow=T)
  matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)
  mismatch.function <- function() 0
  gap <- 1

  # will fill all non-edge with 'test_passed'
  with_mock(calculate.traceback.direction = function(top.left.and.mismatch.values) { "test_passed" },
            calculate.values = function(value.matrix, i, j, mismatch.function, gap) { c(value.top=4, value.left=1, value.mismatch=5) }, {
        out.matrices <- init.non.edge.values(matrices, mismatch.function, gap)
        expect_that(out.matrices, not(is_null()))
        out.traceback.matrix <- out.matrices$traceback.matrix

        expect_that(out.traceback.matrix[2,2], equals("test_passed"))
        expect_that(out.traceback.matrix[2,3], equals("test_passed"))
        expect_that(out.traceback.matrix[3,2], equals("test_passed"))
        expect_that(out.traceback.matrix[3,3], equals("test_passed"))
  })

})

test_that('calculate.matrices calls init.first.col.and.row', {
    mismatch.function <- function(first.letter, second.letter) 0
    gap <- 1
    with_mock(
              init.first.col.and.row = function(matrices, gap) {
                list(value.matrix=matrix(data="pass", nrow=5,ncol=5), traceback.matrix=matrix())
              },
              #stop init.non.edge.values from modifying value.matrix
              init.non.edge.values = function(value.matrix, mismatch.function, gap) value.matrix,
              {
                  out.matrices <- calculate.matrices(c('a','a'), c('a','t'), mismatch.function, gap=1)
                  expect_that(out.matrices$value.matrix[1,1], equals("pass"))
              }
    )
})


test_that('calculate.matrices calls init.non.edge.values', {
    mismatch.function <- function(first.letter, second.letter) 0
    gap <- 1
    with_mock(
              #stop init.first.col.and.row from modifying value.matrix
              init.first.col.and.row = function(matrices, gap) matrices,
              init.non.edge.values = function(matrices, mismatch.function, gap) list(value.matrix=matrix(data="pass", nrow=5,ncol=5), traceback.matrix=matrix()),
              {
                  out.matrices <- calculate.matrices(c('a','a'), c('a','t'), mismatch.function, gap=1)
                  expect_that(out.matrices$value.matrix[1,1], equals("pass"))
              }
    )
})
