calculate.matrices <- function(first.sequence, second.sequence) {
   value.matrix <- matrix(ncol=length(first.sequence) + 1, nrow=length(second.sequence) + 1)
   colnames(value.matrix) <- c('*', first.sequence)
   rownames(value.matrix) <- c('*', second.sequence)

   traceback.matrix <- matrix(ncol=length(first.sequence) + 1, nrow=length(second.sequence) + 1)

   matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)
   #TODO calculate traceback matrix by passing in list to all functions

   matrices$value.matrix <- init.first.col.and.row(matrices$value.matrix)
   matrices$value.matrix <- init.non.edge.values(matrices$value.matrix)

   matrices
}

init.first.col.and.row <- function(value.matrix) {
    value.matrix[1,1] <- 0

    for (i in 2:ncol(value.matrix)) {
        value.matrix[1,i] <- i - 1
    }

    for (j in 2:nrow(value.matrix)) {
        value.matrix[j,1] <- j - 1
    }

    value.matrix
}

calculate.value.top <- function(value.matrix, i, j) {
    value.matrix[j-1, i] + 1
}

calculate.value.left <- function(value.matrix, i, j) {
    value.matrix[j, i-1] + 1
}

calculate.value.mismatch <- function(value.matrix, i, j) {
    first.letter <- colnames(value.matrix)[i]
    second.letter <- rownames(value.matrix)[j]
    mismatch.factor <- mm(first.letter, second.letter)

    value.matrix[j-1, i-1] + mismatch.factor
}

calculate.values <- function(value.matrix, i, j) {
  if (i < 2) stop('i must be greater than or equal to 2')
  if (j < 2) stop('j must be greater than or equal to 2')

  c(
    value.top=calculate.value.top(value.matrix, i, j),
    value.left=calculate.value.left(value.matrix, i, j),
    value.mismatch=calculate.value.mismatch(value.matrix, i, j)
  )
}

calculate.traceback.direction <- function(top.left.and.mismatch.values) {
  min.val <- min(top.left.and.mismatch.values)

  min.label <- names(top.left.and.mismatch.values[top.left.and.mismatch.values == min.val])[1]
  # now convert the label to either 'up', 'left' or 'diag'
  if (min.label == 'value.top') {
    'up'
  } else if (min.label == 'value.left') {
    'left'
  } else {
    'diag'
  }
}

init.non.edge.values <- function(value.matrix) {
    for (i in 2:ncol(value.matrix)) {
        for (j in 2:nrow(value.matrix)) {
            values <- calculate.values(value.matrix, i, j)
            value.matrix[j,i] <- min(values)
        }
    }
    value.matrix
}
