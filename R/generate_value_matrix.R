calculate.matrices <- function(first.sequence, second.sequence, mismatch.function, gap) {
   value.matrix <- matrix(ncol=length(first.sequence) + 1, nrow=length(second.sequence) + 1)
   colnames(value.matrix) <- c('*', first.sequence)
   rownames(value.matrix) <- c('*', second.sequence)

   traceback.matrix <- matrix(ncol=length(first.sequence) + 1, nrow=length(second.sequence) + 1)
   colnames(traceback.matrix) <- c('*', first.sequence)
   rownames(traceback.matrix) <- c('*', second.sequence)

   matrices <- list(value.matrix=value.matrix, traceback.matrix=traceback.matrix)

   matrices <- init.first.col.and.row(matrices, gap)
   matrices <- init.non.edge.values(matrices, mismatch.function, gap)

   matrices
}

init.first.col.and.row <- function(matrices, gap) {
    value.matrix <- matrices$value.matrix
    for (i in 1:ncol(value.matrix)) {
        value.matrix[1,i] <- i * gap
    }

    for (j in 2:nrow(value.matrix)) {
        value.matrix[j,1] <- j * gap
    }

    matrices$value.matrix <- value.matrix

    traceback.matrix <- matrices$traceback.matrix
    traceback.matrix[1,1] <- 'done'

    for (i in 2:ncol(traceback.matrix)) {
      traceback.matrix[1,i] <- 'left'
    }

    for (j in 2:nrow(traceback.matrix)) {
      traceback.matrix[j,1] <- 'up'
    }

    matrices$traceback.matrix <- traceback.matrix
    matrices
}

calculate.value.top <- function(value.matrix, i, j, gap) {
    value.matrix[j-1, i] + gap
}

calculate.value.left <- function(value.matrix, i, j, gap) {
    value.matrix[j, i-1] + gap
}

calculate.value.mismatch <- function(value.matrix, i, j, mismatch.function) {
    first.letter <- colnames(value.matrix)[i]
    second.letter <- rownames(value.matrix)[j]
    mismatch.factor <- mismatch.function(first.letter, second.letter)

    value.matrix[j-1, i-1] + mismatch.factor
}

calculate.values <- function(value.matrix, i, j, mismatch.function, gap) {
  if (i < 2) stop('i must be greater than or equal to 2')
  if (j < 2) stop('j must be greater than or equal to 2')

  c(
    value.top=calculate.value.top(value.matrix, i, j, gap),
    value.left=calculate.value.left(value.matrix, i, j, gap),
    value.mismatch=calculate.value.mismatch(value.matrix, i, j, mismatch.function)
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

init.non.edge.values <- function(matrices, mismatch.function, gap) {
    value.matrix <- matrices$value.matrix
    traceback.matrix <- matrices$traceback.matrix

    for (i in 2:ncol(value.matrix)) {
        for (j in 2:nrow(value.matrix)) {
            values <- calculate.values(value.matrix, i, j, mismatch.function, gap)
            value.matrix[j,i] <- min(values)
            traceback.matrix[j,i] <- calculate.traceback.direction(values)
        }
    }

    matrices$value.matrix <- value.matrix
    matrices$traceback.matrix <- traceback.matrix

    matrices
}
