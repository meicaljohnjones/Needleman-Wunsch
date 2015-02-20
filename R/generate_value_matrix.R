calculate.value.matrix <- function(first.sequence, second.sequence) {
   value.matrix <- matrix(ncol=length(first.sequence) + 1, nrow=length(second.sequence) + 1)
   colnames(value.matrix) <- c('*', first.sequence)
   rownames(value.matrix) <- c('*', second.sequence)

   value.matrix <- init.first.col.and.row(value.matrix)
   value.matrix <- init.non.edge.values(value.matrix)

   value.matrix
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

calculate.value <- function(value.matrix, i, j) {
    if (i < 2) stop('i must be greater than or equal to 2')
    if (j < 2) stop('j must be greater than or equal to 2')

    min(
        calculate.value.top(value.matrix, i, j),
        calculate.value.left(value.matrix, i, j),
        calculate.value.mismatch(value.matrix, i, j)
    )

}

init.non.edge.values <- function(value.matrix) {
    for (i in 2:ncol(value.matrix)) {
        for (j in 2:nrow(value.matrix)) {
            value.matrix[j,i] <- calculate.value(value.matrix, i, j)
        }
    }
    value.matrix
}
