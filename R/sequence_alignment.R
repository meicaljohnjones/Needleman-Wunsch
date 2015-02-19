calculate.value.matrix <- function(first.sequence, second.sequence) {
   value.matrix <- matrix(ncol=length(first.sequence) + 1, nrow=length(second.sequence) + 1)
   colnames(value.matrix) <- c('*', first.sequence)
   rownames(value.matrix) <- c('*', second.sequence)

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

# Outputs the common sequence between the two
find.global.sequence <- function(first.sequence, second.sequence) {

}
