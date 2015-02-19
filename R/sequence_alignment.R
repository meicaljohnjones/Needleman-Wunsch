calculate.value.matrix <- function(first.sequence, second.sequence) {
   value.matrix <- matrix(ncol=length(first.sequence) + 1, nrow=length(second.sequence) + 1)
   colnames(value.matrix) <- c('*', first.sequence)
   rownames(value.matrix) <- c('*', second.sequence)

   value.matrix
}

# Outputs the common sequence between the two
find.global.sequence <- function(first.sequence, second.sequence) {

}
