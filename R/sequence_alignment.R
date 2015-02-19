calculate.value.matrix <- function(first.sequence, second.sequence) {
   value.matrix <- matrix(ncol=length(first.sequence), nrow=length(second.sequence))
   colnames(value.matrix) <- first.sequence
   rownames(value.matrix) <- second.sequence

   value.matrix
}

# Outputs the common sequence between the two
find.global.sequence <- function(first.sequence, second.sequence) {

}
