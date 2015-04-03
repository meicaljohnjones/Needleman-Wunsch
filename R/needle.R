needle <- function(seq1, seq2, gap, match, mismatch) {

}

calculate.alignment <- function(matrices) {
  alignment1 <- c()
  alignment2 <- c()

  traceback.matrix <- matrices$traceback.matrix
  i <- ncol(traceback.matrix)
  j <- nrow(traceback.matrix)

  repeat {
    traceback.direction <- traceback.matrix[j,i]
    if (traceback.direction == "done") break

    seq1.letter <- dimnames(traceback.matrix)[[2]][i]
    seq2.letter <- dimnames(traceback.matrix)[[1]][j]

    alignments <- calculate.alignment.step(seq1.letter, seq2.letter, traceback.direction)
    alignment1 <- c(alignments$alignment1, alignment1)
    alignment2 <- c(alignments$alignment2, alignment2)

    # find next i and j
    if (traceback.direction == "diag") {
      i <- i - 1
      j <- j - 1
    } else if (traceback.direction == "left") {
      i <- i - 1
    } else if (traceback.direction == "up") {
      y <- y - 1
    }
  }
  list(alignment1=alignment1, alignment2=alignment2)
}

calculate.alignment.step <- function(seq1.letter, seq2.letter, traceback.direction) {
  if (traceback.direction == "diag") {
    return(list(alignment1=seq1.letter, alignment2=seq2.letter))
  } else if (traceback.direction == "left") {
    return(list(alignment1=seq1.letter, alignment2="_"))
  }  else if (traceback.direction == "up") {
    return(list(alignment1="_", alignment2=seq2.letter))
  } else {
    stop(paste("Invalid traceback.direction (",traceback.direction,") given. Please use 'up', 'left' or 'diag'", sep=""))
  }
}

calculate.maxscore <- function(matrices) {
  value.matrix <- matrices$value.matrix

  i <- ncol(value.matrix)
  j <- nrow(value.matrix)

  value.matrix[j, i]
}
