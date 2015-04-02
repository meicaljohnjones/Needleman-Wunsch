needle <- function(seq1, seq2, gap, match, mismatch) {

}

calculate.alignment <- function(matrices) {

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
