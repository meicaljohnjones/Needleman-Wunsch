mm <- function(first.letter, second.letter) {
    if (nchar(first.letter) != 1)
        stop('first.letter arg wrong length - should be single character')
    if (nchar(second.letter) != 1)
        stop('second.letter arg wrong length - should be single character')

    if (first.letter == second.letter) 1 else 0
}
