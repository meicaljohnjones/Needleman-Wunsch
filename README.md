# Needleman-Wunsch in R

This is an R package that implements the Needleman and Wunsch sequence
alignment, unit tested using the testthat unit testing framework.

To install:
* using `devtools`: `devtools::install_github("hiraethus/Needleman-Wunsch")`

Example usage:
```r
> library("hiraethus.needleman.wunsch")

> sequence1 <- "ACCCGGTCGTCAATTA"
> sequence2 <- "ACCACCGGTTGGTCCAATAA"

# scoring scheme for the algorithm
> match <- 1
> mismatch <- -1
> gap <- -1

> alignment <- needle(sequence1, sequence2, gap, match, mismatch)
> print(alignment)

Needleman-Wunsch Sequence Alignment
===================================
Alignment 1: A C C _ C _ G G T C G _ T C _ A A T T A
Alignment 2: A C C A C C G G T T G G T C C A A T A A
Max score  : 7

```