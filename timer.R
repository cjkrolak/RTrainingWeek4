webRead <- function () {
    system.time(readLines("http://www.jhsph.edu"))
}

hilbert <- function(n) {
    i <- 1:n
    1 / outer(i-1, i, "+")
}

hilbertExec <- function() {
    x <- hilbert(1000)
    system.time(svd(x))
}