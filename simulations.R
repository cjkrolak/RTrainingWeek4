RandomCorr <- function(func, n=100, m=1, b=0,
                       mu=0, sig=1, ep=0,
                       epmu=0, epsig=1, seed=20) {
    set.seed(seed)
    x <- rnorm(n, mu, sig)
    e <- rnorm(n, epmu, epsig)
    y <- b + m * x + e
    summary(y)
    plot(x, y)
    # z=data.table(x,y)
}