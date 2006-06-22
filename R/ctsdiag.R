"ctsdiag" <- tsdiag.car <- 
function (object, gof.lag = 10, ...) 
{
    oldpar <- par(mfrow = c(2, 2))
    on.exit(par(oldpar))
    stdres <- object$stdres
    plot(stdres, type = "h", main = "Standardized Residuals", 
        ylab = "")
    abline(h = 0)
    acf(object$stdres, plot = TRUE, main = "ACF of Standardized Residuals", 
        na.action = na.pass)
    cpgram(stdres, main = "Cumulative periodogram")
    nlag <- gof.lag
    pval <- numeric(nlag)
    for (i in 1:nlag) pval[i] <- Box.test(stdres, i, type = "Ljung-Box")$p.value
    plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0, 
        1), main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}
