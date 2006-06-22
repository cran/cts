"aic" <-
function (object) 
{
    cat("\nCall:\n", deparse(object$call), "\n\n", sep = "")
    phi <- object$phi
    covmat <- object$ecov
    ARP <- length(phi)
    tphi <- chol(solve(covmat)) %*% phi
    AIC <- NULL
    cums <- cumsum(tphi^2)
    AIC <- -cums + 2 * seq(1:length(phi))
    res <- data.frame("t-statistic" = as.vector(tphi), AIC = AIC, 
        row.names = NULL)
    cat("\nModel selection statistics", "\n\n")
    return(res)
}
