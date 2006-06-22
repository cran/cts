"print.car" <-
function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("Coefficients:\n")
    coef <- drop(round(x$phi, digits = digits))
    names(coef) <- seq(length = x$order)
    print.default(coef, print.gap = 2)
    cat("\nOrder selected", x$order, " sigma^2 estimated as", 
        format(x$sigma2, digits = digits), "\n")
    invisible(x)
}
