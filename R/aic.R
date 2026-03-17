#AIC <- function(object, ...)
#    UseMethod("AIC")
"AIC.car" <-
function (object, ..., verbose = TRUE, k = 2)
{
    phi <- object$phi
    covmat <- object$ecov
    ARP <- length(phi)
    tphi <- chol(solve(covmat)) %*% phi
    cums <- cumsum(tphi^2)
    if (is.null(k) || length(k) != 1L || !is.finite(k) || k < 0) {
        stop("k must be a single nonnegative finite number")
    }
    aic <- -cums + k * seq_len(length(phi))

    res <- data.frame(order = seq_len(length(phi)),
                      t.statistic = round(as.vector(tphi), 2),
                      AIC = round(aic, 2))

    if (isTRUE(verbose)) {
        msg <- c(
            paste0("Call:\n", paste(deparse(object$call), collapse = "\n")),
            "Model selection statistics:",
            paste(capture.output(print(res, row.names = FALSE)), collapse = "\n")
        )
        message(paste(msg, collapse = "\n"))
    }

    res
}
