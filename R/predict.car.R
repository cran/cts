"predict.car" <-
function (object, se.fit = TRUE, digits = max(3, getOption("digits") - 
    3), ...) 
{
    cat("\nCall:\n", deparse(object$call), "\n\n", sep = "")
    ar <- object$phi
    p <- object$order
    cat("\nTime required to forecast", "\n\n")
    tim <- drop(round(object$tim[(length(object$tim) - length(object$predict) + 
        1):length(object$tim)], digits = digits))
    names(tim) <- seq(length = length(object$predict))
    print.default(tim, print.gap = 2)
    cat("\nForecasted value", "\n\n")
    predict <- drop(round(object$predict, digits = digits))
    names(predict) <- seq(length = length(object$predict))
    print.default(predict, print.gap = 2)
    cat("\nEstimated standard error", "\n\n")
    se <- drop(round(sqrt(object$predict.var), digits = digits))
    names(se) <- seq(length = length(object$predict.var))
    print.default(se, print.gap = 2)
}

plotpredict.car <- function(object,xlab = "time", 
    ylab = NULL, type = "l", main = NULL, sub = NULL,...)
  {
    if(length(object$ser) < length(object$tim))
      object$ser <- c(object$ser,object$predict)
    plot(object$tim[1:length(object$ser)],object$ser,ylim=c(min(object$ser)-2*max(sqrt(object$predict.var)),max(object$ser)+2*max(sqrt(object$predict.var))),xlab=xlab,ylab=ylab,type=type,main=main,sub=sub)
    lines(object$tim[(length(object$tim) - length(object$predict) + 
                      1):length(object$tim)],object$predict,lty=1,col="red")
    lines(object$tim[(length(object$tim) - length(object$predict) + 
                      1):length(object$tim)],object$predict+2*sqrt(object$predict.var),lty=2,col="blue")
    lines(object$tim[(length(object$tim) - length(object$predict) + 
                      1):length(object$tim)],object$predict-2*sqrt(object$predict.var),lty=2,col="blue")
  }
