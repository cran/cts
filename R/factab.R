"factab" <-
function (object) 
{
    call <- match.call()
    sortind <- sort.list(abs(object$rootr), decreasing = TRUE)
    carroot <- complex(real = object$rootr, imag = object$rooti)
    carroot <- rev(sort(carroot, partial = sortind))
    car.freq <- abs(Im(carroot))/(2 * pi)
    structure(list(call=call,root=carroot,freq=car.freq),
              class="factab")
#    cat("\nCharacteristic roots and frequencies", "\n\n")
#    freq.table <- data.frame(Roots = round(carroot, 3), Frequency = round(car.freq, 
#        3))
#    return(freq.table)
}

print.factab <- 

function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("\nCharacteristic root", "\n\n")
    root <- drop(round(x$root, digits=digits))
    freq <- drop(round(x$freq, digits=digits))
    names(root) <- seq(length = length(x$freq))
    print.default(root, print.gap = 2)
    cat("\nFrequency", "\n\n")
    names(freq) <- seq(length = length(x$freq))
    print.default(freq, print.gap = 2)
    invisible(x)
  }
