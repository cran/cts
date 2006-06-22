kalsmo.comp <- 
  function (x, comp=NULL, plot = TRUE, na.action = na.fail, ...) {
    cn <- match(c("sser"), names(x))
    if (any(is.na(cn))) 
      stop("x must be a kalsmo.car() object")
    compser <- Re(apply(x$sser[,comp],1,sum))
    spg.out <- list(compser, method = paste("CAR (", 
                               comp, ") component ", sep = ""))
    class(spg.out) <- "kalsmo.comp"
    if (plot) {
      plot(spg.out, ...)
      return(invisible(spg.out))
    }
    else return(spg.out)
  }
