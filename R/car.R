.First.lib <- function(lib, pkg)
        library.dynam("cts", pkg, lib)
car <-
function(x, y=NULL, scale=1.5, order=3, ari=TRUE, phi=rep(0,order), vri=FALSE, vr=0, pfi="MAPS",ccv="CTES", lpv=TRUE, scc=TRUE, n.ahead=10,  nit=40, opm=1, rgm=1, req=0.5, con=1.0e-5, rpe=1.0, ivl=1.0e-2, fac=1.0e1, stl=1.0e-5, sml=1.0e2, gtl=1.0e5, kst=TRUE, fct=TRUE, fty=2)
  {
    call <- match.call()
    if (NCOL(x)==2){
      tim <- x[,1]
      ser <- x[,2]
   series <- deparse(substitute(x))}
    else {
      tim <- x
      ser <- y
      series <- deparse(substitute(y))
    }
    if (length(tim)!=length(ser)) stop("Number of time and observations are not equal")
    len <- length(tim)
    ### corresponding to setup.f
    csz <- 0
    if (pfi=="QLFA") pfi <- 1
    if (pfi=="QLFS") pfi <- 2
    if (pfi=="DIRA") pfi <- 3
    if (pfi=="DIRS") pfi <- 4
    if (pfi=="MAPA") pfi <- 5
    if (pfi=="MAPS") pfi <- 6
    if (pfi==0) stop("Invalid PFI option:",pfi)
#    scale <- sca
    if (scale <= csz) stop("Invalid SCALE value:",scale)
    if (order <= 0 || order > 20) stop("Invalid ARP value:", order)

    if (ari==FALSE) ari <- 0
    if (ari==TRUE){
      if (order==length(phi)) ari <- 1
      else stop("Initial phi value length not equal to the model order")}
    if (ari < 0) stop("Invalid ARI option:", ari)
    if (ari==0) phi <- rep(0,order)

    if (vri==FALSE) vri <- 0
    if (vri==TRUE) vri <- 1
    if (vri < 0) stop("Invalid VRI option:", vri)
    if (vri==0) vr <- csz
    if (vr < 0) stop("Invalid VR value:", vr)

    if (ccv=="NULL") ccv <- 0
    if (ccv=="MNCT") ccv <- 1
    if (ccv=="CTES") ccv <- 2
    if (ccv < 0) stop("Invalid CCV option:", ccv)

    if (lpv==FALSE) lyap <- 0
    if (lpv==TRUE) lyap <- 1
    if (lyap < 0) stop("Invalid LPV option:", lpv)
    if (lyap==1) prdg <- csz
    if (prdg < 0) stop("Invalid PRDG value:", prdg)
    
    if (scc==FALSE) scc <- 0
    if (scc==TRUE) scc <- 1
    if (scc < 0) stop("Invalid SCC option:", scc)
    if (scc==0 && lyap==1)
      {
        lyap <- 0
        prdg <- 1.0e4
        cat("\nSCC=N CAUSES RESET OF LPV=Y TO LPV=N AND PRDG=1.0D4","\n")
      }
    if (n.ahead < 0 || n.ahead > 500) stop("Invalid forecasting steps:", n.ahead)
    cat("\nREADING OF model parameter SUCCESSFUL")
    
    if (nit < 0 || nit > 100) nit <- 25
    if (opm < 0) stop("Invalid OPM value:", opm)
    if (rgm < 0) stop("Invalid RGM value:", rgm)
    if (req < 0) stop("Invalid REQ value:", req)
    concrit <- con
    if (con < 0) stop("Invalid CONCRIT value:", concrit)
    rpert <- rpe
    if (rpe < 0) stop("Invalid RPERT value:", rpert)
    ivlam <- ivl
    if (ivlam < 0) stop("Invalid IVLAM value:", ivlam)
    if (fac < 0) stop("Invalid FAC value:", fac)
    stlam <- stl
    if (stlam < 0) stop("Invalid STLAM value:", stlam)
    smlam <- sml
    if (smlam < stlam) stop("Invalid SMLAM value:", smlam)
    gtlam <- gtl
    if (gtlam < 0) stop("Invalid GTLAM value:", gtlam)
    
    if (kst==FALSE) kst <- 0
    if (kst==TRUE) kst <- 1
    if (kst < 0) stop("Invalid KST value:", kst)

    if (fct==FALSE) fct <- 0
    if (fct==TRUE) fct <- 1
    if (fct < 0) stop("Invalid FCT value:", fct)

    if(fty < 1 || fty > 3) stop("Invalid fty value:", fty)
    
    cat("\nREADING OF control parameter SUCCESSFUL","\n")
    np1 <- 0
    z <-.Fortran("setup",
                 as.integer(pfi),
                 as.integer(order),
                 
                 as.integer(vri),
                 as.integer(ccv),
                 as.double(scale),
                 as.integer(ari),
                 as.double(vr),
                 as.double(phi),
                 as.integer(lyap),
                 as.double(prdg),
                 as.integer(scc),
                 as.integer(fct),
                 as.integer(fty),
                 as.integer(len),
                 as.integer(n.ahead),
                 as.double(tim),
                 as.double(ser),
                 as.integer(nit),
                 as.integer(opm),
                 as.integer(rgm),
                 as.double(req),
                 as.double(concrit),
                 as.double(rpert),
                 as.double(ivlam),
                 as.double(fac),
                 as.double(stlam),
                 as.double(smlam),
                 as.double(gtlam),
                 as.integer(kst),
                 np1=as.integer(np1),
                 package="cts")
    .Fortran("display",package="cts")
    if (nit > 0)
      {
        .Fortran("loop",package="cts")
        .Fortran("complete",package="cts")
      }

    Z <- .Fortran("setcom",
                  pfi1=integer(1),
                  arp1=integer(1),
                  np1=integer(1),
                  vri1=integer(1),
                  ccv1=integer(1),
                  len1=integer(1),
                  scale1=double(1),            
                  vr1=double(1),
                  sigsq1=double(1),
                  ##ESSP1 in Fortran has dimention (22,22)
                  essp1=as.double(matrix(0,22,22)),
                  ecov1=as.double(matrix(0,22,22)),
                  b1= double(z$np1),
                  delb1=double(z$np1),
                  rootr1=double(order),
                  rooti1=double(order),
                  package="cts")
    essp <- matrix(Z$essp1,byrow=FALSE,ncol=22)
    essp <- essp[1:Z$np1,1:Z$np1]
    ecov <- matrix(Z$ecov1,byrow=FALSE,ncol=22)
    ecov <- ecov[1:order,1:order]
    n <- length(tim)
  if (kst==1)
    {     
    .Fortran("kfilsm",package="cts")   
    Zkfil <- .Fortran("setkfilsm",
                      fser1=double(n),
                      fvar1=double(n),
                      sser1=double(n),
                      svar1=double(n),
                      sres1=double(n),
                      package="cts")
    filser <- Zkfil$fser1
    filvar <- Zkfil$fvar1
    sser <- Zkfil$sser1
    svar <- Zkfil$svar1
    sres <- Zkfil$sres1
  }
    else
      {
        filser <-  NULL
        filvar <- NULL
        sser <- NULL
        svar <- NULL
        sres <- NULL
      }

        if (pfi==1)
          ST1 <- 'QLFA'
        else if (pfi==2)
          ST1 <- 'QLFS'
        else if (pfi==3)
          ST1 <- 'DIRA'
        else if (pfi==4)
          ST1 <- 'DIRS'
        else if (pfi==5)
          ST1 <- 'MAPA'
    else
    ST1 <- 'MAPS'

    if (ccv==0)
      ST2 <- NULL
    else if (ccv==1) ST2 <- 'MNCT'
    else ST2 <- 'CTES'
    
    if (vri==1) ST3 <- 'N'
    else ST3 <- 'Y'

    .Fortran("update",package="cts")
    phi <- .Fortran("setupdate",
                    phi1=double(order),
                    package="cts")$phi 
    if (n.ahead > 0)
      {
        .Fortran("forecast",package="cts")
        if(fct==TRUE && fty==1)
          ntim <- len+n.ahead
        else ntim <- len
        Zfor <- .Fortran("setfor",
                         pre1=double(n.ahead),
                         prv1=double(n.ahead),
                         tim1=double(ntim),
                         package="cts")
        pre <- Zfor$pre1
        prv <- Zfor$prv1
        tim <- Zfor$tim1
      }
    else
      {
        pre <- NULL
        prv <- NULL
      }

    structure(list(call=call,series = series,order=Z$arp1,
                   np=Z$np1,scale=Z$scale1,
                   x.mean=Z$b1[order+1],vr=Z$vr1,
                   sigma2=Z$sigsq1,phi=phi,
                   b=Z$b1,delb=Z$delb1,essp=essp,
                   ecov=ecov,
                   rootr=Z$rootr1,rooti=Z$rooti1,
                   tim=tim,ser=ser,n.used=Z$len1,
                   filser=filser,filvar=filvar,
                   sser=sser,svar=svar,
                   stdres=sres,
                   predict=pre,predict.var=prv),
              class="car")
  }

print.car <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")

  cat("Coefficients:\n")
  coef <- drop(round(x$phi, digits = digits))
  names(coef) <- seq(length=x$order)
  print.default(coef, print.gap = 2)
  
  cat("\nOrder selected", x$order, " sigma^2 estimated as",
      format(x$sigsq, digits = digits),"\n")
  invisible(x)
}

predict.car <- function(object, se.fit=TRUE, digits = max(3, getOption("digits") - 3), ...)
{
#  if (!is.null(object$predict))
#    {
      cat("\nCall:\n", deparse(object$call), "\n\n", sep = "")
      ar <- object$phi
      p <- object$order
      cat("\nTime required to forecast","\n\n")
      tim <- drop(round(object$tim[(length(object$tim)-length(object$predict)+1):length(object$tim)],digits = digits))
      names(tim) <- seq(length=length(object$predict))
      print.default(tim, print.gap = 2)
      cat("\nForecasted value","\n\n")
      predict <- drop(round(object$predict,digits = digits))
      names(predict) <- seq(length=length(object$predict))
      print.default(predict, print.gap = 2)
      cat("\nEstimated variance","\n\n")
      prv <- drop(round(object$predict.var,digits = digits))
      names(prv) <- seq(length=length(object$predict.var))
      print.default(predict.var, print.gap = 2)
      
#    }
#  else stop("No forecasting calculation","\n",
#            "\Please recalculate "car" and set n.ahead>0","\n")
}


ctsdiag <- tsdiag.car <- function(object, gof.lag = 10, ...)
{
    ## plot standardized residuals, acf of residuals,
  ## cumulative periodogram and Ljung-Box p-values
  
    oldpar<- par(mfrow = c(2, 2))
    on.exit(par(oldpar))
    stdres <- object$stdres
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0)
    acf(object$stdres, plot = TRUE, main = "ACF of Standardized Residuals",
        na.action = na.pass)
    cpgram(stdres,main="Cumulative periodogram")

    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1:nlag) pval[i] <- Box.test(stdres, i, type="Ljung-Box")$p.value
    plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}

#tsdiag.car <- function(object, gof.lag, ...) UseMethod("tsdiag")
