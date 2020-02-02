
##################################################################################
#' QQ-plot for a NpdeObject object
#' 
#' QQ-plot for a NpdeObject object
#' 
#' @name npde.qqplot 
#' @aliases npde.qqplot.numeric,numeric-method npde.qqplot.numeric,character-method npde.qqplot,numeric-method
#' 
#' @usage npde.qqplot(x, y, ...)
#' @usage npde.qqplot(x, y, col="black", xlab="Theoretical quantiles", ylab="Sample quantiles", pch=20, 
#' lty=1, lwd=1, lcol="black", main="", sub="", cex=1, cex.axis=1, cex.lab=1, cex.main=1, cex.sub=1)
#' @usage npde.qqplot(x, y, add.args=NULL, col="black", xlab="Theoretical quantiles", ylab="Sample quantiles", 
#' pch=20, lty=1, lwd=1, lcol="black", main="", sub="", cex=1, cex.axis=1, cex.lab=1, cex.main=1, cex.sub=1)
#' @param x a vector or an object of type NpdeObject (currently only a vector)
#' @param y a vector or a character string giving the name of a quantile function for a random distribution (eg qnorm); if x is a NpdeObject, y must be a character string
#' @param add.args a list containing arguments to be passed on to y if y is the name of a quantile function
#' @param col colour of plotting symbols
#' @param lcol colour of lines
#' @param xlab label for the X-axis
#' @param ylab label for the Y-axis
#' @param pch plotting symbol
#' @param lty type of line
#' @param lwd width of line
#' @param main title for the plot
#' @param sub subtitle for the plot
#' @param cex a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param cex.axis the magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.lab the magnification to be used for x and y labels relative to the current setting of cex
#' @param cex.main the magnification to be used for main titles relative to the current setting of cex
#' @param cex.sub the magnification to be used for sub-titles relative to the current setting of cex
#' @param ... optional arguments (for the S4 generic definition)
#' 
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @rdname npde.qqplot
#' @exportMethod npde.qqplot
#' @keywords plot 
#' @keywords methods
#' @importFrom graphics abline plot
#' @importFrom stats approx

setGeneric(name="npde.qqplot", def=function(x, y, ...){standardGeneric("npde.qqplot")})
            
#' @rdname npde.qqplot
setMethod(f="npde.qqplot", 
          signature(x="numeric",y="numeric"), 
          definition = function(x, y, col="black", xlab="Theoretical quantiles", ylab="Sample quantiles", pch=20, lty=1, lwd=1, lcol="black", main="", cex=1, cex.axis=1, cex.lab=1, cex.main=1, cex.sub=1) {
#            old.par <- par(no.readonly = TRUE)
#            on.exit(par(old.par))
            sx <- sort(x)
            sy <- sort(y)
            lenx <- length(sx)
            leny <- length(sy)
            if (leny < lenx) 
              sx <- approx(1L:lenx, sx, n = leny)$y
            if (leny > lenx) 
              sy <- approx(1L:leny, sy, n = lenx)$y
            plot(sx,sy, xlab=xlab, ylab=ylab, pch=pch, col=col, main=main, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, cex.sub=cex.sub)
            abline(0,1, col=lcol, lty=lty, lwd=lwd)
#            invisible()
          }
)

#' @rdname npde.qqplot
setMethod(f="npde.qqplot", 
          signature(x="numeric",y="character"), 
          definition = function(x, y, add.args=NULL, col="black", xlab="Theoretical quantiles", ylab="Sample quantiles", pch=20, lty=1, lwd=1, lcol="black", main="", cex=1, cex.axis=1, cex.lab=1, cex.main=1, cex.sub=1) {
            sx <- sort(x)
            lenx <- length(sx)
            if (is.character(y))
              y <- get(y, mode = "function", envir = parent.frame())
            if (!is.function(y)) stop("'y' must be numeric or a function or a string naming a valid quantile function (eg qnorm)")
            sy<-c(1:lenx)/(lenx+1)
            fun.args <- c(list(sy), add.args)
            ply<-do.call("y", fun.args)
            plot(sx, ply, xlab=xlab, ylab=ylab, pch=pch, col=col, main=main, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, cex.sub=cex.sub)
            abline(0,1, col=lcol, lty=lty, lwd=lwd)
          }
)
