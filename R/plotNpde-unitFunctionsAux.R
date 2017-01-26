###############################
# Scatterplot
aux.scatter<-function(plmat,plot.opt,dotline=NULL,distrib="norm",main=NULL,namcat=NULL,sim.ypl=NULL) {
	# Auxiliary function used to plot scatterplots
	# Input
	### plmat
	### plot.opt
	### dotline - dotted lines on the plot
	### sim.ypl - compute true prediction bands from simulated data
	# Settings - distribution (norm/unif), main (title), namcat (category names, for covariate plots)
	alpha<-(1-plot.opt$vpc.interval)/2
	if(is.null(main)) main<-plot.opt$main
	if(is.null(dotline)) {
		dotline<-c(alpha,0.5,1-alpha) 
		if(distrib=="norm") dotline<-qnorm(dotline)
	}
	# Compute prediction bands if plotting required
	if(plot.opt$bands) {
		if(is.numeric(plmat$x)) {
			# ECO TODO: should binning be done on full data or after censoring ?
			xbin<-npde.binning(plmat$x,plot.opt,verbose=plot.opt$interactive)
			plmat<-data.frame(grp=xbin$xgrp,plmat)
			xcent<-xbin$xat
		} else {
			plmat<-data.frame(grp=plmat$x,plmat)
			xcent<-unique(plmat$x)
		}
		xlab<-as.character(1:length(xcent))
		nseuil<-200
		alp.pi<-plot.opt$pi.size
		if(alp.pi<0.5) alp.pi<-(1-alp.pi)
		quant<-c(alpha,0.5,1-alpha)
		if(plot.opt$approx.pi) {
			nobs<-tapply(plmat$grp,plmat$grp,length)
			bnds<-compute.bands(nobs,nseuil,quant,distrib,alpha=alp.pi)
		} else {
#			sim.ypl<-data.frame(grp=plmat$grp,cens=plmat[,3],sim.ypl)
			bnds<-compute.bands.true(grp=plmat$grp,sim.ypl,quant,alpha=alp.pi)
		}
		binf<-bnds$binf
		bsup<-bnds$bsup
		bmed<-bnds$bmed
		if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(min(plmat[,4],binf,na.rm=T), max(plmat[,4],bsup,na.rm=T))
		plot(plmat[,2],plmat[,4],type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab, main=main,sub=plot.opt$sub,col=plot.opt$col,pch=plot.opt$pch, cex=plot.opt$cex, cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xlim=plot.opt$xlim,ylim=plot.opt$ylim, xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
		for(icol in c(1,3)) polygon(c(xcent,rev(xcent)),c(binf[,icol],rev(bsup[,icol])), col=plot.opt$col.fillpi,lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi)
		polygon(c(xcent,rev(xcent)),c(binf[,2],rev(bsup[,2])), col=plot.opt$col.fillmed,lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed,border=plot.opt$col.lmed)
		if(length(dotline)==0) {
			for(icol in c(1,3)) lines(xcent,bmed[,icol],lty=plot.opt$lty.lpi, lwd=plot.opt$lwd.lpi,col=plot.opt$col.lpi)
			lines(xcent,bmed[,2],lty=plot.opt$lty.lmed, lwd=plot.opt$lwd.lmed,col=plot.opt$col.lmed)
		}
		percobs<-matrix(unlist(tapply(plmat[,4],plmat$grp,quantile,quant,na.rm=T)), ncol=3,byrow=T)
		for(icol in c(1,3)) lines(xcent,percobs[,icol],lty=plot.opt$lty.lobs, lwd=plot.opt$lwd.lobs, col=plot.opt$col.lobs)
		lines(xcent,percobs[,2],lty=plot.opt$lty.lobs,lwd=plot.opt$lwd.lobs, col=plot.opt$col.lmed)
	} else {
		plmat<-cbind(grp=plmat[,1],plmat)
		if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(min(plmat[,4],na.rm=T), max(plmat[,4],na.rm=T))
		plot(plmat[,2],plmat[,4],type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab, main=main,sub=plot.opt$sub,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,cex.lab=plot.opt$cex.lab, xlim=plot.opt$xlim,ylim=plot.opt$ylim,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
	}  
	if(plot.opt$plot.obs) {
		points(plmat[plmat[,3]==0,2],plmat[plmat[,3]==0,4],col=plot.opt$col.pobs, pch=plot.opt$pch,cex=plot.opt$cex)
		if(plot.opt$plot.loq) points(plmat[plmat[,3]==1,2],plmat[plmat[,3]==1,4], col=plot.opt$col.pcens, pch=plot.opt$pch.pcens,cex=plot.opt$cex) 
	}
	if(length(dotline)>0) {
		if(length(dotline)==3) {
			for(i in c(1,3)) abline(h=dotline[i],lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi, col=plot.opt$col.lpi)
			abline(h=dotline[2],lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)
		} else for(i in dotline) abline(h=i,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline, col=plot.opt$col.abline)
	}
	invisible(xbin)
}

###############################
# Histogram
aux.plot.hist<-function(plmat,plot.opt,distrib="norm", nclass=10,sim.ypl=NULL) {
	ndat<-dim(plmat)[1]
	plot.bands<-FALSE
	if(plot.opt$bands) {
		if(!plot.opt$approx.pi & length(sim.ypl)>0) plot.bands<-TRUE
		if(plot.opt$approx.pi) {
			xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
			sim.ypl<-matrix(xsamp,nrow=ndat)
			plot.bands<-TRUE
		}
	}	
	if(plot.bands) {
		x<-hist(plmat$xpd,breaks=nclass,plot=FALSE)
		alpha<-plot.opt$pi.size
		if(alpha>0.5) alpha<-1-alpha
		tmat<-matrix(nrow=length(x$breaks)-1,ncol=dim(sim.ypl)[2])
		nB<-length(x$breaks)
		for(j in 1:dim(sim.ypl)[2]) {
			xvec<-cut(sim.ypl[,j],breaks=x$breaks,include.lowest=TRUE, ordered_result=TRUE)
			tmat[,j]<-table(xvec)
		}
		row.names(tmat)<-names(table(xvec))
		bnds<-apply(tmat,1,quantile,c(alpha/2,0.5,1-alpha/2))
		if(is.null(plot.opt$xlim)) plot.opt$xlim<-c(min(x$breaks),max(x$breaks))
		if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(0,max(c(bnds,x$counts)))
		plot(x$breaks[-nB],x$counts,xlim=plot.opt$xlim,ylim=plot.opt$ylim, type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab,main=plot.opt$main,sub=plot.opt$sub, cex.lab=plot.opt$cex.lab,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
		# PI on counts
		rect(x$breaks[-nB],bnds[1,],x$breaks[-1],bnds[3,],lwd=plot.opt$lwd.lpi, lty=plot.opt$lty.lpi, border=plot.opt$col.lpi,col=plot.opt$col.fillpi)
		segments(x$breaks[-nB],bnds[2,],x$breaks[-1],bnds[2,], lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)		
		segments(x$breaks[-c(1,nB)],bnds[2,-1],x$breaks[-c(1,nB)], bnds[2,-(nB-1)],lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)
		# Observed data		
		segments(x$breaks[-nB],x$counts,x$breaks[-1],x$counts,lwd=plot.opt$lwd.lobs+1, lty=plot.opt$lty.lobs,col=plot.opt$col.lobs)		
		#		segments(x$breaks[-c(1,nB)],x$counts[-1],x$breaks[-c(1,nB)], x$counts[-(nB-1)],lwd=plot.opt$lwd+1,lty=plot.opt$lty, col=plot.opt$col)
		rect(x$breaks[-nB],0,x$breaks[-1],x$counts,lwd=plot.opt$lwd.lobs+1, lty=plot.opt$lty.lobs,border=plot.opt$col.lobs)
	} else {
		x<-hist(plmat$xpd,breaks=nclass,xlab=plot.opt$xlab,ylab=plot.opt$ylab, main=plot.opt$main,sub=plot.opt$sub,lwd=plot.opt$lwd.lobs,lty=plot.opt$lty.lobs, border=plot.opt$col.lobs,col=plot.opt$col.fillpi, cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,cex=plot.opt$cex,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes)
		if(distrib=="norm") {
			xpl<-min(plmat$xpd)+c(0:100)/100*(max(plmat$xpd)-min(plmat$xpd))
			ypl<-dnorm(xpl)
			ypl<-ypl/max(ypl)*max(x$counts)
			lines(xpl,ypl,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline,col=plot.opt$col.abline)  
		}
		if(distrib=="unif") {
			abline(h=ndat/nclass,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline,col=plot.opt$col.abline)
		}
	}
}

###############################
# boxplot
aux.scatter.box<-function(plmat,plot.opt,dotline,distrib="norm",main=NULL,namcat=NULL,alpha=0.95) {
	# Auxiliary function used to plot boxplots
	if(!is.null(namcat)) xname<-namcat
	if(is.null(main)) main<-plot.opt$main
	if(is.null(dotline)) dotline<-c((1-alpha)/2,0.5,1-(1-alpha)/2)
	if(is.numeric(plmat$x)) {
		plot.opt$xlim<-c(min(plmat$x),max(plmat$x))
		xbin<-npde.binning(plmat$x,plot.opt,verbose=plot.opt$interactive)
		plmat<-data.frame(grp=xbin$xgrp,plmat)
		xcent<-xbin$xat
		if(is.null(namcat)) xname<-format(xbin$xat,digits=2)
	} else {
		xcent<-1:length(unique(plmat$x))
		plmat<-data.frame(grp=as.factor(plmat$x),plmat)
		xname<-unique(plmat$x)
	}
	boxplot(plmat$y~plmat$grp,at=xcent,main=main,sub=plot.opt$sub, xlim=plot.opt$xlim, ylim=plot.opt$ylim,xlab=plot.opt$xlab,ylab=plot.opt$ylab, cex=plot.opt$cex,cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,pch=plot.opt$pch, varwidth=plot.opt$varwidth,boxwex=plot.opt$boxwex, col=plot.opt$col.fillpi, names=xname)	
	if(length(dotline)>0) {
		for(i in dotline) abline(h=i,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline, col=plot.opt$col.abline)
	}
	invisible(xbin)
}

###############################
# Empirical cdf/QQplot
aux.plot.dist<-function(plmat,plot.opt,dist.type="ecdf",distrib="norm",nrep=0, nclass=0,ties=TRUE,sim.ypl=NULL,verbose=FALSE) {
	# ECO TODO: reprogram this function to make the names consistent, very confusing currently !
	if(nclass==0) binning<-FALSE else {
		binning<-TRUE
		nbin<-plot.opt$bin.number
	}
	ndat<-dim(plmat)[1]
	plot.bands<-FALSE
	if(plot.opt$bands) {
		if(!plot.opt$approx.pi & length(sim.ypl)>0) plot.bands<-TRUE
		if(plot.opt$approx.pi) {
			xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
			sim.ypl<-matrix(xsamp,nrow=ndat)
			plot.bands<-TRUE
		}
	}
	if(ties & nrep>0) yq<-seq(1/(2*nrep),1-1/(2*nrep),length.out=ndat) else  yq<-seq(0,1,length.out=ndat)
	if(distrib=="norm") yq<-qnorm(yq)		
	if(dist.type=="ecdf") {
		theo.samp<-yq
		yq<-seq(1/ndat,1,length.out=ndat)
	}
	ymat<-plmat[order(plmat$xpd),]
	ymat<-cbind(ymat,ecdf=yq)
	if(dist.type=="ecdf") {
		ysh<-data.frame(x=plmat$xpd,y=yq)
		yobs<-data.frame(x=ymat$xpd,y=ymat$ecdf,cens=ymat$cens)
	} else {
		ysh<-data.frame(x=yq,y=plmat$xpd)
		yobs<-data.frame(x=ymat$ecdf,y=ymat$xpd,cens=ymat$cens)
	}
	if(!binning) {
		xvec<-ysh$x
		yvec<-ysh$y
		if(plot.bands) {
			alpha<-plot.opt$pi.size
			if(alpha>0.5) alpha<-1-alpha
			sim.sort<-colsort(sim.ypl)
			bnds<-apply(sim.sort,1,quantile,c(alpha/2,0.5,1-alpha/2))
			if(dist.type=="ecdf") {
				xvec<-c(xvec,c(bnds))
				yvec<-c(yvec,yq)
			}
			if(dist.type=="qqplot") {
				xvec<-c(xvec,yq)
				yvec<-c(yvec,c(bnds))
			}
			if(plot.opt$xlog) xvec<-xvec[!is.na(xvec) & xvec>0]
			if(plot.opt$ylog) yvec<-yvec[!is.na(yvec) & yvec>0]
		}
		if(is.null(plot.opt$xlim))
			plot.opt$xlim<-c(min(xvec,na.rm=TRUE),max(xvec,na.rm=TRUE))
		if(is.null(plot.opt$ylim))
			plot.opt$ylim<-c(min(yvec,na.rm=TRUE),max(yvec,na.rm=TRUE))
		plot(ysh$x,ysh$y,xlim=plot.opt$xlim,ylim=plot.opt$ylim,type="n", xlab=plot.opt$xlab,ylab=plot.opt$ylab,main=plot.opt$main,sub=plot.opt$sub, cex.lab=plot.opt$cex.lab,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,log=plot.opt$logtyp, xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
		if(plot.bands) {
			if(dist.type=="ecdf") {
				polygon(c(bnds[1,],rev(bnds[3,])),c(yq,rev(yq)),col=plot.opt$col.fillpi, lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi,lty=plot.opt$lty.lpi)
				lines(bnds[2,],yq,lty=plot.opt$lty.lmed, col=plot.opt$col.lmed, lwd=plot.opt$lwd.lmed)
			} else { 
				polygon(c(yq,rev(yq)),c(bnds[1,],rev(bnds[3,])),col=plot.opt$col.fillpi, lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi,lty=plot.opt$lty.lpi)
				lines(yq,bnds[2,],lty=plot.opt$lty.lmed, col=plot.opt$col.lmed, lwd=plot.opt$lwd.lmed)
			}
		}
		lines(yobs$x,yobs$y,lty=plot.opt$lty,col=plot.opt$col.lobs,lwd=plot.opt$lwd)
		if(plot.opt$type=="p" | plot.opt$type=="b") {
			points(yobs$x[yobs$cens==0],yobs$y[yobs$cens==0], col=plot.opt$col.pobs, pch=plot.opt$pch.pobs,cex=plot.opt$cex)
			if(plot.opt$plot.loq) points(yobs$x[yobs$cens==1],yobs$y[yobs$cens==1], col=plot.opt$col.pcens, pch=plot.opt$pch.pcens,cex=plot.opt$cex)
		}
		if(!plot.bands) { # theoretical distribution
			if(dist.type=="qqplot" | distrib=="unif") abline(0,1,lty=plot.opt$lty.abline, lwd=plot.opt$lwd.abline,col=plot.opt$col.abline) else lines(theo.samp,c(1:ndat)/ndat, lty=plot.opt$lty.abline, lwd=plot.opt$lwd.abline,col=plot.opt$col.abline)
		}
	} else {
		if(verbose) cat("Binning not implemented yet\n")
	}
}

