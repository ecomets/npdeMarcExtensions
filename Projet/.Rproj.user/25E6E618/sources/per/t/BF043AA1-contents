
#####################  Plots with reference profile  #############################
aux.npdeplot.meanprof<-function(mbin,msim) {
	# Compute a reference profile based on simulations from the model
	# mbin : matrix with columns xat (centre of the bins) and xgrp (bin number/group)
	# msim : matrix with simulations, 2 columns used (grp=which bin, ysim=simulated value)
	ymed<-tapply(msim$ysim,msim$grp,mean)
	sdmed<-tapply(msim$ysim,msim$grp,sd)
	ymed<-ymed[match(mbin$xlab,names(ymed),nomatch=0)]
	sdmed<-sdmed[match(mbin$xlab,names(sdmed),nomatch=0)]
	#	ymed<-ymed[order(names(ymed))]
	#	sdmed<-sdmed[order(names(sdmed))]
	if(length(ymed)<dim(mbin)[1]) {
		# Linear interpolation
		#				ypmed<-approx(xcent,ymed,xout=mbin$xat,rule=2)$y
		# Spline interpolation
		cat("Not all time points/bins are represented in the subset used for the reference profile: spline interpolation will be used to predict the entire profile, but this may distort the aspect of the plot significantly; we advise using another reference profile.\n")
		xcent<-mbin$xat[mbin$xlab %in% names(ymed)]
		ypmed<-spline(xcent,ymed,xout=mbin$xat)$y
		spmed<-spline(xcent,sdmed,xout=mbin$xat)$y
		iint<-1-as.integer(mbin$xlab %in% names(ymed))
		mpref<-data.frame(xat=mbin$xat,grp=mbin$grp,mean=ypmed,sd=spmed,int=iint,xlab=mbin$xlab)	
	} else mpref<-data.frame(xat=mbin$xat,grp=mbin$grp,mean=ymed,sd=sdmed,int=rep(0,length(sdmed)),xlab=mbin$xlab)	
	#	save.mpref<<-mpref # quick and dirty way of storing mpref (TODO: include in output)
	return(mpref)
}

aux.npdeplot.computepi<-function(plmat,plot.opt,xlab,xat,mpref=NULL,dotline=NULL,sim.ypl=NULL,distrib="norm",onlog=FALSE) {
	# Compute prediction interval for the observed data, the size of which depends on the number of observations in each bin
	# Input
	# plmat: matrix of values to plot
	# plot.opt:
	# xat: center of bins
	# xlab: group tag
	# mpref: reference profile
	# dotline: 
	# sim.ypl:
	# distrib: reference distribution
	# onlog: whether E and SD are computed on the observed value or after log transformation	
	# Output
	# bnds: boundaries of the prediction intervals for each bin present in plmat [may be <nbin]
	xinf<-sqrt(12)
	alpha<-(1-plot.opt$vpc.interval)/2
	nseuil<-200
	alp.pi<-plot.opt$pi.size
	if(alp.pi<0.5) alp.pi<-(1-alp.pi)
	quant<-c(alpha,0.5,1-alpha)
	if(!plot.opt$approx.pi & !is.null(sim.ypl)) {
		nrep<-length(sim.ypl)/dim(plmat)[1]
		yprov<-list(grp=plmat$grp,cens=plmat$cens,ypl=matrix(sim.ypl,ncol=nrep))
		bnds<-compute.bands.true(yprov,quant,alpha=alp.pi)
		bnds$xcent<-xat
	} else {
		nobs<-tapply(plmat$grp,plmat$grp,length)
		bnds<-compute.bands(nobs,nseuil,quant,distrib,alpha=alp.pi)
		bnds$xcent<-xat[match(names(nobs),xlab)]
	}
	# Transforming the boundaries if reference profile
	if(!is.null(mpref)) {
		# 		idx<-(names(nobs) %in% mpref$xlab)
		# 		mpref<-mpref[idx,]
		#			for(i in 1:3) bnds[[i]]<-bnds[[i]][unique(xbin$xgrp) %in% names(ymed),]
		if(onlog) {
			if(distrib=="unif") {
				for(i in 1:3) bnds$binf[,i]<-exp((bnds$binf[,i]-0.5)*mpref$sd*xinf+mpref$mean)
				for(i in 1:3) bnds$bmed[,i]<-exp((bnds$bmed[,i]-0.5)*mpref$sd*xinf+mpref$mean)
				for(i in 1:3) bnds$bsup[,i]<-exp((bnds$bsup[,i]-0.5)*mpref$sd*xinf+mpref$mean)
			} else {
				for(i in 1:3) bnds$binf[,i]<-exp(bnds$binf[,i]*mpref$sd+mpref$mean)
				for(i in 1:3) bnds$bmed[,i]<-exp(bnds$bmed[,i]*mpref$sd+mpref$mean)
				for(i in 1:3) bnds$bsup[,i]<-exp(bnds$bsup[,i]*mpref$sd+mpref$mean)
			}								
		} else {
			if(distrib=="unif") {
				for(i in 1:3) bnds$binf[,i]<-(bnds$binf[,i]-0.5)*mpref$sd*xinf+mpref$mean
				for(i in 1:3) bnds$bmed[,i]<-(bnds$bmed[,i]-0.5)*mpref$sd*xinf+mpref$mean
				for(i in 1:3) bnds$bsup[,i]<-(bnds$bsup[,i]-0.5)*mpref$sd*xinf+mpref$mean
			} else {
				for(i in 1:3) bnds$binf[,i]<-bnds$binf[,i]*mpref$sd+mpref$mean
				for(i in 1:3) bnds$bmed[,i]<-bnds$bmed[,i]*mpref$sd+mpref$mean
				for(i in 1:3) bnds$bsup[,i]<-bnds$bsup[,i]*mpref$sd+mpref$mean
			}				
		}
		if(is.null(dotline)) dline<-NULL else {
			dline<-data.frame(xat=mpref$xat)
			for(i in 1:length(dotline)) {
				if(onlog) {
					if(distrib=="unif") x1<-exp((dotline[i]-0.5)*mpref$sd*xinf+mpref$mean) else x1<-exp(dotline[i]*mpref$sd+mpref$mean)
				} else {
					if(distrib=="unif") x1<-(dotline[i]-0.5)*mpref$sd*xinf+mpref$mean else x1<-dotline[i]*mpref$sd+mpref$mean
				}
				dline<-cbind(dline,x1)
			}
		}
	}	else {
		if(is.numeric(plmat$x) & !is.null(dotline)) {
			dline<-data.frame(xat=seq(min(plmat$x,na.rm=TRUE),max(plmat$x,na.rm=TRUE),length.out=100))
			for(i in 1:length(dotline))
				dline<-cbind(dline,rep(dotline[i],100))
		} else dline<-NULL		
	}
	return(list(bnds=bnds,dline=dline))
}

aux.npdeplot.transform<-function(plmat,plot.opt,xat,mpref=NULL,distrib="norm",onlog=FALSE) {
	# Input
	# plmat: values to plot
	# plot.opt: preferences for plots
	# mpref: reference profile
	# distrib: reference distribution
	# onlog: whether E and SD are computed on the observed value or after log transformation
	# Output
	# plmat: updated with a ty column containing the data to plot
	# percobs: percentile of the observed distribution, used in the plots
	xinf<-sqrt(12) # used only if distrib is "unif"
	alpha<-(1-plot.opt$vpc.interval)/2
	quant<-c(alpha,0.5,1-alpha)
	if(is.null(mpref)) {
		ty<-plmat$y
	} else {
		mpr<-mpref[mpref$int==0,] # use only points not interpolated initially, to avoid double interpolation
		yfmed<-spline(mpr$xat,mpr$mean,xout=plmat$x)$y
		sfmed<-spline(mpr$xat,mpr$sd,xout=plmat$x)$y
		if(onlog) {
			if(distrib=="unif") ty<-exp((plmat$y-0.5)*sfmed*xinf+yfmed) else ty<-exp(plmat$y*sfmed+yfmed)
		} else {
			if(distrib=="unif") ty<-(plmat$y-0.5)*sfmed*xinf+yfmed else ty<-plmat$y*sfmed+yfmed				
		}			
	}
	percobs<-matrix(unlist(tapply(ty,plmat$grp,quantile,quant,na.rm=T)),ncol=3,byrow=T)
	row.names(percobs)<-xat
	plmat<-cbind(plmat,ty=ty)
	return(list(plmat=plmat,percobs=percobs))
}

aux.npdeplot.plot<-function(plmat,percobs,plot.opt,main=NULL,bnds=NULL,dotline=NULL) {
	# Input
	# plmat: values to plot
	# plot.opt: preferences for plots
	if(is.null(plot.opt$ylim)) {
		if(length(grep("y",plot.opt$logtyp))==0) plot.opt$ylim<-c(min(c(plmat$y,c(percobs),bnds$binf[,1]),na.rm=T),max(c(plmat$y,c(percobs),bnds$bsup[,3]),na.rm=T)) else {
			vec1<-c(plmat$y,c(percobs),bnds$binf[,1])
			vec2<-c(plmat$y,c(percobs),bnds$bsup[,3])
			plot.opt$ylim<-c(min(vec1[!is.na(vec1) & vec1>0]),max(vec2[!is.na(vec2) & vec2>0]))
		}
	}
	if(is.null(plot.opt$logtyp)) plot.opt$logtyp<-""
	plot(plmat$x,plmat$ty,type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab,main=main,sub=plot.opt$sub,col=plot.opt$col,pch=plot.opt$pch, cex=plot.opt$cex, cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xlim=plot.opt$xlim,ylim=plot.opt$ylim, xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot,log=plot.opt$logtyp)
	# Plot prediction intervals
	#	print(bnds)
	if(plot.opt$bands & !is.null(bnds)) {
		xcent<-bnds[[4]]
		binf<-bnds$binf
		bsup<-bnds$bsup
		bmed<-bnds$bmed
		for(icol in c(1,3)) polygon(c(xcent,rev(xcent)),c(binf[,icol],rev(bsup[,icol])), col=plot.opt$col.fillpi,lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi)
		polygon(c(xcent,rev(xcent)),c(binf[,2],rev(bsup[,2])), col=plot.opt$col.fillmed,lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed,border=plot.opt$col.lmed)		
		if(is.null(dotline)) {
			for(icol in c(1,3)) lines(xcent,bmed[,icol],lty=plot.opt$lty.lpi, lwd=plot.opt$lwd.lpi,col=plot.opt$col.lpi)
			lines(xcent,bmed[,2],lty=plot.opt$lty.lmed, lwd=plot.opt$lwd.lmed,col=plot.opt$col.lmed)
		} else {
			if(dim(dotline)[2]==4) {
				for(i in c(2,4)) lines(dotline[,1],dotline[,i],lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi, col=plot.opt$col.lpi)
				lines(dotline[,1],dotline[,3],lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)					
			} else {
				for(i in 2:dim(dotline)[2]) lines(dotline[,1],dotline[,i],lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline, col=plot.opt$col.abline)					
			}
		}
	} else xcent<-as.double(row.names(percobs))
	# Plot percentiles of observed data	
	for(icol in c(1,3)) lines(xcent,percobs[,icol],lty=plot.opt$lty.lobs, lwd=plot.opt$lwd.lobs, col=plot.opt$col.lobs)
	lines(xcent,percobs[,2],lty=plot.opt$lty.lobs,lwd=plot.opt$lwd.lobs, col=plot.opt$col.lmed)
	# Plot observed data
	if(plot.opt$plot.obs) {
		points(plmat$x[plmat$cens==0],plmat$ty[plmat$cens==0],col=plot.opt$col.pobs, pch=plot.opt$pch,cex=plot.opt$cex)
		if(plot.opt$plot.loq) points(plmat$x[plmat$cens==1],plmat$ty[plmat$cens==1], col=plot.opt$col.pcens, pch=plot.opt$pch.pcens,cex=plot.opt$cex) 
	}
	return()
}

aux.npdeplot.main<-function(plmat,plot.opt,namcat=NULL,msim=NULL,ref.prof=NULL,sim.ypl=NULL, dotline=NULL, distrib="norm",onlog=FALSE) {
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
	if(!is.null(msim)) {
		nrep<-dim(msim)[1]/dim(plmat)[1]
		msim<-cbind(msim,grp=rep(xbin$xgrp,nrep))
		mbin<-data.frame(xat=xbin$xat,grp=names(xbin$xat),xlab=xlab)
	} else {
		if(!is.null(sim.ypl)) nrep<-length(sim.ypl)/dim(plmat)[1]
	}
	zecat<-unique(plmat$cov)
	ncat<-length(zecat)
	if(ncat==1) covsplit<-FALSE else covsplit<-TRUE
	if(!is.null(msim)) mpref<-aux.npdeplot.meanprof(mbin,msim[msim$use==1,]) else mpref<-NULL
	ix<-c(grep("col",names(plot.opt)),grep("lty",names(plot.opt)),grep("lwd",names(plot.opt)))
	for(icol in ix) plot.opt[[icol]]<-rep(plot.opt[[icol]],ncat)
	for(icat in 1:ncat) {
		tit<-plot.opt$main
		if(tit=="") tit<-namcat[icat]
		if(covsplit & is.null(ref.prof)) {
			is.cat<-(plmat$cov==zecat[icat])
			if(!is.null(msim)) mpref<-aux.npdeplot.meanprof(mbin,msim[rep(is.cat,nrep),])
		} else {
			is.cat<-rep(TRUE,dim(plmat)[1])
			if(plot.opt$main=="" & !is.null(ref.prof)) {
				if(icat==1) {
					cat("The plot uses a reference profile\n")
					if(covsplit) tit<-paste("Reference profile:",tit,sep="")
				}
			}
		}
		plot.opt2<-plot.opt
		for(icol in ix) plot.opt2[[icol]]<-plot.opt[[icat]]
		plmat1<-plmat[is.cat,]
		xcal<-aux.npdeplot.transform(plmat1,plot.opt2,xat=xcent[xlab%in%plmat1$grp],mpref=mpref,distrib=distrib,onlog=onlog)
		plmat1<-xcal$plmat # now includes a ty column containing the data to plot
		#		cat(covsplit,ncat,dim(plmat),"-",dim(plmat1),"-",sum(is.cat),"\n")
		percobs<-xcal$percobs
		if(!is.null(mpref)) xat<-mpref$xat[xlab%in%plmat1$grp] else xat<-xcent[xlab%in%plmat1$grp]
		if(!is.null(sim.ypl)) sim.ypl1<-sim.ypl[rep(is.cat,nrep)] else sim.ypl1<-NULL
		xcal<-aux.npdeplot.computepi(plmat1,plot.opt,xlab=xlab,xat=xcent,mpref=mpref,dotline=dotline,sim.ypl=sim.ypl1,distrib=distrib,onlog=onlog)
		aux.npdeplot.plot(plmat1,percobs,plot.opt,main=tit,bnds=xcal$bnds,dotline=xcal$dline)	
	}
}
