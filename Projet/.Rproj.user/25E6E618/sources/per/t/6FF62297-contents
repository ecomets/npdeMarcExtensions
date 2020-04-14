aux.npdeplot.meanprof<-function(mbin,msim) {
# Compute a reference profile based on simulations from the model
# mbin : matrix with columns xat (centre of the bins) and xgrp (bin number/group)
# msim : matrix with simulations, 2 columns used (grp=which bin, ysim=simulated value)
	ymed<-tapply(msim[,"ysim"],msim[,"grp"],mean)
	sdmed<-tapply(msim[,"ysim"],msim[,"grp"],sd)
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
		bnds<-compute.bands.true(yprov,quant,xlab=xlab,alpha=alp.pi)
		bnds$xcent<-xat
	} else {
		nobs<-tapply(plmat$grp,plmat$grp,length)
		bnds<-compute.bands(nobs,nseuil,quant,distrib,alp.pi)
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
# dotprof: dotline profile
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
		if(!is.null(msim)) mpref<-aux.npdeplot.meanprof(mbin,msim[msim[,"use"]==1,]) else mpref<-NULL
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
		plmat1<-plmat[is.cat,]
		xcal<-aux.npdeplot.transform(plmat1,plot.opt,xat=xcent[xlab%in%plmat1$grp],mpref=mpref,distrib=distrib,onlog=onlog)
		plmat1<-xcal$plmat # now includes a ty column containing the data to plot
#		cat(covsplit,ncat,dim(plmat),"-",dim(plmat1),"-",sum(is.cat),"\n")
		percobs<-xcal$percobs
		if(!is.null(mpref)) xat<-mpref$xat[xlab%in%plmat1$grp] else xat<-xcent[xlab%in%plmat1$grp]
		if(!is.null(sim.ypl)) sim.ypl1<-sim.ypl[rep(is.cat,nrep)] else sim.ypl1<-NULL
		xcal<-aux.npdeplot.computepi(plmat1,plot.opt,xlab=xlab,xat=xcent,mpref=mpref,dotline=dotline,sim.ypl=sim.ypl1,distrib=distrib,onlog=onlog)
		aux.npdeplot.plot(plmat1,percobs,plot.opt,main=tit,bnds=xcal$bnds,dotline=xcal$dline)	
	}
}

npde.plot.meanprofile<-function(npdeObject,which="npde",xaxis="x",covsplit=FALSE, xscale=FALSE, onlog=FALSE, ref.prof=NULL, ...) {
	plot.opt.defaults<-function(plot.opt,which="npde") {
		plot.opt$new<-TRUE
		plot.opt$xlab<-switch(xaxis, x=paste(npdeObject["data"]["name.predictor"]," (", npdeObject["data"]["units"]$x,")",sep=""), pred=paste("Predicted ", npdeObject["data"]["name.response"]," (", npdeObject["data"]["units"]$y,")",sep=""), cov="")
		plot.opt$ylab<-switch(which,pd="pd",npd="npd",npde="npde",yobs=paste("Predicted ", npdeObject["data"]["name.response"]," (", npdeObject["data"]["units"]$y,")",sep=""))
		plot.opt$main<-""	
		return(plot.opt)
	}
	
	if(match(which,c("npde","pd","npd","yobs"),nomatch=0)==0) {
		cat("Option which=",which,"not recognised\n")
		return()
	}
	if(match(xaxis,c("x","pred","cov"),nomatch=0)==0) {
		cat("Option xaxis=",xaxis,"not recognised\n")
		return()
	}
	if(xaxis=="cov" & length(npdeObject["data"]["name.covariates"])==0) {
		cat("Option xaxis set to 'cov' but no covariate in dataset\n")
		return()
	}
	# checks on presence of the value to plot (no need to check when which is yobs, the data is always there)
	if(which=="npde" & length(npdeObject["results"]["res"]$npde)==0) {
		cat("    Missing npde object to plot.\n")
		return()
	}
	if(which %in% c("pd","npd") & length(npdeObject["results"]["res"]$pd)==0)  {
		cat("    Missing pd object to plot.\n")
		return()
	}
	if(which=="yobs") {
		xaxis<-"x"
		xscale<-FALSE
	}
	if(xaxis=="pred") xscale<-FALSE
	if(!xscale) ref.prof<-NULL
	if(length(npdeObject["data"]["icens"])>0) has.cens<-TRUE else has.cens<-FALSE
	args1<-match.call(expand.dots=TRUE)
	i1<-match("xlab",names(args1))
	if(!is.na(i1)) {
		change.xlab<-TRUE
	} else change.xlab<-FALSE
	i1<-match("ncat",names(args1))
	if(!is.na(i1)) {
		change.ncat<-TRUE
	} else change.ncat<-FALSE
	plot.opt<-npdeObject["prefs"]
	plot.opt<-plot.opt.defaults(plot.opt,which=which)
	plot.opt<-replace.plotoptions(plot.opt,...)
	ask<-plot.opt$ask
# Checking format for ref.prof is appropriate
	if(xscale & !is.null(ref.prof)) {
		if(!is.list(ref.prof)) {
			cat("The reference profile must be entered as a named list, eg list(ID=c(1,5)) to select subjects with ID=1 and 5 as reference; names should refer to columns in the data file.\n")
		ref.prof<-NULL
		}
	}
	
	if(!covsplit & plot.opt$new) {
		mfrow<-plot.opt$mfrow
		if(length(mfrow)==0) mfrow=c(1,1)
		par(mfrow=mfrow,ask=ask)
	}
	if(xaxis=="cov") {
		# 		plot.opt$box<-TRUE
		if(covsplit & npdeObject@options$verbose) cat("    graph versus covariates requested, setting covsplit to FALSE\n")
		covsplit<-FALSE
	}
	if(covsplit & length(npdeObject["data"]["name.covariates"])==0) {
		if(npdeObject@options$verbose) cat("No covariates in the dataset\n")
		covsplit<-FALSE
	}
#	if(which=="pd") plot.opt$ylim<-c(0,1) else plot.opt$ylim<-NULL
	if(which=="pd") distrib<-"unif" else distrib<-"norm"
	if(covsplit | xaxis=="cov") {
		if(plot.opt$which.cov=="") plot.opt$which.cov<-"all"
		if(is.numeric(plot.opt$which.cov)) plot.opt$which.cov<-npdeObject["data"]["name.covariates"][plot.opt$which.cov]
		if(plot.opt$which.cov=="all") {
			lcov<-npdeObject["data"]["name.covariates"]
			lunit<-npdeObject["data"]["units"]$covariates
		} else {
			icov<-match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
			lcov<-npdeObject["data"]["name.covariates"][icov]
			lunit<-npdeObject["data"]["units"]$covariates[icov]
		}
		ncov<-length(lcov)
		if(ncov==0) {
			cat("Cannot find covariate",plot.opt$which.cov,"in the dataset\n")
			covsplit<-FALSE
			if(xaxis=="cov") invisible(return()) # can't plot
		}
	} else ncov<-0
	if(xaxis=="cov" & length(plot.opt$mfrow)==0 & plot.opt$new) {
		n1<-round(sqrt(ncov))
		n2<-ceiling(ncov/n1)
		plot.opt$mfrow<-c(n1,n2)
		par(mfrow=plot.opt$mfrow,ask=ask)
	} 
	logtyp<-""
	if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
	if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")
	plot.opt$logtyp<-logtyp
	sim.ypl<-NULL
	if(!plot.opt$approx.pi) {
		if(which %in% c("pd","npd")) {
			if(length(npdeObject["results"]["pd.sim"])==0) {
				cat("You have requested to use simulations to provide the prediction intervals, but the simulated pd are not present.\n")
				plot.opt$approx.pi<-TRUE
			} else sim.ypl<-npdeObject["results"]["pd.sim"]
		}
		if(which=="npde") {
			if(length(npdeObject["results"]["npde.sim"])==0) {
				cat("You have requested to use simulations to provide the prediction intervals, but the simulated npde are not present.\n")
				plot.opt$approx.pi<-TRUE
			} else sim.ypl<-npdeObject["results"]["npde.sim"]
		}
	}
	if(which=="yobs" & plot.opt$bands) {
		sim.ypl<-npdeObject["sim.data"]["datsim"]$ysim
		plot.opt$approx.pi<-FALSE
	}
	if(which=="npde") ypl<-npdeObject["results"]["res"]$npde
	if(which %in% c("pd","npd")) ypl<-npdeObject["results"]["res"]$pd
	if(which=="yobs") ypl<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
	if(which=="npd") {
		ypl<-qnorm(ypl)
		if(!plot.opt$approx.pi) sim.ypl<-qnorm(sim.ypl)
	}
	if(xaxis=="x") xpl<-npdeObject["data"]["data"][, npdeObject["data"]["name.predictor"]]
	idobs<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
	if(xaxis=="cov") xpl<-rep(1,length(ypl))
	if(xaxis=="pred") xpl<-npdeObject["results"]["res"]$ypred
	if(xscale) msim<-npdeObject["sim.data"]["datsim"]$ysim else msim<-NULL
	if(has.cens) plmat<-data.frame(x=xpl,cens=npdeObject["data"]["data"][, npdeObject["data"]["name.cens"]],y=ypl) else plmat<-data.frame(x=xpl,cens=rep(0,length(xpl)),y=ypl)
	plmat<-plmat[npdeObject["data"]["not.miss"],]
	if(xscale) msim<-msim[rep(npdeObject["data"]["not.miss"],npdeObject["sim.data"]["nrep"])]
	if(!is.null(sim.ypl) & which=="yobs") sim.ypl<-sim.ypl[rep(npdeObject["data"]["not.miss"],npdeObject["sim.data"]["nrep"])] # for which="pd" or "npde", MDV lines have already been removed
# Reference profile: extracting matrix msim corresponding to the reference profile
	if(!is.null(msim)) {
		#	idx<-rep(1:dim(plmat)[1],npdeObject["sim.data"]["nrep"])
		if(!is.null(ref.prof) & xscale) {
			iuse<-rep(0,dim(plmat)[1])
			dat1<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],]
# 			if(is.logical(ref.prof)) {
# 				dat1<-cbind(my.indx=1:dim(dat1)[1],dat1)
# 				dat1<-subset(dat1,ref.prof)
# 				iuse<-rep(dat1[,1],npdeObject["sim.data"]["nrep"])
# 			} else {
				for(iref in 1:length(ref.prof)) {
					i<-names(ref.prof)[iref]
					i1<-which(dat1[,i] %in% ref.prof[[iref]])
					if(iref==1) idx1<-i1 else idx1<-intersect(idx1,i1)
				}
				iuse[idx1]<-1
				iuse<-rep(iuse,npdeObject["sim.data"]["nrep"])
#			}
		} else iuse<-rep(1,length(msim))
		msim<-data.frame(ysim=msim,use=iuse)	
	}
	if(which=="yobs") dotline<-NULL
	if(which %in% c("npd","npde")) {
		x1<-abs(qnorm((1-plot.opt$pi.size)/2))
		dotline<-c(-x1,0,x1)
	}
	if(which=="pd") dotline<-c((1-plot.opt$pi.size)/2,0.5,1-(1-plot.opt$pi.size)/2)
	if(covsplit | xaxis=="cov") {
	# Same limits for the different plots; can't be defined here, too narrow (or add an arbitrary factor to reflect the PI, which may not work with model misspecification)
		if(FALSE) {
			if(!xscale) {
				if(is.null(plot.opt$xlim) & xaxis!="cov") plot.opt$xlim<-c(min(xpl,na.rm=T), max(xpl,na.rm=T))
				if(is.null(plot.opt$ylim) & xaxis!="cov") plot.opt$ylim<-c(min(ypl,na.rm=T), max(ypl,na.rm=T))			
			}			
		}
		plmat<-cbind(plmat,cov=rep(1,dim(plmat)[1]))		
		for(icov in 1:ncov) {
			namcov<-lcov[icov]
			namunit<-lunit[icov]
			zecov<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],namcov]
			if(xaxis=="cov") plmat$x<-zecov
			ucov<-zecov[match(unique(idobs),idobs)]
			plot.box<-plot.opt$box
			# ECO: SECURISER 
			# detect type of covariate (continuous or )
			if(!is.numeric(ucov) | plot.opt$ncat>=length(unique(ucov))) {
				if(length(unique(ucov))>4) cat("Too many categories, the plot will not be informative.\n")
				if(!is.numeric(ucov) & xaxis=="cov") plot.box<-TRUE
				covcont<-FALSE
				ncat<-length(unique(ucov))
				if(xaxis=="cov") namcat<-sort(unique(zecov)) else namcat<-paste(namcov,sort(unique(zecov)),sep="=")
			} else {
				if(change.ncat | plot.opt$ncat!=3) {
					ncat<-plot.opt$ncat
					seqcat<-seq(0,1,length.out=(ncat+1))
					zecov<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)						
					nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
					namcat<-paste(namcov,nam1,sep=": ")
				} else {
					zecov<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
					ncat<-3
					namcat<-paste(namcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
				}
				covcont<-TRUE
				if(plot.box) plmat$x<-zecov
			}
			if(plot.opt$new & xaxis!="cov") {
				mfrow<-plot.opt$mfrow
				if(length(mfrow)==0) {
					if(ncat<=3) mfrow=c(1,ncat) else {
						n1<-round(sqrt(ncat))
						n2<-ceiling(ncat/n1)
						mfrow<-c(n1,n2)
					}
				}
				par(mfrow=mfrow,ask=ask)
			}
			zecat<-sort(unique(zecov))
			if(xaxis=="cov") {
				plmat$cov<-rep(1,dim(plmat)[1])
				namcat<-""
				if(!change.xlab) plot.opt$xlab<-namcov
				if(plot.box) {
					aux.scatter.box(plmat,plot.opt,dotline=dotline)
				}
				} else {
					plmat$cov<-zecov
					if(plot.box) {
						for(ic in 1:length(zecat)) aux.scatter.box(plmat[plmat$cov==zecat[ic],], plot.opt,dotline=dotline,main=namcat[ic])
					}
				}
			if(!plot.box) aux.npdeplot.main(plmat,plot.opt,namcat=namcat,msim=msim,ref.prof=ref.prof,sim.ypl=sim.ypl, dotline=dotline,distrib=distrib,onlog=onlog)
		}
	} else {
		plmat<-cbind(plmat,cov=rep(1,dim(plmat)[1]))
		if(plot.opt$box) aux.scatter.box(plmat,plot.opt,dotline=dotline,main=NULL) else aux.npdeplot.main(plmat,plot.opt,namcat=NULL,msim=msim,ref.prof=ref.prof,sim.ypl=sim.ypl,dotline=dotline,distrib=distrib,onlog=onlog)
		}
}

