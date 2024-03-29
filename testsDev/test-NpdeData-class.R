context("Testing creation of NpdeData\n")
#data("theopp",package='npde')
data("theopp")
write.table(theopp,file.path(tempdir(),"theopp.tab"),quote=F, row.names=F, na="NA")

test_that("Creating an empty NpdeData object", {
  x<-new(Class="NpdeData")
  expect_is(x,"NpdeData")
  expect_equal(x@N,0)
})

context("Creating an npdeData \n")

test_that("Successful creation of a NpdeData object by hand", {
  x<-new(Class="NpdeData")
  x@name.predictor<-"Time"
  x@name.group <- "ID"
  x@name.response <- "Conc"
  x@name.covariates <- "Wt"
  tab<-read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  x@N <- length(unique(tab[,x@name.group]))
  x@data <- data.frame(tab[,x@name.group], tab[,x@name.predictor], tab[,x@name.response], mdv=as.integer(is.na(tab[,x@name.response])), tab[,x@name.covariates] )
  colnames(x@data) <- c(x@name.group, x@name.predictor, x@name.response, "mdv", x@name.covariates)
  x@ntot.obs <- dim(x@data)[1]-sum(x@data$mdv)
  nobs<-unlist(tapply(x@data[x@data$mdv==0,x@name.group], x@data[x@data$mdv==0,x@name.group], length ))
  x@nind.obs <- as.numeric(nobs)
})
