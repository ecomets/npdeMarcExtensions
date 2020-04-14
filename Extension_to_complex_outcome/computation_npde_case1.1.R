rm(list=ls())

library(ggplot2)
library(data.table)
library(gridExtra)
library(survival)
library(ggpubr)
library(here)
library(mclust)
setwd(here::here())
path="C:/Users/Marc/ownCloud/Documents/Projet 10 github/"
source(paste0(path,'all_funs.R'))


data_L = fread("Case_1.1/data_L.txt",na.strings = ".")
sim_L = fread("Case_1.1/sim_L.txt",na.strings = ".")





### Computation ########


### Base options: compute the npde with cholesky decorrelation method
npde = main_compute_npde(obs = list(data_L),sim = list(sim_L),
                         type_outcome = c("continuous"),verbose = T,options=NULL)

### compute the npde with polar decorrelation method
npde = main_compute_npde(obs = list(data_L),sim = list(sim_L),
                         type_outcome = c("continuous"),verbose = T,
                         options = list(list(decorr.method="polar")))

### compute the npde with ties in npde values: may impact the test
npde = main_compute_npde(obs = list(data_L),sim = list(sim_L),
                         type_outcome = c("continuous"),verbose = T,
                         options = list(list(ties=T)))

### compute the npd only 
npd = main_compute_npde(obs = list(data_L),sim = list(sim_L),
                         type_outcome = c("continuous"),verbose = T,
                         options = list(list(compute_npde=F)))



### Test ########
npde = main_compute_npde(obs = list(data_L),sim = list(sim_L),
                         type_outcome = c("continuous"),verbose = T,options=NULL)


### 
pval=main_compute_pvalue(npde,type_outcome = "continuous")
global_pvalue = pval[[2]]



### Representation ########

### Base plot options: scatter of npd over time
main_compute_plot(res=npde,type_outcome = c("continuous"),options = NULL)

### scatter of npd over binned time by specifying bin=T; default: "width" method, "equal"; "user"; "optimal" otherwise
main_compute_plot(res=npde,type_outcome = c("continuous"),options = list(list(variable="time",bin=T,
                                                                              plot.opt=list(bin.method="width",
                                                                                            bin.number=NULL,
                                                                                            bin.breaks=NULL,
                                                                                            bin.extreme=NULL,
                                                                                            xlog=F))))

### scatter of npd over pred by specifying variable="ypred" 
main_compute_plot(res=npde,type_outcome = c("continuous"),options = list(list(variable="ypred",bin=T,
                                                                              plot.opt=list(bin.method="width",
                                                                                            bin.number=NULL,
                                                                                            bin.breaks=NULL,
                                                                                            bin.extreme=NULL,
                                                                                            xlog=F))))

#### Example with categorical covariate
npde[[1]][[1]][,sex:=sample(c("M","F"),size = length(id),replace = T)]
### scatter of npd over pred by specifying variable="ypred" 
main_compute_plot(res=npde,type_outcome = c("continuous"),options = list(list(variable=c("time","sex"),bin=T,
                                                                              plot.opt=list(bin.method="width",
                                                                                            bin.number=NULL,
                                                                                            bin.breaks=NULL,
                                                                                            bin.extreme=NULL,
                                                                                            xlog=F))))

#### Example with continuous covariate
npde[[1]][[1]][,cont:=rweibull(length(id),1,500)]
### scatter of npd over pred by specifying variable="ypred" 
main_compute_plot(res=npde,type_outcome = c("continuous"),options = list(list(variable=c("cont"),bin=T,
                                                                              plot.opt=list(bin.method="equal",
                                                                                            bin.number=NULL,
                                                                                            # bin.breaks=NULL,
                                                                                            bin.breaks=c(0,300,600,1000,1500,2000),
                                                                                            bin.extreme=NULL,
                                                                                            xlog=F))))
#### Computation of VPC

### vpc with type="vpc
main_compute_plot(res=npde,type_outcome = c("continuous"),options = list(list(variable=c("time"),
                                                                              bin=T,
                                                                              plot.opt=list(bin.method="width",
                                                                                            bin.number=NULL,
                                                                                            bin.breaks=NULL,
                                                                                            # bin.breaks=c(0,300,600,1000,1500,2000),
                                                                                            bin.extreme=NULL,
                                                                                            xlog=F),
                                                                              type="vpc")))





