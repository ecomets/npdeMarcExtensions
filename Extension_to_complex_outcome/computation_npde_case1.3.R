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


data_TTE = fread("Case_1.3/data_TTE_interval_cens.txt",na.strings = ".")
sim_TTE = fread("Case_1.3/sim_TTE.txt",na.strings = ".")





### Computation ########


### Base options: compute the npd
npde = main_compute_npde(obs = list(data_TTE),sim = list(sim_TTE),
                         type_outcome = c("TTE"),verbose = T,options=list(list(censoring_type="interval")))



### Test ########

### 
pval=main_compute_pvalue(npde,type_outcome = "TTE")
(global_pvalue = pval[[2]])



### Representation ########


### De-trended pd over time 
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(outcome="pd_wo_ties")))



#### Example with categorical covariate
npde[[1]][[1]][,sex:=sample(c("M","F"),size = length(id),replace = T)]
### scatter of npd over pred by specifying variable="ypred" 
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(variable=c("time","sex"),
                                                                       plot.opt=list(bin.breaks=seq(0,735,length.out = 7)))))


main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(outcome="pd_wo_ties",variable=c("time","sex"),
                                                                       plot.opt=list(bin.breaks=seq(0,735,length.out = 7)))))



#### Computation of VPC

### vpc with type="vpc
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(variable=c("time"),
                                                                       censoring_type="interval",
                                                                       plot.opt=list(bin.method="width",
                                                                                     bin.number=NULL,
                                                                                     bin.breaks=NULL,
                                                                                     # bin.breaks=c(0,300,600,1000,1500,2000),
                                                                                     bin.extreme=NULL,
                                                                                     xlog=F),
                                                                       type="vpc")))

### Survival shape
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(variable=c("time"),
                                                                       censoring_type="interval",
                                                                       plot.opt=list(bin.method="width",
                                                                                     bin.number=NULL,
                                                                                     bin.breaks=NULL,
                                                                                     # bin.breaks=c(0,300,600,1000,1500,2000),
                                                                                     bin.extreme=NULL,
                                                                                     xlog=F),
                                                                       type="vpc",
                                                                       var="surv")))


