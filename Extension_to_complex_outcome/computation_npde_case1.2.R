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


data_TTE = fread("Case_1.2/data_TTE.txt",na.strings = ".")
sim_TTE = fread("Case_1.2/sim_TTE.txt",na.strings = ".")





### Computation ########


### Base options: compute the npd
npde = main_compute_npde(obs = list(data_TTE),sim = list(sim_TTE),
                         type_outcome = c("TTE"),verbose = T,options=NULL)





### Test ########
npde = main_compute_npde(obs = list(data_TTE),sim = list(sim_TTE),
                         type_outcome = c("TTE"),verbose = T,options=NULL)

### 
pval=main_compute_pvalue(npde,type_outcome = "TTE")
global_pvalue = pval[[2]]



### Representation ########

npde = main_compute_npde(obs = list(data_TTE),sim = list(sim_TTE),
                         type_outcome = c("TTE"),verbose = T,options=NULL)

### Base plot options: scatter of npd over time
main_compute_plot(res=npde,type_outcome = c("TTE"),options = NULL)



### equivalent to npde over time
npde[[1]][[1]][,y:=imputed_time]
main_compute_plot(res=npde,type_outcome = c("continuous"),options = list(list(outcome="npde",variable="medTime",adj=F,covariable="cont",bin=T,
                                                                              plot.opt=list(bin.method="equal",
                                                                                            bin.number=NULL,
                                                                                            bin.breaks=NULL,
                                                                                            bin.extreme=NULL,
                                                                                            xlog=F))))

npde = main_compute_npde(obs = list(data_TTE),sim = list(sim_TTE),
                         type_outcome = c("TTE"),verbose = T,options=NULL)
### De-trended pd over time 
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(outcome="pd_wo_ties")))


### scatter of npd over time with specific x breaks
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(outcome="pd_wo_ties",variable="time",
                                                                       plot.opt=list(bin.breaks=c(0,100,300,700)))))

### scatter of npd over time; imputed for censored event - imputed if true event time is computed !!!!!
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(outcome="pd_wo_ties",variable="imputed_time",
                                                                       plot.opt=list(bin.breaks=seq(0,735,length.out = 7)))))

#### Example with categorical covariate
npde[[1]][[1]][,sex:=sample(c("M","F"),size = length(id),replace = T)]
### scatter of npd over pred by specifying variable="ypred" 
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(variable=c("time","sex"),
                                                                       plot.opt=list(bin.breaks=seq(0,735,length.out = 7)))))





#### Computation of VPC

### vpc with type="vpc
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(variable=c("time"),
                                                                              # bin=T,
                                                                              plot.opt=list(bin.method="width",
                                                                                            bin.number=NULL,
                                                                                            bin.breaks=NULL,
                                                                                            # bin.breaks=c(0,300,600,1000,1500,2000),
                                                                                            bin.extreme=NULL,
                                                                                            xlog=F),
                                                                              type="vpc")))

### Survival shape
main_compute_plot(res=npde,type_outcome = c("TTE"),options = list(list(variable=c("time"),
                                                                       # bin=T,
                                                                       plot.opt=list(bin.method="width",
                                                                                     bin.number=NULL,
                                                                                     bin.breaks=NULL,
                                                                                     # bin.breaks=c(0,300,600,1000,1500,2000),
                                                                                     bin.extreme=NULL,
                                                                                     xlog=F),
                                                                       type="vpc",
                                                                       var="surv")))


















