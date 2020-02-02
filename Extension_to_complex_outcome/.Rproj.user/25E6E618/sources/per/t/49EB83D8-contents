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


data_L = fread("Case_2.2/data_L.txt",na.strings = ".")
sim_L = fread("Case_2.2/sim_L.txt",na.strings = ".")



data_TTE = fread("Case_2.2/data_TTE_interval_cens.txt",na.strings = ".")
sim_TTE = fread("Case_2.2/sim_TTE.txt",na.strings = ".")





### Computation ########


### Base options: compute the npd

npde = main_compute_npde(obs = list(data_L,data_TTE),sim = list(sim_L,sim_TTE),
                         type_outcome = c("continuous","TTE"),dependancies = list(2,0),verbose = T,
                         options=list(NULL,list(censoring_type="interval")))

### Test ########

### 
pval=main_compute_pvalue(res = npde,type_outcome = c("continuous","TTE"),options=list(NULL,NULL))

global_pvalue = pval[[3]]



### Representation ########


## npde
list_p = main_compute_plot(npde,type_outcome = c("continuous","TTE"),
                           options=list(list(outcome="npd",variable="time"),
                                        list(outcome="pd_wo_ties",variable="time")))
for(p in 1:length(list_p)) assign(paste0("p",p),list_p[[p]])
grid.arrange(p1,p2,ncol=1)


## VPC
list_p = main_compute_plot(npde,type_outcome = c("continuous","TTE"),
                           options=list(list(outcome="npd",variable="time",type="vpc"),
                                        list(outcome="pd_wo_ties",variable="time",type="vpc",censoring_type="interval")))
for(p in 1:length(list_p)) assign(paste0("p",p),list_p[[p]])
grid.arrange(p1,p2,ncol=1)
