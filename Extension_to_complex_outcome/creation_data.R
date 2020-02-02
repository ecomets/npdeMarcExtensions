rm(list=ls())

library(ggplot2)
library(data.table)
library(gridExtra)
library(survival)
library(mlxR)
library(ggpubr)
library(here)
setwd(here::here())
path="C:/Users/Marc/ownCloud/Documents/Projet 10 github/"
source(paste0(path,'all_funs_v2.R'))


####### Load data and simulation for the longitudinal part

## data column : id time y status OFT
## sim column : rep id time y OFT


param="epsilon"
value_obs=0.3
value_sim=0.3
REP=3

load(paste0("obs_PSA.rda"))
load(paste0("sim_PSA.rda"))


#########################################################################
### One outcome #####
###### Case 1.1: creation of continuous data ######

data_L = obs$psa_wo_dropout[rep==REP][,c("rep","id","time","y"),with=F];data_L$rep=NULL
sim_L = sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]

data_L=format_extended_npde(data_L)
sim_L=format_extended_npde(sim_L)

path="Case_1.1"; dir.create(path)
write_data(list_data = data_L,path,name = "data_L.txt")
write_data(sim_L,path,"sim_L.txt")

###### Case 1.2:  creation of TTE data ######

data_TTE = obs$TTE[rep==REP,c("id","OFT","status"),with=F]
names(data_TTE) = c("id","time","y")
sim_TTE = sim$TTE[,c("rep","id","OFT","status"),with=F]
names(sim_TTE) = c("rep","id","time","y")

data_TTE=format_extended_npde(data_TTE)
sim_TTE=format_extended_npde(sim_TTE)

path="Case_1.2"; dir.create(path)
write_data(data_TTE,path,"data_TTE.txt")
write_data(sim_TTE,path,"sim_TTE.txt")

###### Case 1.3:  creation of interval censored TTE data ######

data_TTE = obs$TTE[rep==REP,c("id","OFT","status"),with=F]
names(data_TTE) = c("id","time","y")
sim_TTE = sim$TTE[,c("rep","id","OFT","status"),with=F]
names(sim_TTE) = c("rep","id","time","y")

data_TTE_interval_cens = copy(data_TTE)

names(data_TTE_interval_cens)[2]="X"

x = seq(0,max(data_TTE_interval_cens$X),by=21)

find_grp = function(time,x){
  sapply(time,function(z){
    temp = z-x
    temp2 = min(temp[temp>0])
    which(temp==temp2)  
  })
}

data_TTE_interval_cens[,grp:=find_grp(X,x)]
data_TTE_interval_cens[,`:=`(time=x[grp],time2=x[grp+1])]
data_TTE_interval_cens[y==0,`:=`(time=time2,time2=NA)]
data_TTE_interval_cens$rep=1
data_TTE_interval_cens=data_TTE_interval_cens[,c("rep","id","time","time2","y"),with=F]
# data_TTE_interval_cens=format_extended_npde(data_TTE_interval_cens)
sim_TTE=format_extended_npde(sim_TTE)

path="Case_1.3"; dir.create(path)
write_data(data_TTE_interval_cens,path,"data_TTE_interval_cens.txt")
write_data(sim_TTE,path,"sim_TTE.txt")



###### Case 1.4:  creation of categorical data ######

data_L = obs$psa_wo_dropout[rep==REP][,c("rep","id","time","y"),with=F];data_L$rep=NULL
sim_L = sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]

data_L=format_extended_npde(data_L)
sim_L=format_extended_npde(sim_L)

data_cat = copy(data_L)
data_cat[,y2:=as.numeric(factor(cut(y,breaks = c(-40,1,3,4,6,40))))]
data_cat[,y:=y2]
data_cat[,y2:=NULL]

sim_cat = copy(sim_L)
sim_cat[,y2:=as.numeric(factor(cut(y,breaks = c(-40,1,3,4,6,40))))]
sim_cat[,y:=y2]
sim_cat[,y2:=NULL]



path="Case_1.4"; dir.create(path)
write_data(data_cat,path,"data_cat.txt")
write_data(sim_cat,path,"sim_cat.txt")


#########################################################################
### 2 outcomes #####

###### Case 2.1:  creation of joint continuous+TTE data ######

data_L = obs$psa_with_dropout[rep==REP][,c("rep","id","time","y"),with=F];data_L$rep=NULL
sim_L = sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]

sim_L = merge(sim_L,data_L[,c("id","time"),with=T],by=c("id","time"))
setkeyv(sim_L,c("rep","id","time"))


data_TTE = obs$TTE[rep==REP,c("id","OFT","status"),with=F]
names(data_TTE) = c("id","time","y")
sim_TTE = sim$TTE[,c("rep","id","OFT","status"),with=F]
names(sim_TTE) = c("rep","id","time","y")


data_L=format_extended_npde(data_L)
data_TTE=format_extended_npde(data_TTE)
sim_L=format_extended_npde(sim_L)
sim_TTE=format_extended_npde(sim_TTE)


path="Case_2.1"; dir.create(path)
write_data(list_data = list(data_L,data_TTE),path,c("data_L.txt","data_TTE.txt"))
write_data(list(sim_L,sim_TTE),path,c("sim_L.txt","sim_TTE.txt"))

###### Case 2.2:  creation of joint continuous+interval censored TTE data ######

data_L = obs$psa_with_dropout[rep==REP][,c("rep","id","time","y"),with=F];data_L$rep=NULL
sim_L = sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]

sim_L = merge(sim_L,data_L[,c("id","time"),with=T],by=c("id","time"))
setkeyv(sim_L,c("rep","id","time"))


data_TTE = obs$TTE[rep==REP,c("id","OFT","status"),with=F]
names(data_TTE) = c("id","time","y")
sim_TTE = sim$TTE[,c("rep","id","OFT","status"),with=F]
names(sim_TTE) = c("rep","id","time","y")


data_L=format_extended_npde(data_L)
data_TTE=format_extended_npde(data_TTE)
sim_L=format_extended_npde(sim_L)
sim_TTE=format_extended_npde(sim_TTE)


data_TTE_interval_cens = copy(data_TTE)

names(data_TTE_interval_cens)[3]="X"

x = seq(0,max(data_TTE_interval_cens$X),by=21)

find_grp = function(time,x){
  sapply(time,function(z){
    temp = z-x
    temp2 = min(temp[temp>0])
    which(temp==temp2)  
  })
}

data_TTE_interval_cens[,grp:=find_grp(X,x)]
data_TTE_interval_cens[,`:=`(time=x[grp],time2=x[grp+1])]
data_TTE_interval_cens[y==0,`:=`(time=time2,time2=NA)]


path="Case_2.2"; dir.create(path)
write_data(list(data_L,data_TTE_interval_cens),path,c("data_L.txt","data_TTE_interval_cens.txt"))
write_data(list(sim_L,sim_TTE),path,c("sim_L.txt","sim_TTE.txt"))
###### Case 2.3:  creation of joint categorical+TTE data ######


data_L = obs$psa_with_dropout[rep==REP][,c("rep","id","time","y"),with=F];data_L$rep=NULL
sim_L = sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]

sim_L = merge(sim_L,data_L[,c("id","time"),with=T],by=c("id","time"))
setkeyv(sim_L,c("rep","id","time"))


data_TTE = obs$TTE[rep==REP,c("id","OFT","status"),with=F]
names(data_TTE) = c("id","time","y")
sim_TTE = sim$TTE[,c("rep","id","OFT","status"),with=F]
names(sim_TTE) = c("rep","id","time","y")


data_L=format_extended_npde(data_L)
data_TTE=format_extended_npde(data_TTE)
sim_L=format_extended_npde(sim_L)
sim_TTE=format_extended_npde(sim_TTE)

data_cat = copy(data_L)
data_cat[,y2:=as.numeric(factor(cut(y,breaks = c(-15,1,3,4,6,15))))]
data_cat[,y:=y2]
data_cat[,y2:=NULL]

sim_cat = copy(sim_L)
sim_cat[,y2:=as.numeric(factor(cut(y,breaks = c(-15,1,3,4,6,15))))]
sim_cat[,y:=y2]
sim_cat[,y2:=NULL]


path="Case_2.3"; dir.create(path)
write_data(list(data_cat,data_TTE),path,c("data_cat.txt","data_TTE.txt"))
write_data(list(sim_cat,sim_TTE),path,c("sim_cat.txt","sim_TTE.txt"))



