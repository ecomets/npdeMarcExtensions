
Different scenarios are presented to illustrate the computation of npde, with its test and representations. 

We consider in Case1.x a single outcome:
  - Case1.1 continuous outcome but it's better to use the npde package !
  - Case1.2 TTE outcome ; presence of right censoring only
  - Case1.3 TTE outcome ; all event are interval or right censored. 
  - Case1.4 categorical outcome which derives from the Case1.1 (categorisation of a continuous outcome)
  
We consider in Case2.x two outcomes:
  - Case2.1 Joint model with continuous+TTE outcome (TTE possibly right censored)
  - Case2.2 Joint model with continuous+TTE outcome (TTE possibly interval or right censored)
  - Case2.3 Joint model with categorical+TTE outcome (TTE possibly right censored)

3 functions are used:
  - main_compute_npde
  - main_compute_pvalue
  - main_compute_plot

The output of main_compute_npde is a list of list. The first element (res[[1]]) is associated with the first outcome, the second (res[[2]]) to the second outcome, etc...
When looking on the results of an outcome (res[[x]]), there are a list of 3 elements:
  - res[[x]][[1]] which corresponds to the observed data with the different values computed: pd/npd/pde/npde and some without ties (pd_wo_ties). Other variables can also be computed depending on the outcome. For more details, available soon !
  - res[[x]][[2]] which corresponds to the simulation used to computed the pd/npd. If it's not in the context of joint model, it corresponds to res[[x]][[3]]
  - res[[x]][[2]] which corresponds to the full simulation. 
  

The output of main_compute_pvalue is a list of list. The first element (res[[1]]) is associated with the first outcome, the second (res[[2]]) to the second outcome, etc.... The last element res[[n_outcome+1]] is the global corrected pvalue !
When looking on the results of an outcome (res[[x]]), there are a list of 6 elements:
  - the mean of the npd/npde
  - the se of the mean of the npd/npde
  - the variance of the npd/npde
  - the se of the variance of the npd/npde
  - a list of pvalue (Wilcoxon for median, Fisher for variance, Shapiro-Wilk for normality, Global adjusted pvalue)

The output of main_compute_plot is a list of list. The first element (res[[1]]) is associated with the first outcome, the second (res[[2]]) to the second outcome, etc.... The last element res[[n_outcome+1]] is the global corrected pvalue !
When looking on the results of an outcome (res[[x]]), there is a ggplot object, which can be customised. 


For each of the main functions, it is possible to add options. Note that all combinations of options are not available. 
As before, they are defined as list of list. First for each outcome, and then a list of options like the nature of the outcome etc...

ex: options = list(list(outcome="npd"),list(outcome="npd"))


List of options with default values: 
  - order_cat = paste0(sort(unique(c(npde_o$y,sim_o$y))))    ;;; if categorical values, the name of the categories, detected from the data if not specified 
  - censoring_type="right"                                   ;;; if TTE data, censoring_type is in c("right","interval"). If interval is specified, time2 must be presented in the data (formalisation of "interval2" in the Surv function: time: lower limit of the interval, time2: upper limit). time2 can be NA or Inf if right censored event
  - outcome="npd"                                           ;;; for plot
  - variable="time"                                         ;;; scatter plot against the variable
  - conf=0.9                                                ;;; prediction interval
  - type="scatter"                                          ;;; plot of npd/npde over variable: "scatter", classical VPC: type="vpc"
  - bin=F                                                   ;;; if the variable (time or pred) should be binned
  - plot.opt = list(bin.method="width",                     ;;; binning options "width","equal","user","optimal" (last one with the package mclust)
                  bin.number=NULL,
                  bin.breaks=NULL,                          ;;; either the breaks for the bin, eiter the breaks for the plot
                  bin.extreme=NULL,
                  xlog=F)
  - adj=T                                                   ;;; de-trended pd/npd over time. 
  - covariate="cat"                                         ;;; the type of the covariate. ex: variable=c("time","sex"). "sex" is categorical
  - var="prob"                                              ;;; the interpretation of the data: "prob"; probability of event. Any other character "vpc" or even "John Doe"; survival. Ex: var="prob" ; if dots are over the PI => under-prediction of the probability of event. 
  
  
