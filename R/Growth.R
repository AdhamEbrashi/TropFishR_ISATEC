#' title: "Introduction to TropFishR <br> Estimation of growth parameters"
#' author: "Lisa Chong"
#' date: "May 2018"



# Contents ####
#' 
#' Estimation of growth parameters (Linf, K, t0) from length-at-age data, tagging data, and length-frequency data.
#' Ford-Walford
#' von Bertalanffy
#' non-linear least square method
#' Munro
#' Gulland and Holt
#' LFQ data
#' ELEFAN SA
#' ELEFAN GA

# devtools::install_github("tokami/TropFishR")
require(TropFishR)

# working directory
getwd()
# setwd()


#' Generating length-at-age data
set.seed(1) # makes data reproducible
dat <- list(age = rep(1:7,each = 5),
            length = c(rnorm(5,25.7,0.9),rnorm(5,36,1.2),rnorm(5,42.9,1.5),rnorm(5,47.5,2),
                       rnorm(5,50.7,0.4),rnorm(5,52.8,0.5),rnorm(5,54.2,0.7))) # generated data



# Ford-Walford Plot ####

res <- growth_length_age(param = as.list(dat), method = "FordWalford")
#' param: list consisting of age and length measurements
#' method: "FordWalford"

res$Linf ; res$K


# t0 estimate
get_t0 <- function(Lt,t,Linf,K){
  t0 <- t + (1/K) * log(1-(Lt/Linf))
  return(t0)
} # function created based on equation to get values for t0

# highlight get_t0(Lt...)... (below) to look at the t0 values (for each data point)
mean(get_t0(Lt = res$length, t = res$age, Linf = res$Linf, K = res$K))



# von Bertalanffy growth function ####

res <- growth_length_age(dat, method ="BertalanffyPlot", Linf_est = 70)
# method: "BertalanffyPlot"
# need an initial estimate of Linf
res$Linf; res$K; res$t0


# check influence of Linf
opar <- par(mfrow=c(3,2))
seqi <- seq(from= -2.5, to= 3, by= 1)
seqi

# create for loop to go through different estimates of Linf
for(i in seqi){
  res <- growth_length_age(as.list(dat), method = "BertalanffyPlot",
                           Linf_est = (70 + i))
  r2 <- res$r2
  print(c(75+i,r2))
}

# first column - estimate of Linf
# second column - r2 value (regression coefficient), want to maximize this

dev.off() # resets graphics



# non-linear least square ####
lsm <- growth_length_age(as.list(dat), 
                         method= "LSM",
                         Linf_init = 30, 
                         CI = TRUE, 
                         age_plot=NULL)
# method: "LSM"
# Linf_int: initial parameter for non-linear squares fitting
# CI: confidence interval
# age_plot: sequence with ages used for plotting

lsm$Linf; lsm$K ; lsm$t0



# generate tagging data
dat <- list(L1 = c(40,46,29,30,18,31,48,49,59,58,61,65,57,55),
            L2 = c(85,53,55,56,25,43,70,59,62,80,72,83,65,56),
            delta_t = c(289,26,84,77,14,38,89,38,28,149,89,74,38,21))



# Munro ####
res <- growth_tagging(param = dat, method = "Munro", time_unit = "day", Linf_range=c(80,120))
# method: Munro
# Linf_range: lower and upper limits of range --> optimize searches for Linf value with best fit (lowest CV value)
# time_unit: time interval (year, month, week, day)

# mean of individual K values
mean(res$indi_Ks, na.rm= TRUE)



# Gulland and Holt ####
res <- growth_tagging(param = dat, "GullandHolt", time_unit = "day")
# method: "GullandHolt"
res$Linf; res$K; res$r2



# Length-frequency (LFQ) ####
# One of simulated data-sets within package
dat <- read.csv2("synLFQ7.csv") # list of individuals, lengths, dates
head(dat)
str(dat)

# need to make dates into date class
dat$date <- as.Date(dat$date)
# create length frequency with `lfqCreate()`
lfq_list <- lfqCreate(data = dat, 
                      Lname = "length",
                      Dname = "date",
                      bin_size = 4) # size of bins (where lengths are placed into)
# output = LFQ data
# create midLengths, and catch (not actual catch, frequency of lengths within bins)


# plot results
opar <- par(mfrow= c(2,1), mar= c(2,5,2,3), oma= c(2,0,0,0))
lfqbin1 <- lfqRestructure(param = lfq_list, MA = 5, addl.sqrt = FALSE) # restructure lfq with moving average (MA)
# MA setting of 5 is in FiSat, can only be odd numbers
# first step of ELEFAN
# param: list of midLengths, dates, catch
# MA: moving average- number of how many length classes the MA should be performed
# addl.sqrt: squareroot transformation of positive values - useful if many observations have low frequency
lfqbin2 <- lfqRestructure(lfq_list, MA = 13) 
plot(lfqbin1)
plot(lfqbin2)

# Compare...
# black bins - positive scores (more likely growth curve will go through)
# white bins - negative scores (less likely growth curve will go through)

dev.off() # reset graphics



# ELEFAN SA (simulated annealing) ####
sa_res <- ELEFAN_SA(x = lfq_list, seasonalised = FALSE,
                    MA = 13,
                    addl.sqrt = TRUE,
                    SA_temp = 3e5, SA_time = 60*1,
                    init_par = list(Linf = max(lfq_list$midLengths)/0.95, 
                                    K = 0.5, 
                                    t_anchor = 0.5),
                    low_par = list(Linf = (max(lfq_list$midLengths/0.95) - 20), 
                                   K = 0,
                                   t_anchor = 0),
                    up_par = list(Linf = (max(lfq_list$midLengths/0.95) + 20),
                                  K = 1, 
                                  t_anchor = 1))
# x: list of midLengths, dates, catch
# seasonalised: seasonalised VBGF --> TRUE
# SA_temp: initial value for temperature
# SA_time: maximum running time in seconds (should increase this, at least 3 minutes, so 60*3)
# init_par: initial values for components to be optimized
# low_par: lower bounds
# up_par: upper bounds
# using maximum length bin/0.95 in this case

sa_res$par$Linf; sa_res$par$K


plot(sa_res)



# ELEFAN GA (genetic algorithm) ####
ga_res <- ELEFAN_GA(x = lfq_list,
                    seasonalised = FALSE,
                    MA = 13,
                    addl.sqrt = TRUE,
                    maxiter = 10,
                    low_par = list(Linf = (max(lfq_list$midLengths)/0.95) - 20, K = 0),
                    up_par = list(Linf = (max(lfq_list$midLengths)/0.95) + 20, K = 1))

# maxiter: maximum number of iterations to run before GA search is halted (should increase, usually 100 - takes a while)

ga_res$par$Linf; ga_res$par$K

plot(ga_res)


# no real big difference between GA and SA... 


# save data for Mortality Rscript
lfq_list2 <- c(lfq_list, ga_res$par, list(agemax = ga_res$agemax))
getwd()
saveRDS(lfq_list2, file = "lfq_list2.rds")
