#' title: "Introduction to TropFishR <br> Estimation of yield"
#' author: "Lisa Chong"
#' date: "May 2018"
#' 


# Contents ####
#' 
#' Estimation of yield with YPR.
#' Beverton and Holt
#' Thompson and Bell
#' Schaefer and Fox
#' Schaefer
#' Fox

# devtools::install_github("tokami/TropFishR")
require(TropFishR)



## Beverton and Holt YPR ####
# create a list with input parameters, e.g.:
dat2 <- list(FM = 1.5,  # fishing mortality
             M = 0.35,  # natural mortality
             tc = 0.8,  # age at first capture 
             tr = 0.2,  # age at recruitment (to fishery)
             K = 1.99,  # growth coefficient
             t0 = 0.15, # theoretical age at length zero
             Winf = 50) # asymptotic weight in [g]


# model can be run with 'predict_mod'
output <- predict_mod(param = dat2, type = "ypr",
                      stock_size_1 = 1, 
                      FM_change = seq(0,6,0.1),
                      tc_change = seq(0.2,1,0.2),
                      hide.progressbar = TRUE)

# stock_size_1: get results per recruit (first age group)


plot(output)



# without tc_change

outputa <- predict_mod(dat2, type = "ypr",
                       stock_size_1 = 1, 
                       FM_change = seq(0,6,0.1),
                       hide.progressbar = TRUE)


plot(outputa)
# now gives outputs of F0.1, F0.5, Fmsy



## Thompson and Bell ####
# snapshot version 
lfq_list5 <- readRDS("lfq_list5.rds")

res <- predict_mod(param = lfq_list5,
                   type = "ThompBell",
                   FM_change = seq(0,2,0.01),
                   plot = TRUE,
                   hide.progressbar = TRUE)
# only looks at FM changes to give YPR values


# now look at different mesh sizes
res <- predict_mod(param = lfq_list5,
                   type = "ThompBell",
                   FM_change = seq(0,2.5,0.1), Lc_change = seq(5,40,0.1),
                   stock_size_1 = 1,
                   curr.E= lfq_list5$E, curr.Lc = lfq_list5$L50,
                   s_list= list(selecType= "trawl_ogive",
                                L50 = lfq_list5$L50, L75= lfq_list5$L75),
                   plot= TRUE, hide.progressbar = TRUE)

# now Lc_change
# s_list: selectivity list created (trawl-like)
# creates biomass Isopleth graph



## Schaefer and Fox model- equilibrium production model####
# create data
# Y - yield
# f - effort
dat3 <- data.frame(year = 1969:1977,
                   Y = c(50,49,47.5,45,51,56,66,58,52),
                   f = c(623,628,520,513,661,919,1158,1970,1317))

res <- prod_mod(dat3, plot = TRUE)
# compares Schaefer and Fox
# requires yield and effort for each year



##  Schaefer - dynamic production model ####

# no linear --> no equilibrium
res <- prod_mod_ts(dat3, method = "Schaefer")

## Fox - dynamics production model ####

res <- prod_mod_ts(dat3, method = "Fox")


