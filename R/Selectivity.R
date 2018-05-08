#' title: "Introduction to TropFishR <br> Estimation of selectivity parameters"
#' author: "Lisa Chong"
#' date: "May 2018"
#' 


# Contents ####
#' 
#' Estimation of selectivity parameters.
#' Trawl-like
#' Gill net
#' Catch curve

# devtools::install_github("tokami/TropFishR")
require(TropFishR)



# Trawl-like ####

trawl_dat <- read.csv2("bottom_trawl.csv")


# need to convert dataframe into list
# m0 - small net
# m13 - large net
trawl_list <- list(type = "trawl_net",
                   midLengths = trawl_dat$midLengths, 
                   meshSizes = c(0,13),
                   CatchPerNet_mat = matrix(c(trawl_dat$m0, trawl_dat$m13), ncol = 2))


# use `select()` for selectivities 
res <- select(param = trawl_list, plot = FALSE)

# param: list of type (gillnet/trawl_net), midLengths, meshSizes in increasing order, and CatchPerNet_mat (matrix with catches per net in corresponding order of mesh sizes)

plot(res, regression_fit = FALSE)

# gives probabilities of capture



# Gillnet ####

gillnet <- readRDS("gillnetex.rds")

# looking at two gill net sizes

res <- select(param = tilapia, plot = FALSE)

plot(res, regression_fit = FALSE)



# Catch Curve with selectivity ####

lfq_list3 <- readRDS("lfq_list3.rds")

res <- catchCurve(lfq_list3, catch_columns = 1:8, calc_ogive = TRUE, reg_int = c(12,32))

# what's different here is that you set 'calc_ogive' to TRUE
# gives selectivity parameters

res$L50
res$L75


# assign values to lfq_list 
lfq_list3$L50 <- res$L50
lfq_list3$L75 <- res$L75


saveRDS(lfq_list3, file = "lfq_list4.rds")
