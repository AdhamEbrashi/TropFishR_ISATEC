#' title: "Introduction to TropFishR <br> Estimation of mortality parameters"
#' author: "Lisa Chong"
#' date: "May 2018"
#' 


# Contents ####
#' 
#' Estimation of mortality rates (M, F, Z) and exploitation (E).

# devtools::install_github("tokami/TropFishR")
require(TropFishR)

lfq_list2 <- readRDS("lfq_list2.rdS")

# Natural mortality ####
(Ms <- M_empirical(Linf = res$Linf, 
                   K_l = res$K, 
                   tmax = res$agemax, 
                   temp = 27.2, 
                   method = c("Pauly_Linf", "Hoenig", "Then_tmax", "Then_growth")))
# calculate instantaneous mortality rate (M)- 12 different formulas
# K_l: K
# tmax: maximum age
# temp: average annual temperature at surface in degrees centrigrade
# method: "AlversonCarney", "Gislason" (size dependent mortality estimates), "GundersonDygert", "Hoenig", "Lorenzen", "Pauly_Linf", "Pauly_Winf", "PetersonWroblewski", "RikhterEfanov", "Roff", "Then_growth", or "Then_tmax"

(res$M <- mean(Ms[(2:5)]))



# Catch curve ####

res_cc <- catchCurve(param = res, 
                     catch_columns = 1:8,
                     reg_int = c(11,33))

# param: list of midLengths, Linf, K, t0, catch
# catch_columns: column of catch matrix which should be used for analysis 
# reg_int: can determine range with c(), or use NULL- pick points on plot

res$Z <- res_cc$Z



# Fishing Mortality ####

res$FM <- res$Z - res$M



# Exploitation ####

res$E <- res$FM/res$Z


# save data for next Rscript ####
lfq_list3 <- res
saveRDS(lfq_list3, file ="lfq_list3.rds")
