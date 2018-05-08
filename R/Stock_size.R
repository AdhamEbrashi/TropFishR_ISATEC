#' title: "Introduction to TropFishR <br> Estimation of stock size"
#' author: "Lisa Chong"
#' date: "May 2018"
#' 


# Contents ####
#' 
#' Estimation of stock size.
#' Leslie and Davis model
#' VPA (cohort analysis- age and length)

# devtools::install_github("tokami/TropFishR")
require(TropFishR)



## Leslie and Davis (depletion/fishing success model) ####

months <- 1:7
effort <- c(70,100,170,300,250,200,100)
catch <- c(36.96,47.52,62.83,95.04,52.80,36.96,13.10)

# estimate CPUE
CPUE <- catch/ effort

# estimate cumulative catch with for loop
# create empty vecgtor
# fill vectors with loop
catch_cum <- rep(NA,length(catch))
catch_cum[1] <- catch[1]/2
for(i in 2:length(catch)){
  catch_cum[i] <- catch[i]/2 + sum(catch[1:i-1])
}

# use lm() to run linear regression on CPUE vs cummulative catch
mod <- lm(CPUE ~ catch_cum)
sum_mod <- summary(mod)

# save intercept and slope for q and N0 estimation
a <- sum_mod$coefficients[1]
b <- sum_mod$coefficients[2]


# estimate q and N0
q <- round(abs(b), digits = 5)
N0 <- round(abs(a/b), digits = 0)


plot(catch_cum, CPUE)
abline(a,b, col = "blue")
# starting size when blue line intersects x-axis
# q - slope of blue line



## Cohort Analysis (age) ####
# creating example
age <- 1:8
catch2 <- c(867,1732,1612,931,362,153,65,85)
# combine the vectors in a list:
example_dat <- list(age=age,catch=catch2, M = 0.2, a = 0.01, b=3)

res <- VPA(param = example_dat, terminalF = 0.3, analysis_type = "CA", plot = TRUE)

# param: list of midLengths, Linf, K, t0, M, length-weight (a and b), catch (for age based - catch provided in numbers)
# terminalF: fishing mortality rate of last age/length group
# analysis_type: "VPA", "CA"

sum(res$annualMeanNr)


##  Cohort Analysis (length) ####

lfq_list4 <- readRDS("lfq_list4.rds")
class(lfq_list4) <- "lfq"

lfq_list4 <- lfqModify(lfq_list4, vectorise_catch = TRUE, plus_group = 122)

# plus_group: indicating if last length group is a plus group (largest length bin or length bin with Linf in it)

lfq_list4$a <- 0.01
lfq_list4$b <- 3
lfq_list4$FM <- lfq_list4$Z - lfq_list4$M

res <- VPA(param = lfq_list4, terminalF = lfq_list4$FM, analysis_type = "CA", plot = TRUE)

sum(res$annualMeanNr)

lfq_list4$FM <- res$FM_calc
saveRDS(lfq_list4, file ="lfq_list5.rds")
```