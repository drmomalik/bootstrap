library(here)
library(boot)
library(tidyverse)

data <- read.csv(here("data/finaldata.csv"), header = TRUE)
data2017 <- data %>% filter(year == 2017)

vars <- c("infmor", "neomor", "un5mor")
vars_ci <- data.frame(
  row.names = c("Basic", "Percentile", "BCA")
) 
  
for (i in 1:length(vars)) {
  getmeddiff <- function(data, indices) {
    sample_data <- data[indices, ]
    group_meds <- tapply(sample_data[[vars[i]]], sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
    meddiff <- group_meds[2] - group_meds[1]
    return(meddiff)
  }
  
  bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
  boot_med <- bootout$t0
  boot.ci <- boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))
  vars_ci[1,i] <- paste0(boot_med,"[",round(boot.ci$basic[4], digit = 1),",", round(boot.ci$basic[5], digit = 1),"]")
  vars_ci[2,i] <- paste0(boot_med,"[",round(boot.ci$percent[4], digit = 1),",", round(boot.ci$percent[5], digit = 1),"]")
  vars_ci[3,i] <- paste0(boot_med,"[",round(boot.ci$bca[4], digit = 1),",", round(boot.ci$bca[5], digit = 1),"]")
}
colnames(vars_ci) <- vars  
print(vars_ci)