library(stats)
library(dplyr)
library(data.table)
library(olsrr)
library(tictoc)
library(stringr)
data("rb")

dat <- rb |> filter(G >= 10 & G_act >= 10 & OPP_mean_act >= 10) |> select(-contains(c("FPTS", "PPR", "DPCHT"))) |> as.data.table()
dat$HALF_mean <- dat$HALF_mean + dat$FD_mean * 0.5
dat$HALF_mean_act <- dat$HALF_mean_act + dat$FD_mean_act * 0.5

mean_rb <- step_upwards(dat, "HALF_mean_act")

mean <- lm(formula(mean_rb$form[2]), dat) # choose n = 11
summary(mean)
generate_python_formula(mean, output_var = "proj_HALF_mean")

tot_rb <- step_upwards(dat, "HALF_act")

tot <- lm(formula(tot_rb$form[1]), dat) # choose n = 11
summary(tot)
generate_python_formula(tot, output_var = "proj_HALF")


# save step_upward runs
usethis::use_data(mean_rb, overwrite = TRUE)
usethis::use_data(tot_rb, overwrite = TRUE)
