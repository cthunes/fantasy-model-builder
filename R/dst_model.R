library(stats)
library(dplyr)
library(data.table)
library(olsrr)
library(tictoc)
library(stringr)
data("dst")

dat <- dst |> select(-c("YOE", "G")) |> select(-contains(c("FPTS", "PPR", "DPCHT"))) |> as.data.table()

mean_dst <- step_upwards(dat, "HALF_mean_act")

mean <- lm(formula(mean_dst$form[2]), dat) # choose n = 11
summary(mean)
generate_python_formula(mean, output_var = "proj_HALF_mean")

tot_dst <- step_upwards(dat, "HALF_act")

tot <- lm(formula(tot_dst$form[3]), dat) # choose n = 8
summary(tot)
generate_python_formula(tot, output_var = "proj_HALF")


# save step_upward runs
usethis::use_data(mean_dst, overwrite = TRUE)
usethis::use_data(tot_dst, overwrite = TRUE)
