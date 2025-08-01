library(stats)
library(dplyr)
library(data.table)
library(olsrr)
library(tictoc)
library(stringr)
data("k")

dat <- k |> filter(G >= 10 & G_act >= 10 & FGA_mean > 1 & FGA_mean_act > 1) |> select(-contains(c("FPTS", "PPR", "DPCHT"))) |> as.data.table()

mean_k <- step_upwards(dat, "HALF_mean_act")

mean <- lm(formula(mean_k$form[1]), dat) # choose n = 9
summary(mean)
generate_python_formula(mean, output_var = "proj_HALF_mean")

tot_k <- step_upwards(dat, "HALF_act")

tot <- lm(formula(tot_k$form[3]), dat) # choose n = 7
summary(tot)
generate_python_formula(tot, output_var = "proj_HALF")


# save step_upward runs
usethis::use_data(mean_k, overwrite = TRUE)
usethis::use_data(tot_k, overwrite = TRUE)



dat <- k |> filter(G >= 10 & G_act >= 10 & FGA_mean > 1 & FGA_mean_act > 1) |> select(-contains(c("FPTS", "PPR", "DPCHT")))
cors1 <- cor(dat[4:(length(dat)/2)], dat$HALF_mean_act) |> as.data.frame()
cors2 <- cor(dat[4:(length(dat)/2)], dat$HALF_act) |> as.data.frame()
cors <- rbind(cors1, cors2)
cors <- cors |> arrange(-V1)
View(cors)

k |> filter(G >= 6 & OPP_mean >= 6) |> group_by(year) |> summarize(n = n())
