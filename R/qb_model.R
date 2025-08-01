library(stats)
library(dplyr)
library(data.table)
library(olsrr)
library(tictoc)
library(stringr)
data("qb")

dat <- qb |> filter(G >= 10 & G_act >= 10 & OPP_mean_act >= 20) |> select(-contains(c("FPTS", "PPR", "DPCHT"))) |> as.data.table()
dat$HALF_mean <- dat$HALF_mean + dat$FD_mean * 0.5
dat$HALF_mean_act <- dat$HALF_mean_act + dat$FD_mean_act * 0.5

mean_qb <- step_upwards(dat, "HALF_mean_act")

mean <- lm(formula(mean_qb$form[2]), dat) # choose n = 7
summary(mean)
generate_python_formula(mean, output_var = "proj_HALF_mean")

tot_qb <- step_upwards(dat, "HALF_act")

tot <- lm(formula(tot_qb$form[1]), dat) # choose n = 7
summary(tot)
generate_python_formula(tot, output_var = "proj_HALF")


# save step_upward runs
usethis::use_data(mean_qb, overwrite = TRUE)
usethis::use_data(tot_qb, overwrite = TRUE)



dat <- qb |> filter(G >= 10 & G_act >= 10 & OPP_mean_act >= 20) |> select(-contains(c("FPTS", "PPR", "DPCHT")))
cors1 <- cor(dat[4:(length(dat)/2)], dat$HALF_mean_act) |> as.data.frame()
cors2 <- cor(dat[4:(length(dat)/2)], dat$HALF_act) |> as.data.frame()
cors <- rbind(cors1, cors2)
cors <- cors |> arrange(-V1)
View(cors)


qb |> filter(G >= 6 & OPP_mean >= 20) |> group_by(year) |> summarize(n = n())
