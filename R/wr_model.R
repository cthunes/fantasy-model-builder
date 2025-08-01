library(stats)
library(dplyr)
library(data.table)
library(olsrr)
library(tictoc)
library(stringr)
data("wr")

dat <- wr |> filter(G >= 10 & G_act >= 10 & OPP_mean_act >= 5) |> select(-contains(c("FPTS", "PPR", "DPCHT"))) |> as.data.table()
dat$HALF_mean <- dat$HALF_mean + dat$FD_mean * 0.5
dat$HALF_mean_act <- dat$HALF_mean_act + dat$FD_mean_act * 0.5

mean_wr <- step_upwards(dat, "HALF_mean_act")

mean <- lm(formula(mean_wr$form[3]), dat) # choose n = 14
summary(mean)
generate_python_formula(mean, output_var = "proj_HALF_mean")

tot_wr <- step_upwards(dat, "HALF_act")

tot <- lm(formula(tot_wr$form[2]), dat) # choose n = 14
summary(tot)
generate_python_formula(tot, output_var = "proj_HALF")


# save step_upward runs
usethis::use_data(mean_wr, overwrite = TRUE)
usethis::use_data(tot_wr, overwrite = TRUE)



dat <- wr |> filter(G >= 10 & G_act >= 10 & OPP_mean_act >= 5) |> select(-contains(c("FPTS", "PPR", "DPCHT")))
cors1 <- cor(dat[4:(length(dat)/2)], dat$HALF_mean_act) |> as.data.frame()
cors2 <- cor(dat[4:(length(dat)/2)], dat$HALF_act) |> as.data.frame()
cors <- rbind(cors1, cors2)
cors <- cors |> arrange(-V1)
View(cors)

wr |> filter(G >= 6 & OPP_mean >= 4) |> group_by(year) |> summarize(n = n())
