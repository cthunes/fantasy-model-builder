## code to prepare `DATASET` dataset goes here

load_and_join <- function(pos) {
  year <- 2014
  pred <- read.csv(sprintf("aggregated/%s/all%d.csv", pos, year))
  act <- read.csv(sprintf("aggregated/%s/%d.csv", pos, year))
  act$year <- year
  merged <- merge(x = pred,y = act,by = c("Player", "TEAM", "POS"), all = TRUE, suffixes = c("", "_act"))
  for (year in 2015:2024) {
    pred <- read.csv(sprintf("aggregated/%s/all%d.csv", pos, year))
    act <- read.csv(sprintf("aggregated/%s/%d.csv", pos, year))
    act$year <- year
    merged <- rbind(merged, merge(x = pred, y = act,by = c("Player", "TEAM", "POS"), all = TRUE, suffixes = c("", "_act")))
  }
  merged
}

qb <- load_and_join("qb")
rb <- load_and_join("rb")
wr <- load_and_join("wr")
te <- load_and_join("te")
k <- load_and_join("k")
dst <- load_and_join("dst")


usethis::use_data(qb, overwrite = TRUE)
usethis::use_data(rb, overwrite = TRUE)
usethis::use_data(wr, overwrite = TRUE)
usethis::use_data(te, overwrite = TRUE)
usethis::use_data(k, overwrite = TRUE)
usethis::use_data(dst, overwrite = TRUE)
