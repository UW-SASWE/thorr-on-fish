library(ggplot2)
library(ggthemes)
library(dplyr)
library(readr)

library(patchwork)

setwd("~/OneDrive - UW/01-Research/02-Fish/methods/02-fish_mass_bal")

data = read.table("hanford_chinook_mass_bal_no_na.csv", header = TRUE, sep = ",")
# filter out only the fall data and temperature > 20
data = data %>% filter(run == "fall")

ggplot(
  data = data,
  mapping = aes(x = mean_temp, y = delta, color=mcnary)
) + geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = data,
  mapping = aes(x = mcnary, y = mean_temp, color=delta)
) + geom_point() +
  geom_smooth(method = "loess")
