library(ggplot2)
library(ggthemes)
library(dplyr)
library(readr)

library(patchwork)

setwd("~/OneDrive - UW/01-Research/02-Fish/methods/02-fish_mass_bal")

data = read.table("hanford_chinook_mass_bal_no_na.csv", header = TRUE, sep = ",")
up_priest_rapids = read.table("up_priest_rapids.csv", header = TRUE, sep = ",")

# filter out only the fall data and temperature > 20
data = data %>% filter(run == "fall")
up_priest_rapids = up_priest_rapids %>% filter(run == "fall")
data$date = as.Date(data$date)
data$location = "hanford"
up_priest_rapids$location = "up_priest_rapids"

# up_priest_rapids = up_priest_rapids %>% filter(run == "fall")
up_priest_rapids$date = as.Date(up_priest_rapids$date)

combined = rbind(data[, c("date", "mean_temp", "location")], up_priest_rapids[, c("date", "mean_temp", "location")])

data$del_ratio = data$delta / data$mcnary

ggplot(
  data = data,
  mapping = aes(x = mean_temp, y = delta
                # , color=mcnary
                )
) + geom_point() +
  # geom_smooth(method = "loess") +
  scale_y_continuous(name = "Delta Fish (at Hanford Reach)") +
  scale_x_continuous(name = "Mean Temperature (deg C)")

ggplot(
  data = data,
  mapping = aes(x = mean_temp, y = del_ratio, color=mcnary)
) + geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = data,
  mapping = aes(y = mcnary, x = mean_temp, color=delta)
) + geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = data,
  mapping = aes(y = mcnary, x = date, color=delta)
) + geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = data,
  mapping = aes(y = mcnary, x = delta, color=del_ratio)
) + geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = data,
  mapping = aes(y = del_ratio, x = date, color=delta)
) + geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = data,
  mapping = aes(y = delta, x = date, color=delta)
) + geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = data,
  mapping = aes(y = mean_temp, x = date, color=delta)
) + geom_point() +
  geom_smooth(method = "loess")+ coord_cartesian(ylim = c(16.5, 20))

ggplot(
  data = up_priest_rapids,
  mapping = aes(y = mean_temp, x = date)
) + geom_point() +
  geom_smooth(method = "loess")+ coord_cartesian(ylim = c(10, 15))

ggplot(combined, aes(x = date, y = mean_temp, color = location)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue"), name='Location', labels=c("Hanford", "Up Priest")) +
  geom_smooth(method = "loess") +
  theme_minimal() + coord_cartesian(ylim = c(17, 20))

han_pri = read.table("combined_han_pri.csv", header = TRUE, sep = ",")
han_pri = han_pri %>% filter(run_y == "fall")
han_pri$date = as.Date(han_pri$date)

ggplot(han_pri, aes(x = date, y = temp_gradient)) +
  geom_point() +
  geom_smooth(method = "loess") +
  coord_cartesian(ylim = c(-.5, 1))

han_pri$del_ratio = han_pri$delta / han_pri$mcnary

ggplot(han_pri, aes(date, del_ratio)) +
  # geom_point() +
  geom_smooth(method = "loess", color="blue") +
  # coord_cartesian(ylim = c(0, 1)) +
  geom_smooth(data = ~ transform(., temp_gradient_mod = scales::rescale(temp_gradient, range(del_ratio), range(temp_gradient))), aes(y = temp_gradient_mod+.965, color="red"),method = "loess") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Delta ratio (Del F / Mcnary F)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ scales::rescale(.-.965, range(han_pri$temp_gradient), range(han_pri$del_ratio)), name="Temperature Gradient (deg C)", )
  )

range (c(0, 1))
