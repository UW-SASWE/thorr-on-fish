library(ggplot2)
library(ggthemes)
library(dplyr)
library(readr)
library(trend)
library(ggtext)

library(patchwork)

setwd("~/OneDrive - UW/01-Research/02-Fish/")

data = read.table("methods/02-fish_mass_bal/hanford_chinook_mass_bal.csv",
                  header = TRUE,
                  sep = ",")
data = data %>% filter(run == "fall") %>% filter(year > 1985)
data$date = as.Date(data$date)

central_tendencies = read.table("methods/02-fish_mass_bal/hanford_central_tendencies.csv",
                                header = TRUE,
                                sep = ",")

mcn_prd_pittag = read.table("methods/02-fish_mass_bal/mcn_prd_pittag.csv",
                            header = TRUE,
                            sep = ",")

mcn_prd_binned = read.table("methods/02-fish_mass_bal/mcn_prd_pittag_binned.csv",
                            header = TRUE,
                            sep = ",")

run_day_v_temp = ggplot(data = data) +
  geom_point(mapping = aes(x = days_from_fall, y = mean_temp)) +
  geom_smooth(mapping = aes(x = days_from_fall, y = mean_temp),
              method = "loess") +
  theme_bw() +
  xlab("Fall Run Day") +
  ylab("Temperature (°C)") +
  ggtitle('(a)')

# Figure 7 - 
run_day_v_outflow = ggplot(data = data) +
  geom_point(mapping = aes(x = days_from_fall, y = outflow.m3.d. / 1e6)) +
  geom_smooth(mapping = aes(x = days_from_fall, y = outflow.m3.d. / 1e6),
              method = "loess") +
  theme_bw() +
  xlab("Fall Run Day") +
  ylab(expression(paste("Outflow (x", 10 ^ 6, " ", m ^ 3, "/d)"))) +
  ggtitle('(b)')

run_day_v_fishcount = ggplot(data = data) +
  geom_point(mapping = aes(x = days_from_fall, y = mcnary / 1e3)) +
  geom_smooth(mapping = aes(x = days_from_fall, y = mcnary / 1e3),
              method = "loess") +
  theme_bw() +
  xlab("Fall Run Day") +
  ylab(expression(paste("Fish Count (x", 10 ^ 3, ")"))) +
  ggtitle('(c)')

figure7 = run_day_v_temp + run_day_v_outflow + run_day_v_fishcount
ggsave("methods/99-data_visualization/ch2/figure7.png", figure7, width = 12, height = 4, dpi = 600)
# ggsave("/Users/gdarkwah/Library/CloudStorage/OneDrive-UW/04-Conferences/AGU24/Oral Presentation/graphics/figure7.png", figure7, width = 12, height = 4, dpi = 600)


# Figure 8 - 
mcnary_del_corr = cor.test(data$mcnary, data$delta, method = c("pearson"))
mcnary_del_corr

mcnary_del_r_corr = cor.test(data$mcnary, data$delta_ratio, method = c("pearson"))
mcnary_del_r_corr

doy_delta = ggplot(data = data) +
  geom_point(mapping = aes(x = days_from_fall, y = delta / 10e3)) +
  geom_smooth(
    mapping = aes(x = days_from_fall, y = delta / 10e3),
    method = "loess",
    se = TRUE
  ) +
  labs(title = "(a)",
       x = "Fall Run Day",
       y = expression(paste("Fish Retention, ΔF (x", 10 ^ 3, ")"))) +
  theme_bw()

mcnary_delta = ggplot(data = data) +
  geom_point(mapping = aes(x = mcnary / 10e3, y = delta / 10e3)) +
  geom_smooth(
    mapping = aes(x = mcnary / 10e3, y = delta / 10e3),
    method = "lm",
    se = FALSE
  ) +
  # add annotation with correlation coefficient
  annotate("text",
           x = 10,
           y = 1,
           label = paste("r = ", round(mcnary_del_corr$estimate, 2))) +
  labs(title = "(b)",
       x = expression(paste("Fish Count (x", 10 ^ 3, ")")),
       y = expression(paste("Fish Retention, ΔF (x", 10 ^ 3, ")"))) +
  theme_bw() +
  theme(legend.position = "inside")

mcnary_delta_ratio = ggplot(data = data) +
  geom_point(mapping = aes(x = mcnary / 10e3, y = delta_ratio)) +
  # add annotation with correlation coefficient using cartesian coordinates
  annotate("text",
           x = 10,
           y = -1.5,
           label = paste("r = ", round(mcnary_del_r_corr$estimate, 2))) +
  labs(title = "(c)",
       x = expression(paste("Fish Count, ΔF (x", 10 ^ 3, ")")),
       y = "Retention Ratio, R") +
  theme_bw()

figure8 = doy_delta + mcnary_delta + mcnary_delta_ratio
ggsave("methods/99-data_visualization/ch2/figure8.png", figure8, width = 12, height = 4, dpi=600)

# Figure 9 -
doy_delta_r = ggplot(data = data) +
  geom_point(mapping = aes(x = days_from_fall, y = delta_ratio)) +
  geom_smooth(
    mapping = aes(x = days_from_fall, y = delta_ratio),
    method = "loess",
    se = TRUE
  ) +
  labs(title = "(a)",
       x = "Fall Run Day",
       y = "Retention Ratio, R") +
  theme_bw()

ggplot(data = data) +
  geom_line(mapping = aes(x = days_from_fall, y = delta_ratio, color = factor(year))) +
  theme_bw()

mean_temp_delta_r = ggplot(data = data) +
  geom_point(mapping = aes(x = mean_temp, y = delta_ratio)) +
  geom_smooth(
    mapping = aes(x = mean_temp, y = delta_ratio),
    method = "loess",
    se = TRUE
  ) +
  # # add a vertical line at y = 0
  # geom_vline(xintercept = 19.0, linetype = "dashed") +
  labs(title = "(b)",
       x = "Mean Temperature (°C)",
       y = "Retention Ratio, R") +
  theme_bw()

figure9 = doy_delta_r + mean_temp_delta_r
ggsave("methods/99-data_visualization/ch2/figure9.png", figure9, width = 8, height = 4)
# ggsave("/Users/gdarkwah/Library/CloudStorage/OneDrive-UW/04-Conferences/AGU24/Oral Presentation/graphics/figure9.png", figure9, width = 8, height = 4, dpi = 600)

ggplot(data = data) +
  geom_point(mapping = aes(x = date, y = mean_temp)) +
  geom_smooth(
    mapping = aes(x = date, y = mean_temp),
    method = "loess",
    se = TRUE
  ) +
  theme_bw()



ggplot(data = central_tendencies) +
  geom_point(mapping = aes(x = mean_mean_temp, y = yearly_delta_ratio)) +
  geom_smooth(mapping = aes(x = mean_mean_temp, y = yearly_delta_ratio),
              method = "lm")

#fit a regression model
model <- lm(yearly_delta_ratio ~ mean_mean_temp, data = central_tendencies)

#fit a regression model
# model <- lm(delta~mcnary, data=data)

#get list of residuals
res <- resid(model)

#produce residual vs. fitted plot
plot(fitted(model), res)

#add a horizontal line at 0
abline(0, 0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res)
#Create density plot of residuals
plot(density(res))

ggplot(data = data) +
  geom_point(mapping = aes(x = mean_temp, y = outflow.m3.d., color = delta_ratio))



ggplot(data = data) +
  geom_point(mapping = aes(x = mean_temp, y = delta_ratio)) +
  geom_smooth(
    mapping = aes(x = mean_temp, y = delta_ratio),
    method = "loess",
    se = TRUE
  )

ggplot(data = data) +
  geom_point(mapping = aes(x = mcnary, y = delta_ratio))




ggplot(data = data) +
  geom_point(mapping = aes(x = days_from_fall, y = delta_ratio)) +
  geom_smooth(
    mapping = aes(x = days_from_fall, y = delta_ratio),
    method = "loess",
    se = TRUE
  ) +
  coord_cartesian(xlim = c(1, NA)) +
  labs(title = "(a)", x = "Fall Run Day", y = "Retention Ratio, R") +
  theme_bw()

ggplot(data = data) +
  geom_point(mapping = aes(x = mean_temp, y = outflow.m3.d.)) +
  geom_smooth(mapping = aes(x = mean_temp, y = outflow.m3.d.),
              method = "loess")

ggplot(data = data) +
  geom_point(mapping = aes(x = date, y = outflow.m3.d.))

ggplot(data = data) +
  geom_point(mapping = aes(x = doy, y = outflow.m3.d. / 1e8)) +
  geom_smooth(mapping = aes(x = doy, y = outflow.m3.d. / 1e8),
              method = "loess")

ggplot(data = data) +
  geom_point(mapping = aes(x = date, y = mean_temp))

ggplot(data = data) +
  geom_point(mapping = aes(x = doy, y = delta_ratio)) +
  geom_smooth(mapping = aes(x = doy, y = delta_ratio), method = "loess") +
  coord_cartesian(ylim = c(-0, 1))

ggplot(data = data) +
  geom_point(mapping = aes(x = doy, y = mcnary)) +
  geom_smooth(mapping = aes(x = doy, y = mcnary), method = "loess")

ggplot(data = data) +
  geom_point(mapping = aes(x = doy, y = delta_ratio)) +
  geom_smooth(mapping = aes(x = doy, y = delta_ratio), method = "loess")

# mcnary_fish = data %>% filter(run == "fall") %>% select(date, days_from_fall, mcnary)
# mcnary_fish$site = "fish_in"
#
# out_fish = data %>% filter(run == "fall") %>% select(date, days_from_fall, prosser, ice_harbor, priest_rapids)
#
# out_fish$sum = rowSums(out_fish[,3:5], na.rm = TRUE)
# out_fish$site = "fish_out"
#
# colnames(mcnary_fish)[colnames(mcnary_fish) == 'mcnary'] <- 'fish_count'
# colnames(out_fish)[colnames(out_fish) == 'sum'] <- 'fish_count'
#
# fish = rbind(mcnary_fish, out_fish[,c(1,2,6,7)])
#
# ggplot(data = fish) +
#   geom_point(mapping = aes(x = days_from_fall, y = fish_count, shape = site)) +
#   geom_smooth(mapping = aes(x = days_from_fall, y = fish_count, color = site), method = "loess") +
#   theme_bw() +
#   xlab("Fall Run Day") +
#   ylab("Fish Count")





# # add a horizontal line at max temp
# geom_hline(yintercept = max(data$mean_temp), linetype="dashed", color = "red") +
# geom_hline(yintercept = 20.75, linetype="dashed", color = "blue") +
# geom_hline(yintercept = 19.4, linetype="dashed", color = "blue")

max(data$mean_temp)

ggplot(data, aes(doy, delta_ratio)) +
  # geom_point() +
  geom_smooth(method = "loess", color = "blue") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_smooth(
    data = ~ transform(., mean_temp_mod = scales::rescale(mean_temp, range(0, 1), range(mean_temp))),
    aes(y = mean_temp_mod + .01, color = "red"),
    method = "loess"
  ) +
  scale_y_continuous(
    # Features of the first axis
    name = "Delta ratio (Del F / Mcnary F)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ scales::rescale(. - .01, range(data$mean_temp), range(0, 1)), name =
                          "Temperature (deg C)", )
  )

central_tendencies = read.table("methods/02-fish_mass_bal/hanford_central_tendencies.csv",
                                header = TRUE,
                                sep = ",")

ggplot(data = central_tendencies) +
  geom_point(mapping = aes(x = mean_mean_temp, y = mean_delta_ratio)) +
  geom_text(
    mapping = aes(x = mean_mean_temp, y = mean_delta_ratio, label = group_5_year),
    nudge_x = 0.025,
    nudge_y = 0.025,
    size = 2.5
  ) +
  geom_smooth(mapping = aes(x = mean_mean_temp, y = mean_delta_ratio),
              method = "lm")

# Figure 10 - Interannual temperature and delta ratio
interannual_mean_temp = ggplot(data = central_tendencies) +
  geom_point(mapping = aes(x = group_5_year, y = mean_mean_temp)) +
  geom_line(mapping = aes(x = group_5_year, y = mean_mean_temp)) +
  geom_ribbon(aes(x = group_5_year,ymin = p25_mean_temp, ymax = p75_mean_temp), fill = "grey70", alpha = 0.5) +
  geom_smooth(mapping = aes(x = group_5_year, y = mean_mean_temp), method='lm', linetype = "dashed", se=FALSE, color="black") +
  geom_hline(yintercept = 19, color="red") +
  labs(title = "(a)",
       x = expression(paste("Year")),
       y = expression(paste("Temperature (°C)"))) +
  theme_bw()

interannual_mean_temp

interannual_mean_temp_mk = mk.test(central_tendencies$mean_mean_temp, alternative = "two.sided")
interannual_mean_temp_mk

interannual_delta_ratio = ggplot(data = central_tendencies) +
  geom_point(mapping = aes(x = group_5_year, y = yearly_delta_ratio)) +
  geom_line(mapping = aes(x = group_5_year, y = yearly_delta_ratio)) +
  geom_smooth(mapping = aes(x = group_5_year, y = yearly_delta_ratio), method='lm', linetype = "dashed", se=FALSE, color="black") +
  labs(title = "(b)",
       x = expression(paste("Year")),
       y = expression(paste("Retention Ratio, R"))) +
  theme_bw()

interannual_delta_ratio

interannual_delta_ratio_mk = mk.test(central_tendencies$yearly_delta_ratio, alternative = "two.sided")
interannual_delta_ratio_mk

figure10 = interannual_mean_temp / interannual_delta_ratio
ggsave("methods/99-data_visualization/ch2/figure10.png", figure10, width = 8, height = 6, dpi=600)

ggplot(data = central_tendencies) +
  geom_point(mapping = aes(x = group_5_year, y = mean_mean_temp)) +
  geom_smooth(mapping = aes(x = group_5_year, y = mean_mean_temp),
              method = "lm")

# Figure 11
ggplot() +
  # geom_boxplot(data=mcn_prd_pittag, aes(days_from_fall, speed, group = cut_interval(speed, 10)), outliers = FALSE, orientation="y", coef=0) +
  geom_line(data=mcn_prd_binned, aes(days_from_fall, mean_speed)) +
  geom_point(data=mcn_prd_binned, aes(days_from_fall, mean_speed)) +
  # add error bars
  geom_errorbar(data=mcn_prd_binned, aes(y=mean_speed, xmin=days_from_fall - (days_from_fall-p25_days_from_fall), xmax=days_from_fall + (p75_days_from_fall-days_from_fall)), width=0.1) +
  theme_bw()

swim_speed = ggplot(data=mcn_prd_binned, aes(days_from_fall, mean_speed)) +
  geom_pointrange(aes(xmin=p25_days_from_fall, xmax = p75_days_from_fall)) +
  geom_line() +
  labs(x = expression(paste("Fall Run Day")),
       y = expression(paste("Swim speed (", {day} ^ -1, ")"))) +
  theme_bw()

ggsave("methods/99-data_visualization/ch2/figure11.png", swim_speed, width = 4, height = 4, dpi=600)

table(cut_interval(0:100, 10))
