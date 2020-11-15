
# load packages
library(tidyverse)
library(readxl)
library(tidybayes)
library(patchwork)
library(climwin)
library(lubridate)
library(brms)
library(ggrepel)
library(wesanderson)

# set theme, call source code for MAE/RMSE
theme_set(theme_bw() + theme(panel.grid.minor = element_blank()))
source(here::here('source/error_functions.R'))

# load temperature and laydate data ---------------------------------------------

boise_temps <- read_rds(here::here('data/boise_temps.rds'))

laydates <- read_excel(here::here('data/nestboxes.xlsx'))

# laydates need cleaning
laydates <- laydates %>%
  janitor::clean_names() %>%
  select(year, second_nest_attempt, julian_lay) %>%
  filter(is.na(second_nest_attempt) | second_nest_attempt == 0) %>%
  select(-second_nest_attempt) %>%
  drop_na()
laydates


# plot raw laydates -------------------------------------------------------

# individual years
p1 <- laydates %>%
  ggplot() +
  geom_jitter(aes(year, julian_lay), alpha = 0.5, width = 0.1, color = 'grey') +
  stat_summary(aes(year, julian_lay), fun = "mean", colour = "red", size = 2, geom = "point") +
  stat_summary(aes(year, julian_lay), fun.data = "mean_cl_boot", colour = "red", size = 0.8, geom = "errorbar", width = 0.2) +
  labs(
    x = 'Year',
    y = 'Laydate (day-of-year)'
  )
p1

# plot average across all years
p2 <- laydates %>%
  mutate(all = 'All years') %>%
  ggplot() +
  geom_jitter(aes(all, julian_lay), alpha = 0.5, width = 0.1, color = 'grey') +
  stat_summary(aes(all, julian_lay), fun = "mean", colour = "red", size = 2, geom = "point") +
  stat_summary(aes(all, julian_lay), fun.data = "mean_cl_boot", colour = "red", size = 0.8, geom = "errorbar", width = 0.2) +
  theme(axis.title = element_blank(), axis.text.y = element_blank())
p2

# combine plots
p1 + p2 + plot_layout(widths = c(2, 0.5))
ggsave(here::here('figures/raw_data.png'), width = 6, height = 4, units = 'in')


# prepare data for climwin ------------------------------------------------

# dates need to be in string format (dd-mm-yyyy) for climwin
bird_data <- laydates %>%
  group_by(year) %>%
  summarise(
    julian_lay = mean(julian_lay) # average laydate per year
  ) %>%
  mutate(
    date = as.Date(strptime(paste(year, julian_lay), format = "%Y %j")),
    date_string = (paste(
      format(date, format = "%d"),
      format(date, format = "%m"),
      format(date, format = "%Y"),
      sep = "/")
    )
  )
bird_data

# pull out average laydates, climate dates, and temperatures
bdate <- bird_data$date_string
cdate <- boise_temps$date_string
tavg <- boise_temps$tavg

# set a baseline (null) model for average laydate (needed by climwin)
baseline = lm(julian_lay ~ 1, data = bird_data)

# calculate average across all years
mean_julian_day <- bird_data %>% 
  summarise(mean = mean(julian_lay)) %>% 
  round() %>%
  pull()

# the average calendar date is 15 April
mean_date <- as.Date(mean_julian_day - 1, origin = "2018-01-01")


# run climwin -------------------------------------------------------------

# sliding window analysis
Win = slidingwin( 
  exclude = c(14, -1), # exclude windows less than 7 days long
  cdate = cdate, 
  bdate = bdate,
  baseline = baseline,
  xvar = list(temp = tavg),
  type = "absolute", 
  range = c(180, 15), # look back X days from the ref day
  stat = c('mean'),
  func = c("lin"),
  refday = c(15, 4), # reference day is 15 April
  cinterval = "day",
  cmissing = 'method1'
) 

# look at plots for top model
i = 1
obj = Win

# plot best model
plotall(
  dataset = obj[[1]]$Dataset,
  bestmodel = obj[[1]]$BestModel, 
  bestmodeldata = obj[[1]]$BestModelData,
  cw1 = 0.95,
  cw2 = 0.50,
  cw3 = 0.25,
  arrow = F
)

# pull out AIC-weighted window open and close dates 
# i.e., dates averaged for models within 2AIC of top model
window_span <- head(Win[[1]]$Dataset) %>%
  as_tibble() %>%
  filter(deltaAICc <= min(deltaAICc + 2)) %>%
  summarise(
    open = round(weighted.mean(WindowOpen, ModWeight), 0),
    close = round(weighted.mean(WindowClose, ModWeight), 0)
  )
window_span

# plot open and close dates (number of days prior to 15 April)
head(Win[[1]]$Dataset) %>%
  as_tibble() %>%
  select(Open = WindowOpen, Close = WindowClose) %>%
  gather(variable, doy) %>%
  ggplot() +
  geom_boxplot(aes(doy, variable), fill = 'gainsboro') +
  labs(
    x = 'Days before Apr-15',
    y = 'Climate window'
  )
ggsave(here::here('figures/window.png'), width = 5, height = 4, units = 'in')

# in calendar format
head(Win[[1]]$Dataset) %>%
  as_tibble() %>%
  select(Open = WindowOpen, Close = WindowClose) %>%
  gather(variable, doy) %>%
  mutate(cal_date = mean_date - days(doy)) %>%
  ggplot() +
  geom_boxplot(aes(cal_date, variable), fill = 'gainsboro') +
  labs(
    x = 'Date',
    y = 'Climate window'
  )
ggsave(here::here('figures/window_cal.png'), width = 5, height = 4, units = 'in')

# get the open/close julian dates to take annual tavg averages
close <- yday(mean_date - window_span$close)
open <- yday(mean_date - window_span$open)

# average temperature within climate window
boise_temps_mean <- boise_temps %>%
  mutate(
    day = yday(date),
    year = year(date)
  ) %>%
  group_by(year) %>%
  filter(day >= open & day <= close) %>%
  summarise(tavg = mean(tavg))
boise_temps_mean

# join average annual temperatures to average annual laydates
bird_data_final <- bird_data %>%
  left_join(., boise_temps_mean) %>%
  mutate(cyear = year - mean(year))
bird_data_final


# model change in laydate with change in temperature ---------------------------

# laydate ~ tavg
m1 <- brm(
  julian_lay ~ tavg, 
  data = bird_data_final, 
  chains = 4,
  cores = 4,
  seed = 450
  )

# inspect model
plot(m1)
pp_check(m1, nsamples = 50)
summary(m1)


# plotting ----------------------------------------------------------------

# plot MAE, RMSE to show how well the model performs
bird_data_final %>%
  add_fitted_draws(., m1) %>%
  ungroup() %>%
  select(year, julian_lay, tavg, .draw, .value) %>%
  distinct() %>%
  group_by(.draw) %>%
  mutate(
    error = julian_lay - .value,
    mae = mae(error),
    rmse = rmse(error)
  ) %>%
  ungroup() %>%
  select(mae, rmse) %>%
  gather(metric, value) %>%
  ggplot() +
  geom_violin(aes(y = metric, x = value, fill = metric), trim = TRUE, alpha = 0.8) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  guides(fill = FALSE) +
  labs(x = 'Days') +
  coord_cartesian(xlim = c(0, 5))
ggsave(here::here('figures/violin_error.png'), width = 4, height = 3, units = 'in')

# plot observed vs. predicted laydates from the linear model
fitted(m1) %>%
  data.frame() %>%
  bind_cols(bird_data_final) %>%
  ggplot(aes(x = julian_lay, y = Estimate)) +
  geom_abline(linetype = 2, color = "grey50", size = 0.7) +
  geom_point(size = 1.5, color = "firebrick3", alpha = 3/4) +
  geom_linerange(aes(ymin = Q2.5, ymax = Q97.5), size = 1/2, color = "firebrick3") +
  geom_text_repel(aes(label = year), size = 2.5) +
  labs(
    x = "Observed mean laydate (day-of-year)", 
    y = "Predicted mean laydate  (day-of-year)"
    ) +
  theme(panel.grid = element_blank()) +
  coord_cartesian(xlim = c(96, 112), ylim = c(96, 112))
ggsave(here::here('figures/predictions.png'), width = 4, height = 3, units = 'in')

# create new data for predicting laydates by temperature
tavg_seq <- tibble(
  tavg = seq(min(bird_data_final$tavg), max(bird_data_final$tavg), length.out = 100)
)

# fit laydate model to new data
pred_lay <- fitted(m1, newdata = tavg_seq) %>%
  data.frame() %>%
  bind_cols(tavg_seq)
pred_lay

# plot mean laydate by climate window temperature
# along with annual average laydates (labeled by year)
bird_data_final %>%
  ggplot(aes(x = tavg)) +
  geom_ribbon(data = pred_lay, aes(ymin = Q2.5, ymax = Q97.5), fill = "grey83", alpha = 3/4) +
  geom_line(data = pred_lay, aes(y = Estimate), color = "navyblue", size = 1) +
  geom_point(aes(y = julian_lay), color = "navyblue", size = 1.5, alpha = 2/3) +
  guides(color = FALSE) +
  scale_color_manual(values = c('navyblue', 'firebrick')) +
  geom_text_repel(aes(label = year,  y = julian_lay)) +
  labs(
    x = expression("Climate window temperature " ( degree*C)),
    y = 'Mean laydate (day-of-year)'
  )
ggsave(here::here('figures/ribbon.png'), width = 4, height = 3, units = 'in')


# out of sample prediction ------------------------------------------------

# how well does the model predict for years of data that are currently unavailable?
pred_data <- fitted(m1, newdata = boise_temps_mean) %>%
  data.frame() %>%
  bind_cols(boise_temps_mean) %>%
  filter(year >= 2008) %>%
  select(year, Estimate, Q2.5, Q97.5) %>%
  mutate(type = 'Predicted')
pred_data

# prepare raw data (take averages and bootstrapped CIs)
avg_lay <- laydates %>% 
  group_by(year) %>% 
  summarise(
    ci = list(mean_cl_boot(julian_lay) %>% rename(Estimate = y, Q2.5 = ymin, Q97.5 = ymax)
    )
  ) %>% 
  unnest() %>%
  mutate(type = 'Observed')
avg_lay

# bind with predictions
laydates <- bind_rows(pred_data, avg_lay)

# dodge width for plotting
pos <- position_dodge(width = 0.2)

# plot raw averages vs. model predictions (including unavailable years)
laydates %>%
  ggplot() +
  annotate(geom = 'rect', xmin = 2018.8, xmax = 2025, ymin = 80, ymax = 125, fill = 'grey', alpha = 0.3) +
  geom_line(aes(year, Estimate, color = type, linetype = type, group = type), position = pos) +
  geom_point(aes(year, Estimate, color = type, group = type), size = 2, alpha = 3/4, position = pos) +
  geom_linerange(aes(ymin = Q2.5, ymax = Q97.5, x = year, color = type, group = type), size = 1/2, position = pos) +
  labs(x = "Year", y = "Laydate (day-of-year)") +
  scale_color_manual(values = wes_palette("Moonrise2"), name = NULL) +
  scale_linetype_manual(values = c(3, 2), name = NULL) +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  coord_cartesian(ylim = c(88,120), xlim = c(2008, 2020)) +
  theme(legend.position = 'top') 
ggsave(here::here('figures/predictions_2020.png'), width = 5, height = 3, units = 'in')
