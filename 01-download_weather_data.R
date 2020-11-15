
library(tidyverse)
library(rnoaa)
library(lubridate)


# download weather data ---------------------------------------------------

# need key to use rnoaa
options(noaakey = "mMwtILsLTqlXCnQbELrXHkuKLFrCZEir")

# there are download restrictions from NOAA; need to download by year in a loop
years <- seq(from = 1980, to = 2020, by = 1)

# create lists of start and end dates for each year
years <- years %>%
  as_tibble() %>%
  rename(year = value) %>%
  mutate(
    startDate = paste(year, '01-01', sep = '-'),
    endDate = paste(year, '12-31', sep = '-')
  )
years

# vector of start and end dates for for-loop
startDates <- years %>%
  pull(startDate)
startDates

endDates <- years %>%
  pull(endDate)
endDates

# to store output of for loop
tempsList <- list()

# loop through each year
for (i in seq_along(startDates)) {
  
  # get daily minimum temp from Boise airport
  temps <- ncdc(
    datasetid='GHCND',
    stationid = 'GHCND:USW00024131', # boise
    datatypeid = c('TMAX', 'TMIN'),
    startdate = startDates[i],
    enddate = endDates[i],
    limit = 1000
  )
  
  # turn into tibble, convert 1/10s of C to C
  temps <- temps$data %>%
    as_tibble() %>%
    mutate(
      value = value/10,
      date = as.Date(str_sub(date, start = 1, end = 10))
    )
  
  # store each year of data here
  tempsList[[i]] <- temps
  
}

# turn into dataframe, extract year and month
boise_temps <- tempsList %>%
  bind_rows() %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  select(-matches('fl')) # remove flag variables, data are fine
boise_temps

# put into wide format and create tavg variable
boise_temps <- boise_temps %>%
  pivot_wider(values_from = value, names_from = datatype) %>%
  rename(
    tmax = TMAX,
    tmin = TMIN
  ) %>%
  mutate(tavg = (tmax + tmin) / 2) %>%
  drop_na() # remove any NAs
boise_temps

# prepare dates for climwin-needed format
boise_temps <- boise_temps %>%
  mutate(
    date_string = (paste(
      format(date,format = "%d"),
      format(date,format = "%m"),
      format(date,format = "%Y"),
      sep = "/")
    )
  )
boise_temps

# save weather data
boise_temps %>%
  write_rds(here::here('data/boise_temps.rds'))
