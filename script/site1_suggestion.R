library(tidyverse)
library(lubridate)
library(waterData)
library(ncdf4)
library(rnoaa)
library(plotly)
library(cowplot)

source("script/dailyFunctions.R")

# Define start and end date of the plot
startDate <- ymd_hms("2017-08-01 00:00:00")
endDate <- ymd_hms("2018-07-30 23:00:00")

#### Data carpentry ####
## Sensor water quality carpentry
wq <- read.csv("data/wq.csv", header= T) %>%
  filter(Site == 2) %>%
  select(Date, Temperature, Salinity)
wq$Date <- wq$Date %>% ymd_hms %>% round_date(unit="hour")

## Water discharge carpentry (dynamically updating local discharge file)
station = '02323500' 
stinfo  = siteInfo(station)
dis <- read_rds("data/dis.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', 
                      sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "data/dis.rds")
}
dis2 <- dis %>%
  filter(dates <= date(endDate) & dates >= date(startDate))

## Wind data carpentry
# Run once to build original wind.rds
# wind17 <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=2017)
# wind18 <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=2018)
# wind <- rbind(wind17$data, wind18$data) %>% dplyr::distinct()
# write_rds(wind, "data/wind.rds")

# Dynamically update wind table, if the endDate is 30 days away from the last date of
# our existing wind data, the code will pull the new data from NOAA and update our local
# data
d <- seq(startDate, endDate, by = "hour")
yr <- year(d) %>% unique
wind <- read_rds("data/wind.rds")
last.date <- max(wind$time) %>% date
if (last.date < date(endDate) - 30) {
  # In case the local wind.rds hasn't been updated for more than a year
  yr.missing <- seq(last.date, date(endDate), by = "day")
  yr.missing <- year(yr.missing) %>% unique
  for (yr in yr.missing) {
    wind.new <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=yr)
    wind <- rbind(wind, wind.new$data) %>% dplyr::distinct()
  }
  write_rds(wind, "data/wind.rds")  
}

# Shrink the wind table, and convert the format of time
wind$time <- ymd_hms(wind$time)
wind <- wind %>%
  filter(time >= startDate & time <= endDate) %>%
  select(time, WindSpeed = wind_spd)

## Create daily summary table for all four measure
# Create a new table of dates that has sequence of datetime between startDate & endDate
# spaced by one hour, and then join with the wq and wind table
stw <- data.frame(Date=d) %>%
  left_join(wq, by=c("Date" = "Date")) %>%
  left_join(wind, by=c("Date" = "time"))
stw$d2 <- date(stw$Date)

# Calculate mean, min and max daily Sal, Temp and wind speed
stw_summ <- stw %>%
  gather(key = "Measure", value = "Value", Salinity, Temperature, WindSpeed) %>%
  group_by(d2, Measure) %>%
  summarise(meanVal = dailyMean(Value, 0.75), 
            minVal = dailyMin(Value, 0.75), 
            maxVal = dailyMax(Value, 0.75)) %>%
  ungroup()

# Water discharge data manipulation, filter out the irrelevant dates
dis2 <- dis %>%
  mutate(Datetime = paste(dates, " 12:00:00") %>% ymd_hms()) %>%
  filter(Datetime >= startDate & Datetime <= endDate)

# Since water discharge doesn't have hourly value, we create a table that looks like
# stw_summ so we can bind the water discharge information to the stw_summ
# In this case we only have the meanVal
dis3 <- dis2 %>%
  mutate(meanVal=val/1000) %>%
  select(d2=dates, meanVal) %>%
  mutate(Measure = "Discharge")

stwd_summ <- bind_rows(stw_summ, dis3)

#### Plotting using plotly ####
# Color chart is to define what color you want for each Measure, in this case black for
# Discharge, blue for Salinity, Orange for Temperature and Pink for windspeed
colorChart <- data.frame(Measure = c("Discharge", "Salinity", "Temperature", "WindSpeed"),
                         color = c('rgba(0, 0, 0, 0.8)', 'rgba(0, 114, 178, 0.8)',
                                   'rgba(213, 94, 0, 0.8)', 'rgba(204, 121, 167, 0.8)'))

# This part is a bit more complicated...
# The function is to create a plotly plot for one of the measure.
# Each of the plotly plot has a line (for daily mean), and a ribbon (for min and max)
# The function also check the colorChart to assign the color associated with the measure
plot4 <- function(df) {
  name <- df$Measure %>% unique
  col <- colorChart$color[colorChart$Measure == name]
  p <- plot_ly(data = df, x = ~d2, y = ~meanVal) %>%
    add_lines(name = name, 
              line = list(color = col),
              hoverinfo = 'text',
              text = ~paste("Date :", d2, 
                            "</br></br> Mean", Measure, ":", round(meanVal, 2),
                            "</br> Min : ", round(minVal, 2), "Max :", round(maxVal, 2))) %>%
    add_ribbons(ymin = ~minVal, ymax = ~maxVal, 
                line = list(color = 'rgba(255, 255, 255, 0)'),
                fillcolor = col,
                opacity = 0.5,
                showlegend = F,
                hoverinfo = 'none')
  return(p)
}

# Take stwd_summ, based on the Measure, split the table into a list of four table
# so you get a list of (say k), then k$Discharge or k[[1]] will be a table with only Discharge
# information. Then using lapply function, it basically says, for each element of the list,
# apply the plot4 function. So after the lapply step, we get a list of four plotly plots.
# Finally, using the plotly function subplot, we arrange the four plots into one column, aligning
# according to the xaxis ('shareX = T')
combine.plot <- stwd_summ %>%
  split(stwd_summ$Measure) %>%
  lapply(plot4) %>%
  subplot(nrows = 4, shareX = T)

combine.plot