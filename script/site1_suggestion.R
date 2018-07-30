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

# Sensor water quality carpentry
wq <- read.csv("data/wq.csv", header= T) %>%
  filter(Site == 1) %>%
  select(Date, Temperature, Salinity)
wq$Date <- wq$Date %>% ymd_hms %>% round_date(unit="hour")

# Water discharge carpentry (dynamically updating local discharge file)
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

# Wind data carpentry
d <- seq(startDate, endDate, by = "hour")
yr <- year(d) %>% unique
wind <- read_rds("data/wind.rds") %>% select(time, WindSpeed=wind_spd)
if (max(wind$time) < endDate) {
  # Update
}
# wind <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=yr)

# Create sequence of dates (so that NA is recognized by geom_line) and build a table of
# salinity and temperature (remove other irrelevant ones)
stw <- data.frame(Date=d) %>%
  left_join(wq, by=c("Date" = "Date")) %>%
  left_join(wind, by=c("Date" = "time"))
stw$d2 <- date(stw$Date)

# Calculate mean, min and max daily Sal and Temp
# Note: tidyr's gather is in use here, by converting the "wide" table to
# "long" one, we can use the aes(color=Measure) in ggplot
# Note2: I've created functions in dailyFunction.R to make calculating daily mean, min,
# max easy. The function allows certain proportions of NA here. In this example, 0.75
# of the daily numbers can be NA, and we'll still calculate the meant, min, max
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

dis3 <- dis2 %>%
  mutate(meanVal=val/1000) %>%
  select(d2=dates, meanVal) %>%
  mutate(Measure = "Discharge")

stwd_summ <- bind_rows(stw_summ, dis3)
st_summ <- stwd_summ %>% filter(Measure %in% c("Temperature", "Salinity"))
wd_summ <- stwd_summ %>% filter(Measure %in% c("WindSpeed", "Discharge"))

p1 <- ggplot() +
  geom_line(data=st_summ, aes(x=d2, y=meanVal, color=Measure)) +
  geom_ribbon(data=st_summ, aes(x=d2, ymax=maxVal, ymin=minVal, fill=Measure),
              alpha=0.5)

p1

p2 <- ggplot() +
  geom_line(data=wd_summ, aes(x=d2, y=meanVal, color=Measure)) +
  geom_ribbon(data=wd_summ, aes(x=d2, ymax=maxVal, ymin=minVal, fill=Measure),
              alpha=0.5)
p2

plot4 <- function(df) {
  name <- df$Measure %>% unique
  p <- plot_ly(data = df, x = ~d2, y = ~meanVal) %>%
    add_lines(name = name) %>%
    add_ribbons(ymin = ~minVal, ymax = ~maxVal)
  return(p)
}

k <- stwd_summ %>%
  split(stwd_summ$Measure) %>%
  lapply(plot4) %>%
  subplot(nrows = 4, shareX = T)

p <- stwd_summ %>% filter(Measure != "Discharge") %>%
  ggplot() +
  geom_line(aes(x=d2, y=meanVal, color=Measure)) +
  geom_ribbon(aes(x=d2, ymin=minVal, ymax=maxVal), fill="grey", alpha=0.3) +
  facet_wrap(~ Measure, ncol = 1)

p <- ggplotly()
p
# Second step is to specify the scaling of x and y axes, by using the breaks argument
# we can control what to display on the y main and secondary axes, e.g. 0 to 40 for main,
# and 0 to 20000 for secondary
p <- p + 
  scale_y_continuous(name = "Temperature / Salinity (deg C / ppt)", 
                     limits=c(-20,40), 
                     breaks = seq(0, 40, 10),
                     sec.axis = sec_axis(~(.+20), 
                                         name = "River Discharge ('000 cfs)",
                                         breaks = seq(0, 20, 10))) +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y/%m", expand = c(0, 0))

# Final step is to specify the theme
p <- p + theme(legend.position=("top"),
          panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=13,face="bold"),
          plot.title =element_text(size=13, face='bold'),
          axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)
