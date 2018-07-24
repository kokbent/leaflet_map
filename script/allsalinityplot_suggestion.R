source("script/dailyFunctions.R")

# Sensor water quality carpentry
wq <- read.csv("data/wq.csv", header= T) %>%
  filter(Site != 0)
wq$Date <- wq$Date %>% ymd_hms
wq$Site <- factor(wq$Site, levels = c("6", "1", "7", "5", "2", "8","4", "3", "9"))

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

# Define start and end date of the plot
startDate <- ymd_hms("2017-08-01 00:00:00")
endDate <- ymd_hms("2018-07-30 23:00:00")

d <- seq(startDate, endDate, by = "hour")

sal_temp <- expand(wq, Site, Date=d) %>%
  left_join(wq, by=c("Site" = "Site", "Date" = "Date")) %>%
  select(Site, Date, Salinity, Temperature)
sal_temp$d2 <- date(sal_temp$Date)

sal_temp_summ <- sal_temp %>%
  gather(key = "Measure", value = "Value", Salinity, Temperature) %>%
  group_by(Site, d2, Measure) %>%
  summarise(meanVal = dailyMean(Value, 0.2), 
            minVal = dailyMin(Value, 0.2), 
            maxVal = dailyMax(Value, 0.2)) %>%
  ungroup()

dis2 <- dis %>%
  mutate(Datetime = paste(dates, " 12:00:00") %>% ymd_hms()) %>%
  filter(Datetime >= startDate & Datetime <= endDate)

p <- ggplot() +
  geom_ribbon(data=dis2, aes(x=dates, ymax=val/1000 - 20, ymin=-20), 
              alpha=0.5) +
  geom_line(data=sal_temp_summ, aes(x=d2, y=meanVal, color=Measure)) +
  geom_ribbon(data=sal_temp_summ, aes(x=d2, ymax=maxVal, ymin=minVal, fill=Measure), 
              alpha=0.5) +
  facet_wrap(~ Site)

p <- p + 
  scale_y_continuous(sec.axis = sec_axis(~(.+20), 
                                         name = "River Discharge ('000 cfs)",
                                         breaks = seq(0, 20, 10)),
                     limits=c(-20,40), breaks = seq(0, 40, 10)) +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%Y/%m"),
    expand = c(0, 0))
  

p + theme(legend.position=("top"),
          panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=13,face="bold"),
          plot.title =element_text(size=13, face='bold'),
          axis.text.x = element_text(angle = 45, hjust = 1))

