startDate <- ymd_hms("2017-08-01 00:00:00")
endDate <- ymd_hms("2018-07-30 23:00:00")

d <- seq(startDate, endDate, by = "hour")

df <- expand(wq, Site, Date=d) %>%
  left_join(wq, by=c("Site" = "Site", "Date" = "Date")) %>%
  select(Site, Date, Salinity, Temperature)
df$d2 <- date(df$Date)

df1 <- df %>%
  gather(key = "Measure", value = "Value", Salinity, Temperature) %>%
  group_by(Site, d2, Measure) %>%
  summarise(meanVal = dailyMean(Value, 0.2), 
            minVal = dailyMin(Value, 0.2), 
            maxVal = dailyMax(Value, 0.2))

dis2 <- dis %>%
  mutate(Datetime = paste(Date, " 12:00:00") %>% ymd_hms()) %>%
  filter(Datetime >= startDate & Datetime <= endDate)

p <- ggplot() +
  geom_ribbon(data=dis2, aes(x=Date, ymax=Values/1000 - 20, ymin=-20), 
              alpha=0.5) +
  geom_line(data=df1, aes(x=d2, y=meanVal, color=Measure)) +
  geom_ribbon(data=df1, aes(x=d2, ymax=maxVal, ymin=minVal, fill=Measure), 
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