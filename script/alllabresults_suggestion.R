library(tidyverse)
library(lubridate)
library(waterData)

# Lab data carpentry
lab <- read.csv("rmd/data/discrete_measurement.csv")
colnames(lab) <- c("Site", "Date", "Time_org", "Time_UTC", "Sun_code", "Phosphorus", "Nitrogen", 
                   "Chlorophyll", "Secchi", "Color", "DO", "Temperature","Conductivity", "Salinity", 
                   "Depth", "Sensor_Type")

lab$Secchi<- (lab$Secchi/ 3.28)
Site.lvl <- c("6", "1", "5", "2", "4", "3")
lab <- lab %>%
  filter(Sensor_Type == "lakewatch") %>%
  filter(Site %in% Site.lvl)
lab$Date <- ymd(lab$Date)

lab$Site <- factor(lab$Site, levels = Site.lvl) # Didn't need to create extra facet column
lab$Site2 <- sapply(as.character(lab$Site), function (x) switch(x, "1"="A", "2"="B", "3"="C", 
                                                                "4"="C", "5"="B", "6"="A"))

# Water discharge (Dynamic updates like other codes)
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
colnames(dis) <- c("staid", "Discharge", "Date", "qualcode")

# Define start and end date of the plot
# Preserving the date time combo so that it's rich and flexible to use accross document
startDate <- ymd_hms("2017-08-01 00:00:00") %>% date
endDate <- ymd_hms("2018-07-30 23:00:00") %>% date

# Filter discharge data
dis2 <- dis %>%
  filter(Date >= startDate & Date <= endDate)

# Basic templates for all the graphs
# Since all graphs share some of the common elements, we can first build it up first
# e.g. ggplot, theme and etc
p <- ggplot(data=lab, aes(x=Date)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%Y/%m",
               expand = c(0, 0),
               limits = c(startDate, endDate)) +
  theme_bw()+
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title =element_text(size=30, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Site2, ncol=1)

# Now use the "template and add other stuff (Note: my personal preference to have geoms go first)
p1 <- p + geom_line(data=dis2, aes(y=Discharge/160), color= "cornflowerblue", size=2, alpha=0.8) +
  geom_point(aes(y= Phosphorus, shape=Site, color=Site), size=3) +
  scale_y_continuous(sec.axis = sec_axis(~.*160, name = "River Discharge (cfs)"), limits=c(0,125)) +
  ggtitle("Phosphorus") +
  ylab ("Phosphorus(?g/L)")
p1
